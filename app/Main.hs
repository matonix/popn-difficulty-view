{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (...), (^.), (^..), (^?))
import qualified Data.ByteString.Lazy as L
import Data.Csv
  ( DefaultOrdered,
    ToField (toField),
    ToNamedRecord,
    encodeDefaultOrderedByName,
  )
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp)
import System.Directory (doesFileExist)
import Text.HTML.DOM (parseLBS)
import Text.Printf (printf)
import Text.XML (Element)
import Text.XML.Lens
  ( Element,
    attr,
    attributeIs,
    named,
    root,
    text,
    (...),
  )
import Data.Aeson (decodeFileStrict)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Text.Pretty.Simple (pPrint)
import Data.Maybe (fromMaybe)

-- ファイルが存在すれば読み込み、存在しなければダウンロードして保存
fetchAndSaveUrl :: (String, String) -> Int -> IO L.ByteString
fetchAndSaveUrl (fileName, url) delay = do
  let filePath = "data/" ++ fileName
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      putStrLn $ "Reading from file: " ++ filePath
      L.readFile filePath
    else do
      putStrLn $ "Downloading: " ++ url
      content <- simpleHttp url
      L.writeFile filePath content
      putStrLn $ "Downloaded and saved: " ++ url
      threadDelay (delay * 1000000) -- ダウンロード後にのみ待機
      return content

fetchUrlsEvery :: [(String, String)] -> Int -> IO [L.ByteString]
fetchUrlsEvery fileUrlPairs delay = mapM (`fetchAndSaveUrl` delay) fileUrlPairs

parseTable :: L.ByteString -> [Song]
parseTable content = tableToSongs level header body
  where
    doc = parseLBS content
    level = doc ^. root . named "html" ... named "head" ... named "title" . text & parseLevel
      where
        parseLevel = fst . fromRight (0, T.empty) . T.decimal . T.drop 2 . T.take 4
    Just table = doc ^? root . named "html" ... named "body" ... attributeIs "id" "dokuwiki__site" ... attributeIs "id" "dokuwiki__top" ... named "div" ... named "main" ... named "div" ... attributeIs "class" "page group" ... named "div" ... named "div" ... named "div" ... named "table"
    header = table ^.. named "table" ... named "thead" ... named "tr" ... text
    trs = table ^.. named "table" ... named "tr"
    body = map parseRow trs
      where
        parseRow tr = tr ^.. named "tr" ... named "td"

-- cols == 8 のときと7のときがある
tableToSongs :: Int -> [Text] -> [[Element]] -> [Song]
tableToSongs level header body
  | length header == 8 = map parse8col body
  | length header == 7 = map parse7col body
  | otherwise = undefined
  where
    parse8col [version, copyright, genre, title, bpm, time, notes, difficulty] =
      Song
        level
        (parseVersion version)
        (parseLegend version)
        (parseCopyright copyright)
        (parseGenre genre)
        (parsePath genre)
        (parseTitle title)
        (parseBpm bpm)
        (parseTime time)
        (parseNotes notes)
        (parseDifficulty difficulty)
        T.empty
    parse8col e = error $ show e ++ show (length e)
    parse7col [version, genre, title, bpm, time, notes, difficulty] =
      Song
        level
        (parseVersion version)
        (parseLegend version)
        T.empty
        (parseGenre genre)
        (parsePath genre)
        (parseTitle title)
        (parseBpm bpm)
        (parseTime time)
        (parseNotes notes)
        (parseDifficulty difficulty)
        T.empty
    -- 今は単純に展開している部分が多いけど、それぞれ適切な値にパーズする予定
    parseVersion = parseTdOptSpanText
    parseLegend e =
      e ^. named "td" ... attr "style" & T.drop 6 & \text ->
        if
          | "red" `T.isPrefixOf` text -> LegendRed
          | "blue" `T.isPrefixOf` text -> LegendBlue
          | "gray" `T.isPrefixOf` text -> LegendGray
          | otherwise -> LegendNothing
    parseCopyright = parseTdOptSpanText
    parseGenre e = case e ^? named "td" ... named "a" . text of
      Just genre -> genre
      Nothing -> e ^. named "td" ... named "span" ... named "a" . text
    parsePath e = case e ^? named "td" ... attr "href" of
      Just path -> path
      Nothing -> e ^. named "td" ... named "span" ... attr "href"
    parseTitle e = case e ^? named "td" ... named "abbr" of
      Just _ -> e ^.. named "td" ... text & T.unwords -- KISS KISS KISS
      Nothing -> parseTdOptSpanText e
    parseBpm = Bpm . rightToMaybe . readBpm . parseTdText
      where
        readBpm t = do
          (minBpm, rest) <- T.decimal t
          if not (T.null rest) && T.head rest == '～'
            then do
              (maxBpm, _) <- T.decimal $ T.drop 1 rest
              return (minBpm, maxBpm)
            else return (minBpm, minBpm)
    parseTime = Time . rightToMaybe . readTime . parseTdText
      where
        readTime t = do
          (minute, rest) <- T.decimal t
          (second, _) <- T.decimal $ T.drop 1 rest
          return (minute, second)
    parseNotes = fst . fromRight (0, T.empty) . T.decimal @Int . parseTdText
    parseDifficulty = Difficulty . rightToMaybe . readDifficulty . parseTdText
      where
        readDifficulty t = do
          let (diftag, rest) = T.break (== '(') t
          (mean, rest2) <- T.double $ T.drop 1 rest
          if not (T.null rest2) && T.head rest2 == '±'
            then do
              (std, rest3) <- T.double $ T.drop 1 rest2
              return (diftag, mean, std)
            else return (diftag, mean, 0.0)
    parseTdText e = e ^. named "td" . text
    parseTdOptSpanText e = case e ^? named "td" . text of
      Just t -> t
      Nothing -> e ^. named "td" ... named "span" . text

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

type Level = Int

type Version = Text

data Legend = LegendRed | LegendBlue | LegendGray | LegendNothing
  deriving (Show, Generic)

instance ToField Legend where
  toField LegendRed = "red"
  toField LegendBlue = "blue"
  toField LegendGray = "gray"
  toField LegendNothing = ""

type Copyright = Text

type Genre = Text

type Path = Text

type Title = Text

newtype Bpm = Bpm (Maybe (Int, Int))
  deriving (Show, Generic)

instance ToField Bpm where
  toField (Bpm Nothing) = ""
  toField (Bpm (Just (minBpm, maxBpm))) =
    if minBpm == maxBpm
      then toField minBpm
      else toField minBpm <> T.encodeUtf8 "～" <> toField maxBpm

newtype Time = Time (Maybe (Int, Int))
  deriving (Show, Generic)

instance ToField Time where
  toField (Time Nothing) = ""
  toField (Time (Just (minute, second))) = toField minute <> ":" <> toField @String (printf "%02d" second)

type Notes = Int

newtype Difficulty = Difficulty (Maybe (Text, Double, Double))
  deriving (Show, Generic)

instance ToField Difficulty where
  toField (Difficulty Nothing) = ""
  toField (Difficulty (Just (text, mean, std))) = toField text <> "(" <> toField @String (printf "%+.3f" mean) <> T.encodeUtf8 "±" <> toField @String (printf "%.1f" std) <> ")"

type Attribute = Text

-- データ構造の定義
data Song = Song
  { level :: Level,
    version :: Version,
    legend :: Legend,
    copyright :: Copyright,
    genre :: Genre,
    path :: Path,
    title :: Title,
    bpm :: Bpm,
    time :: Time,
    notes :: Notes,
    difficulty :: Difficulty,
    attribute :: Attribute
  }
  deriving (Show, Generic)

instance ToNamedRecord Song

instance DefaultOrdered Song

retriveWikiLevels :: IO [Song]
retriveWikiLevels = do
  let levels = map show [29 .. 50]
  let baseURL = "https://popn.wiki/%E9%9B%A3%E6%98%93%E5%BA%A6%E8%A1%A8/lv"
  let urls = map (baseURL ++) levels
  let fileUrlPairs = zip levels urls
  contents <- fetchUrlsEvery fileUrlPairs 10
  let songs = map parseTable contents
  let all_songs = concat songs
  return all_songs

-- jsonファイルの読み込み
pomizikuJSONLoad :: IO (HashMap Level (HashMap Title Attribute))
pomizikuJSONLoad = do
  let file = "data/ポミジク属性メモ.json"
  Just json <- decodeFileStrict @(HashMap Text (HashMap Text Text)) file
  let hmap = M.mapKeys (fst . fromRight (0 :: Int, T.empty) . T.decimal) json
  return hmap


-- 出力用データ構造の定義
data Song2 = Song2
  { level2 :: Level,
    version2 :: Version,
    linkedGenre :: Text,
    title2 :: Title,
    minBpm :: Maybe Int,
    maxBpm :: Maybe Int,
    time2 :: Time,
    notes2 :: Notes,
    levelDiff :: Double,
    difficulty2 :: Difficulty,
    attribute2 :: Attribute
  }
  deriving (Show, Generic)

instance ToNamedRecord Song2

instance DefaultOrdered Song2

songToSong2 :: Song -> Song2
songToSong2 (Song {..}) = Song2 {
  level2 = level,
  version2 = version,
  linkedGenre = "<a href=\"https://popn.wiki/" <> path <> "\">" <> genre <> "</a>",
  title2 = title,
  minBpm = let Bpm b = bpm in fmap fst b,
  maxBpm = let Bpm b = bpm in fmap snd b,
  time2 = time,
  notes2 = notes,
  levelDiff = let Difficulty d = difficulty in  fromIntegral level + maybe 0.0 (\(_, m, _) -> m) d,
  difficulty2 = difficulty,
  attribute2 = attribute
}

main :: IO ()
main = do
  all_songs <- retriveWikiLevels
  hmap <- pomizikuJSONLoad
  let attr_all_songs = map (songToSong2 . \song -> fromMaybe song $ do
        titles <- hmap M.!? level song
        attribute <- titles M.!? title song
        return $ song { attribute = attribute }
        ) all_songs
  -- print attr_all_songs
  let csvData = encodeDefaultOrderedByName attr_all_songs
  L.writeFile "data/view.csv" csvData

