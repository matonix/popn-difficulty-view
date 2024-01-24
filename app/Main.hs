{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (...), (^.), (^..), (^?))
import Data.Aeson (FromJSON, decodeFileStrict)
import qualified Data.ByteString.Lazy as L
import Data.Csv
  ( DefaultOrdered,
    Options,
    ToField (toField),
    ToNamedRecord (toNamedRecord),
    defaultOptions,
    encodeDefaultOrderedByName,
    fieldLabelModifier,
    genericToNamedRecord,
  )
import Data.Either (fromRight)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
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
import Text.Pretty.Simple (pPrint)
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

type Clear = Text

newtype FloatDiff = FloatDiff Double
  deriving (Show, Generic)

instance ToField FloatDiff where
  toField (FloatDiff f) = toField @String (printf "%.3f" f)

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
    attribute :: Attribute,
    clear :: Clear
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

-- 属性jsonファイルの読み込み
pomizikuJSONLoad :: IO (HashMap Level (HashMap Title Attribute))
pomizikuJSONLoad = do
  let file = "data/ポミジク属性メモ.json"
  Just json <- decodeFileStrict @(HashMap Text (HashMap Text Text)) file
  let hmap = M.mapKeys (fst . fromRight (0 :: Int, T.empty) . T.decimal) json
  return hmap

-- 出力用データ構造の定義
data Output = Output
  { _lv :: Level,
    -- version2 :: Version,
    _genre :: Text,
    _title :: Title,
    -- minBpm :: Maybe Int,
    -- maxBpm :: Maybe Int,
    -- time2 :: Time,
    -- notes2 :: Notes,
    _float :: FloatDiff,
    _diff :: Difficulty,
    _attr :: Attribute,
    _clear :: Clear
  }
  deriving (Show, Generic)

instance ToNamedRecord Output

instance DefaultOrdered Output

songToOutput :: Song -> Output
songToOutput (Song {..}) =
  Output
    { _lv = level,
      -- version2 = version,
      _genre = "<a href=\"https://popn.wiki/" <> path <> "\">" <> genre <> "</a>",
      _title = title,
      -- minBpm = let Bpm b = bpm in fmap fst b,
      -- maxBpm = let Bpm b = bpm in fmap snd b,
      -- time2 = time,
      -- notes2 = notes,
      _float = let Difficulty d = difficulty in FloatDiff $ fromIntegral level + maybe 0.0 (\(_, m, _) -> m) d,
      _diff = difficulty,
      _attr = attribute,
      _clear = clear
    }

type Result = (Int, Int, Int)

type ResultByDiff = (Result, Result, Result, Result)

type Score = (Text, Text, Text, Int, ResultByDiff)

data ScoresJSON = ScoresJSON
  { profile :: [Text],
    info :: [Text],
    scores :: [Score]
  }
  deriving (Show, Generic)

instance FromJSON ScoresJSON

-- スコアjsonファイルの読み込み
scoresJSONLoad :: IO ScoresJSON
scoresJSONLoad = do
  let file = "data/localStorage.json"
  -- (Int, Int, Int) は (クリアランプ、スコアランプ、スコア)で、(4 = ◯クリア、6 = Bランク、スコア値) とか (-1 = 未プレイ, -1 = 未プレイ, 0) とか。
  Just json <- decodeFileStrict @ScoresJSON file
  return json

scoresToClearMap :: ScoresJSON -> HashMap Genre Clear
scoresToClearMap ScoresJSON {..} = M.fromList . concat . flip map scores $
  \(_, genreText, _, _, (e, n, h, ex)) ->
    [ (genreText `T.append` "(E)", getClear e),
      (genreText `T.append` "(N)", getClear n),
      (genreText `T.append` "(H)", getClear h),
      (genreText `T.append` "(EX)", getClear ex)
    ]
  where
    getClear (clear, _, _)
      | clear == -1 = ""
      | clear >= 4 = "y"
      | otherwise = "n"

main :: IO ()
main = do
  allSongs <- retriveWikiLevels
  attrMap <- pomizikuJSONLoad
  clearMap <- scoresToClearMap <$> scoresJSONLoad
  let attrAllSongs =
        flip map allSongs $ \song -> fromMaybe song $ do
              titleMap <- attrMap M.!? level song
              attribute <- titleMap M.!? title song
              return $ song {attribute = attribute}
  let clearAttrAllSongs =
        flip map attrAllSongs $ \song -> fromMaybe song $ do
              clear <- clearMap M.!? genre song
              return $ song {clear = clear}
  let csvData = encodeDefaultOrderedByName $ map songToOutput clearAttrAllSongs
  L.writeFile "site/view.csv" csvData
