-- https://sirocco.hatenadiary.org/entry/20110619/1308445601
module HanZen where

import Data.Char
import Numeric

han = map chr [0x21..0x7e]

zen = map chr [0xff01..0xff5e]

zenSpace = '　'

toZenChar :: Char -> Char
toZenChar c = chr (ord c + 0xfee0)

toHanChar :: Char -> Char
toHanChar c = chr (ord c - 0xfee0)

zenToHan :: String -> String
zenToHan []     = []
zenToHan (c:cs) = if ord c >= 0xff01 && ord c <= 0xff5e 
                      then toHanChar c : zenToHan cs
                      else c:zenToHan cs

zenSpToHanSp :: String -> String
zenSpToHanSp []     = []
zenSpToHanSp (c:cs) = if c == zenSpace
                      then ' ' : zenSpToHanSp cs
                      else c:zenToHan cs

hanToZen :: String -> String
hanToZen []     = []
hanToZen (c:cs) = if ord c >= 0x21 && ord c <= 0x7e 
                      then toZenChar c : hanToZen cs
                      else c:hanToZen cs

hanSpToZenSp :: String -> String
hanSpToZenSp []     = []
hanSpToZenSp (c:cs) = if c == ' '
                      then zenSpace : hanSpToZenSp cs
                      else c : hanSpToZenSp cs