{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import System.IO
import qualified Data.Set as S
import qualified Data.Text as T

wordValue = sum . map ((+ 1) . (+ (- ord 'A')) . ord)

triangle = map (truncate . (\n -> n * (n+1) / 2)) [1 ..]

main = do
  handle <- openFile "p042_words.txt" ReadMode
  contents <- hGetContents handle
  let ws = map T.unpack . T.splitOn "\",\"" . T.pack . init $ tail contents
      ns = map wordValue ws
      max = maximum ns
      ts = S.fromList $ takeWhile (< max) triangle
      tWords = filter (flip S.member ts . snd) $ zip ws ns
    in print $ length tWords
  hClose handle
