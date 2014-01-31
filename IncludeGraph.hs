{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.ByteString.Builder hiding (word8)
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import System.IO
import System.Posix.FilePath
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

data Location = Local | System

include :: Parser RawFilePath
include = string "#include" *> skipSpace *> (system <|> local)
  where
    system = between '<' '>'
    local  = between '"' '"'

    between a b = char a *> takeWhile1 (/= b)

includes :: [B.ByteString] -> [RawFilePath]
includes = mapMaybe (mby . parseOnly include)
  where
    mby (Right a) = Just a
    mby _         = Nothing

type Includes = (RawFilePath, [RawFilePath])

graph :: [Includes] -> Builder
graph xs = "digraph includes {" <> nodes (groups xs) <> edges xs <> "}"
  where
    groups = foldl' f M.empty
      where
        f m x = let (d, n) = splitFileName (fst x) in M.insertWith (++) d [n] m

    nodes = mconcat . map subgraph . M.toList
    edges = mconcat . map (\(x, ys) -> mconcat $ map (edge x) ys)

    subgraph (dir, xs) = "subgraph " <> id dir <> " {" <>
      "label = " <> byteString dir <> ";" <>
      mconcat (map node xs) <>
      "};"

    edge x y = id x <> " -> " <> id y <> ";"

    node name = id name <> " [label=\"" <> byteString name <> "\"];"

    id = byteString . B.map f
      where
        f x = if x == '/' || x == '.' then '_' else x

fromFile :: RawFilePath -> IO Includes
fromFile f = ((,) f . includes . B.lines) <$> B.readFile (B.unpack f)

main :: IO ()
main = B.lines <$> B.getContents >>= mapM fromFile >>= hPutBuilder stdout . graph
