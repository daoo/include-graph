{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import Data.ByteString.Builder hiding (word8)
import Data.Either
import Data.List
import Data.Monoid
import System.IO
import System.Posix.FilePath
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- TODO: Fully qualify all paths

includeCPP :: Parser RawFilePath
includeCPP = string "#include" *> skipSpace *> (system <|> local)
  where
    system = between '<' '>'
    local  = between '"' '"'

    between a b = char a *> takeWhile1 (/= b)

includeHaskell :: Parser RawFilePath
includeHaskell = string "import" *> skipSpace *> mby (string "qualified") *> (topath <$> modref)
  where
    mby = option () . void

    modref = takeWhile1 modchar

    modchar c = isAlpha_ascii c || isDigit c || c == '.'

    topath = (`B.append` ".hs") . B.map f
      where
        f '.' = '/'
        f x   = x

-- TODO: Check file type
includes :: (RawFilePath, [RawFilePath]) -> Includes
includes = id `bimap` (rights . map (parseOnly incl))
  where
    incl = includeCPP <|> includeHaskell

fromFile :: RawFilePath -> IO Includes
fromFile path = includes . ((,) path . B.lines) <$> B.readFile (B.unpack path)

type Includes = (RawFilePath, [RawFilePath])

graph :: [Includes] -> Builder
graph xs = "digraph includes {" <> nodes (groups xs) <> edges xs <> "}"
  where
    groups = foldl' f M.empty . concatMap (uncurry (:))
      where
        f m x = M.insertWith (++) (takeDirectory x) [x] m

    nodes = mconcat . map subgraph . M.toList
    edges = mconcat . map (\(x, ys) -> mconcat $ map (edge x) ys)

    subgraph (g, ys) = "subgraph cluster" <> buildid g <> " {" <>
      mconcat (map node ys) <>
      "label=\"" <> byteString g <> "\";" <>
      "color=blue;" <>
      "};"

    edge x y = buildid x <> " -> " <> buildid y <> ";"

    node y = buildid y <> " [label=\"" <> byteString (takeFileName y) <> "\"];"

    buildid = byteString . B.map f
      where
        f '/' = '_'
        f '.' = '_'
        f '-' = '_'
        f x   = x

main :: IO ()
main = prog
  where
    prog = input >>= readfiles >>= output . graph

    input     = B.lines <$> B.getContents
    readfiles = mapM fromFile
    output    = hPutBuilder stdout
