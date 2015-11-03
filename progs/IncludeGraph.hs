{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import Data.ByteString.Builder hiding (word8)
import Data.Char (toLower)
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Monoid
import System.IO
import System.Posix.FilePath
import Wicker.Module
import Wicker.Parser
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- TODO: Check file type
includes :: (RawFilePath, [RawFilePath]) -> Includes
includes = fromFilePath `bimap` (rights . map (parseOnly incl))
  where
    incl = includeCPP <|> includeHaskell

fromFile :: RawFilePath -> IO Includes
fromFile path = includes . (,) path . B.lines <$> B.readFile (B.unpack path)

type Includes = (Module, [Module])

graph :: [Includes] -> Builder
graph xs = digraph "includes" (nodes (groups xs) <> edges xs)
  where
    digraph name content  = "digraph " <> name <> " {" <> content <> "}"
    subgraph name content = "subgraph cluster" <> name <> " {" <> content <> "};"

    groups :: [Includes] -> Map Module [Module]
    groups = foldl' f M.empty
      where
        f m (a, bs) = foldl' add (add m a) bs

        add m a = M.insertWith (++) (parentModule a) [a] m

    nodes :: Map Module [Module] -> Builder
    nodes = mconcat . map subgraph' . M.toList

    edges :: [Includes] -> Builder
    edges = mconcat . map (\(x, ys) -> mconcat $ map (edge x) ys)

    subgraph' :: (Module, [Module]) -> Builder
    subgraph' (parent, children) = subgraph (buildid parent) $
      mconcat (map node children) <>
      "label=\"" <> modname parent <> "\";" <> "color=blue;"

    edge :: Module -> Module -> Builder
    edge a b = buildid a <> " -> " <> buildid b <> ";"

    node :: Module -> Builder
    node a = buildid a <> " [label=\"" <> modname a <> "\"];"

    modname :: Module -> Builder
    modname = mconcat . map byteString . intersperse (B.singleton '.') . mkModule

    buildid :: Module -> Builder
    buildid = mconcat . map (byteString . B.map f) . mkModule
      where
        f '-' = '_'
        f x   = toLower x

main :: IO ()
main = prog
  where
    prog = input >>= readfiles >>= output . graph

    input     = B.lines <$> B.getContents
    readfiles = mapM fromFile
    output    = hPutBuilder stdout
