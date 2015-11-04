{-# LANGUAGE OverloadedStrings #-}
module Wicker.Graph where

import Data.ByteString.Builder hiding (word8)
import Data.Char (toLower)
import Data.List
import Data.Map (Map)
import Data.Monoid
import Wicker.Includes
import Wicker.Module
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

groups :: [Includes] -> Map Module [Module]
groups = foldl' f M.empty
  where
    f m (a, bs) = foldl' add (add m a) bs

    add m a = M.insertWith (++) (parentModule a) [a] m

makeDot :: [Includes] -> Builder
makeDot xs = digraph "includes" (nodes (groups xs) <> edges xs)
  where
    digraph name content  = "digraph " <> name <> " {" <> content <> "}"
    subgraph name content = "subgraph cluster" <> name <> " {" <> content <> "};"

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
