{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative
import Data.Attoparsec.Text hiding (parse)
import Data.Char
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lazy.Builder
import System.Environment
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as L

main :: IO ()
main = getArgs >>= \case
  ["dot"] -> input >>= put . buildDot . flatten
  _       -> input >>= put . mconcat . map buildModule
  where
    input = getContents >>= mapM readImports . lines
    put = L.putStr . toLazyText

data Module = Module { moduleName :: Text, moduleImports :: [Module] }

readImports :: FilePath -> IO Module
readImports path = Module (T.pack path) <$> io path
  where
    io = fmap (mapMaybe parse) . input

    input = fmap T.lines . T.readFile
    parse = fmap (`Module` []) . parseImport

parseImport :: Text -> Maybe Text
parseImport = parseMaybe includeCPP

includeCPP :: Parser Text
includeCPP = string "#include" *> skipSpace *> (system <|> local)
  where
    system = char '<' *> takeWhile1 (/='>') <* char '>'
    local  = char '"' *> takeWhile1 (/='"') <* char '"'

parseMaybe :: Parser a -> Text -> Maybe a
parseMaybe f = either (const Nothing) Just . parseOnly f

buildModule :: Module -> Builder
buildModule x =
  file x <> singleton '\n' <> mconcat (map imprt (moduleImports x))
  where
    file = fromText . moduleName
    imprt = (<> "\n") . ("  " <>) . file

flatten :: [Module] -> Map Text [Text]
flatten = go mempty
  where
    go m []     = m
    go m (x:xs) = go (g (f x m) (moduleImports x)) xs

    f :: Module -> Map Text [Text] -> Map Text [Text]
    f x = M.insert (moduleName x) (map moduleName (moduleImports x))

    g :: Map Text [Text] -> [Module] -> Map Text [Text]
    g = foldl (\m x -> M.insertWith (flip const) (moduleName x) [] m)

buildDot :: Map Text [Text] -> Builder
buildDot mods = digraph "includes" (mconcat (map node nodes) <> mconcat (map edge edges))
  where
    nodes = M.keys mods
    edges = concatMap (uncurry (zip . repeat)) (M.assocs mods)

    digraph name content = "digraph " <> name <> " {" <> content <> "}"

    edge (a, b) = modid a <> "->" <> modid b <> ";"
    node a = modid a <> " [label=\"" <> modname a <> "\"];"

    modid = fromText . T.map f
      where
        f c
          | isAsciiLower c = c
          | isAsciiUpper c = toLower c
          | isDigit c = c
          | otherwise = '_'

    modname = fromText
