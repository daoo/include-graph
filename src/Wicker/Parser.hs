{-# LANGUAGE OverloadedStrings #-}
module Wicker.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import System.Posix.FilePath
import Wicker.Module

modName :: Char -> Bool
modName c = isAlpha_ascii c || isDigit c || c == '_' || c == '-'

includeCPP :: Parser Module
includeCPP = string "#include" *> skipSpace *> (system <|> local)
  where
    system = char '<' *> modref <* char '>'
    local  = char '"' *> modref <* char '"'

    modref = Module <$> sepBy1' (takeWhile1 modName) (char '/')

includeHaskell :: Parser Module
includeHaskell = string "import" *> skipSpace *> mby (string "qualified") *> modref
  where
    mby = option () . void

    modref = Module <$> sepBy1' (takeWhile1 modName) (char '.')

parseInclude :: RawFilePath -> Either String Module
parseInclude = parseOnly (includeCPP <|> includeHaskell)
