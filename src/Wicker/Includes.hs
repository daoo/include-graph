module Wicker.Includes where

import Data.Bifunctor
import Data.Either
import System.Posix.FilePath
import Wicker.Module
import Wicker.Parser
import qualified Data.ByteString.Char8 as B

type Includes = (Module, [Module])

-- TODO: Check file type
includes :: (RawFilePath, [RawFilePath]) -> Includes
includes = fromFilePath `bimap` (rights . map parseInclude)

fromFile :: RawFilePath -> IO Includes
fromFile path = includes . (,) path . B.lines <$> B.readFile (B.unpack path)
