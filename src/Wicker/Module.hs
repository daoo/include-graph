module Wicker.Module where

import Data.ByteString.Char8 (ByteString, uncons, null)
import Prelude hiding (null)
import System.Posix.FilePath

-- |An language-independent module identifier.
--
-- The module identifier is created from the file path and is treated as unqiue
-- within the scope it is used.
newtype Module = Module { mkModule :: [ByteString] }
  deriving (Eq, Ord, Show)

fromFilePath :: RawFilePath -> Module
fromFilePath = Module . filter (not . isslash) . splitDirectories . dropExtension
  where
    isslash bs = case uncons bs of
      Just ('/', bs') -> null bs'
      _ -> False

parentModule :: Module -> Module
parentModule (Module a) = Module (init a)
