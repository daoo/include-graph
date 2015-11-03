{-# LANGUAGE OverloadedStrings #-}
module Wicker.Module where

import Data.ByteString.Char8 (ByteString)
import System.Posix.FilePath

-- |An language-independent module identifier.
--
-- The module identifier is created from the file path and is treated as unqiue
-- within the scope it is used.
newtype Module = Module { mkModule :: [ByteString] }
  deriving (Eq, Ord, Show)

fromFilePath :: RawFilePath -> Module
fromFilePath = Module . filter (/="/") . splitDirectories . dropExtension

parentModule :: Module -> Module
parentModule (Module a) = Module (init a)
