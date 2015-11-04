{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString.Builder hiding (word8)
import System.IO
import Wicker.Graph
import Wicker.Includes
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = prog
  where
    prog = input >>= readfiles >>= output . makeDot

    input     = B.lines <$> B.getContents
    readfiles = mapM fromFile
    output    = hPutBuilder stdout
