module Main where

import Control.Applicative
import Data.List

type Include = String

includes :: [String] -> [Include]
includes = map (takeWhile nsep . drop 1 . dropWhile nsep) . filter (isPrefixOf "#include")
  where
    nsep c = c /= '"' && c /= '<' && c /= '>'

graph :: [(String, [Include])] -> String
graph xs = unlines $ "digraph includes {" : (nodes ++ edges ++ ["}"])
  where
    nodes = map ((++ ";") . fst) xs
    edges = concatMap (\(f, ys) -> map (\y -> f ++ " -> " ++ y ++ ";") ys) xs

fromFile :: FilePath -> IO (String, [Include])
fromFile f = ((,) f . includes . lines) <$> readFile f

main :: IO ()
main = lines <$> getContents >>= mapM fromFile >>= putStrLn . graph
