module Main (main) where

import Graphics.Gnuplot.Simple
import Lib
import System.Environment

reader :: Bool -> IO [[String]]
reader debug = do
  fmap (map words . lines) $ if not debug then getContents else readFile "test.txt"

printer :: (Show a) => [a] -> IO ()
printer [] = putStrLn ""
printer (x : xs) = do
  print x
  printer xs

main :: IO ()
main = do
  args <- getArgs
  let [start, step, end, m, db] = map read args :: [Double]
  let debug = db == 1
  let methods = [[linInter], [lagInter], [linInter, lagInter]] !! round m
  list <- reader debug
  let range = [start, start + step .. end]
  let points = map (\[w1, w2] -> (read w1, read w2)) list :: [(Double, Double)]
  let res = map (\f -> f points range) methods
  if not debug
    then mapM_ (printer . zip range) res
    else do
      plotPaths
        [ Key Nothing,
          Title "The best interpolation ever",
          Custom "grid" []
          {--,XRange xs
          ,YRange ys--}
        ]
        $ map (zip range) res
      _ <- getLine
      return ()
