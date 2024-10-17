import qualified Data.List as L
import Lib
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Cheking linear interpolation ..."
  quickCheck (withMaxSuccess 1000 propLinId)
  putStrLn "Cheking lagrangia interpolation ..."
  quickCheck (withMaxSuccess 1000 propLagId)

propLinId :: [Double] -> [Double] -> Bool
propLinId xs ys = and $ zipWith (\a b -> abs (a - b) < 0.00001) (linInter points x) y
  where
    points = L.nubBy (\a b -> fst a == fst b) (zip (L.sort xs) ys)
    (x, y) = unzip points

propLagId :: [Double] -> [Double] -> Bool
propLagId xs ys = and $ zipWith (\a b -> abs (a - b) < 0.00001) (lagInter points x) y
  where
    points = L.nubBy (\a b -> fst a == fst b) (zip (L.sort xs) ys)
    (x, y) = unzip points
