module Lib
  ( linInter,
    lagInter,
  )
where

type Point a = (a, a)

type Func a = a -> a

linInter :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
linInter p l = fst $ help (const 0) p l
  where
    help :: (Fractional a, Ord a, Show a) => Func a -> [Point a] -> [a] -> ([a], Func a)
    help f points list = case points of
      [] -> (map f list, f)
      [(x1, y1)] ->
        let newF x = if f x1 /= y1 then y1 else f x in (map newF list, newF)
      ((x1, y1) : (x2, y2) : _) -> case list of
        [] -> ([], f)
        (cur : gen)
          | cur < x1 ->
              let newF = if f x1 /= y1 then linearF else f
                  (rest, finalF) = help newF points gen
               in (newF cur : rest, finalF)
          | cur >= x1 && cur <= x2 ->
              let newF x
                    | x >= x1 = linearF x
                    | otherwise = f x
                  (rest, finalF) = help newF points gen
               in (newF cur : rest, finalF)
          | cur > x2 -> help f (tail points) list-- if cur == 4 then error "3" else 
          --     let (rest, finalF) = help f (tail points) gen
          --      in (f cur : rest, finalF)
        where
          linearF x = k * x + b
          k = (y1 - y2) / (x1 - x2)
          b = y1 - k * x1

lagInter :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
lagInter = help []
  where
    help :: (Fractional a, Ord a, Show a) => [Point a] -> [Point a] -> [a] -> [a]
    help _ _ [] = []
    help acc points gen@(curr : restGen) = case points of
      [] -> map predict gen
      (point@(x, y) : restP) -> case compare curr x of
        LT -> predict curr : help acc points restGen
        EQ -> y : help acc points restGen
        GT -> help (point : acc) restP gen
      where
        ps = if null points then acc else head points : acc
        predict a = sum (map (calcCoef a) ps)
        xs = map fst ps

        calcCoef t (xi, yi) = yi * foldl (foldWithout t xi) 1 xs / foldl (foldWithout xi xi) 1 xs

        foldWithout x skip accum xi
          | skip == xi = accum
          | otherwise = accum * (x - xi)
