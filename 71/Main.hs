{-# language MultiWayIf, TypeApplications, ExplicitForAll, LambdaCase #-}
module Main (main) where

import Data.Foldable
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Foldable ( traverse_ )
main :: IO ()
main = traverse_ print $ solve @Int (3 % 7)

monotonously :: (a -> a -> Bool) -> [a] -> [a]
monotonously p = go
  where
  go = \case
    [] -> []
    (x:xs) -> x : go (dropWhile (p x) xs)

increasing :: Ord a => [a] -> [a]
increasing = monotonously (>=)

solve :: Integral n => Ratio n -> [Ratio n]
solve t = increasing $ one t <$> [1..]

one :: Integral n => Ratio n -> n -> Ratio n
one t d = n % d
  where
  n = (numerator t * d) `div'` denominator t

a `div'` b = if r == 0 then pred n else n
  where
  (n, r) = a `divMod` b
