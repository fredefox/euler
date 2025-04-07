{-# language GHC2024, MultiWayIf #-}
{-# options_ghc -Wall #-}
module Main (main) where

import Data.Array as Array
import Data.Foldable
import Text.Read (readMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  k :: Int <- getArgs >>= \case
    (x:_) | Just n <- readMaybe @Int x -> pure n
    _ -> pure 10
  print $ solve k

solve :: Int -> Int
solve n = fst $ maximumOn snd $ Array.assocs $ collatz n

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn p = maximumBy (\a b -> p a `compare` p b)

collatz :: Int -> Array Int Int
collatz m = a
  where
  a = arrayFromList (1, m) f
  f :: Int -> Int
  f ix
    | ix <= 1   = 1
    | w > m     = succ $ f w
    | otherwise = succ $ a ! w
    where
    w = step ix

step :: Int -> Int
step n
  | even n = n `div` 2
  | otherwise = succ $ n * 3

arrayFromList :: Ix ix => (ix, ix) -> (ix -> e) -> Array ix e
arrayFromList b f = Array.array b $ (\i -> (i, f i)) <$> Array.range b
