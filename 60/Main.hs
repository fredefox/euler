{-# language DerivingStrategies, DefaultSignatures, StandaloneDeriving, DeriveAnyClass, LambdaCase #-}
{-# options_ghc -Wall #-}
module Main (main, increasing, increasingOn, monotonously) where

import Data.Foldable
import Data.Function
import Data.Kind

main :: IO ()
main = do
  let ps = fmap runPrime . fromTuple <$> enumerate @FivePrimes
  traverse_ print $ filter prop ps

pairs :: [a] -> [(a, a)]
pairs = \case
  [] -> []
  (x:xs) -> ((x,) <$> xs) <> pairs xs

prop :: [Integer] -> Bool
prop = all p . pairs
  where
  p (a, b) = isPrime (concatenate a b) && isPrime (concatenate b a)
  concatenate :: Integer -> Integer -> Integer
  concatenate a b = read (show a <> show b)

increasing :: Ord a => [a] -> [a]
increasing = increasingOn id

increasingOn :: Ord b => (a -> b) -> [a] -> [a]
increasingOn f = monotonously ((>) `on` f)

monotonously :: (a -> a -> Bool) -> [a] -> [a]
monotonously p = \case
  []     -> []
  (x:xs) -> x : monotonously p (dropWhile (p x) xs)

type FivePrimes = Five (Prime Integer)

type Five a = Empty a :> a :> a :> a :> a :> a

type (:>) = (,)

class Enumerate a where
  enumerate :: [a]
  default enumerate :: Enum a => [a]
  enumerate = enumFrom (toEnum 0)

instance (Enumerate a, Enumerate b) => Enumerate (a, b) where
  enumerate :: (Enumerate a, Enumerate b) => [(a, b)]
  enumerate = go 0
    where
    go n = zip (take n as) (reverse $ take n bs) <> go (succ n)
    as = enumerate @a
    bs = enumerate @b

instance {-# OVERLAPS #-} Enumerate b => Enumerate (Empty a, b) where
  enumerate = (Empty,) <$> enumerate @b

deriving anyclass instance Enumerate Int
deriving anyclass instance Enumerate Integer

newtype Prime a = Prime { runPrime :: a }

deriving stock instance Show a => Show (Prime a)
deriving stock instance Eq a => Eq (Prime a)
deriving newtype instance Num a => Num (Prime a)
deriving newtype instance Ord a => Ord (Prime a)

class Tuple (t :: Type -> Type) a where
  fromTuple :: t a -> [a]

-- | Like `()` but with kine `* -> *`
data Empty k = Empty

deriving stock instance Show (Empty k)

instance Tuple Empty a where
  fromTuple = const mempty

instance Tuple t a => Tuple ((,) (t a)) a where
  fromTuple :: (t a, a) -> [a]
  fromTuple (t, a) = a : fromTuple t

instance Integral n => Enumerate (Prime n) where
  enumerate = Prime <$> primes

primes :: Integral n => [n]
primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(`mod`p)) t)
    sieve _ _ = error "IMPOSSIBLE"

listFactors :: Integral n => n -> [n]
listFactors = go primes
  where
    go _      1 = []
    go (p:ps) n
      | p*p > n = [n]
      | n `mod` p == 0 = p : go (p:ps) (n `div` p)
      | otherwise      = go ps n
    go _ _ = error "IMPOSSIBLE"

isPrime :: Integral n => n -> Bool
isPrime
  = (== 1)
  . length
  . listFactors
