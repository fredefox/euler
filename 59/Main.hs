{-# options_ghc -Wall #-}
module Main (main) where

import Data.Foldable
import qualified Data.Bits as Bits
import Data.Function
import qualified Data.Char as Char
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio (Ratio, (%))

-- | Read a cipher text. Try to decode it with a combination of
-- keys. All decoded strings will be considered in turn: All strings
-- that contain /any/ non-printable characters will be filtered
-- out. The remaining clear-texts are printed in order of the
-- following metric: The ratio of valid to invalid words in the text.
main :: IO ()
main = do
  s <- fmap (Char.chr . read @Int) . words <$> getContents
  ws <- Set.fromList . words <$> readFile "/usr/share/dict/words"
  let clears :: [String] = decrypts keys s
  let clearsSorted :: [String] = increasingOn (score ws) clears
  traverse_ (print . score ws) clearsSorted
  traverse_ putStrLn clearsSorted

increasingOn :: Ord b => (a -> b) -> [a] -> [a]
increasingOn f = monotonously ((>) `on` f)

monotonously :: (a -> a -> Bool) -> [a] -> [a]
monotonously _ [] = []
monotonously p (x:xs) = x : monotonously p (dropWhile (p x) xs)

score :: Set String -> String -> Ratio Int
score ws txt = Set.size valid % Set.size invalid
  where
  valid = s `Set.intersection` ws
  invalid = s Set.\\ ws
  txtW = fmap (filter Char.isAlpha) $ words txt
  s = Set.fromList txtW

decrypts :: [String] -> String -> [String]
decrypts ks cipher = do
  key <- ks
  let clear = encrypt key cipher
  guard $ all Char.isPrint clear
  pure clear

encrypt :: String -> String -> String
encrypt k s = zipWith xor (cycle k) s

xor :: forall a . Enum a => a -> a -> a
xor a b = toEnum @a $ a `f` b
  where
  f = Bits.xor `on` fromEnum @a

-- | List of all possible keys.
keys :: [String]
keys = (\a b c -> [a, b, c]) <$> lower <*> lower <*> lower
  where
  lower = ['a'..'z']
