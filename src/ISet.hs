module ISet where

import           Surreal (Surreal)
import qualified Surreal as S

type ISet a = [a -> Bool]

empty :: ISet a
empty = [const False]

integers :: ISet Surreal
integers = [\x -> null (S.left x) || null (S.right x)]

fromList :: (Eq a) => [a] -> ISet a
fromList = map (==)

isInt x = x `ISet.elem` integers

elem :: a -> ISet a -> Bool
elem x = all ($x)

union :: ISet a -> ISet a -> ISet a
union a b = [\x -> x `ISet.elem` a || x `ISet.elem` b]

intersection :: ISet a -> ISet a -> ISet a
intersection = (++)

-- Round a floating point number to a given number of decimal places
roundTo :: (Integral a, RealFloat b) => a -> b -> b
roundTo digits n =
    let offset = 10 ^ digits in fromIntegral (round $ n * offset) / offset
