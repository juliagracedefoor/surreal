module Surreal
( Surreal
, isValid
, (*&)
, like
, fromLists
, fromInt
, zero
, one
, minusone
, showWith
, printWith
, fundamentalNames
, integerNames
, noNames
) where

import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Surreal = Surreal {left :: Set Surreal, right :: Set Surreal}

instance Show Surreal where
    show = showWith integerNames

instance Eq Surreal where
    a == b = a `like` b

instance Ord Surreal where
    a <= b = a *& b

-- Axiom one: Is this a valid surreal number?
isValid :: Surreal -> Bool
isValid a =
    let pairings = Set.cartesianProduct (right a) (left a)
    in not . any (uncurry (*&)) $ pairings

-- Axiom two: Is the first number less than or equal to the second number?
(*&) :: Surreal -> Surreal -> Bool
a *& b = 
    let test1 = not . any (b *&) $ left a
        test2 = not . any (*& a) $ right b
    in test1 && test2

-- Definition: Are two numbers alike in value?
like :: Surreal -> Surreal -> Bool
like a b = a *& b && b *& a

-- Construct a surreal number from two lists
fromLists :: [Surreal] -> [Surreal] -> Surreal
fromLists ls rs = Surreal {left=Set.fromList ls, right=Set.fromList rs}

-- Construct a surreal number from an integer (large numbers are slow and may cause a stack overflow)
fromInt :: Int -> Surreal
fromInt 0 = zero
fromInt a
    | a < 0  = fromLists [] [fromInt (a+1)]
    | a > 0  = fromLists [fromInt (a-1)] []

zero :: Surreal
zero = fromLists [] []

one :: Surreal
one = fromLists [zero] []

minusone :: Surreal
minusone = fromLists [] [zero]

-- Create a string representation of a surreal number using names for certain values
-- For example, "(( : ) : (( : ) : ))" might be replaced with "(0 : 1)"
showWith :: Map Surreal String -> Surreal -> String
showWith names a
    | isJust name = fromJust name
    | otherwise   = "(" ++ showSet (left a) ++ " : " ++ showSet (right a) ++ ")"
    where name = Map.lookup a names
          showSet = intercalate ", " . map (showWith names) . Set.toList 

-- Print a surreal number to the screen using names for certain values
-- For example, "(( : ) : (( : ) : ))" might be replaced with "(0 : 1)"
printWith :: Map Surreal String -> Surreal -> IO ()
printWith names = putStrLn . showWith names

-- Name substitutions for the three fundamental surreal numbers
fundamentalNames :: Map Surreal String
fundamentalNames = Map.fromList [(zero, "0"), (one, "1"), (minusone, "-1")]

-- Name substitutions for the integers between -1000 and 1000
integerNames :: Map Surreal String
integerNames = 
    let nums = [-1000..1000]
    in Map.fromList $ zip (map fromInt nums) (map show nums)

-- For printing with no name substitutions
noNames :: Map Surreal String
noNames = Map.empty