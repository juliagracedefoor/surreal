module Surreal where

import           Data.List       (intercalate)
import           Data.Maybe      (fromJust, isJust)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

data Surreal = Surreal {left :: Set Surreal, right :: Set Surreal} deriving (Eq, Ord)

instance Show Surreal where
    show = showWith integerNames

zero :: Surreal
zero = fromLists [] []

one :: Surreal
one = fromLists [zero] []

minusone :: Surreal
minusone = fromLists [] [zero]

shadowzero :: Surreal
shadowzero = fromLists [zero] [zero]

-- Axiom one: Is this a valid surreal number?
isValid :: Surreal -> Bool
isValid a =
    let test1    = all isValid (left a) && all isValid (right a)
        pairings = Set.cartesianProduct (right a) (left a)
        test2    = not . any (uncurry (<==)) $ pairings
    in  test1 && test2

-- Axiom two: Is the first number less than or alike in value to the second number?
(<==) :: Surreal -> Surreal -> Bool
a <== b =
    let test1 = not . any (b <==) $ left a
        test2 = not . any (<== a) $ right b
    in  test1 && test2

(>==) :: Surreal -> Surreal -> Bool
(>==) = flip (<==)

(.<) :: Surreal -> Surreal -> Bool
a .< b = a <== b && not (b <== a)

(.>) :: Surreal -> Surreal -> Bool
a .> b = not (a <== b) && b <== a

-- Definition: Are two surreal numbers alike in value?
like :: Surreal -> Surreal -> Bool
like a b = a <== b && b <== a

(===) :: Surreal -> Surreal -> Bool
(===) = like

-- Definition: Are two surreal numbers adjacent? That is, not GT, LT, or EQ?
-- (This can only be true if one of the numbers is not properly formed)
adjacent :: Surreal -> Surreal -> Bool
adjacent a b = not (a <== b) && not (b <== a)

-- Definition: What is the additive inverse of a surreal number?
neg :: Surreal -> Surreal
neg a = fromSets (Set.map neg (right a)) (Set.map neg (left a))

-- Definition: What is the sum of two surreal numbers?
-- note: VERY slow for numbers greater than 10
add :: Surreal -> Surreal -> Surreal
add a b =
  let sa = simplify a
      sb = simplify b
      l = Set.union (Set.map (add sb) (left sa)) (Set.map (add sa) (left sb))
      r = Set.union (Set.map (add sb) (right sa)) (Set.map (add sa) (right sb))
  in fromSets l r

-- Subtract numbers by (a - b) = (a + (-b))
sub :: Surreal -> Surreal -> Surreal
sub a b = add a (neg b)

-- Remove extraneous values from a surreal numbers left and right sets
simplify :: Surreal -> Surreal
simplify a = fromSets (maxSet . left $ a) (minSet . right $ a)

-- Keep only the minimal number in a set
minSet :: Set Surreal -> Set Surreal
minSet s
  | null s = s
  | otherwise = Set.singleton minN
  where minN = foldr1 (\x m -> if x <== m then x else m) s

-- Keep only the maximal number in a set
maxSet :: Set Surreal -> Set Surreal
maxSet s
  | null s    = s
  | otherwise = Set.singleton maxN
  where maxN = foldr1 (\x m -> if x >== m then x else m) s

-- Construct a surreal number from two lists
fromLists :: [Surreal] -> [Surreal] -> Surreal
fromLists ls rs = Surreal { left = Set.fromList ls, right = Set.fromList rs }

-- Construct a surreal number from two sets
fromSets :: Set Surreal -> Set Surreal -> Surreal
fromSets left right = Surreal { left = left, right = right }

-- Construct a surreal number from an integer (large numbers are slow and may cause a stack overflow)
fromInt :: Int -> Surreal
fromInt 0 = zero
fromInt a | a < 0 = fromLists [] [fromInt (a + 1)]
          | a > 0 = fromLists [fromInt (a - 1)] []

-- Construct a surreal number from a left integer and a right integer
between :: Int -> Int -> Surreal
between l r = fromLists [fromInt l] [fromInt r]

-- Create a string representation of a surreal number using names for certain values
-- For example, "(( : ) : (( : ) : ))" might be replaced with "(0 : 1)"
showWith :: Map Surreal String -> Surreal -> String
showWith names a
    | nameExists
    = fromJust name
    | otherwise
    = "(" ++ stringFromSet (left a) ++ ":" ++ stringFromSet (right a) ++ ")"
  where
    name          = Map.lookup a names
    nameExists    = isJust name
    stringFromSet s
      | null s = "•"
      | otherwise = intercalate ", " . map (showWith names) . Set.toList $ s

-- Equivalent to showWith, but as an IO action
printWith :: Map Surreal String -> Surreal -> IO ()
printWith names = putStrLn . showWith names

-- Name substitutions for the three fundamental surreal numbers
fundamentalNames :: Map Surreal String
fundamentalNames = Map.fromList [(zero, "0"), (one, "1"), (minusone, "-1"), (shadowzero, "&0")]

-- Name substitutions for the integers between -1000 and 1000
integerNames :: Map Surreal String
integerNames =
    let nums = [-1000 .. 1000]
        integerMap = Map.fromList $ zip (map fromInt nums) (map show nums)
    in  Map.union integerMap fundamentalNames

-- For printing with no name substitutions
noNames :: Map Surreal String
noNames = Map.empty
