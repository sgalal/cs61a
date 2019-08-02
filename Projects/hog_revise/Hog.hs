-- Generate the final strategy for Hog
-- stack ghc -- Hog.hs -threaded -O2 -o Hog.exe
-- ./Hog.exe +RTS -N

{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Foldable (fold)
import Data.Monoid (Sum(..), getSum)
import Data.Ratio ((%))
import Data.Tuple (swap)

diceResult1 :: [(Rational, Word)]
diceResult1 =
  [ (1 % 6, 1)
  , (1 % 6, 2)
  , (1 % 6, 3)
  , (1 % 6, 4)
  , (1 % 6, 5)
  , (1 % 6, 6)
  ]

diceResult2 :: [(Rational, Word)]
diceResult2 =
  [ (11 % 36, 1)
  , (1 % 36, 4)
  , (1 % 18, 5)
  , (1 % 12, 6)
  , (1 % 9, 7)
  , (5 % 36, 8)
  , (1 % 9, 9)
  , (1 % 12, 10)
  , (1 % 18, 11)
  , (1 % 36, 12)
  ]

diceResult3 :: [(Rational, Word)]
diceResult3 =
  [ (91 % 216, 1)
  , (1 % 216, 6)
  , (1 % 72, 7)
  , (1 % 36, 8)
  , (5 % 108, 9)
  , (5 % 72, 10)
  , (1 % 12, 11)
  , (19 % 216, 12)
  , (1 % 12, 13)
  , (5 % 72, 14)
  , (5 % 108, 15)
  , (1 % 36, 16)
  , (1 % 72, 17)
  , (1 % 216, 18)
  ]

diceResult4 :: [(Rational, Word)]
diceResult4 =
  [ (671 % 1296, 1)
  , (1 % 1296, 8)
  , (1 % 324, 9)
  , (5 % 648, 10)
  , (5 % 324, 11)
  , (35 % 1296, 12)
  , (13 % 324, 13)
  , (17 % 324, 14)
  , (5 % 81, 15)
  , (85 % 1296, 16)
  , (5 % 81, 17)
  , (17 % 324, 18)
  , (13 % 324, 19)
  , (35 % 1296, 20)
  , (5 % 324, 21)
  , (5 % 648, 22)
  , (1 % 324, 23)
  , (1 % 1296, 24)
  ]

diceResult5 :: [(Rational, Word)]
diceResult5 =
  [ (4651 % 7776, 1)
  , (1 % 7776, 10)
  , (5 % 7776, 11)
  , (5 % 2592, 12)
  , (35 % 7776, 13)
  , (35 % 3888, 14)
  , (121 % 7776, 15)
  , (185 % 7776, 16)
  , (85 % 2592, 17)
  , (10 % 243, 18)
  , (365 % 7776, 19)
  , (127 % 2592, 20)
  , (365 % 7776, 21)
  , (10 % 243, 22)
  , (85 % 2592, 23)
  , (185 % 7776, 24)
  , (121 % 7776, 25)
  , (35 % 3888, 26)
  , (35 % 7776, 27)
  , (5 % 2592, 28)
  , (5 % 7776, 29)
  , (1 % 7776, 30)
  ]

diceResult6 :: [(Rational, Word)]
diceResult6 =
  [ (31031 % 46656, 1)
  , (1 % 46656, 12)
  , (1 % 7776, 13)
  , (7 % 15552, 14)
  , (7 % 5832, 15)
  , (7 % 2592, 16)
  , (41 % 7776, 17)
  , (71 % 7776, 18)
  , (37 % 2592, 19)
  , (317 % 15552, 20)
  , (623 % 23328, 21)
  , (251 % 7776, 22)
  , (281 % 7776, 23)
  , (1751 % 46656, 24)
  , (281 % 7776, 25)
  , (251 % 7776, 26)
  , (623 % 23328, 27)
  , (317 % 15552, 28)
  , (37 % 2592, 29)
  , (71 % 7776, 30)
  , (41 % 7776, 31)
  , (7 % 2592, 32)
  , (7 % 5832, 33)
  , (7 % 15552, 34)
  , (1 % 7776, 35)
  , (1 % 46656, 36)
  ]

diceResult7 :: [(Rational, Word)]
diceResult7 =
  [ (201811 % 279936, 1)
  , (1 % 279936, 14)
  , (7 % 279936, 15)
  , (7 % 69984, 16)
  , (7 % 23328, 17)
  , (35 % 46656, 18)
  , (455 % 279936, 19)
  , (875 % 279936, 20)
  , (95 % 17496, 21)
  , (805 % 93312, 22)
  , (3535 % 279936, 23)
  , (4795 % 279936, 24)
  , (6055 % 279936, 25)
  , (595 % 23328, 26)
  , (875 % 31104, 27)
  , (8135 % 279936, 28)
  , (875 % 31104, 29)
  , (595 % 23328, 30)
  , (6055 % 279936, 31)
  , (4795 % 279936, 32)
  , (3535 % 279936, 33)
  , (805 % 93312, 34)
  , (95 % 17496, 35)
  , (875 % 279936, 36)
  , (455 % 279936, 37)
  , (35 % 46656, 38)
  , (7 % 23328, 39)
  , (7 % 69984, 40)
  , (7 % 279936, 41)
  , (1 % 279936, 42)
  ]

diceResult8 :: [(Rational, Word)]
diceResult8 =
  [ (1288991 % 1679616, 1)
  , (1 % 1679616, 16)
  , (1 % 209952, 17)
  , (1 % 46656, 18)
  , (5 % 69984, 19)
  , (55 % 279936, 20)
  , (49 % 104976, 21)
  , (413 % 419904, 22)
  , (131 % 69984, 23)
  , (1825 % 559872, 24)
  , (275 % 52488, 25)
  , (365 % 46656, 26)
  , (1145 % 104976, 27)
  , (665 % 46656, 28)
  , (1225 % 69984, 29)
  , (2125 % 104976, 30)
  , (515 % 23328, 31)
  , (38165 % 1679616, 32)
  , (515 % 23328, 33)
  , (2125 % 104976, 34)
  , (1225 % 69984, 35)
  , (665 % 46656, 36)
  , (1145 % 104976, 37)
  , (365 % 46656, 38)
  , (275 % 52488, 39)
  , (1825 % 559872, 40)
  , (131 % 69984, 41)
  , (413 % 419904, 42)
  , (49 % 104976, 43)
  , (55 % 279936, 44)
  , (5 % 69984, 45)
  , (1 % 46656, 46)
  , (1 % 209952, 47)
  , (1 % 1679616, 48)
  ]

diceResult9 :: [(Rational, Word)]
diceResult9 =
  [ (8124571 % 10077696, 1)
  , (1 % 10077696, 18)
  , (1 % 1119744, 19)
  , (5 % 1119744, 20)
  , (55 % 3359232, 21)
  , (55 % 1119744, 22)
  , (71 % 559872, 23)
  , (487 % 1679616, 24)
  , (335 % 559872, 25)
  , (1265 % 1119744, 26)
  , (19855 % 10077696, 27)
  , (1193 % 373248, 28)
  , (5431 % 1119744, 29)
  , (23225 % 3359232, 30)
  , (325 % 34992, 31)
  , (275 % 23328, 32)
  , (3965 % 279936, 33)
  , (18065 % 1119744, 34)
  , (19525 % 1119744, 35)
  , (180325 % 10077696, 36)
  , (19525 % 1119744, 37)
  , (18065 % 1119744, 38)
  , (3965 % 279936, 39)
  , (275 % 23328, 40)
  , (325 % 34992, 41)
  , (23225 % 3359232, 42)
  , (5431 % 1119744, 43)
  , (1193 % 373248, 44)
  , (19855 % 10077696, 45)
  , (1265 % 1119744, 46)
  , (335 % 559872, 47)
  , (487 % 1679616, 48)
  , (71 % 559872, 49)
  , (55 % 1119744, 50)
  , (55 % 3359232, 51)
  , (5 % 1119744, 52)
  , (1 % 1119744, 53)
  , (1 % 10077696, 54)
  ]

diceResult10 :: [(Rational, Word)]
diceResult10 =
  [ (50700551 % 60466176, 1)
  , (1 % 60466176, 20)
  , (5 % 30233088, 21)
  , (55 % 60466176, 22)
  , (55 % 15116544, 23)
  , (715 % 60466176, 24)
  , (83 % 2519424, 25)
  , (545 % 6718464, 26)
  , (605 % 3359232, 27)
  , (3685 % 10077696, 28)
  , (20735 % 30233088, 29)
  , (72403 % 60466176, 30)
  , (14795 % 7558272, 31)
  , (182005 % 60466176, 32)
  , (66055 % 15116544, 33)
  , (121055 % 20155392, 34)
  , (78949 % 10077696, 35)
  , (24475 % 2519424, 36)
  , (38525 % 3359232, 37)
  , (780175 % 60466176, 38)
  , (209275 % 15116544, 39)
  , (856945 % 60466176, 40)
  , (209275 % 15116544, 41)
  , (780175 % 60466176, 42)
  , (38525 % 3359232, 43)
  , (24475 % 2519424, 44)
  , (78949 % 10077696, 45)
  , (121055 % 20155392, 46)
  , (66055 % 15116544, 47)
  , (182005 % 60466176, 48)
  , (14795 % 7558272, 49)
  , (72403 % 60466176, 50)
  , (20735 % 30233088, 51)
  , (3685 % 10077696, 52)
  , (605 % 3359232, 53)
  , (545 % 6718464, 54)
  , (83 % 2519424, 55)
  , (715 % 60466176, 56)
  , (55 % 15116544, 57)
  , (55 % 60466176, 58)
  , (5 % 30233088, 59)
  , (1 % 60466176, 60)
  ]

-- Get the minimal digit of a natural number.
minDigit :: Word -> Word
minDigit 0 = 0
minDigit n = uncurry aux (n `quotRem` 10)
  where
  aux :: Word -> Word -> Word
  aux m everMin =
    if m == 0
      then everMin
      else aux allButLast (min lastDigit everMin)
    where
    (allButLast, lastDigit) = m `quotRem` 10

-- Get the leftmost digit of a natural number.
leftmostDigit :: Word -> Word
leftmostDigit = until (< 10) (`quot` 10)

-- Apply swap if necessary according to the rule of Swine Swap,
-- and return as it is if unnecessary.
applySwap :: (Word, Word) -> (Word, Word)
applySwap a@(score, oppon) = if shouldSwap then swap a else a
  where
  scoreLeftmost = leftmostDigit score
  opponRightmost = oppon `rem` 10
  shouldSwap = scoreLeftmost == opponRightmost

-- Sum up all possibilities.
foldProbs :: [(Rational, Rational)] -> Rational
foldProbs = getSum . fold . fmap (Sum . uncurry (*))

-- Find the maximum value and its index in a list.
maxIndexAndValue :: Ord a => [a] -> (Word, a)
maxIndexAndValue [] = undefined
maxIndexAndValue (x : xs) = aux xs x 1 0
  where
  aux :: Ord a => [a] -> a -> Word -> Word -> (Word, a)
  aux [] everMax _ everMaxIndex = (everMaxIndex, everMax)
  aux (y : ys) everMax currIndex everMaxIndex =
    if y > everMax
      then aux ys y (currIndex + 1) currIndex
      else aux ys everMax (currIndex + 1) everMaxIndex

winProb :: Word -> Word -> (Word, Rational)
winProb score oppon
  | score >= 100 = (0, 1.0)
  | oppon >= 100 = (0, 0.0)
  | otherwise = maxIndexAndValue $
      [ diceProb0
      , diceProb1
      , diceProb2
      , diceProb3
      , diceProb4
      , diceProb5
      , diceProb6
      , diceProb7
      , diceProb8
      , diceProb9
      , diceProb10
      ]
    where
    calcProb :: Word -> Rational
    calcProb gain = (1 -) $ snd $ winProbMemo $ swap $ applySwap (score + gain, oppon)

    diceProb0 :: Rational
    diceProb0 = calcProb (10 - minDigit oppon)

    diceProbN :: [(Rational, Word)] -> Rational
    diceProbN = foldProbs . (parMap rseq) (fmap calcProb)

    diceProb1 = diceProbN diceResult1
    diceProb2 = diceProbN diceResult2
    diceProb3 = diceProbN diceResult3
    diceProb4 = diceProbN diceResult4
    diceProb5 = diceProbN diceResult5
    diceProb6 = diceProbN diceResult6
    diceProb7 = diceProbN diceResult7
    diceProb8 = diceProbN diceResult8
    diceProb9 = diceProbN diceResult9
    diceProb10 = diceProbN diceResult10

winProbMemo :: (Word, Word) -> (Word, Rational)
winProbMemo (s, o) = (f <$> zip [0..] (repeat [0..])) !! (fromEnum s) !! (fromEnum o)
  where
  f :: (Word, [Word]) -> [(Word, Rational)]
  f (i, xs) = fmap (winProb i) xs

getWinProb :: Word -> Word -> Word
getWinProb score oppon = fst $ winProbMemo (score, oppon)

main :: IO ()
main = print $ do
  x <- [0..99]
  pure $ do
    y <- [0..99]
    pure $ getWinProb x y
