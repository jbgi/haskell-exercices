-----------------------------------------------------------------------------
--
-- Module      :  Intro
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  jb@giraudeau.info
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Intro (

) where


-- Exercice 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = r : toDigitsRev q where (q,r) = (n `divMod` 10)

toDigits = reverse . toDigitsRev

 -- seen on http://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
 -- rev_digits = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

-- Exercice 2
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev (a:(b:xs)) = a : ((b*2) : doubleEveryOtherRev xs)
doubleEveryOtherRev l = l

doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- Excerice 3
sumDigits = sum . (>>= toDigitsRev)

-- Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOtherRev . toDigitsRev) n `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ (a,b) : (hanoi (n-1) c b a)



