module Week1
    ( validateCC
    , hanoi
    , hanoi2
    , Move
    , Peg
    ) where
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- Convert an integer to a list of digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x `div` 10 == 0 = [x `rem` 10]
  | otherwise       = x `rem` 10 : toDigitsRev (x `div` 10)

-- Double every other integer in a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ds = reverse (doubleEveryOther' (reverse ds))

-- Inner function for doubling every other number in a list,
-- starting with the second item in the list.
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [n] = [n]
doubleEveryOther' (n : m : t) = n : 2*m : doubleEveryOther' t

validateCC :: Integer -> Bool
validateCC n = let
  digitsDoubled = doubleEveryOther (toDigits n)
  flattened = concat (map toDigits digitsDoubled)
  sumDigits = sum flattened
  in sumDigits `rem` 10 == 0

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _   _   _   = []
hanoi n src tgt tmp =
  hanoi (n - 1) src tmp tgt ++
  [(src, tgt)] ++
  hanoi (n - 1) tmp tgt src

-- Towers of Hanoi with an extra peg
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _   _   _    _    = []
hanoi2 1 src tgt _    _    = [(src, tgt)]
hanoi2 n src tgt tmp1 tmp2 =
  hanoi (n - 2) src tmp1 tgt ++
  [(src, tmp2), (src, tgt), (tmp2, tgt)] ++
  hanoi (n - 2) tmp1 tgt src
