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
doubleEveryOther ds = reverse (doubleEveryOther' False (reverse ds))

-- Inner function for doubling every other number in a list. The boolean flag is
-- whether or not to double the next number.
doubleEveryOther' :: Bool -> [Integer] -> [Integer]
doubleEveryOther' _     []       = []
doubleEveryOther' True  (x : xs) = 2 * x : doubleEveryOther' False xs
doubleEveryOther' False (x : xs) = x     : doubleEveryOther' True xs

validateCC :: Integer -> Bool
validateCC n = let
  digitsDoubled = doubleEveryOther (toDigits n)
  flattened = concat (map toDigits digitsDoubled)
  sumDigits = sum flattened
  in sumDigits `rem` 10 == 0


-- Validate credit card numbers
-- validateCC :: Integer -> Bool
-- validateCC n = False -- YOUR CODE GOES HERE

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi n src tgt tmp = [] -- YOUR CODE GOES HERE

-- Towers of Hanoi with an extra peg. This is an optional exercise. It is more
-- an algorithms problem than a Haskell problem.
-- hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi2 n src tgt tmp1 tmp2 = [] -- YOUR CODE GOES HERE

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
