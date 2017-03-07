module Week1
    ( validateCC
    , hanoi
    , hanoi2
    , Move
    , Peg
    ) where

-- Validate credit card numbers
validateCC :: Integer -> Bool
validateCC n = False -- YOUR CODE GOES HERE

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src tgt tmp = [] -- YOUR CODE GOES HERE

-- Towers of Hanoi with an extra peg. This is an optional exercise. It is more
-- an algorithms problem than a Haskell problem.
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 n src tgt tmp1 tmp2 = [] -- YOUR CODE GOES HERE
