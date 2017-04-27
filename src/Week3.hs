module Week3(skips, localMaxima, histogram) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (`skipN` xs) [0 .. length xs - 1]

skipN :: Int -> [a] -> [a]
skipN n = go 0
  where
  go _ []                   = []
  go m (y : ys) | m >= n    = y : go 0 ys
                | otherwise = go (m + 1) ys

localMaxima :: [Integer] -> [Integer]
localMaxima (x1 : xs1@(x2 : x3 : _)) =
  if x2 > x1 && x2 > x3
  then x2 : localMaxima xs1
  else localMaxima xs1
localMaxima _ = []

histogram :: [Integer] -> String
histogram = chart . foldl (\m i -> M.insertWith (+) i 1 m) M.empty
  where
  chart :: M.Map Integer Integer -> String
  chart m = if M.null m then axis else
    let flipped = flipMap m
        height = fst $ M.findMax flipped
        rows = map
          (\rowNum -> map
            (\n -> maybe
              ' '
              (\i -> if i >= rowNum then '*' else ' ')
              (M.lookup n m))
            [0..9]
          )
          (reverse [1..height])
    in
      intercalate "\n" rows
      ++ "\n" ++ axis
  axis = replicate 10 '=' ++ "\n" ++ ['0'..'9']

flipMap :: (Ord k1, Ord k2) => M.Map k1 k2 -> M.Map k2 (S.Set k1)
flipMap = M.foldlWithKey
  (\m k v ->
     M.alter
     (Just . maybe (S.singleton k) (S.insert k))
     v
     m)
  M.empty
