module Baseutils (capitalized, formatNumber) where
  
import qualified Data.Char as Char

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

formatNumber :: Int -> String
formatNumber = intersperseN ' ' 3 . show

intersperseN :: a -> Int -> [a] -> [a]
intersperseN x n = uncurry (<>) . foldr alg ([], [])
  where
    alg a (buf', acc)
      | length buf >= n = ([], (x:buf) <> acc)
      | otherwise = (buf, acc)
      where buf = a:buf'