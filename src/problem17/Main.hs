split :: [a] -> Int -> ([a], [a])
split xs n
  | n < 0 = ([], xs)
  | otherwise =
    let
      splitL' :: [a] -> Int -> [a]
      splitL' [] _ = []
      splitL' _ 0 = []
      splitL' (y : ys) k = y : (splitL' ys (k - 1))
      splitR' :: [a] -> Int -> [a]
      splitR' [] _ = []
      splitR' (y : ys) 0 = y : (splitR' ys 0)
      splitR' (_ : ys) k = splitR' ys (k - 1)
    in (splitL' xs n, splitR' xs n)

main :: IO ()
main =
  print (split "abcdefghijk" 3) >>
  print (split ([] :: [Int]) 1) >>
  print (split "abc" (-1)) >>
  print (split "gcd" 0) >>
  print (split [1,2,3,5,6,7] 6)
