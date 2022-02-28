split :: [a] -> Int -> ([a], [a])
split [] n = ([], [])
split all@(x : xs) n
  | n <= 0 = ([], all)
  | otherwise = let (f, b) = split xs (n - 1) in (x : f, b)

main :: IO ()
main =
  print (split "abcdefghijk" 3) >>
  print (split ([] :: [Int]) 1) >>
  print (split "abc" (-1)) >>
  print (split "gcd" 1) >>
  print (split [1,2,3,5,6,7] 6)
