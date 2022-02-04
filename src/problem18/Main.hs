slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ 1 0 = []
slice (x : xs) 1 k = x : slice xs 1 (k - 1)
slice (_ : xs) n k
  | n <= k = slice xs (n - 1) (k - 1)
  | otherwise = []

main :: IO ()
main =
  print (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7) >>
  print (slice ([] :: [Int]) 1 1) >>
  print (slice [1,2,3,4,5] 4 5) >>
  print (slice "abcdefgh" 10 12)
