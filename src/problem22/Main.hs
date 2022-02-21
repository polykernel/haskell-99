range :: Int -> Int -> [Int]
range n m
  | n <= m =  n : (range (n + 1) m)
  | otherwise = []

main :: IO ()
main =
  print (range 4 9) >>
  print (range 3 2) >>
  print (range 1 1)
