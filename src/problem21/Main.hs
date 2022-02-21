insertAt :: a -> [a] -> Int -> [a]
insertAt z xs 1 = z : xs
insertAt _ [] _ = []
insertAt z (x : xs) n = x : (insertAt z xs (n - 1))

main :: IO ()
main =
  print (insertAt 'X' "abcd" 2) >>
  print (insertAt 0 ([] :: [Int]) 2) >>
  print (insertAt "abc" ["abcd", "efgh"] 3)
