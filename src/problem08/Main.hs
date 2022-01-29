import Data.List

compress :: Eq a => [a] -> [a]
compress = map head . (groupBy (==))

main :: IO ()
main =
  print (compress "aaaabccaadeeee") >>
  print (compress [1, 2, 3, 4, 4, 4, 5, 5]) >>
  print (compress ([] :: [Int]))
