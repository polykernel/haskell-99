groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p (x : xs) =
  let
    go :: (a -> a -> Bool) -> [a] -> [a] -> [[a]]
    go _ acc [] = [acc]
    go p acc (y : ys)
      | p y (head acc) = go p (y : acc) ys
      | otherwise = acc : (go p [y] ys)
  in go p [x] xs

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . groupBy (==)

main :: IO ()
main =
  print (encode "aaaabccaadeeee") >>
  print (encode ([] :: [Int])) >>
  print (encode [1, 2, 2, 3, 3, 3, 4])
