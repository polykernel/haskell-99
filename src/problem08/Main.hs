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

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

compress :: Eq a => [a] -> [a]
compress = map head . group

main :: IO ()
main =
  print (compress "aaaabccaadeeee") >>
  print (compress [1, 2, 3, 4, 4, 4, 5, 5]) >>
  print (compress ([] :: [Int]))
