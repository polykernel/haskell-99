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

pack :: Eq a => [a] -> [[a]]
pack = groupBy (==)

main :: IO ()
main =
  print (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']) >>
  print (pack ([] :: [Int])) >>
  print (pack [1, 2, 3, 3, 4, 4])
