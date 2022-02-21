data Occurence a = Multiple Int a | Single a deriving Show

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

encodeModified :: Eq a => [a] -> [Occurence a]
encodeModified = map (\(n, x) -> if n == 1 then Single x else Multiple n x) . encode

main :: IO ()
main =
  print (encodeModified "aaaabccaadeeee") >>
  print (encodeModified ([] :: [Int])) >>
  print (encodeModified [1, 2, 2, 3, 3, 3, 4])
