data Occurence a = Multiple Int a | Single a deriving Show

toOccurence :: Int -> a -> Occurence a
toOccurence n a
  | n == 1 = Single a
  | otherwise = Multiple n a

encodeModified :: Eq a => [a] -> [Occurence a]
encodeModified [] = []
encodeModified (x : xs) =
  let
    go :: Eq a => Int -> a -> [a] -> [Occurence a]
    go n z [] = [toOccurence n z]
    go n z (y : ys)
      | y == z = go (n + 1) z ys
      | otherwise = (toOccurence n z) : (go 1 y ys)
  in go 1 x xs

main :: IO ()
main =
  print (encodeModified "aaaabccaadeeee") >>
  print (encodeModified ([] :: [Int])) >>
  print (encodeModified [1, 2, 2, 3, 3, 3, 4])
