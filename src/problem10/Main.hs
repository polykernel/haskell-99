import Data.Maybe

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = 
  let
    encode' :: Eq a => (Int, Maybe a) -> [a] -> [(Int, a)]
    encode' (len, a) [] = [(len, fromJust a)]
    encode' (0, _) (x : xs) = encode' (1, Just x) xs
    encode' acc@(len, a) y@(x : xs)
      | x == fromJust a = encode' (len + 1, a) xs
      | otherwise = (len, fromJust a) : (encode' (0, Nothing) y)
  in encode' (0, Nothing) xs


main :: IO ()
main =
  print (encode "aaaabccaadeeee") >>
  print (encode ([] :: [Int])) >>
  print (encode [1, 2, 2, 3, 3, 3, 4])
