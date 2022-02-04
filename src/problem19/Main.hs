import Data.Ratio

rotate :: [a] -> Int -> [a]
rotate xs n =
  let
    rsplit' :: [a] -> Int -> ([a], [a])
    rsplit' (x : xs) 1 = (xs, [x])
    rsplit' (x : xs) n = let (l, f) = rsplit' xs (n - 1) in (l, x : f)
  in uncurry (++) $ rsplit' xs (mod n (length xs))

main :: IO ()
main =
  print (rotate ['a','b','c','d','e','f','g','h'] 3) >>
  print (rotate ['a','b','c','d','e','f','g','h'] (-2))
