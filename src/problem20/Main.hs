import Data.Maybe
import Data.List

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs = 
  let
    iterate' :: ((Maybe a, [a]), Int) -> a -> ((Maybe a, [a]), Int)
    iterate' ((v, r), acc) x
      | acc == 0 = ((Just x, r), acc - 1)
      | otherwise = ((v, x : r), acc - 1)
  in fst $ foldl' iterate' ((Nothing, []), length xs - n) (reverse xs)

main :: IO ()
main =
  print (removeAt 2 [1, 2, 3, 4]) >>
  print (removeAt 4 [1, 2, 3, 4]) >>
  print (removeAt 5 [1, 2, 3]) >>
  print (removeAt 2 "abcd")
