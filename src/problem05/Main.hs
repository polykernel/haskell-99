myReverse :: [a] -> [a]
myReverse =
  let
    reverse' :: [a] -> [a] -> [a]
    reverse' xs [] = xs
    reverse' xs (y : ys) = reverse' (y : xs) ys
  in reverse' []

main :: IO ()
main =
  print (myReverse ([] :: [Int])) >>
  print (myReverse "A man, a plan, a canal, panama!") >>
  print (myReverse [1,2,3,4])
