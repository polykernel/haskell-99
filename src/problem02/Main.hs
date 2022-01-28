import Data.Maybe

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (_ : []) = Nothing
myButLast (x : (_ : [])) = Just x
myButLast (_ : xs) = myButLast xs

main :: IO ()
main =
  print (myButLast ([] :: [Int])) >>
  print (myButLast [1]) >>
  print (myButLast [1, 2, 3, 4]) >>
  print (myButLast ['x', 'y', 'z'])
