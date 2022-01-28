import Data.Maybe

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_ : xs) = myLast xs

main :: IO ()
main =
  print (myLast ([] :: [Int])) >>
  print (myLast [1, 2, 3, 4]) >>
  print (myLast ['x', 'y', 'z'])
