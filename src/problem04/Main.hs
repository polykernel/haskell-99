import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

main :: IO ()
main =
  print (myLength ([] :: [Int])) >>
  print (myLength [123, 456, 789]) >>
  print (myLength "Hello, world!")
