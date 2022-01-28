import Data.Maybe

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x : _) 1 = Just x
elementAt (_ : xs) n = elementAt xs (n - 1)

main :: IO ()
main =
  print (elementAt [1] 2) >>
  print (elementAt [1,2,3] 2) >>
  print (elementAt "haskell" 5)
