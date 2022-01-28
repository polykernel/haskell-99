import qualified GHC.List as List

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = List.and . take (List.length xs `div` 2) $ zipWith (==) xs (reverse xs)

main :: IO ()
main =
  print (isPalindrome ([] :: [Int])) >>
  print (isPalindrome [1,2,3]) >>
  print (isPalindrome "madamimadam") >>
  print (isPalindrome [1,2,4,8,16,8,4,2,1])
