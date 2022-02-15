pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs =
  let
    pack' :: Eq a => [a] -> [a] -> [[a]]
    pack' acc [] = [acc]
    pack' [] (x : xs) = pack' [x] xs
    pack' acc y@(x : xs)
      | x == head acc = pack' (x : acc) xs
      | otherwise = acc : (pack' [] y)
  in pack' [] xs


main :: IO ()
main =
  print (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']) >>
  print (pack ([] :: [Int])) >>
  print (pack [1, 2, 3, 3, 4, 4])
