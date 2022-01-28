dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
  let
    drop' acc [] _ = acc
    drop' acc (x : xs) 0 = drop' acc xs n
    drop' acc (x : xs) k = drop' (x : acc) xs (k - 1)
  in reverse $ drop' [] xs n


main :: IO ()
main =
  print (dropEvery "abcdefghik" 3)
