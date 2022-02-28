dropEvery :: [a] -> Int -> [a]
dropEvery =
  let
    drop :: [a] -> [a] -> Int -> [a]
    drop' acc [] _ = acc
    drop' acc (_ : xs) 0 = drop' acc xs n
    drop' acc (x : xs) k = drop' (x : acc) xs (k - 1)
  in reverse $ drop' []


main :: IO ()
main =
  print (dropEvery "abcdefghik" 3)
