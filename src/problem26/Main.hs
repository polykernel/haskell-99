combinations :: Int -> [a] -> [[a]]
combinations =
  let
    comb' :: [a] -> Int -> [a] -> [[a]]
    comb' acc 0 _ = [acc]
    comb' _ n [] = []
    comb' acc n (y : ys) = comb' (y : acc) (n - 1) ys ++ comb' acc n ys
  in comb' []

main :: IO ()
main =
  print (combinations 3 [1, 2, 5, 6, 7]) >>
  print (combinations 3 "abcdef") >>
  print (combinations 4 ([] :: [Int]))
