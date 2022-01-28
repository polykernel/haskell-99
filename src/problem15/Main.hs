repli :: [a] -> Int -> [a]
repli xs n = concatMap (\_ -> xs) [1..n]

main :: IO ()
main =
  print (repli "abc" 3)
