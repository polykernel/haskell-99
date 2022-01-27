data NestedList a = Elem a | List [NestedList a]

-- TODO: rewrite using the bind operator (>>=).
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap (flatten) xs

main :: IO ()
main =
  print (flatten (Elem 5)) >>
  print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) >>
  print (flatten (List ([] :: [NestedList Int])))
