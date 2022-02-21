data Occurence a = Multiple Int a | Single a deriving Show

decodeModified :: [Occurence a] -> [a]
decodeModified =
  let
    decodeModified' acc [] = acc
    decodeModified' acc (x : xs) =
      case x of
         Multiple n a -> decodeModified' (acc ++ replicate n a) xs
         Single a -> decodeModified' (acc ++ [a]) xs
  in decodeModified' []


main :: IO ()
main =
  print (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e'])
