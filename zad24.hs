{- 
24) Dla danej liczby naturalnej n podaj największą, jeśli istnieje, parę liczb zaprzyjaźnionych a i b
takich, ze a≤ n, b≤ n.
220 i 284
1184 i 1210
2620 i 2924
5020 i 5564
6232 i 6368
10744 i 10856
12285 i 14595
17296 i 18416
63020 i 76084
66928 i 66992
67095 i 71145
69615 i 87633
79750 i 88730
100485 i 124155
-}

sumaDzielnikow :: Int -> Int
sumaDzielnikow a = sum [i | i <- [1..(a `div` 2)], a `mod` i == 0] -- liczenie sumy dzielnikow danej liczby

szukajPrzyjaciol :: Int -> Maybe (Int, Int)
szukajPrzyjaciol n = go n
  where
    go 0 = Nothing
    go i =
      let a1 = sumaDzielnikow i -- wyznacznie kandydata na liczbę zaprzyjaznioną z i
          b1 = sumaDzielnikow a1 -- sprawdzanie sumy dzielników kandydata kandydat 
      in if i == b1 && a1 /= b1 && b1 <= n && a1 <= n -- porównanie sumy dzielników kandydata z i, sprawdzanie czy liczby mieszczą się w przedziale (0,n>
         then Just (a1, b1)
         else go (i - 1)
         
main :: IO ()
main = do
  putStrLn "Wprowadz liczbe:"
  userInput <- getLine
  let n = read userInput :: Int
  putStrLn $ "n = " ++ show n
  case szukajPrzyjaciol n of
    Just (a, b) -> putStrLn $ "a = " ++ show a ++ ", b = " ++ show b
    Nothing     -> putStrLn "brak przyjaciol"