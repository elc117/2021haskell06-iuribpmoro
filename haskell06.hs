-- Nome: Iuri Bernardo Picolini Moro

ends :: [Int] -> [Int]
ends (x:xs) = x : [last xs]


deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame (xs)


deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs


geraTabela :: Int -> [(Int,Int)]
geraTabela n = if n > 0
    then (n,n^2): geraTabela(n-1)
    else []


contido :: Char -> String -> Bool
contido char "" = False
contido char (x:xs) = if char == x
    then True 
    else contido char xs


translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x,y):xs) = (x+2,y+2) : translate (xs)


countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if length x > 5
    then 1 + countLongs (xs)
    else 0 + countLongs (xs)


onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5
    then x : onlyLongs (xs)
    else onlyLongs (xs)