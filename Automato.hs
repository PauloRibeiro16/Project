import System.IO


totuple :: [String] -> [(String,String)]
totuple [] = []
totuple (x:y:ys) = (x,y) : totuple ys


totreeptuple:: [String] -> [(String,String,String)]
totreeptuple [] = []
totreeptuple (x:y:z:t) = (x,y,z):totreeptuple t

contem:: String -> [(String,String)] -> Bool
contem [] [] = False
contem x [(a,c)]= if a/=x then False else True
contem a ((b,c):xs) = if a == c then True else contem a xs 

contem1:: [String] -> [(String,String)] -> Bool
contem1 [] [] = True 
contem1 (x:xs) y = if contem x y then contem1 xs y else False

split :: String -> [String]
split [] = [""]
split (c:cs) | (c == ','||  c == '(' || c == ')' || c == ';') = "":rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

main = do
    input <- readFile "enable1.txt"
    let [a,b,cs,d] = lines input 
    let firstLine  = split a
    let secLine    = words [if c == ',' then ' ' else c|c <- b]
    let thrtLine   = totuple([x | x <- (split cs), x /= ""]) 
    let frtLine    = totreeptuple([x | x <- (split d), x /= ""])
    print firstLine
    print secLine
    print thrtLine
    print frtLine