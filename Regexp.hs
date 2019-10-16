import System.IO
import Text.ParserCombinators.Parsec
import Text.Show


type Stater = String
type Clock = String
type Numbers = Int

data Expr =  Cl Clock|
            Trans (Char, Expr) |
            St Stater |
            Conj (Expr,Expr) deriving (Show)




csvFile = endBy line eol
line = sepBy cell (char ',' <|> char ';' <|> char '(' <|> char ')')
cell = many (noneOf ")(,;\n")
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> fail "Couldn't find EOL"

parseS::String-> Expr
parseS x = St x

parseC:: String->Expr
parseC x = Cl x


stringL :: String -> Parser String
stringL [] = return []
stringL (c:cs) = do { char c; stringL cs; return (c:cs)}

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input




main = do
    input <- readFile "enable1.txt"
    let l = parseCSV(input)
    print l