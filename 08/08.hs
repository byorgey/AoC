import           Text.Parsec
import           Text.Parsec.String

type Prog = [Instr]

data Way = Row | Col
  deriving Show

data Instr
  = Rect Int Int
  | Rot Way Int Int
  deriving Show

int :: Parser Int
int = read <$> many1 digit

parseInstr :: Parser Instr
parseInstr =
  Rect <$  try (string "rect ")
       <*> int
       <*  char 'x'
       <*> int
  <|>
  Rot <$   try (string "rotate ")
      <*>  (Row <$ string "row y=" <|> Col <$ string "column x=")
      <*>  int
      <*   string " by "
      <*>  int

readInstr :: String -> Instr
readInstr s = case runParser parseInstr () "" s of
  Left err -> error (show err)
  Right i -> i

data Screen = Screen Int Int (Int -> Int -> Bool)

emptyScreen :: Int -> Int -> Screen
emptyScreen w h = Screen w h (\_ _ -> False)

drawScreen :: Screen -> String
drawScreen (Screen w h f)
  = unlines $ map drawRow [0..h-1]
  where
    drawRow y = map (\x -> if f x y then '#' else '.') [0..w-1]

interpInstr :: Instr -> Screen -> Screen
interpInstr (Rect rw rh) (Screen w h f)
  = Screen w h (\x y -> if (x < rw && y < rh) then True else f x y)
interpInstr (Rot Row r d) (Screen w h f)
  = Screen w h (\x y -> if y == r then f ((x - d) `mod` w) y else f x y)
interpInstr (Rot Col c d) (Screen w h f)
  = Screen w h (\x y -> if x == c then f x ((y - d) `mod` h) else f x y)

interpProg :: Int -> Int -> Prog -> Screen
interpProg w h = foldl (\s i -> interpInstr i s) (emptyScreen w h)

-- Part 1
-- main = interact (show . length . filter (=='#') . drawScreen . interpProg 50 6 . map readInstr . lines)

-- Part 2
main = interact (drawScreen . interpProg 50 6 . map readInstr . lines)
