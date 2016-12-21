import           Data.List
import           Data.Maybe

data I
  = SwapPos Int Int
  | SwapLet Char Char
  | Rot Int
  | RotPos Char
  | Rev Int Int
  | Move Int Int

  | UnRotPos Char
  deriving Show

readI :: String -> I
readI s = case words s of
  ["swap","position",x,_,_,y] -> SwapPos (read x) (read y)
  ["swap","letter",[x],_,_,[y]] -> SwapLet x y
  ["rotate","left",x,_] -> Rot (-read x)
  ["rotate","right",x,_] -> Rot (read x)
  ["rotate","based",_,_,_,_,[x]] -> RotPos x
  ["reverse",_,x,_,y] -> Rev (read x) (read y)
  ["move",_,x,_,_,y]  -> Move (read x) (read y)

type Str = (Int, Int -> Char)

toStr :: String -> Str
toStr s = (length s, (s !!))

fromStr :: Str -> String
fromStr (len,s) = map s [0 .. len-1]

infix 1 !
(!) :: Str -> Int -> Char
(_,s) ! i = s i

infix 0 .=
(.=) :: Int -> Char -> Str -> Str
(i .= c) (len,s) = (len, \x -> if x == i then c else s x)

interp :: I -> Str -> Str
interp (SwapPos x y) s = (x .= s ! y) . (y .= s ! x) $ s
interp (SwapLet a b) (len,s) = (len, \i -> if s i == a then b else (if s i == b then a else s i))
interp (Rot r) (len,s) = (len, \i -> s ((i - r) `mod` len))
interp (RotPos c) s = interp (Rot (p + 1 + (if p >= 4 then 1 else 0))) s
  where
    p = fromJust $ findIndex (==c) (fromStr s)
interp (UnRotPos c) s = interp (Rot ((-1) - (p `div` 2) + (if even p then (4 + (if p == 0 then 4 else 0)) else 0))) s
  where
    p = fromJust $ findIndex (==c) (fromStr s)

interp (Rev x y) (len,s) = (len, \i -> if (x <= i && i <= y) then s (x + y - i) else s i)
interp (Move x y) (len,s)
  | x <= y = (len, \i -> if x <= i && i < y then s (i+1) else (if i == y then (s x) else s i))
  | x > y  = (len, \i -> if y < i && i <= x then s (i-1) else (if i == y then (s x) else s i))

invert :: I -> I
invert (Rot r) = Rot (-r)
invert (RotPos c) = UnRotPos c
invert (Move x y) = Move y x
invert i = i  -- SwapPos, SwapLet, Rev

input = "abcdefgh"
-- input = "abcde"

input2 = "fbgdceah"

main = do
  instrs <- (map readI . lines) <$> getContents

  putStrLn . fromStr $ foldl' (flip interp) (toStr input) instrs
  putStrLn . fromStr $ foldl' (flip interp) (toStr input2) (reverse . map invert $ instrs)

