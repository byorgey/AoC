import           Text.Parsec
import           Text.Parsec.String

import           Codec.Picture

data Switch = On | Off | Toggle
  deriving Show

type Coord = (Int, Int)
type Rect = (Coord, Coord)

validRect :: Rect -> Bool
validRect ((x1,y1),(x2,y2)) = x1 <= x2 && y1 <= y2

inRect :: Coord -> Rect -> Bool
inRect (x,y) ((x1,y1),(x2,y2)) = x1 <= x && x <= x2 && y1 <= y && y <= y2

data Instr = Instr Switch Rect
  deriving Show

getRect :: Instr -> Rect
getRect (Instr _ r) = r

parseInstr :: Parser Instr
parseInstr =
  Instr <$> parseSwitch <* char ' ' <*> parseRect

  where
    parseSwitch =
          On     <$ try (string "turn on")
      <|> Off    <$ try (string "turn off")
      <|> Toggle <$ string "toggle"
    parseRect =
      (,) <$> parseCoord <* string " through " <*> parseCoord
    parseCoord =
      (,) <$> int <* char ',' <*> int
    int = read <$> many1 digit

readInstr :: String -> Instr
readInstr s = case runParser parseInstr () "" s of Right i -> i

type Lights = Coord -> Bool

interpInstr :: Lights -> Instr -> Lights
interpInstr ls (Instr s r) =
  \c -> if inRect c r
          then (case s of {On -> True; Off -> False; Toggle -> not (ls c)})
          else ls c

interpProg :: [Instr] -> Lights
interpProg = foldl interpInstr (const False)

countLights :: Lights -> Int
countLights ls = length [() | x <- [0..999], y <- [0..999], ls (x,y)]

saveLights :: FilePath -> Lights -> IO ()
saveLights fn ls = do
  let img    = ImageRGB8 $ generateImage (\r c -> toPixel $ ls (r,c)) 1000 1000
  savePngImage fn img

toPixel :: Bool -> PixelRGB8
toPixel True = PixelRGB8 255 255 255
toPixel False = PixelRGB8 0 0 0

type Lights2 = Coord -> Int

interpInstr2 :: Lights2 -> Instr -> Lights2
interpInstr2 ls (Instr s r) =
  \c -> if inRect c r
           then (switchFunction s (ls c))
           else ls c
  where
    switchFunction On  x = x+1
    switchFunction Off x = max 0 (x-1)
    switchFunction Toggle x = x+2

interpProg2 :: [Instr] -> Lights2
interpProg2 = foldl interpInstr2 (const 0)

sumLights :: Lights2 -> Int
sumLights ls = sum [ls (x,y) | x <- [0..999], y <- [0..999]]

main = do
  f <- getContents
  let prog = map readInstr (lines f)
      lights1 = interpProg prog
  print (countLights lights1)
  -- saveLights "lights1.png" lights1

  let lights2 = interpProg2 prog
  print (sumLights lights2)
