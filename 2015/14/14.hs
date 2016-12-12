import           Data.Char
import           Data.List
import           Data.Ord

-- Name, speed, on, off
data Reindeer = Reindeer String Integer Integer Integer
  deriving Show

reindeerName :: Reindeer -> String
reindeerName (Reindeer s _ _ _) = s

readReindeer str = Reindeer (head (words str)) a b c
  where
    [a,b,c] = map read . words . map reveal $ str
    reveal c
      | isDigit c = c
      | otherwise = ' '

-- How far can the given reindeer travel in the given time?
distance :: Integer -> Reindeer -> Integer
distance time (Reindeer name speed on off) = mainDistance + extraDistance
  where
    mainDistance  = (time `div` (on + off)) * speed * on
    extraTime = time `mod` (on + off)
    extraDistance = (min on extraTime) * speed

main = do
  f <- getContents
  let rs = map readReindeer (lines f)
  print $ maximum (map (distance 2503) rs)

  print . maximum . map length . group . sort . map reindeerName
    $ map (\d -> maximumBy (comparing $ distance d) rs) [1..2503]
