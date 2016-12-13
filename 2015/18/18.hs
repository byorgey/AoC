import           Data.Array
import           Data.List

type Grid = Array (Int,Int) Bool

readGrid :: String -> Grid
readGrid s = array ((0,0), maximum . map fst $ elts) elts
  where
    elts = concat
         . zipWith (\r -> zipWith (\c x -> ((r,c),x=='#')) [0..]) [0..]
         . lines
         $ s

drawGrid :: Grid -> String
drawGrid g
  = unlines
  . (map . map) (\b -> if b then '#' else '.')
  $ (map . map) (g!) [ [ (r,c) | c <- [0..mc] ] | r <- [0..mr] ]
  where
    (_, (mr,mc)) = bounds g

step :: Grid -> Grid
step g = array (bounds g) [((r,c), alive r c) | r <- [0..mc], c <- [0..mc]]
  where
    (_, (mr,mc)) = bounds g
    alive r c = let n = neighborCount r c
                in  n == 3 || ((g ! (r,c)) && n == 2)
    neighborCount r c
      = length . filter id . map (g!) . delete (r,c) . filter valid
      $ [(r+dr,c+dc) | dr <- [-1..1], dc <- [-1..1]]
    valid (r,c) = 0 <= r && r <= mr && 0 <= c && c <= mc

countLights :: Grid -> Int
countLights = length . filter id . elems

cornersOn :: Grid -> Grid
cornersOn g = g // [((r,c),True) | r <- [lr,mr], c <- [lc,mc]]
  where ((lr,lc),(mr,mc)) = bounds g

main = do
  g <- readGrid <$> getContents
  -- mapM_ (putStrLn . drawGrid) . take 5 $ iterate step g
  print . countLights . (!!100) . iterate step $ g

  print . countLights . (!!100) . iterate (cornersOn . step) $ cornersOn g
