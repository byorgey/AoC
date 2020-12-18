import           Control.Comonad.Sheet
import           Data.Bool
import           Data.List             (intersperse)
import           Prelude               hiding (take)
import qualified Prelude               as P

data Cell = X | O deriving ( Eq , Show )
type Universe = Sheet3 Cell
type Ruleset = ([Int],[Int]) -- list of numbers of neighbors to trigger
                             -- being born, and staying alive, respectively

life :: Ruleset -> [[Cell]] -> Universe
life ruleset seed = evaluate $ insert [map (map const) seed] blank
   where blank = sheet (const X) (repeat . tapeOf . tapeOf $ rule)
         rule place  = case (neighbors place `elem`) `onBoth` ruleset of
                            (True,_) -> O
                            (_,True) -> cell inward place
                            _        -> X
         neighbors   = length . filter (O ==) . cells bordering
         bordering   = map (inward &) (diagonals ++ verticals ++ horizontals)
         diagonals   = (&) <$> horizontals <*> verticals
         verticals   =        [above, below]
         horizontals = map d2 [right, left]

onBoth :: (a -> b) -> (a,a) -> (b,b)
f `onBoth` (x,y) = (f x,f y)

conway :: [[Cell]] -> Universe
conway = life ([3],[2,3])

printLife :: Int -> Int -> Int -> Universe -> IO ()
printLife c r t = mapM_ putStr
   .            ([separator '┌' '─' '┐'] ++)
   .         (++ [separator '└' '─' '┘'])
   . intersperse (separator '├' '─' '┤')
   . map (unlines . map (("│ " ++) . (++ " │")) . frame)
   . take (rightBy c & belowBy r & outwardBy t)
   where
      separator x y z = [x] ++ P.replicate (1 + (1 + c) * 2) y ++ [z] ++ "\n"
      frame = map $ intersperse ' ' . map (bool ' ' '●' . (O ==))

countLife :: Int -> Int -> Int -> Universe -> Int
countLife c r t = sum . map (sum . map (length . filter (==O))) . take (rightBy c & belowBy r & outwardBy t)

glider :: Universe
glider = conway [[X,X,O],
                 [O,X,O],
                 [X,O,O]]
