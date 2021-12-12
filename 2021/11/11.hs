{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Data.List (partition)
import           Data.Map            (Map, (!))
import qualified Data.Map.Strict     as M

------------------------------------------------------------

data St = St { _grid :: Map (Int,Int) Int, _flashers :: [(Int,Int)], _flashes :: Int }
  deriving Show

makeLenses ''St

------------------------------------------------------------

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = Map (Int,Int) Int

readInput :: String -> Input
readInput = lines >>> map (map ((:[]) >>> read)) >>>
  (flip zipWith [0 .. 9] $ \r -> flip zipWith [0 .. 9] $ \c -> ((r,c),)) >>>
  concat >>> M.fromList

type Output = Int

solveA, solveB :: Input -> Output
solveA m = _flashes (execState (replicateM_ 100 step) (St m [] 0))
solveB m = evalState (replicateM 1000 (get <* step)) (St m [] 0) >$>
  takeWhile (not . allFlashed) >>> length

------------------------------------------------------------

step :: State St ()
step = do
  grid %= fmap (+1)
  g <- use grid
  flashers .= (g >$> M.assocs >>> filter (snd >>> (>9)) >>> map fst)

  flashAll

flashAll :: State St ()
flashAll = do
  g <- use grid
  fs <- use flashers
  case fs of
    []   -> return ()
    f:fs -> do
      flashers .= fs
      when (g!f > 0) $ do
        flashes += 1
        let (fs',ns) = partition ((g!) >>> (>= 9)) $ filter ((g!) >>> (>0)) (neighbors f)
        forM (fs' ++ ns) $ \n -> grid . ix n += 1
        flashers %= (fs'++)
        grid . ix f .= 0
      flashAll

neighbors :: (Int, Int) -> [(Int,Int)]
neighbors l@(r,c) =
  [ l'
  | dr <- [-1..1], dc <- [-1..1], let l' = (r+dr,c+dc)
  , l' /= l, valid l'
  ]

valid (r,c) = 0 <= r && r < 10 && 0 <= c && c < 10

drawMap :: Map (Int,Int) Int -> String
drawMap m = unlines $ map (concatMap ((m!) >>> show)) [[(r,c) | c <- [0..9]] | r <- [0..9]]

printMap :: Map (Int,Int) Int -> IO ()
printMap = putStr . drawMap

allFlashed :: St -> Bool
allFlashed = view grid >>> all (==0)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
