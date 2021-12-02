{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.List
import Linear

data Cmd = Vert Int | Horiz Int
  deriving (Eq, Ord, Show)

type Input = [Cmd]

data St = St { _aim :: V2 Int, _pos :: V2 Int }

makeLenses ''St

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

readInput :: String -> Input
readInput = lines >>> map (words >>> readCmd)

readCmd ["forward",n] = Horiz (read n)
readCmd ["up",n] = Vert (-read n)
readCmd ["down",n] = Vert (read n)

asV (Vert n) = V2 0 n
asV (Horiz n) = V2 n 0

type Output = Int

solveA :: Input -> Output
solveA = map asV >>> foldl' (^+^) zero >>> productOf traverse

solveB :: Input -> Output
solveB cmds = productOf (pos . traverse) $ execState (mapM_ exec cmds) (St (V2 1 0) (V2 0 0))
  where
    exec (Horiz n) = modify (\(St a p) -> St a (p ^+^ (n *^ a)))
    exec (Vert n)  = aim %= (^+^ (V2 0 n))

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
