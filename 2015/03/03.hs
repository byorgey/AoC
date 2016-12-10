{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import qualified Data.Set            as S
import           Data.Tuple

type Loc = (Int, Int)

move :: Char -> Loc -> Loc
move '^' (x,y) = (x,y+1)
move 'v' (x,y) = (x,y-1)
move '<' (x,y) = (x-1,y)
move '>' (x,y) = (x+1,y)
move _   l     = l

data St = St
  { _loc    :: Loc
  , _houses :: S.Set Loc
  }

makeLenses ''St

initSt = St (0,0) (S.singleton (0,0))

interp :: String -> St
interp str = execState (mapM_ interpOne str) initSt
  where
    interpOne :: Char -> State St ()
    interpOne i = do
      newLoc <- loc <%= move i
      houses %= S.insert newLoc

deinterleave [] = ([],[])
deinterleave (x:xs) = first (x:) (swap (deinterleave xs))

main = do
  script <- getLine
  print . S.size . view houses . interp $ script
  let (script1, script2) = deinterleave script
      h1 = view houses . interp $ script1
      h2 = view houses . interp $ script2
  print . S.size $ S.union h1 h2
