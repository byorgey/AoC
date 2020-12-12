{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State

------------------------------------------------------------

data V2 s = V2 { getX :: !s, getY :: !s } deriving (Eq, Ord, Show, Functor)
type P2 s = V2 s

instance Foldable V2 where
  foldMap f (V2 x y) = f x <> f y

zero :: Num s => V2 s
zero = V2 0 0

(^+^), (^-^) :: Num s => V2 s -> V2 s -> V2 s
V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1+x2) (y1+y2)
V2 x1 y1 ^-^ V2 x2 y2 = V2 (x1-x2) (y1-y2)

(*^) :: Num s => s -> V2 s -> V2 s
(*^) k = fmap (k*)

perp :: Num s => V2 s -> V2 s
perp (V2 x y) = V2 (-y) x

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

------------------------------------------------------------

data St = St { _heading :: V2 Int, _loc :: V2 Int }
makeLenses ''St

-- Which thing do N,S,E,W commands move?
data Navigation = Ship | Waypoint deriving (Eq, Ord, Show)

initSt :: Navigation -> St
initSt Ship     = St (V2 1 0) zero
initSt Waypoint = St (V2 10 1) zero

main = interact $
  readInput >>> applyAll [solve Ship, solve Waypoint] >>> map show >>> unlines

type Input = [Instruction]
data Instruction = I Action Int
data Action = N | S | E | W | L | R | F deriving (Eq, Ord, Show, Read)

readInput :: String -> Input
readInput = lines >>> map readInstruction
  where
    readInstruction (a:n) = I (read [a]) (read n)

type Output = Int

exec :: Navigation -> Instruction -> State St ()
exec n (I a v) | a `elem` [N,S,E,W]
  = (case n of { Ship -> loc; Waypoint -> heading }) %= (^+^ (v *^ dir a))
exec n (I R v)   = exec n (I L (360 - v))
exec _ (I L 90)  = heading %= perp
exec _ (I L 180) = heading %= (perp . perp)
exec _ (I L 270) = heading %= (perp . perp . perp)
exec _ (I F v)   = do
  h <- use heading
  loc %= (^+^ (v *^ h))

dir N = V2 0 1
dir S = V2 0 (-1)
dir E = V2 1 0
dir W = V2 (-1) 0

solve :: Navigation -> Input -> Output
solve n prog = execState (mapM_ (exec n) prog) (initSt n) >$> view loc >>> manhattan

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
