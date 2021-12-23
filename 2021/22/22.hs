#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Arrow              ((***), (>>>))
import           Control.Lens               (Lens', makeLenses, (&), (.~), (^.))
import           Control.Monad              (replicateM)
import           Data.Array                 (Array, bounds, listArray, (!))
import           Data.Function              (on)
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Void                  (Void)
import           Text.Megaparsec            hiding (Pos, region)
import           Text.Megaparsec.Char       (char, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

------------------------------------------------------------
-- Boxes

data Pos = Pos { _x :: !Int, _y :: !Int, _z :: !Int } deriving (Eq, Ord, Show)

data Box = Box { _lo :: !Pos, _hi :: !Pos } deriving (Eq, Ord, Show)

makeLenses ''Pos
makeLenses ''Box

data Axis = X | Y | Z deriving (Eq, Ord, Show, Read, Enum, Bounded)

axisLens :: Axis -> Lens' Pos Int
axisLens = \case
  X -> x
  Y -> y
  Z -> z

volume :: Box -> Int
volume (Box (Pos x1 y1 z1) (Pos x2 y2 z2)) = (x2 -. x1) * (y2 -. y1) * (z2 -. z1)
  where
    x -. y = max 0 (x - y + 1)

posMax (Pos x1 y1 z1) (Pos x2 y2 z2) = Pos (max x1 x2) (max y1 y2) (max z1 z2)
posMin (Pos x1 y1 z1) (Pos x2 y2 z2) = Pos (min x1 x2) (min y1 y2) (min z1 z2)

intersection :: Box -> Box -> Box
intersection (Box c11 c12) (Box c21 c22) = Box (posMax c11 c21) (posMin c12 c22)

disjoint :: Box -> Box -> Bool
disjoint b1 b2 = volume (intersection b1 b2) == 0

intersect :: Box -> Box -> Bool
intersect b1 b2 = not (disjoint b1 b2)

------------------------------------------------------------
-- Main

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

data Step = Step { turnOn :: Bool, region :: Box } deriving (Eq, Show)
type Input = [Step]

readInput :: String -> Input
readInput = lines >>> map (readParser parseStep)

type Output = Int

solveA, solveB :: Input -> Output
solveA = filter (intersect initRegion . region) >>> foldl' execStep S.empty >>> S.size
solveB i =
  i >$> filter (not . intersect initRegion . region) >>>
  reverse >>> foldl' execStep' [] >>> filter snd >>> map (fst >>> volume) >>> sum >>> (+r)
  where
    r = solveA i

------------------------------------------------------------

initRegion :: Box
initRegion = Box (Pos (-50) (-50) (-50)) (Pos 50 50 50)

execStep :: Set Pos -> Step -> Set Pos
execStep cubes (Step o reg) = (if o then S.union else S.difference) cubes (boxToSet reg)

boxToSet :: Box -> Set Pos
boxToSet (Box (Pos xl yl zl) (Pos xh yh zh))
  = S.fromList [Pos x y z | x <- [xl..xh], y <- [yl..yh], z <- [zl..zh]]

boxGraphEdges :: [Box] -> [(Int,Int)]
boxGraphEdges = zip [0..] >>> pairs >>>
  filter (uncurry intersect . (snd *** snd)) >>>
  map (fst *** fst)

stepGraph :: [Step] -> (Array Int Step, Step -> Step -> Bool)
stepGraph ss = (listArray (0, length ss - 1) ss, intersect `on` region)

toDot :: [(Int,Int)] -> String
toDot edges = unlines $
  [ "graph G {" ]
  ++
  map (\v -> show v ++ " [shape=point]") vertices
  ++
  map (\(a,b) -> show a ++ " -- " ++ show b ++ ";") edges
  ++
  [ "}" ]
  where
    vertices = S.toList . S.fromList $ concatMap (\(a,b) -> [a,b]) edges

nextDimension :: Array Int a -> (a -> a -> Bool) -> Set (Set Int) -> Set (Set Int)
nextDimension vs e = S.toList >>> pairs >>> filter (uncurry clique) >>> map (uncurry S.union) >>> S.fromList
  where
    clique f1 f2 = and
      [ S.size (f1 `S.intersection` f2) == S.size f1 - 1
      , e
          ((vs!) . head . S.toList $ S.difference f1 f2)
          ((vs!) . head . S.toList $ S.difference f2 f1)
      ]

isClique :: Array Int a -> (a -> a -> Bool) -> Set Int -> Bool
isClique vs e = S.toList >>> pairs >>> all (uncurry (e `on` (vs!)))

faces :: Array Int a -> (a -> a -> Bool) -> [Set (Set Int)]
faces vs e
  = takeWhile ((>0) . S.size) . iterate (nextDimension vs e)
  . S.fromList . map S.singleton $ [0 .. n]
  where
    n = snd (bounds vs)

slice :: Axis -> Int -> Box -> [Box]
slice axis v box
  | box ^. lo . axisLens axis > v || box ^. hi . axisLens axis <= v = [box]
  | otherwise = [box & hi . axisLens axis .~ v, box & lo . axisLens axis .~ v+1]

sliceBy :: Box -> Box -> [Box]
sliceBy b1 b2
  | disjoint b1 b2 = [b1]
  | otherwise = foldl' doSlice [b1] (sliceAxis X ++ sliceAxis Y ++ sliceAxis Z)
  where
    sliceAxis :: Axis -> [(Axis, Int)]
    sliceAxis axis =
      [ (axis, (b2 ^. lo . axisLens axis) - 1)
      , (axis, b2 ^. hi . axisLens axis)
      ]

    doSlice bs (axis, v) = concatMap sliceOne bs
      where
        sliceOne b
          | disjoint b b2 = [b]
          | otherwise     = slice axis v b

remove :: Box -> Box -> [Box]
remove b1 b2 = filter (disjoint b2) (b1 `sliceBy` b2)

-- Do a step in *reverse* order.
execStep' :: [(Box,Bool)] -> Step -> [(Box,Bool)]
execStep' bs (Step onoff b) = map (,onoff) (foldl' delete [b] bs) ++ bs
  where
    delete frags (toDel,_) = concatMap (`remove` toDel) frags

------------------------------------------------------------
-- Parsing

type Parser = Parsec Void String

parseStep :: Parser Step
parseStep = Step <$> parseOnOff <*> (space *> parseBox)

parseOnOff :: Parser Bool
parseOnOff = (True <$ string "on") <|> (False <$ string "off")

parseBox :: Parser Box
parseBox = mkBox <$> (parseLoHi `sepBy` char ',')
  where
    mkBox = unzip >>> (mkPos *** mkPos) >>> uncurry Box
    mkPos [x,y,z] = Pos x y z

parseLoHi :: Parser (Int,Int)
parseLoHi = (,) <$> (anySingle *> char '=' *> signed space decimal) <*> (string ".." *> signed space decimal)

------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

readParser p = parse p "" >>> either undefined id
