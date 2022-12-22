#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array --package mtl --package lens --package generic-lens

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

import           Control.Arrow         ((>>>))
import           Control.Lens          (use, (%=), (.=))
import           Control.Monad.State
import           Data.Array.Unboxed
import           Data.Generics.Product
import           GHC.Generics          (Generic)
-- import           Data.Bits
import           Data.Char             (isDigit)
-- import           Data.Function
import           Data.List             (find, findIndex)
import           Data.List.Split       (condense, dropFinalBlank, dropInitBlank,
                                        split, splitOn, whenElt)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)

------------------------------------------------------------

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

------------------------------------------------------------
-- Reading input

data Dir = L | R deriving (Eq, Show)
data Instruction = Move Int | Turn Dir deriving (Eq, Show)
data Input = Input (UArray Coord Char) [Instruction] deriving (Eq, Show)
type Face = Coord

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> (\[b,is] -> Input (readBoard b) (readInstructions is))

readBoard :: [String] -> UArray Coord Char
readBoard rows = listArray ((0,0),(r-1,c-1)) (concatMap pad rows)
  where
    r = length rows
    c = maximum (map length rows)
    pad row = row ++ replicate (c - length row) ' '

readInstructions :: [String] -> [Instruction]
readInstructions [is] = is >$> split (dropFinalBlank . dropInitBlank . condense $ whenElt isDigit) >>> map readInstruction

readInstruction :: String -> Instruction
readInstruction "L" = Turn L
readInstruction "R" = Turn R
readInstruction n   = Move (read n)

------------------------------------------------------------
-- Utilities

applyTurn :: Dir -> Coord -> Coord
applyTurn L (r,c) = (-c,r)
applyTurn R (r,c) = (c,-r)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p = findIndex p >>> fromJust

iter :: Int -> (a -> a) -> a -> a
iter 0 _ = id
iter k f = iter (k-1) f . f

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int,Int)

addCoord :: Coord -> Coord -> Coord
addCoord (r1,c1) (r2,c2) = (r1+r2, c1+c2)

modCoord :: Coord -> Coord -> Coord
modCoord (r,c) (mr,mc) = (r `mod` mr, c `mod` mc)

scaleCoord :: Int -> Coord -> Coord
scaleCoord k (r,c) = (k*r, k*c)

neighbors (r,c) = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

hdg :: Coord -> Int
hdg h = case h of
  (0,1)  -> 0
  (1,0)  -> 1
  (0,-1) -> 2
  (-1,0) -> 3

------------------------------------------------------------
-- State + path-following framework

data St = St { board :: UArray Coord Char, fm :: Map Face [Int], pos :: Coord, heading :: Coord }
  deriving (Eq, Show, Generic)

initSt board = St board (faceMap board) (find' ((board!) >>> (=='.')) [(0,c) | c <- [0..]]) (0,1)

exec :: State St Bool -> Instruction -> State St ()
exec _ (Turn d) = field @"heading" %= applyTurn d
exec next (Move n) = go n
  where
    go 0 = return ()
    go k = do
      moved <- next
      when moved $ go (k-1)

password :: State St Int
password = do
  St _ _ (r,c) h <- get
  return (1000*(r+1) + 4*(c+1) + hdg h)

------------------------------------------------------------
-- Part 1

findNext :: UArray Coord Char -> Coord -> Coord -> Maybe Coord
findNext board p h = case board!p' of
  ' ' -> findNext board p' h
  '#' -> Nothing
  '.' -> Just p'
  where
    (_, (succ -> rows, succ -> cols)) = bounds board
    p' = (p `addCoord` h) `modCoord` (rows, cols)

next1 :: State St Bool
next1 = do
  St board _ p h <- get
  case findNext board p h of
    Just p' -> field @"pos" .= p' >> return True
    _       -> return False

------------------------------------------------------------
-- Part 2

--------------------------------------------------
-- Permutations

{-

   +---2----+
  /|       /|
 3 |      1 |
+---0----+  5
|  6     |  |            up
|  +-10--|--+            /
7 /      4 /     left --+-- right
|11      |9            /
+----8---+           down

-}

-- A Perm tells us which numbered edge is currently at a particular
-- numbered spot.  e.g.  p 0 = 8 tells us that the edge labeled 8 is
-- currently at the top front (in the position of the edge labeled 0
-- in the above picture).
type Perm = Int -> Int

inv :: Perm -> Perm
inv p i = find' (\j -> p j == i) [0 .. 11]

up, down, left, right :: Perm
up = ([8,4,0,7,9,1,3,11,10,5,2,6]!!)
down = inv up
left = ([4,9,5,1,8,10,2,0,7,11,6,3]!!)
right = inv left

bottomFace :: Perm -> [Int]
bottomFace p = map p [8..11]

--------------------------------------------------
-- Face map

faceSize :: UArray Coord Char -> Int
faceSize board = round (sqrt (fromIntegral (count (/= ' ') (elems board) `div` 6)))

-- Each face is identified by its coordinates, found by dividing the
-- coordinates of any cell in the face by the face size.
-- The faceMap records, for each face, the labels on the edges around that face,
-- in the order   +-2-+
--                3   1
--                +-0-+   .

faceMap :: UArray Coord Char -> Map Face [Int]
faceMap board = execState (dfs startFace id) M.empty
  where
    f = faceSize board
    (_,(succ -> rows, succ -> cols)) = bounds board
    faceRows = rows `div` f
    faceCols = cols `div` f

    startFace = find' (\(r,c) -> board!(f*r, f*c) /= ' ') [(0,c) | c <- [0..]]

    dfs face@(r,c) perm = do
      fm <- get
      if r < 0 || r >= faceRows || c < 0 || c >= faceCols || board!(f*r,f*c) == ' ' || face `M.member` fm
        then return ()
        else do
          modify $ M.insert face (bottomFace perm)
          dfs (r-1,c) (perm . up)
          dfs (r+1,c) (perm . down)
          dfs (r,c-1) (perm . left)
          dfs (r,c+1) (perm . right)

--------------------------------------------------
-- Part 2 movement

getEdgeAndMatchingFace :: State St (Maybe (Int, Face))
getEdgeAndMatchingFace = do
  St board fm p@(r,c) h@(dr,dc) <- get
  let f = faceSize board
      (fr,fc) = (r `div` f, c `div` f)
      edgeIndex = case (r `mod` f, dr, c `mod` f, dc) of
        (r',1,_,_) | r' == f-1 -> Just 0
        (0,-1,_,_)             -> Just 2
        (_,_,c',1) | c' == f-1 -> Just 1
        (_,_,0,-1)             -> Just 3
        _                      -> Nothing
      medge = ((fm M.! (fr,fc))!!) <$> edgeIndex
  case medge of
    Nothing -> return Nothing
    Just edge -> do
      let otherFace = find' (/=(fr,fc)) . map fst $ filter (snd >>> (edge `elem`)) (M.assocs fm)
      return $ Just (edge, otherFace)

wrap :: Int -> Face -> Face -> State St Bool
wrap edge origFace destFace = do
  St board fm p h <- get
  let f = faceSize board
      origEdgeIndex = findIndex' (==edge) (fm M.! origFace)
      destEdgeIndex = findIndex' (==edge) (fm M.! destFace)
      rot = (destEdgeIndex + 2 - origEdgeIndex) `mod` 4

      h' = iter rot (applyTurn L) h
      p' = iter rot (rotSquare f) (p `modCoord` (f,f))
      p'' = ((p' `addCoord` h') `modCoord` (f,f)) `addCoord` scaleCoord f destFace
  case board!p'' of
    '#' -> return False
    '.' -> field @"pos" .= p'' >> field @"heading" .= h' >> return True

rotSquare :: Int -> Coord -> Coord
rotSquare f (r,c) = (f-1-c,r)

next2 :: State St Bool
next2 = do
  St board fm p@(r,c) h@(dr,dc) <- get
  let p'@(r',c') = (r+dr,c+dc)
      f = faceSize board
      newFace = (r' `div` f, c' `div` f)
  if newFace `M.member` fm
    then
      if board!p' == '.'
        then field @"pos" .= p' >> return True
        else return False
    else do
      mef <- getEdgeAndMatchingFace
      let Just (e, face) = mef
      wrap e (r `div` f, c `div` f) face

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA = solve next1
solveB = solve next2

solve next (Input board is) = evalState (mapM_ (exec next) is >> password) (initSt board)
