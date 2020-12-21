{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Arrow   hiding (left, right)
import           Control.Lens    (both)
import           Data.Array
import           Data.Bool       (bool)
import           Data.Char       (isDigit)
import           Data.Function   (on)
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)
import           Data.Ord        (comparing)
import           Prelude         hiding (flip)
import qualified Prelude         as P

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type ID = Int
data Tile = Tile { tileID :: ID, grid :: [[Bool]] } deriving Show
type Input = [Tile]

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> filter (not.null) >>> map readTile

readTile :: [String] -> Tile
readTile (n:g) = Tile (read (filter isDigit n)) (map (map (=='#')) g)

type Output = Int

------------------------------------------------------------

-- Use the minimum of an edge and its reverse as the canonical
-- representative for that edge pattern.
normalize :: Edge -> Edge
normalize e = minimum [e, reverse e]

-- Generate the list of normalized representatives for the edges of a tile.
normalized :: Tile -> [Edge]
normalized (Tile _ g) = map normalize [head g, last g, head g', last g']
  where
    g' = transpose g

-- Make a dictionary from edge representatives to tiles containing
-- that edge.
type EdgeMap = Map Edge [Tile]

mkEdgeMap :: Input -> EdgeMap
mkEdgeMap = map (\t -> map (,[t]) (normalized t)) >>> concat >>> M.fromListWith (++)

isCorner :: EdgeMap -> Tile -> Bool
isCorner e t = count (isUnmatched e) (normalized t) == 2

isUnmatched :: EdgeMap -> Edge -> Bool
isUnmatched edgeMap = normalize >>> (edgeMap #!) >>> length >>> (==1)

------------------------------------------------------------
-- Part A

type Edge = [Bool]

-- The input has the special property that every possible edge pattern
-- occurs on exactly 1 or 2 tiles.  So just identify the four tiles
-- that have two edge patterns that occur only once; those have to be
-- the corners.  Kind of cheating but not really: taking special
-- structure of the input into account is part of the game!

solveA :: Input -> Output
solveA ts = ts >$> filter (isCorner edgeMap) >>> map tileID >>> product
  where
    edgeMap = mkEdgeMap ts

------------------------------------------------------------
-- Part B

-- As I suspected, for Part B we can't get away so easily. But given
-- the uniqueness of the edges we can just pick an arbitrary corner to
-- start with, and proceed with matching up edges.  The tricky part is
-- finding the correct orientation of each tile, but for that we can
-- just try every possible orientation and find the one that matches.

-- Orientable things -----------------------------

class Orientable t where
  rotate :: t -> t
  flip   :: t -> t

orientations :: Orientable t => t -> [t]
orientations t = rotations t ++ rotations (flip t)
  where
    rotations = iterate rotate >>> take 4

-- Tile utilities --------------------------------

instance Orientable Tile where
  rotate (Tile t g) = Tile t (map reverse . transpose $ g)
  flip (Tile t g) = Tile t (map reverse g)

bottom, left, right, top :: Tile -> Edge
bottom (Tile _ g) = last g
left   (Tile _ g) = head (transpose g)
right  (Tile _ g) = last (transpose g)
top    (Tile _ g) = head g

-- TileGrids -------------------------------------

type TileGrid = Array (Int,Int) Tile

-- Put together a list of tiles into a tile grid, flipping and
-- rotating each tile so they all match.  This would be really hard in
-- general, but we assume (as with the given inputs) that each edge of
-- each tile matches a unique other tile edge.
makeGrid :: [Tile] -> TileGrid
makeGrid ts = a
  where
    a :: TileGrid
    a = array ((1,1),(n,n)) $

      -- Pick an arbitrary corner tile to start.
      [ ((1,1), corner) ]
      ++

      -- Fill in the first column, matching each tile with the one
      -- already placed above.
      [ ((i,1), t')
      | i <- [2..n]
      , let neighbor = a!(i-1,1)
      , let [t'] = [ t'
                   | t <- edgeMap #! (normalize (bottom neighbor))
                   , tileID t /= tileID neighbor
                   , t' <- orientations t
                   , top t' == bottom neighbor
                   ]
      ]
      ++

      -- Now fill in the rest, matching each with the one already
      -- placed to the left.
      [ ((i,j), t')
      | i <- [1..n]
      , j <- [2..n]
      , let neighbor = a!(i,j-1)
      , let [t'] = [ t'
                   | t <- edgeMap #! (normalize (right neighbor))
                   , tileID t /= tileID neighbor
                   , t' <- orientations t
                   , left t' == right neighbor
                   ]
      ]

    edgeMap = mkEdgeMap ts

    n :: Int
    n = round . sqrt @Double . fromIntegral . length $ ts

    corner :: Tile
    corner = ts >$> filter (isCorner edgeMap) >>> head >>>
      orientations >>>
      find (\t -> (isUnmatched edgeMap (left t) && isUnmatched edgeMap (top t))) >>>
      fromJust

-- Images ----------------------------------------

type Image = Array (Int,Int) Bool

instance Orientable Image where
  rotate g = ixmap (bounds g) (\(r,c) -> (c,n-r)) g
    where
      (_,(n,_)) = bounds g

  flip g = ixmap (bounds g) (\(r,c) -> (r,n-c)) g
    where
      (_,(n,_)) = bounds g

-- Turn a grid of tiles into a single image, removing the border strips.
gridToImage :: TileGrid -> Image
gridToImage g = array ((0,0),(n-1,n-1))
  [ ((r,c), x)
  | r <- [0 .. n-1], c <- [0 .. n-1]
  , let tile = g!(r `div` tileSize + 1, c `div` tileSize + 1)
  , let x = (grid tile !! (r `mod` tileSize + 1)) !! (c `mod` tileSize + 1)
  ]
  where
    (_,(numTiles,_)) = bounds g
    tileSize = length (grid (g!(1,1))) - 2
    n = tileSize * numTiles

showImage :: Image -> String
showImage = assocs >>> groupBy ((==) `on` (fst.fst)) >>> map (map (snd >>> bool '.' '#')) >>> unlines

-- A SEA MONSTER
seaMonster :: Image
seaMonster = listArray ((0,0),(r-1,c-1)) . map (=='#') . concat $ img
  where
    r = length img
    c = length (head img)
    img =
      [ "                  # "
      , "#    ##    ##    ###"
      , " #  #  #  #  #  #   "
      ]

-- Count the number of times the first image occurs somewhere in the
-- second.
countMatches :: Image -> Image -> Int
countMatches needle haystack = count matchesAt [(r,c) | r <- [0..rH-rN], c <- [0..cH-cN]]
  where
    (rN,cN) = snd (bounds needle)
    (rH,cH) = snd (bounds haystack)

    matchesAt :: (Int,Int) -> Bool
    matchesAt (x,y) =
      all (\(r,c) -> (needle!(r,c)) ==> (haystack!(r+x,c+y))) (range (bounds needle))

    x ==> y = not x || y

-- Count the number of 'on' pixels in an image.
popCount :: Image -> Int
popCount = elems >>> count id

-- Finally, the solution to part 2. Match up the tiles into a grid,
-- and turn it into an image.  Try all possible orientations of the
-- image, and keep the one in which the maximum number of sea monsters
-- occur.  Count the number of non-sea-monster pixels by subtracting
-- the pixel count of the number of matched sea monsters from the
-- pixel count of the image.
solveB :: Input -> Output
solveB = makeGrid >>> gridToImage >>> orientations >>>
  map (id &&& countMatches seaMonster) >>>
  maximumBy (comparing snd) >>>
  (\(img, numMatches) -> popCount img - numMatches * popCount seaMonster)

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = P.flip ($)

(#!) :: Ord k => Map k a -> k -> a
(#!) = (M.!)
