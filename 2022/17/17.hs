#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package lens --package mtl --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Arrow       (first, (***), (>>>))
import           Control.Lens        (imap, makeLenses, use, (%=), (+=), (-=),
                                      (.=), (<-=), (^.))
import           Control.Monad.State
import           Data.Array
import           Data.List           (foldl')
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as S

------------------------------------------------------------

type Coord = (Int,Int)

above, below, left, right :: Coord -> Coord
above (r,c) = (r-1,c)
below (r,c) = (r+1,c)
left (r,c) = (r,c-1)
right (r,c) = (r,c+1)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

------------------------------------------------------------

type Jet = Char
type Jets = [Jet]
type Rock = [Coord]

readRocks :: FilePath -> IO [Rock]
readRocks rocksFile = do
  f <- readFile rocksFile
  return $ f >$> lines >>> splitOn [""] >>> map readRock

readRock :: [String] -> Rock
readRock = imap (\r -> imap (\c -> ((r,c),))) >>> concat >>> filter (snd >>> (=='#')) >>> map fst

data Board = Board { _topRow :: Int, _filled :: Set Coord } deriving (Eq, Show)

makeLenses ''Board

showBoard :: Jet -> Maybe Rock -> Maybe Int -> Board -> String
showBoard j mr mt (Board tr cs) = unlines $
  [ show tr ]
  ++
  [ lt : [ showCell (r,c) | c <- [1..7]] ++ [rt] | r <- [tr', tr'+1 .. maybe (-1) (tr'+) mt] ]
  ++
  ["+-------+"]
  where
    rock = fromMaybe [] mr
    tr' = case rock of { [] -> tr; _ -> min tr (minimum . map fst $ rock) }
    lt = if j == '>' then j else '|'
    rt = if j == '<' then j else '|'

    showCell p
      | p `elem` rock = '@'
      | p `S.member` cs = '#'
      | otherwise = '.'

emptyBoard :: Board
emptyBoard = Board 0 S.empty

initRockPosition :: Board -> Rock -> Rock
initRockPosition b r = map ((+ (b ^. topRow - 4 - maxRow)) *** (+3)) r
  where
    maxRow = maximum . map fst $ r

------------------------------------------------------------

data St = St
  { _curJets            :: Jets
  , _curBoard           :: Board
  , _rockArray          :: Array Int Rock
  , _rockIndex          :: Int
  , _rocksLeft          :: Int
  , _jetCycle           :: Int
  , _rockCount          :: Int
  , _prevCycleRockCount :: Int
  , _prevCycleHeight    :: Int
  , _savedHeight        :: Int
  }
  deriving (Eq, Show)

makeLenses ''St

initSt :: Jets -> Int -> [Rock] -> St
initSt js n rocks =
  St js emptyBoard (listArray (0,length rocks - 1) rocks) 0 n (-1) 0 0 0 0

showSt :: Maybe Int -> Maybe Rock -> St -> String
showSt mt mr st = showBoard (head (st ^. curJets)) mr mt (st ^. curBoard)

nextJet :: State St (Coord -> Coord)
nextJet = do
  js <- use curJets
  curJets %= tail
  case head js of
    ' ' -> do
      jetCycle += 1
      nextJet
    '<' -> return left
    _   -> return right

sim :: Int -> [Rock] -> Jets -> St
sim n rocks js = execState runSim (initSt js n rocks)

runSim :: State St ()
runSim = do
  b <- use curBoard
  i <- use rockIndex
  rs <- use rockArray
  let r = rs!i
  fall (initRockPosition b r)
  rockCount += 1
  rockIndex += 1
  rockIndex %= (`mod` (snd (bounds rs) + 1))
  rl <- rocksLeft <-= 1
  case rl > 0 of
    True -> runSim
    False -> do
      p <- use prevCycleHeight
      h <- height
      savedHeight += (h - p)

height :: State St Int
height = negate <$> use (curBoard . topRow)

fall :: Rock -> State St ()
fall r0 = do
  js <- use curJets
  when (head js == ' ') $ do
    st <- get
    prevR <- use prevCycleRockCount

    -- traceM (showSt (Just 10) (Just r0) st)

    case prevR == 0 of

      -- If this is just the first cycle, record the rock count + height
      True -> do
        rc <- use rockCount
        prevCycleRockCount .= rc
        h <- height
        prevCycleHeight .= h

      -- Otherwise, it's going to repeat.  Ascertained by hand that it
      -- repeats after the first jet cycle for my real input, although
      -- that's not true for the sample input so this doesn't work in
      -- general!  Spent a long time trying to debug on the sample
      -- input before realizing this.
      False -> do
        h <- height
        ph <- use prevCycleHeight
        prevCycleHeight .= h
        let heightPeriod = h - ph

        n <- use rockCount
        pn <- use prevCycleRockCount
        prevCycleRockCount .= n
        let rockPeriod = n - pn

        l <- use rocksLeft
        savedHeight .= h + (l `div` rockPeriod) * heightPeriod

        rocksLeft .= l `mod` rockPeriod

  b <- use (curBoard . filled)
  j <- nextJet
  let r1 = fromMaybe r0 $ moveRock b j r0
  case moveRock b below r1 of
    Just r2 -> fall r2
    Nothing -> freeze r1

moveRock :: Set Coord -> (Coord -> Coord) -> Rock -> Maybe Rock
moveRock b m r0
  | rockOK b r1 = Just r1
  | otherwise = Nothing
  where
    r1 = map m r0

rockOK :: Set Coord -> Rock -> Bool
rockOK b = all unblocked
  where
    unblocked p@(r,c) = r < 0 && c > 0 && c < 8 && p `S.notMember` b

freeze :: Rock -> State St ()
freeze r = do
  curBoard . topRow %= min (minimum . map fst $ r)
  curBoard . filled %= (\cs -> foldl' (flip S.insert) cs r)

------------------------------------------------------------

main = do
  rocks <- readRocks "rocks.txt"
  interact $
    init >>> applyAll [solveA rocks, solveB rocks] >>> map show >>> unlines

solveA, solveB :: [Rock] -> Jets -> Int
solveA rocks jets = sim 2022 rocks (cycle jets) >$> (^. curBoard . topRow) >>> negate
solveB rocks jets = sim (10^12) rocks (cycle (' ':jets)) >$> (^. savedHeight)
