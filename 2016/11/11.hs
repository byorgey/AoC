import           Control.Arrow
import           Data.Bits
import           Data.List
import qualified Data.Map      as M
import qualified Data.Set      as S
import           Data.Word
import           Text.Printf

data Substance
  = Promethium
  | Cobalt
  | Curium
  | Ruthenium
  | Plutonium
  | Elerium
  | Dilithium

  -- = Hydrogen
  -- | Lithium
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

substances :: [Substance]
substances = [minBound .. maxBound]

data Thing = Generator | Microchip
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

things :: [Thing]
things = [minBound .. maxBound]

type Item = (Substance, Thing)

items :: [Item]
items = [(s,t) | s <- substances, t <- things]

itemString :: Item -> String
itemString (s,t) = take 2 (show s) ++ take 1 (show t)

type Floor = Word32
floors :: [Floor]
floors = [0..3]

-- We're going to have ~ a million potential states (although many are
-- invalid) so let's make sure they don't use a ton of memory.  2 bits
-- for current floor of each item.  substance 0 generator - substance
-- 0 microchip - substance 1 generator - ...  Finally the last 2 bits
-- are the elevator location.
type PuzzleState = Word32

-- initialExampleState :: PuzzleState
-- initialExampleState
--   = fromMap
--     ( 0
--     , M.fromList
--       [ ((Hydrogen, Microchip), 0)
--       , ((Lithium, Microchip), 0)
--       , ((Hydrogen, Generator), 1)
--       , ((Lithium, Generator), 2)
--       ]
--     )

initialState :: PuzzleState
initialState
  = fromMap
    ( 0
    , M.fromList $
      [ ((Promethium, t), 0) | t <- things ]
      ++
      [ ((e, Generator), 1)  | e <- [Cobalt .. Plutonium] ]
      ++
      [ ((e, Microchip), 2)  | e <- [Cobalt .. Plutonium] ]
    )

initialState2 :: PuzzleState
initialState2
  = fromMap . second (M.union newStuff) . toMap $ initialState
  where
    newStuff = M.fromList [ ((s,t),0) | s <- [Elerium, Dilithium], t <- things ]

-- The winning state is when everything is on the top floor, i.e. all
-- 1's in the bit-field representation.
winningState :: PuzzleState -> Bool
winningState st = (st + 1) == (1 `shiftL` (elevatorShift + 2))

elevatorShift :: Int
elevatorShift = itemShift (maxBound, maxBound) + 2

elevatorFloor :: PuzzleState -> Word32
elevatorFloor st = (st .&. (3 `shiftL` elevatorShift)) `shiftR` elevatorShift

itemShift :: Item -> Int
itemShift (s,t) = (4 * fromEnum s) + (2 * fromEnum t)

itemMask :: Item -> Word32
itemMask i = 3 `shiftL` itemShift i

itemFloor :: Item -> PuzzleState -> Word32
itemFloor i st = (st .&. itemMask i) `shiftR` itemShift i

floorItems :: Floor -> PuzzleState -> [Item]
floorItems f st = filter (\i -> itemFloor i st == f) items

floorItemsSep :: Floor -> PuzzleState -> ({- gens -} [Substance], {- chips -} [Substance])
floorItemsSep f st =
  ( filter (\s -> itemFloor (s,Generator) st == f) substances
  , filter (\s -> itemFloor (s,Microchip) st == f) substances
  )

validState :: PuzzleState -> Bool
validState st = all microchipsSafe floors
  where
    microchipsSafe f = case floorItemsSep f st of
      (_, [])       -> True
      ([], _)       -> True
      (gens, chips) -> all (`elem` gens) chips

-- For debugging

toMap :: PuzzleState -> (Floor, M.Map Item Floor)
toMap st = (elevatorFloor st, M.fromList [(i, itemFloor i st) | i <- items])

fromMap :: (Floor, M.Map Item Floor) -> PuzzleState
fromMap (ef, m) =
  (foldl' xor 0 [(m M.! i) `shiftL` itemShift i | i <- M.keys m])
  `xor`
  (ef `shiftL` elevatorShift)

drawPuzzleState :: PuzzleState -> String
drawPuzzleState st = unlines . map spaceOut . transpose . map reverse $ table
  where
    table :: [[String]]
    table =
      ["F1", "F2", "F3", "F4"]
      :
      layoutItem "E" (elevatorFloor st)
      :
      [ layoutItem (itemString i) (itemFloor i st)
      | i <- items
      ]
    spaceOut :: [String] -> String
    spaceOut [] = ""
    spaceOut [s] = s
    spaceOut (s:ss) = printf "%-4s" s ++ spaceOut ss

layoutItem :: String -> Floor -> [String]
layoutItem str fl =
  let f = fromIntegral fl
  in  replicate f "." ++ [str] ++ replicate (3 - f) "."

-- Graph search

nextStates :: PuzzleState -> [PuzzleState]
nextStates st =
    [ st' | ef' <- filter validFloor [ef-1, ef+1]
          , toMove <- moveableItems
          , let st' = moveItems [toMove] ef' st
          , validState st'
    ]
    ++
    [ st' | ef' <- filter validFloor [ef-1, ef+1]
          , toMove <- choose 2 moveableItems
          , let st' = moveItems toMove ef' st
          , validState st'
    ]
  where
    ef = elevatorFloor st
    moveableItems = floorItems ef st

    validFloor fl = 0 <= fl && fl <= 3
    moveItems is newFloor st
      = moveElevator newFloor
      . foldr (.) id (map (moveItem newFloor) is)
      $ st
    moveItem newFloor i st
      =   (st .&. complement (itemMask i))
      .|. (newFloor `shiftL` (itemShift i))
    moveElevator newFloor st
      =   (st .&. complement (3 `shiftL` elevatorShift))
      .|. (newFloor `shiftL` elevatorShift)

choose 0 _      = [[]]
choose _ []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

-- This finds ALL solution paths.  I was hoping perhaps things are
-- constrained enough that this would not blow up, but it turns out
-- there are a TON of distinct valid solution paths.
dfs :: PuzzleState -> [[PuzzleState]]
dfs start = dfs' S.empty [start] start
  where
    dfs' :: S.Set PuzzleState -> [PuzzleState] -> PuzzleState -> [[PuzzleState]]
    dfs' visited path cur
      | winningState cur = [path]
      | otherwise = concatMap (\n -> dfs' (S.insert n visited) (n:path) n) next
        where
          next = (nextStates cur)

bfs :: PuzzleState -> [S.Set PuzzleState]
bfs start = bfs' S.empty (S.singleton start)
  where
    bfs' :: S.Set PuzzleState -> S.Set PuzzleState -> [S.Set PuzzleState]
    bfs' visited layer
      | any winningState layer = [layer]
      | otherwise = layer : bfs' (S.union visited layer) layer'
        where
          layer'   = (foldMap (S.fromList . nextStates) layer) `S.difference` visited
          -- Note we don't need to subtract layer as well; it's
          -- impossible for a state reachable in n moves to also be
          -- reachable in n+1, because the parity of the elevator
          -- position must strictly alternate.

main = do
--  print . subtract 1 . length $ bfs initialState
  print . subtract 1 . length $ bfs initialState2
