#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package lens --package mtl

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow (second, (&&&), (>>>))
import Control.Lens (ix, makeLenses, use, (%=), (+=), (.=))
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.State (State, evalState, execState)
import Data.List (foldl')
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as M
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)

------------------------------------------------------------

type Name = String
data ModuleType = Plain | FlipFlop | Conjunction deriving (Eq, Ord, Show)
data ModuleDesc = MD {moduleName :: String, moduleType :: ModuleType, outputs :: [Name]}
    deriving (Eq, Show)
type Input = [ModuleDesc]

readInput :: String -> Input
readInput = lines >>> map readModule

readModule :: String -> ModuleDesc
readModule =
    filter (/= ',')
        >>> words
        >>> \((readModuleName -> (ty, nm)) : _ : outs) -> MD nm ty outs

readModuleName :: String -> (ModuleType, Name)
readModuleName = \case
    ('%' : n) -> (FlipFlop, n)
    ('&' : n) -> (Conjunction, n)
    n -> (Plain, n)

------------------------------------------------------------

data PulseType = Low | High deriving (Eq, Ord, Show, Enum)

invPulse :: PulseType -> PulseType
invPulse = fromEnum >>> (1 -) >>> toEnum

(&&.) :: PulseType -> PulseType -> PulseType
High &&. p = p
Low &&. _ = Low

data Pulse = Pulse {pulseType :: PulseType, pulseSource :: Name, pulseDest :: Name} deriving (Eq, Show)
data Module where
    P :: Module
    F :: PulseType -> Module
    C :: Map Name PulseType -> Module
    deriving (Show)
data St = St
    { _pulseQueue :: !(Seq Pulse)
    , _modules :: !(Map String Module)
    , _pulseCount :: !(Map PulseType Int)
    , _dests :: !(Map Name [Name])
    , _rx :: !Bool
    , _pushCount :: !Int
    }
    deriving (Show)

makeLenses ''St

initSt :: Input -> St
initSt mds =
    St
        { _pulseQueue = Seq.empty
        , _modules = M.fromList $ map (moduleName &&& mkModule) (mds ++ orphans)
        , _pulseCount = M.fromList [(Low, 0), (High, 0)]
        , _dests = M.fromList $ map (moduleName &&& outputs) (mds ++ orphans)
        , _rx = False
        , _pushCount = 0
        }
  where
    -- Have to compute the INPUTS of each module, for conjunction modules
    inputs :: Map Name [Name]
    inputs = invertMap (M.fromList $ map (moduleName &&& outputs) mds)

    mkModule :: ModuleDesc -> Module
    mkModule md = case moduleType md of
        Plain -> P
        FlipFlop -> F Low
        Conjunction -> C . M.fromList $ map (,Low) (inputs ! moduleName md)

    orphans :: [ModuleDesc]
    orphans = map mkPlain (S.toList $ allOutputs `S.difference` listedModules)
      where
        allOutputs = S.fromList $ concatMap outputs mds
        listedModules = S.fromList $ map moduleName mds

        mkPlain n = MD n Plain []

-- Simulate one round of button-pushing
sim :: State St ()
sim = pushButton >> run
  where
    run = do
        more <- simPulse
        when more run

-- Push button until rx gets a low pulse.  This is surely not going to
-- work.
runToRx :: State St Int
runToRx = do
    sim
    r <- use rx
    c <- use pushCount
    -- traceShowM c
    if r then return c else runToRx

pushButton :: State St ()
pushButton = do
    pushCount += 1
    sendPulse $ Pulse Low "button" "broadcaster"

sendPulse :: Pulse -> State St ()
sendPulse p = do
    pulseQueue %= (Seq.|> p)
    pulseCount . ix (pulseType p) += 1

-- Simulate a single pulse; return a Bool indicating whether
-- there are any pulses remaining to simulate in the queue
simPulse :: State St Bool
simPulse = do
    q <- use pulseQueue
    case Seq.viewl q of
        Seq.EmptyL -> return False
        (p Seq.:< q') -> do
            pulseQueue .= q'
            receivePulse p
            return True

receivePulse :: Pulse -> State St ()
receivePulse (Pulse ty src dest) = do
    m <- use modules
    when (dest == "rx" && ty == Low) $ rx .= True
    -- Receive pulse and decide what pulse to send, if any
    mToSend <- case m ! dest of
        P -> return $ Just ty
        F f -> case ty of
            High -> return Nothing
            Low -> do
                let f' = invPulse f
                modules . ix dest .= F f'
                return $ Just f'
        C inputs -> do
            let inputs' = M.insert src ty inputs
                output = invPulse $ foldl' (&&.) High (M.elems inputs')
            modules . ix dest .= C inputs'
            return $ Just output

    -- Broadcast pulse to all outputs
    case mToSend of
        Nothing -> return ()
        Just toSend -> do
            ds <- use dests
            forM_ (ds ! dest) $ sendPulse . Pulse toSend dest

------------------------------------------------------------

toDot :: [ModuleDesc] -> String
toDot mds =
    unlines $
        ["digraph G {"]
            ++ map moduleVertex mds
            ++ map (\(a, b) -> a ++ " -> " ++ b ++ ";") edges
            ++ ["}"]
  where
    moduleVertex md = moduleName md ++ " [shape=" ++ moduleShape (moduleType md) ++ "]"
    moduleShape = \case
        Plain -> "point"
        FlipFlop -> "diamond"
        Conjunction -> "invtrapezium"
    edges = concatMap (\md -> map (moduleName md,) (outputs md)) mds

------------------------------------------------------------

main =
    interact $
        readInput
            >>> applyAll [solveA, solveB]
            >>> map show
            >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Output = Int

solveA, solveB :: Input -> Output
solveA = initSt >>> execState (replicateM_ 1000 sim) >>> _pulseCount >>> M.elems >>> product
solveB _ = foldl' lcm 1 [0b111111010011, 0b111101001101, 0b111110101101, 0b111011010001]

------------------------------------------------------------

invertMap :: (Ord a, Ord b) => Map a [b] -> Map b [a]
invertMap = M.assocs >>> concatMap strength >>> map (swap >>> second (: [])) >>> M.fromListWith (++)

strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = fmap (a,) fb
