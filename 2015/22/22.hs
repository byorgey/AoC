{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.List       ((\\))
import           Data.Maybe
import           Data.Monoid
import qualified Data.PQueue.Min as PQ
import qualified Data.Set        as S
import           Text.Printf

import           Debug.Trace

type Mana = Int

data Spell
  = MagicMissile
  | Drain
  | Shield
  | Poison
  | Recharge
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

spells :: [Spell]
spells = [minBound .. maxBound]

spellCost :: Spell -> Int
spellCost MagicMissile = 53
spellCost Drain        = 73
spellCost Shield       = 113
spellCost Poison       = 173
spellCost Recharge     = 229

data Effect = Effect
  { _effectSpell :: Spell
  , _effectTimer :: Int
  }
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _hp            :: Int
  , _armor         :: Int
  , _mana          :: Int
  , _playerTurn    :: Bool
  , _bossHP        :: Int
  , _bossDamage    :: Int
  , _effects       :: S.Set Effect
  , _manaSpent     :: Int
  , _spellSequence :: [Spell]
  }
  deriving (Eq, Show)

instance Ord GameState where
  compare g1 g2 = compare (_manaSpent g1) (_manaSpent g2)

initGameState :: GameState
initGameState = GameState 0 0 0 True 0 0 S.empty 0 []

makeLenses ''Effect
makeLenses ''GameState

exampleState :: GameState
exampleState = initGameState
  & hp         .~ 10
  & mana       .~ 250
  & bossHP     .~ 13
  & bossDamage .~ 8

exampleState2 :: GameState
exampleState2 = exampleState
  & bossHP     .~ 14

next :: Bool -> GameState -> [GameState]
next = next' spells

next' :: [Spell] -> Bool -> GameState -> [GameState]
next' spellsToUse hardMode g
  | winning g'            = [g']
  | (g' ^. hp <= 0)       = []
  | not (g ^. playerTurn) = validate [g' & bossAttack]
  | otherwise             = validate $ map (flip cast g') (spellsToUse \\ activeSpells)
  where
    g' = g & handleHard & handleEffects & playerTurn %~ not
    activeSpells = map (^. effectSpell) (S.toList (g' ^. effects))
    handleHard st
      | hardMode && (st ^. playerTurn) = st & hp -~ 1
      | otherwise = st

simulate :: Bool -> GameState -> [Spell] -> [GameState]
simulate _ g [] = [g]
simulate hardMode g (s:ss)
  | (g ^. playerTurn) = g : maybe [] (flip (simulate hardMode) ss)     g'
  | otherwise         = g : maybe [] (flip (simulate hardMode) (s:ss)) g'
  where
    g' = listToMaybe (next' [s] hardMode g)

validate :: [GameState] -> [GameState]
validate = filter valid
  where
    valid g = (g ^. hp) > 0 && (g ^. mana) >= 0

winning :: GameState -> Bool
winning g = (g ^. bossHP) <= 0

bossAttack :: GameState -> GameState
bossAttack g = g & hp -~ max 1 ((g ^. bossDamage) - (g ^. armor))

applyAll :: Foldable f => f (a -> a) -> a -> a
applyAll = ala Endo foldMap

handleEffects :: GameState -> GameState
handleEffects g
  = g & applyAll ((applyEffect . view effectSpell) <$> (S.toList $ g ^. effects))
      & effects %~ S.map (effectTimer -~ 1)
      & expireEffects
  where
    applyEffect :: Spell -> GameState -> GameState
    applyEffect Poison   = bossHP -~ 3
    applyEffect Recharge = mana   +~ 101
    applyEffect _        = id

    expireEffects :: GameState -> GameState
    expireEffects g = g & expireSome & effects .~ remaining
      where
        (expiring, remaining) = S.partition ((<=0) . view effectTimer) (g ^. effects)
        expireSome = applyAll ((expireOne . view effectSpell) <$> S.toList expiring)

        expireOne :: Spell -> GameState -> GameState
        expireOne Shield = armor -~ 7
        expireOne _      = id


cast :: Spell -> GameState -> GameState
cast spell
  = (mana -~ spellCost spell)
  . (manaSpent +~ spellCost spell)
  . (spellSequence %~ (spell:))
  . specific spell
  where
    specific MagicMissile = bossHP -~ 4
    specific Drain        = (bossHP -~ 2) . (hp +~ 2)
    specific Shield       = (armor +~ 7) . (effects %~ S.insert (Effect Shield 6))
    specific Poison       = effects %~ S.insert (Effect Poison 6)
    specific Recharge     = effects %~ S.insert (Effect Recharge 5)

main = do
  [bhp, dmg, php, pmana] <- (map (read . last . words) . lines) <$> getContents
  let st = initGameState
         & hp         .~ php
         & mana       .~ pmana
         & bossHP     .~ bhp
         & bossDamage .~ dmg

  putStrLn "Easy mode:"
  putStrLn $ analyze False st

  putStrLn "Hard mode:"
  putStrLn $ analyze True  st

analyze :: Bool -> GameState -> String
analyze hardMode st =
  case bestWin hardMode st of
    Nothing -> "Player cannot win."
    Just g  -> printf "Player can win by casting: %s\nTotal mana cost: %d"
                 (show . reverse $ g ^. spellSequence) (g ^. manaSpent)


bestWin :: Bool -> GameState -> Maybe GameState
bestWin hardMode start = bestWin' (PQ.singleton start)
  where
    bestWin' pq = case PQ.minView pq of
      Nothing -> Nothing
      Just (g, pq') ->
        case winning g of
          True  -> Just g
          False -> bestWin' (foldr PQ.insertBehind pq' (next hardMode g))
