{-# LANGUAGE TemplateHaskell #-}

import           Text.Parsec
import           Text.Parsec.String

import           Control.Arrow      (first)
import           Control.Lens
import           Data.Char
import           Data.List          (isInfixOf, sort)
import qualified Data.Map           as M
import           Data.Ord
import           Data.Tuple

data Room = Room
  { _roomName   :: [String]
  , _roomSector :: Int
  , _roomCheck  :: String
  }
  deriving Show

makeLenses ''Room

parseRoom :: Parser Room
parseRoom = Room <$> (many1 lower `sepEndBy` char '-')
                 <*> (read <$> many digit)
                 <*> (char '[' *> many1 lower <* char ']')

checksum :: String -> String
checksum
  = map snd
  . take 5
  . sort
  . (map . first) Down
  . map swap
  . M.assocs
  . foldr (\c -> M.insertWith (+) c 1) M.empty

validRoom :: Room -> Bool
validRoom (Room ss _ chk) = checksum (concat ss) == chk

doParse :: Parser a -> String -> a
doParse p s = case runParser p () "" s of
  Left err -> error (show err)
  Right a  -> a

decryptRoom :: Room -> String
decryptRoom r = unwords $ (map . map) decryptChar (r ^. roomName)
  where
    decryptChar c = chr ((ord c - ord 'a' + (r ^. roomSector)) `mod` 26 + ord 'a')

-- Part 1
-- main = interact (show . sum . map (view roomSector) . filter validRoom . map (doParse parseRoom) . lines)

-- Part 2
main = interact
  ( show
  . filter (("pole" `isInfixOf`) . decryptRoom)
  . filter validRoom . map (doParse parseRoom) . lines
  )
