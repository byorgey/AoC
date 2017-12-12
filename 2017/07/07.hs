{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Arrow
import Data.Maybe
import           Data.Map (Map, (!))
import qualified Data.Map               as M
import           Text.Parsec            hiding (State)
import           Text.Parsec.String
import           Data.Tree

data Prog = Prog { nodeName :: String, weight :: Int, children :: [String] }

Part 1
main = do
  s <- getContents
  let nodes = (lines >>> map readProg) s
      pMap  = mkParentMap nodes
      nMap  = mkProgMap nodes
      root  = getRoot pMap
      t     = buildTree root nMap
  putStrLn $ "Root: " ++ root
  putStrLn $ "Fixed weight: " ++ findFixedWeight t

readProg :: String -> Prog
readProg = readParser parseProg
  where
    parseProg = Prog
      <$> (many letter <* space)
      <*> (char '(' *> int <* char ')' <* spaces)
      <*> (fromMaybe [] <$> optionMaybe (string "-> " *> (many letter `sepBy` (char ',' *> spaces))))

mkParentMap :: [Prog] -> Map String String
mkParentMap = concatMap (\(Prog s _ cs) -> map (,s) cs) >>> M.fromList

mkProgMap :: [Prog] -> Map String Prog
mkProgMap = map (nodeName &&& id) >>> M.fromList

getRoot :: Ord a => Map a a -> a
getRoot m = (M.keys >>> head >>> go) m
  where
    go k = case M.lookup k m of
      Nothing -> k
      Just k' -> go k'

getWeight :: Tree (Int,String) -> Int
getWeight (Node (w,_) _) = w

buildTree :: String -> Map String Prog -> Tree (Int, String)
buildTree root nMap = go root
  where
    go n = Node (weight n + map getWeight cs, nodeName n) cs
      where
        cs = map go (children n)

findFixedWeight :: Tree (Int, String) -> Int
findFixedWeight = go Nothing
  where
    go Nothing (Node _ cs) = undefined

------------------------------------------------------------

readParser :: Parser a -> String -> a
readParser p s = case runParser p () "" s of
  Left err -> error (show err)
  Right a  -> a

int :: Parser Int
int = read <$> many1 digit
