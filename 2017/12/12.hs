{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import qualified Data.Map               as M
import qualified Data.Set               as S
import           Text.Parsec            hiding (State)
import           Text.Parsec.String

parseConn :: Parser (Int,S.Set Int)
parseConn = (,) <$> int <*> (string " <-> " *> (S.fromList <$> (int `sepBy` (string ", "))))

main = do
  conns <- (map (readParser parseConn) . lines) <$> getContents
  let connMap = M.fromList conns
  --  print (connMap M.! 0)

  -- Part 1
  print (S.size $ dfs (connMap M.!) 0)

  -- Part 2
  print (components connMap)

components :: M.Map Int (S.Set Int) -> Int
components connMap = go (M.keysSet connMap)
  where
    go remaining
      | S.null remaining = 0
      | otherwise = 1 + go (remaining `S.difference` dfs (connMap M.!) start)
      where
        start = S.fold const 0 remaining


------------------------------------------------------------
-- Utilities

dfs :: Ord a => (a -> S.Set a) -> a -> S.Set a
dfs fnext = dfs' S.empty
  where
    dfs' visited cur
      | cur `S.member` visited = S.empty
      | otherwise = S.singleton cur `S.union` (S.fold S.union S.empty $ S.map (\n -> dfs' (S.insert cur visited) n) next)
        where
          next = fnext cur

readParser :: Parser a -> String -> a
readParser p s = case runParser p () "" s of
  Left err -> error (show err)
  Right a  -> a

int :: Parser Int
int = read <$> many1 digit
