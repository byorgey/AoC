{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Text.Parsec            hiding (State)
import           Text.Parsec.String

data Group = Group [Either String Group]

parseGroup :: Parser Group
parseGroup = Group <$> (char '{' *> (parseGroupOrGarbage `sepBy` char ',') <* char '}')

parseGroupOrGarbage :: Parser (Either String Group)
parseGroupOrGarbage
  =   Left  <$> parseGarbage
  <|> Right <$> parseGroup

parseGarbage :: Parser String
parseGarbage = char '<' *> many cancel *> many (noneOf ">" <* many cancel) <* char '>'
  where
    cancel = char '!' >> anyChar

score :: Group -> Int
score = go 1 . Right
  where
    go :: Int -> Either String Group -> Int
    go _ (Left _)           = 0
    go s (Right (Group gs)) = s + sum (map (go (s+1)) gs)

countGarbage :: Group -> Int
countGarbage (Group gs) = sum (map (either length countGarbage) gs)

readParser :: Parser a -> String -> a
readParser p s = case runParser p () "" s of
  Left err -> error (show err)
  Right a  -> a

main = do
  g <- readParser parseGroup <$> getContents

  print (score g)

  print (countGarbage g)

