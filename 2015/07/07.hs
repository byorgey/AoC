import           Data.Bits
import           Data.Map           ((!))
import qualified Data.Map           as M
import           Data.Word
import           Text.Parsec
import           Text.Parsec.String

type Wire = String

data Input
  = Pass Wire
  | Val Word16
  | NOT Wire
  | AND Wire Wire
  | MASK Wire        -- special case for 1 AND xx
  | OR Wire Wire
  | LSHIFT Wire Int
  | RSHIFT Wire Int
  deriving Show

data Component = Component Input Wire
  deriving Show

outputWire :: Component -> Wire
outputWire (Component _ w) = w

parseComponent :: Parser Component
parseComponent =
  Component
    <$> parseInput
    <*  string " -> "
    <*> parseWire

parseWire :: Parser Wire
parseWire = many1 (oneOf ['a'..'z'])

parseInput :: Parser Input
parseInput
  =        NOT <$ string "NOT " <*> parseWire
  <|> try (AND <$> parseWire <* string " AND " <*> parseWire)
  <|> try (MASK <$ string "1 AND " <*> parseWire)
  <|> try (OR  <$> parseWire <* string " OR " <*> parseWire)
  <|> try (LSHIFT <$> parseWire <* string " LSHIFT " <*> val)
  <|> try (RSHIFT <$> parseWire <* string " RSHIFT " <*> val)
  <|> Val <$> val
  <|> Pass <$> parseWire

val :: Read a => Parser a
val = read <$> many1 digit

readCircuit :: String -> [Component]
readCircuit = map readComponent . lines
  where
    readComponent l = case runParser parseComponent () "" l of
      Left err -> error (show err)
      Right c  -> c

runCircuit :: [Component] -> M.Map Wire Word16
runCircuit comps = outputs
  where
    outputs = M.fromList $ map setResult comps
    setResult (Component inp out) = (out, eval inp)
    eval (Pass w)     = outputs ! w
    eval (Val v)      = v
    eval (NOT w)      = complement (outputs ! w)
    eval (AND w1 w2)  = (outputs ! w1) .&. (outputs ! w2)
    eval (MASK w)     = 1 .&. (outputs ! w)
    eval (OR w1 w2)   = (outputs ! w1) .|. (outputs ! w2)
    eval (LSHIFT w v) = (outputs ! w) `shiftL` v
    eval (RSHIFT w v) = (outputs ! w) `shiftR` v

main = do
  f <- getContents
  print $ runCircuit (readCircuit f)
