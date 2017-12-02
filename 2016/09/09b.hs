import           Control.Monad
import           Data.Char
import           Data.List
import           Text.Parsec
import           Text.Parsec.String

main = interact ((++"\n") . show . decompressedLength . readCompressed)

data Segment = Repeat Integer Compressed | Literal String
  deriving Show
type Compressed = [Segment]

readCompressed :: String -> Compressed
readCompressed s = case runParser parseCompressed () "" s of
  Left err -> error (show err)
  Right res -> res

parseCompressed :: Parser Compressed
parseCompressed = many (parseLiteral <|> parseRepeat)
  where
    parseLiteral = (Literal . filter (not . isSpace)) <$> many1 (noneOf "(")
    parseRepeat  = do
      char '('
      len <- read <$> many1 digit
      char 'x'
      reps <- read <$> many1 digit
      char ')'
      (Repeat reps . readCompressed) <$> replicateM len anyChar

decompressedLength :: Compressed -> Integer
decompressedLength = sum . map segmentLength
  where
    segmentLength (Literal s) = genericLength s
    segmentLength (Repeat n s) = n * decompressedLength s
