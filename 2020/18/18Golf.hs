import           Text.Parsec          hiding (State)
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as T
main = interact $ unlines . map show . (\i -> map ($i) [s [["+","*"]],s [["+"],["*"]]]) . lines
s ops = sum . map (either undefined id . parse (expr ops) "")
expr ops = e
  where
    e       = buildExpressionParser opTable term
    opTable = map (map (\op -> Infix ((case op of {"+"->(+);"*"->(*)}) <$ T.symbol t op) AssocLeft)) ops
    term    = T.integer t <|> T.parens t e
    t       = T.makeTokenParser emptyDef

