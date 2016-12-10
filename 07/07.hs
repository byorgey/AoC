import qualified Data.Set           as S

import           Text.Parsec
import           Text.Parsec.String

type IP = [Segment]

data Segment = Super String | Hyper String
  deriving Show

readIP :: String -> IP
readIP s = case runParser parseIP () "" s of
  Left err -> error (show err)
  Right ip -> ip

parseIP :: Parser IP
parseIP = many parseSegment
  where
    parseSegment = Hyper <$> parseHypernet <|> Super <$> parseSupernet
    parseHypernet = char '[' *> many1 (noneOf "]") <* char ']'
    parseSupernet = many1 (noneOf "[")

hasABBA :: String -> Bool
hasABBA (a:b:b':a':rest)
  = a /= b && a == a' && b == b' || hasABBA (b:b':a':rest)
hasABBA _ = False

supernets :: IP -> [String]
supernets ip = [s | Super s <- ip]

hypernets :: IP -> [String]
hypernets ip = [s | Hyper s <- ip]

supportsTLS :: IP -> Bool
supportsTLS ip = any hasABBA (supernets ip) && all (not . hasABBA) (hypernets ip)

getABA :: String -> S.Set String
getABA (a:b:a':rest)
  | a /= b && a == a' = S.insert [a,b] (getABA (b:a':rest))
  | otherwise         = getABA (b:a':rest)
getABA _ = S.empty

supportsSSL :: IP -> Bool
supportsSSL ip = not (S.null $ S.intersection abas (S.map reverse babs))
  where
    abas = S.unions (map getABA (supernets ip))
    babs = S.unions (map getABA (hypernets ip))

main = interact (show . length . filter supportsSSL . map readIP . lines)
