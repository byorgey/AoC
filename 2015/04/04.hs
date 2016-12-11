import           Control.Arrow
import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.List
import           Data.Maybe             (catMaybes)
import           System.Environment

adventCoin :: String -> Int
adventCoin salt
  = snd . head
  . filter (("000000" `isPrefixOf`) . fst)
  . map (mkHash &&& id)
  $ [0..]
  where
    mkHash = unpack . encode . hash . pack . (salt++) . show

main = do
  [salt] <- getArgs
  print (adventCoin salt)
