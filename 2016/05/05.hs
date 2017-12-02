import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Maybe             (catMaybes)
import           System.Environment

password salt
  = take 8
  . catMaybes
  . map extractChar
  . map mkHash
  $ [0..]
  where
    mkHash = unpack . encode . hash . pack . (salt++) . show

extractChar h
  = case (splitAt 5 h) of
      ("00000", c:_) -> Just c
      _              -> Nothing

main = do
  [salt] <- getArgs
  putStrLn (password salt)
