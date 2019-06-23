import System.IO (hGetContents, stdin)

import Cipher (decrypt)

main :: IO ()
main = do
  input <- hGetContents stdin
  print $ decrypt input
