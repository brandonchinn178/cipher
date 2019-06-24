import System.IO (hGetContents, stdin)

import Cipher (decrypt)

main :: IO ()
main = do
  input <- unwords . lines <$> hGetContents stdin
  mapM_ putStrLn $ decrypt input
