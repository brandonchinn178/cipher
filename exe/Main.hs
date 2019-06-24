{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import System.IO (hGetContents, stdin)

import Cipher (decrypt)
import Cipher.Dictionary (getDictionary)

data Options = Options
  { limit      :: Maybe Int
  , output     :: Maybe FilePath
  , dictionary :: FilePath
  }

getArgs :: IO Options
getArgs = execParser $ info (parseOptions <**> helper) $ progDesc description
  where
    description = "Decrypt ciphertext from stdin encrypted with a substitution cipher"
    parseOptions = Options
      <$> parseLimit
      <*> parseOutput
      <*> parseDictionary
    parseLimit = parseNoLimit <|> parseLimit'
    parseNoLimit = flag' Nothing $ mconcat
      [ long "no-limit"
      , help "Show all decrypted sentences"
      ]
    parseLimit' = fmap Just $ option auto $ mconcat
      [ long "limit"
      , help "The maximum number of decrypted sentences to find"
      , showDefault
      , value 10
      ]
    parseOutput = optional $ strOption $ mconcat
      [ long "output"
      , short 'o'
      , help "Save the found decrypted sentences to given file"
      ]
    parseDictionary = strOption $ mconcat
      [ long "dictionary"
      , short 'd'
      , help "The dictionary to use (see dictionaries/)"
      , showDefault
      , value "google-10000-english-usa.txt"
      ]

main :: IO ()
main = do
  Options{..} <- getArgs

  input <- unwords . lines <$> hGetContents stdin
  let dict = getDictionary dictionary
      decrypted = maybe id take limit $ decrypt dict input

  case output of
    Nothing -> mapM_ putStrLn decrypted
    Just fp -> writeFile fp $ unlines decrypted
