{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import System.IO (hGetContents, stdin)

import Cipher (decrypt)

data Options = Options
  { limit  :: Maybe Int
  , output :: Maybe FilePath
  }

getArgs :: IO Options
getArgs = execParser $ info (parseOptions <**> helper) fullDesc
  where
    parseOptions = Options
      <$> parseLimit
      <*> parseOutput
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

main :: IO ()
main = do
  Options{..} <- getArgs

  input <- unwords . lines <$> hGetContents stdin
  let decrypted = maybe id take limit $ decrypt input

  case output of
    Nothing -> mapM_ putStrLn decrypted
    Just fp -> writeFile fp $ unlines decrypted
