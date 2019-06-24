module Cipher.Dictionary
  ( Dictionary
  , getDictionary
  , allWordsWithLength
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)

newtype Dictionary = Dictionary { getDictByWordLengths :: IntMap [String] }

{-# NOINLINE getDictionary #-}
getDictionary :: FilePath -> Dictionary
getDictionary fp = Dictionary $ byWordLengths dict
  where
    fp' = "dictionaries/" ++ fp
    dict = unsafePerformIO $ lines <$> readFile fp'
    byWordLengths words' = IntMap.fromListWith (++)
      [ (length word, [word]) | word <- reverse words' ]

allWordsWithLength :: Dictionary -> Int -> [String]
allWordsWithLength dict len = IntMap.findWithDefault [] len $ getDictByWordLengths dict
