module Cipher.Dictionary
  ( DictName(..)
  , Dictionary
  , allDictNames
  , getDictionary
  , allWordsWithLength
  ) where

import Data.Char (toUpper)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

data DictName
  = Google
    -- ^ https://github.com/first20hours/google-10000-english/blob/master/google-10000-english-usa.txt
  | Words
    -- ^ Forgot where I found this...
  deriving (Show,Read,Bounded,Enum)

allDictNames :: [DictName]
allDictNames = [minBound..maxBound]

newtype Dictionary = Dictionary { getDictByWordLengths :: IntMap [String] }

{-# NOINLINE getDictionary #-}
getDictionary :: DictName -> Dictionary
getDictionary dictName = Dictionary $ byWordLengths dict
  where
    fp = case dictName of
      Google -> "google-10000-english-usa.txt"
      Words -> "words.txt"
    dict = unsafePerformIO $ lines <$> readFile ("dictionaries/" ++ fp)
    byWordLengths words' = Set.toList <$> IntMap.fromListWith Set.union
      [ (length word, Set.singleton $ map toUpper word) | word <- reverse words' ]

allWordsWithLength :: Dictionary -> Int -> [String]
allWordsWithLength dict len = IntMap.findWithDefault [] len $ getDictByWordLengths dict
