module Cipher.Dictionary
  ( allWordsWithLength
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)

allWordsWithLength :: Int -> [String]
allWordsWithLength len = IntMap.findWithDefault [] len englishDictWordLengths

{- Helpers -}

englishDictWordLengths :: IntMap [String]
englishDictWordLengths = IntMap.fromListWith (++)
  [ (length word, [word]) | word <- reverse englishDict ]

{-# NOINLINE englishDict #-}
englishDict :: [String]
englishDict = unsafePerformIO $
  -- TODO: fix for linux
  lines <$> readFile "words.txt"
