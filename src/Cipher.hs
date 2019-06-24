module Cipher
  ( decrypt
  , getCipherMaps
  ) where

import Data.Char (isAlpha)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes, mapMaybe)

import Cipher.Dictionary
import Cipher.Map

-- | Return a list of all possible phrases for the given ciphertext.
decrypt :: Dictionary -> String -> [String]
decrypt dict s = map (decryptWith s) . mergeAllCipherMaps . map (getCipherMaps dict) $ cipherWords
  where
    cipherWords = wordsBy (`elem` "- ") . keepRelevant $ s

    -- ignore any characters not relevant to decryption; e.g. punctuation or numbers
    keepRelevant = filter (\c -> isAlpha c || c `elem` " -'")

    mergeAllCipherMaps
      :: [[CipherMap]] -- ^ all possible CipherMaps for each word
      -> [CipherMap]   -- ^ all possible CipherMaps for the entire ciphertext
    mergeAllCipherMaps = flip foldl [emptyCipherMap] $ \cipherMaps1 cipherMaps2 ->
      catMaybes
        [ mergeCipherMaps cipherMap1 cipherMap2
        | cipherMap1 <- cipherMaps1
        , cipherMap2 <- cipherMaps2
        ]

-- | For the given encrypted word, return all the possible CipherMaps that
-- would decrypt the word to an English word.
getCipherMaps :: Dictionary -> String -> [CipherMap]
getCipherMaps dict cipherWord = mapMaybe (getCipherMap cipherWord) $ allWordsWithLength dict (length cipherWord)
