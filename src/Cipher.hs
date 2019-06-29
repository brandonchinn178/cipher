{-# LANGUAGE RecordWildCards #-}

module Cipher (decrypt) where

import Data.Char (isAlpha)
import Data.List.Split (wordsBy)
import Data.Maybe (mapMaybe)

import Cipher.Dictionary
import Cipher.Error
import Cipher.Map
import Cipher.Options
import Cipher.Search

-- | Return a list of all possible phrases for the given ciphertext.
decrypt :: DecryptOptionsResult -> String -> DecryptM [String]
decrypt opts s = mapM (decryptWith s) =<< getCipherMaps opts s

-- | Get all possible cipher maps that can decrypt the given ciphertext.
getCipherMaps :: DecryptOptionsResult -> String -> DecryptM [CipherMap]
getCipherMaps DecryptOptions{..} = fmap (findCompatible mergeCipherMaps) . filterMissing . map getCipherMapsWithWord . splitWords
  where
    -- True if character is relevant to decryption; e.g. not punctuation or numbers
    isRelevant :: Char -> Bool
    isRelevant c = isAlpha c || c `elem` " -'"

    -- split ciphertext into cipherwords
    splitWords :: String -> [String]
    splitWords = wordsBy (`elem` "- ") . filter isRelevant

    getCipherMapsForWord :: String -> [CipherMap]
    getCipherMapsForWord cipherWord =
      let possibleWords = allWordsWithLength dictionary $ length cipherWord
      in mapMaybe (getCipherMap cipherWord) possibleWords

    getCipherMapsWithWord :: String -> (String, [CipherMap])
    getCipherMapsWithWord cipherWord = (cipherWord, getCipherMapsForWord cipherWord)

    -- handle cipherwords without any CipherMaps
    filterMissing :: [(String, [CipherMap])] -> DecryptM [(String, [CipherMap])]
    filterMissing cipherMaps = if strict
      then case filter (null . snd) cipherMaps of
        [] -> Right cipherMaps
        missingWords -> Left $ MissingWords $ map fst missingWords
      else Right $ filter (not . null . snd) cipherMaps
