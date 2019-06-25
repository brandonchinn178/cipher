{-# LANGUAGE RecordWildCards #-}

module Cipher (decrypt) where

import Data.Char (isAlpha)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes, mapMaybe)

import Cipher.Dictionary
import Cipher.Error
import Cipher.Map
import Cipher.Options

-- | Return a list of all possible phrases for the given ciphertext.
decrypt :: DecryptOptionsResult -> String -> DecryptM [String]
decrypt opts s = mapM (decryptWith s) =<< getCipherMaps opts s

-- | Get all possible cipher maps that can decrypt the given ciphertext.
getCipherMaps :: DecryptOptionsResult -> String -> DecryptM [CipherMap]
getCipherMaps DecryptOptions{..} = fmap mergeAllCipherMaps . handleMissing . map getCipherMapsWithWord . splitWords
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
    handleMissing :: [(String, [CipherMap])] -> DecryptM [[CipherMap]]
    handleMissing cipherMaps = if strict
      then case filter (null . snd) cipherMaps of
        [] -> Right $ map snd cipherMaps
        missingWords -> Left $ MissingWords $ map fst missingWords
      else Right $ filter (not . null) . map snd $ cipherMaps

mergeAllCipherMaps
  :: [[CipherMap]] -- ^ all possible CipherMaps for each word
  -> [CipherMap]   -- ^ all possible CipherMaps for the entire ciphertext
mergeAllCipherMaps = flip foldl [emptyCipherMap] $ \cipherMaps1 cipherMaps2 ->
  catMaybes
    [ mergeCipherMaps cipherMap1 cipherMap2
    | cipherMap1 <- cipherMaps1
    , cipherMap2 <- cipherMaps2
    ]
