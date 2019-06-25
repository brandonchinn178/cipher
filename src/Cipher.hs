{-# LANGUAGE RecordWildCards #-}

module Cipher (decrypt) where

import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes, mapMaybe)

import Cipher.Dictionary
import Cipher.Map
import Cipher.Options

-- | Return a list of all possible phrases for the given ciphertext.
decrypt :: DecryptOptionsResult -> String -> [String]
decrypt opts s = map (decryptWith s) $ getCipherMaps opts s

-- | Get all possible cipher maps that can decrypt the given ciphertext.
getCipherMaps :: DecryptOptionsResult -> String -> [CipherMap]
getCipherMaps DecryptOptions{..} = mergeAllCipherMaps . handleMissing . map getCipherMapsWithWord . splitWords
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
    handleMissing :: [(String, [CipherMap])] -> [[CipherMap]]
    handleMissing cipherMaps = if strict
      then case filter (null . snd) cipherMaps of
        [] -> map snd cipherMaps
        missingWords -> error $ "No words found for ciphertexts: " ++ intercalate ", " (map fst missingWords)
      else filter (not . null) . map snd $ cipherMaps

mergeAllCipherMaps
  :: [[CipherMap]] -- ^ all possible CipherMaps for each word
  -> [CipherMap]   -- ^ all possible CipherMaps for the entire ciphertext
mergeAllCipherMaps = flip foldl [emptyCipherMap] $ \cipherMaps1 cipherMaps2 ->
  catMaybes
    [ mergeCipherMaps cipherMap1 cipherMap2
    | cipherMap1 <- cipherMaps1
    , cipherMap2 <- cipherMaps2
    ]
