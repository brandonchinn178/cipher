{-# LANGUAGE RecordWildCards #-}

module Cipher (decrypt) where

import Data.Char (isAlpha)
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
getCipherMaps DecryptOptions{..} = mergeAllCipherMaps . map getCipherMapsForWord . splitWords
  where
    splitWords = wordsBy (`elem` "- ") . keepRelevant

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

    getCipherMapsForWord cipherWord =
      let possibleWords = allWordsWithLength dictionary $ length cipherWord
      in mapMaybe (getCipherMap cipherWord) possibleWords
