{-# LANGUAGE RecordWildCards #-}

module Cipher (decrypt) where

import Control.Applicative (liftA2)
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

    mergeAllCipherMaps :: [[CipherMap]] -> [CipherMap]
    mergeAllCipherMaps [] = []
    mergeAllCipherMaps (first:rest) =
      let choose [] = error "should not happen"
          choose (a:as) = (a, as)
      in foldBy choose (productMaybe mergeCipherMaps) first rest

-- | Fold, except using the given function to choose the next item to fold in.
-- The input list to the choose function is guaranteed to be non-empty.
--
-- > foldBy (\(a:as) -> (a, as)) == foldl
foldBy :: ([a] -> (a, [a])) -> (b -> a -> b) -> b -> [a] -> b
foldBy _ _ b [] = b
foldBy choose f b as =
  let (a, as') = choose as
      b' = f b a
  in foldBy choose f b' as'

-- | Cartesian product, filtering out the Nothings.
productMaybe :: (a -> a -> Maybe a) -> [a] -> [a] -> [a]
productMaybe f as bs = catMaybes $ liftA2 f as bs
