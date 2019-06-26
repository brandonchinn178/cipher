{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cipher.Map
  ( CipherMap
  , emptyCipherMap
  , getCipherMap
  , mergeCipherMaps
  , decryptWith
  ) where

import Data.Char (chr, isUpper, ord, toLower, toUpper)
import Data.Foldable (foldlM)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Cipher.Error

-- | The cipher mapping of letters that represents the substitution pairs used
-- to encrypt a ciphertext.
--
-- Invariants:
--   * Keys must be the result of 'getIndex'
--   * Values must be ['A'..'Z']
newtype CipherMap = CipherMap (IntMap Char)
  deriving (Eq, Ord)

instance Show CipherMap where
  show (CipherMap charMap) = "CipherMap { " ++ showMap ++ " }"
    where
      showMap = intercalate "; " . map fromPair . IntMap.toList $ charMap
      fromPair (i, p) = [fromIndex i] ++ " -> " ++ [p]

-- | An empty CipherMap.
emptyCipherMap :: CipherMap
emptyCipherMap = CipherMap IntMap.empty

-- | A CipherMap with a single value.
singletonCipherMap :: Int -> Char -> CipherMap
singletonCipherMap i p = CipherMap $ IntMap.singleton i p

-- | For the given ciphertext and possible plaintext, return the associated
-- CipherMap. Return Nothing if the plaintext is incompatible with the
-- ciphertext.
getCipherMap :: String -> String -> Maybe CipherMap
getCipherMap ciphertext plaintext = foldlM mergeCipherMaps emptyCipherMap =<< cipherMapsByLetter
  where
    cipherMapsByLetter = mapM toCipherMap $ zip ciphertext plaintext
    toCipherMap (c, p) =
      case getIndex c of
        -- punctuation in ciphertext needs to be unchanged from the plaintext
        Nothing ->
          if c == p
            then Just emptyCipherMap
            else Nothing
        Just i ->
          -- plaintext needs to be alphabetical
          if isNothing (getIndex p)
            then Nothing
            else Just $ singletonCipherMap i (toUpper p)

-- | Combine two CipherMaps, returning Nothing if the CipherMaps are incompatible.
mergeCipherMaps :: CipherMap -> CipherMap -> Maybe CipherMap
mergeCipherMaps (CipherMap charMap1) (CipherMap charMap2) =
  if null conflictingKeys && null conflictingVals
    then Just $ CipherMap $ IntMap.union charMap1 charMap2
    else Nothing
  where
    conflictingKeys = getConflictingKeys charMap1 charMap2
    conflictingVals = getConflictingKeys (swap charMap1) (swap charMap2)

    getConflictingKeys map1 map2 = IntMap.keys $ IntMap.filter id $ IntMap.intersectionWith (/=) map1 map2
    swap = IntMap.fromList . map (\(i, p) -> (getIndexUnsafe p, fromIndex i)) . IntMap.toList

-- | Decrypt the given ciphertext with the given CipherMap.
decryptWith :: String -> CipherMap -> DecryptM String
decryptWith s cipherMap = mapM decryptChar s
  where
    decryptChar c =
      let toCased = if isUpper c then toUpper else toLower
      in case lookupCipherMap cipherMap c of
        Nothing -> Right $ c
        Just Nothing -> Left $ AmbiguousChar c
        Just (Just p) -> Right $ toCased p

{- Helpers -}

-- | Get the plaintext character for the given ciphertext character in the given CipherMap.
-- Returns Nothing if character is not alphabetical and Just Nothing if the ciphertext
-- character is not assigned yet.
lookupCipherMap :: CipherMap -> Char -> Maybe (Maybe Char)
lookupCipherMap (CipherMap charMap) c = (`IntMap.lookup` charMap) <$> getIndex c

-- | Get the index of the given character. Returns Nothing if character is not alphabetical.
getIndex :: Char -> Maybe Int
getIndex c =
  if i >= 0 && i < 26
    then Just i
    else Nothing
  where
    i = subtract 65 . ord . toUpper $ c

-- | Get the index of the given character. Returns Nothing if character is not alphabetical.
getIndexUnsafe :: Char -> Int
getIndexUnsafe c = case getIndex c of
  Nothing -> error $ "Bad index: " ++ [c]
  Just c' -> c'

fromIndex :: Int -> Char
fromIndex = chr . (+ 65)
