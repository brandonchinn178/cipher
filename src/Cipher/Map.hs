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
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- | The cipher mapping of letters that represents the substitution pairs used
-- to encrypt a ciphertext.
--
-- Invariant: Vector contains 26 elements containing elements of ['A'..'Z'],
-- where ['D', 'Z', ...] represents a ciphermap where 'A' decrypts to 'D',
-- 'B' decrypts to 'Z', etc.
newtype CipherMap = CipherMap (Vector (Maybe Char))
  deriving (Eq, Ord)

instance Show CipherMap where
  show (CipherMap cipherVec) = "CipherMap { " ++ showMap ++ " }"
    where
      showMap = intercalate "; " . catMaybes . V.toList . V.imap (curry fromPair) $ cipherVec
      fromPair = \case
        (_, Nothing) -> Nothing
        (i, Just p) -> Just $ [chr $ i + 65] ++ " -> " ++ [p]

-- | An empty CipherMap.
emptyCipherMap :: CipherMap
emptyCipherMap = CipherMap $ V.replicate 26 Nothing

-- | For the given ciphertext and possible plaintext, return the associated
-- CipherMap. Return Nothing if the plaintext is incompatible with the
-- ciphertext.
getCipherMap :: String -> String -> Maybe CipherMap
getCipherMap ciphertext plaintext = foldlM mergeCipherMaps emptyCipherMap $ mapMaybe toCipherMap $ zip ciphertext plaintext
  where
    toCipherMap (c, p) =
      case getIndex c of
        -- punctuation in ciphertext needs to be unchanged from the plaintext
        Nothing ->
          if c == p
            then Just emptyCipherMap
            else Nothing
        Just index -> Just $ CipherMap $ V.generate 26 $ \i ->
          if i == index
            then Just $ toUpper p
            else Nothing

-- | Combine two CipherMaps, returning Nothing if the CipherMaps are incompatible.
mergeCipherMaps :: CipherMap -> CipherMap -> Maybe CipherMap
mergeCipherMaps (CipherMap cipherVec1) (CipherMap cipherVec2) =
  CipherMap <$> merge Set.empty [] (V.toList $ V.zip cipherVec1 cipherVec2)
  where
    merge _ result [] = Just $ V.fromList $ reverse result
    merge seen result (curr:rest) =
      let continue Nothing = merge seen (Nothing : result) rest
          continue (Just p) =
            if Set.member p seen
              -- this plaintext character is already decrypted from another ciphertext character
              then Nothing
              else merge (Set.insert p seen) (Just p : result) rest
      in case curr of
        (Just p1, Just p2) ->
          if p1 == p2
            then continue $ Just p1
            -- this ciphertext character can't decrypt to two different plaintext characters
            else Nothing
        (Nothing, Nothing) -> continue Nothing
        (Just p, Nothing) -> continue $ Just p
        (Nothing, Just p) -> continue $ Just p

-- | Decrypt the given ciphertext with the given CipherMap.
decryptWith :: String -> CipherMap -> String
decryptWith s cipherMap = map decryptChar s
  where
    decryptChar c =
      let toCased = if isUpper c then toUpper else toLower
      in case lookupCipherMap cipherMap c of
        Nothing -> c
        Just Nothing -> error $ "Could not decrypt character: " ++ [c]
        Just (Just p) -> toCased p

{- Helpers -}

-- | Get the plaintext character for the given ciphertext character in the given CipherMap.
-- Returns Nothing if character is not alphabetical and Just Nothing if the ciphertext
-- character is not assigned yet.
lookupCipherMap :: CipherMap -> Char -> Maybe (Maybe Char)
lookupCipherMap (CipherMap cipherVec) c = (cipherVec !) <$> getIndex c

-- | Get the index of the given character. Returns Nothing if character is not alphabetical.
getIndex :: Char -> Maybe Int
getIndex c =
  if i >= 0 && i < 26
    then Just i
    else Nothing
  where
    i = subtract 65 . ord . toUpper $ c
