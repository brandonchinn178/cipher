{-# LANGUAGE LambdaCase #-}

module Cipher.Error
  ( DecryptError(..)
  , DecryptM
  ) where

import Data.List (intercalate)

data DecryptError
  = MissingWords [String] -- ^ cipherwords that don't have any CipherMap possibilities in the dictionary

instance Show DecryptError where
  show = \case
    MissingWords cipherwords -> "No words found for ciphertexts: " ++ intercalate ", " cipherwords

type DecryptM = Either DecryptError
