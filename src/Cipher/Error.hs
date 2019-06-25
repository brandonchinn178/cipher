{-# LANGUAGE LambdaCase #-}

module Cipher.Error
  ( DecryptError(..)
  , DecryptM
  ) where

import Data.List (intercalate)

data DecryptError
  = MissingWords [String] -- ^ cipherwords that don't have any CipherMap possibilities in the dictionary
  | AmbiguousChar Char

instance Show DecryptError where
  show = \case
    MissingWords cipherwords -> "No words found for ciphertexts: " ++ intercalate ", " cipherwords
    AmbiguousChar c -> "Ambiguous character could not be decrypted: " ++ [c]

type DecryptM = Either DecryptError
