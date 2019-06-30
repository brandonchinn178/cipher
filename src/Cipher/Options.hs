{-# LANGUAGE RecordWildCards #-}

module Cipher.Options
  ( DecryptOptions(..)
  , DecryptOptionsConfig
  , DecryptOptionsResult
  , defaultOptions
  , resolveOptions
  ) where

import Cipher.Dictionary (Dictionary, DictName(..), getDictionary)

data DecryptOptions dict = DecryptOptions
  { dictionary :: dict
  , strict     :: Bool
    -- ^ If true, exit if a word has no possible matches. If false, ignore the word
    -- and try decrypting everything else.
  }

type DecryptOptionsConfig = DecryptOptions DictName
type DecryptOptionsResult = DecryptOptions Dictionary

defaultOptions :: DecryptOptionsConfig
defaultOptions = DecryptOptions
  { dictionary = Google20k
  , strict = False
  }

resolveOptions :: DecryptOptionsConfig -> DecryptOptionsResult
resolveOptions DecryptOptions{..} = DecryptOptions
  { dictionary = getDictionary dictionary
  , ..
  }
