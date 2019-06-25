{-# LANGUAGE RecordWildCards #-}

module Cipher.Options
  ( DecryptOptions(..)
  , DecryptOptionsConfig
  , DecryptOptionsResult
  , resolveOptions
  ) where

import Cipher.Dictionary (Dictionary, getDictionary)

data DecryptOptions dict = DecryptOptions
  { dictionary :: dict
  , strict     :: Bool
    -- ^ If true, exit if a word has no possible matches. If false, ignore the word
    -- and try decrypting everything else.
  }

type DecryptOptionsConfig = DecryptOptions String
type DecryptOptionsResult = DecryptOptions Dictionary

resolveOptions :: DecryptOptionsConfig -> DecryptOptionsResult
resolveOptions DecryptOptions{..} = DecryptOptions
  { dictionary = getDictionary dictionary
  , ..
  }
