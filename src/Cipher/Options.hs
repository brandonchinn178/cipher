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
  }

type DecryptOptionsConfig = DecryptOptions String
type DecryptOptionsResult = DecryptOptions Dictionary

resolveOptions :: DecryptOptionsConfig -> DecryptOptionsResult
resolveOptions DecryptOptions{..} = DecryptOptions
  { dictionary = getDictionary dictionary
  }
