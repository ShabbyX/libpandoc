{-# LANGUAGE DeriveDataTypeable #-}

module LibPandoc.Settings where

import Data.Data
import Data.Default
import Text.Pandoc

data LibPandocSettings =
    LibPandocSettings { writerOptions :: WriterOptions
                      , readerOptions :: ReaderOptions
                      } deriving (Data, Typeable)

defaultLibPandocSettings :: LibPandocSettings
defaultLibPandocSettings =
    LibPandocSettings (def WriterOptions) (def ReaderOptions)
