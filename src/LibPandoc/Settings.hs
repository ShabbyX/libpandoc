{-# OPTIONS -XDeriveDataTypeable #-}

module LibPandoc.Settings where

import Data.Data
import Data.Default
import LibPandoc.Instances
import Text.Pandoc

data LibPandocSettings =
    LibPandocSettings { writerOptions :: WriterOptions
                      , readerOptions :: ReaderOptions
                      } deriving (Data, Typeable)

defaultLibPandocSettings :: LibPandocSettings
defaultLibPandocSettings =
    LibPandocSettings (def WriterOptions) (def ReaderOptions)
