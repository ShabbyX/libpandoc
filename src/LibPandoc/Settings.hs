{-# OPTIONS -XDeriveDataTypeable #-}

module LibPandoc.Settings where

import Data.Data
import LibPandoc.Instances
import Text.Pandoc

data LibPandocSettings =
    LibPandocSettings { writerOptions :: WriterOptions
                      , parserState   :: ParserState
                      } deriving (Data, Typeable)

defaultLibPandocSettings :: LibPandocSettings
defaultLibPandocSettings = 
    LibPandocSettings defaultWriterOptions defaultParserState
