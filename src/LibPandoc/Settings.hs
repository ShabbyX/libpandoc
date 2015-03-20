{-
 - Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 -
 - This file is part of libpandoc, providing C bindings to Pandoc.
 - libpandoc is licensed under BSD 3-clause.  However, note that Pandoc
 - itself is licensed under GPL version 2 or later.
 -}

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
