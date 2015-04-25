{-
 - Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 - Copyright (C) 2015  Shahbaz Youssefi <ShabbyX@gmail.com>
 -
 - This file is part of libpandoc, providing C bindings to Pandoc.
 -
 - libpandoc is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 2 of the License, or
 - (at your option) any later version.
 -
 - libpandoc is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with libpandoc.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module LibPandoc.Settings where

import Data.Data
import Data.Default
import Text.Pandoc

data LibPandocSettings =
    LibPandocSettings { writerOptions :: WriterOptions
                      , readerOptions :: ReaderOptions
                      } deriving (Data, Typeable, Show)

defaultLibPandocSettings :: LibPandocSettings
defaultLibPandocSettings =
  LibPandocSettings (def WriterOptions) (def ReaderOptions)
