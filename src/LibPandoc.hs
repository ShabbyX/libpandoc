{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-
 - Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 - Copyright (C) 2015  Shahbaz Youssefi <ShabbyX@gmail.com>
 - Copyright (C) 2015  Katherine Whitlock <toroidalcode@gmail.com>
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

-- | Provides FFI interface to Pandoc.
module LibPandoc (pandoc, LibPandocSettings(..), defaultLibPandocSettings) where

import           Control.Arrow              ((>>>))
import           Control.Exception          (catch, Exception(..), SomeException(..))
import           Control.Monad              ((>=>), liftM)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Char                  as Char
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Typeable              (typeOf)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           LibPandoc.IO
import           LibPandoc.Settings
import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.MediaBag
import           Text.JSON
import           Text.JSON.Generic          (toJSON,fromJSON)

-- | The type of the main entry point.
type CPandoc = CInt -> CString -> CString -> CString
             -> FunPtr CReader -> FunPtr CWriter -> Ptr ()
             -> IO CString

foreign export ccall "pandoc" pandoc     :: CPandoc
foreign import ccall "dynamic" peekReader :: FunPtr CReader -> CReader
foreign import ccall "dynamic" peekWriter :: FunPtr CWriter -> CWriter

-- | Gives preferential treatment to first argument (should be user options)
joinJSON :: JSValue -> JSValue -> JSValue
joinJSON (JSArray a) (JSArray b) = JSArray (List.zipWith joinJSON a b)
joinJSON (JSObject a) (JSObject b) =
  let aMap = Map.fromList . fromJSObject $ a
      bMap = Map.fromList . fromJSObject $ b
  in JSObject . toJSObject . Map.toList $ Map.unionWith joinJSON aMap bMap
joinJSON a _ = a

getSettings :: CString -> IO LibPandocSettings
getSettings settings | settings == nullPtr = return defaultLibPandocSettings
getSettings settings = do
  let defaults = defaultLibPandocSettings
  s <- peekCString settings
  let userSettings = fromResult . decodeStrict $ s
      combined     = userSettings `joinJSON` toJSON defaults
  return . fromResult . fromJSON $ combined
  where
    fromResult :: Result a -> a
    fromResult (Ok a)    = a
    fromResult (Error e) = error e

pandoc :: CPandoc
pandoc bufferSize input output settings reader writer userData = do
  let cr = peekReader reader
      cw = peekWriter writer
  i <- peekCString input
  o <- peekCString output
  s <- getSettings settings

  read <- return $ getReader i >>= \rd -> case rd of
    -- if the reader returns just a string, add an empty media bag to it
    StringReader r -> Right $ \o s -> r o s >>= \p -> return $ p >>= \x -> Right (x, mempty::MediaBag)
    -- otherwise, the output (Pandoc, MediaBag) is already fine
    ByteStringReader r -> Right $ \o s -> r o (BLC.pack s)
  -- Note: currently, the media bag is actually later thrown away.  Perhaps in the future there could be support for it

  write <- return $ getWriter o >>= \wr -> case wr of
    -- if the writer returns just a string, add an IO
    PureStringWriter w -> Right $ \o s -> return $ w o s
    -- if it returns an IO string, it's fine
    IOStringWriter w -> Right w
    -- if it returns an IO byte string, convert it to IO string
    IOByteStringWriter w -> Right $ \o s -> w o s >>= \s -> return $ BLC.unpack s

  case (read, write) of
   (Left e, _)            -> newCString e
   (_, Left e)            -> newCString e
   (Right r, Right w) ->
     do let run = (r (readerOptions s) >=> return . liftM fst) >>> liftM handleError >>> \x -> x >>= w (writerOptions s)
        -- run takes the media bag out of reader, as it is currently unused.  Since the reader returns IO, everything is lifted
        result <- tryMaybe (transform (decodeInt bufferSize) run cr cw userData)
        case result of
         Just (SomeException res) -> newCString (show (typeOf res) ++ ": " ++ show res)
         Nothing -> return nullPtr

  where
    tryMaybe :: IO a -> IO (Maybe SomeException)
    tryMaybe a = catch (a >> return Nothing) (return . Just)

decodeInt :: CInt -> Int
decodeInt = fromInteger . toInteger
