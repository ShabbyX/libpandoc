module LibPandoc.IO (CReader, CWriter, transform) where

import Foreign
import Foreign.C
import Foreign.C.String
import System.IO.Unsafe
import qualified Codec.Binary.UTF8.String as Utf8

-- | Represents an input stream as a function.  The text is assumed to
-- | be encoded as UTF-8.
type CReader = CString -> CInt -> CInt -> IO CInt

-- | Represents an output stream as a function.  The text is assumed
-- | to be encoded as UTF-8.
type CWriter = CString -> CInt -> CInt -> IO ()

-- | Lifts a string tranformer to a filter on C streams.
transform :: Int -> (String -> String) -> CReader -> CWriter -> IO ()
transform bufferSize transformer reader writer =
    withBuffer bufferSize $ \rbuf ->
    withBuffer bufferSize $ \wbuf ->
    do s <- readStream rbuf reader
       writeStream wbuf writer (transformer s)

newtype Buffer = Buffer (CString, Int) 

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer bufferSize action = withCString init act where
    init    = take bufferSize $ repeat ' '
    act str = action (Buffer (str, bufferSize))

readStream :: Buffer -> CReader -> IO String
readStream (Buffer (buf, size)) reader = unsafeInterleaveIO result where
    sz     = encodeInt size
    result = reader buf 0 sz >>= loop . decodeInt
    loop 0 = return []
    loop n = do
      s <- peekCStringLen (buf, n)
      k <- reader buf 0 sz
      fmap (Utf8.decodeString s ++) (loop (decodeInt k))

writeStream :: Buffer -> CWriter -> String -> IO ()
writeStream (Buffer (buf, size)) writer text = loop bytes where
    buffer = castPtr buf
    bytes  = Utf8.encode text
    loop bytes = do
      let (head, tail) = splitAt size bytes
      pokeArray buffer head
      writer buffer 0 (encodeInt (length head))
      case tail of
        [] -> return ()
        _  -> loop tail

decodeInt :: CInt -> Int
decodeInt x = fromInteger (toInteger x)

encodeInt :: Int -> CInt
encodeInt x = fromInteger (toInteger x)
