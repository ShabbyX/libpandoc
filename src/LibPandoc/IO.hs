-- | Provides unterleaved UTF-8 input/output to and from C functions.
module LibPandoc.IO (CReader, CWriter, transform) where

import Foreign
import Foreign.C
import Foreign.C.String
import System.IO.Unsafe
import qualified Codec.Binary.UTF8.String as Utf8

-- | Represents an input stream as a function.  Reads UTF-8 encoded
-- | characters by copying them into the buffer and returns the number
-- | of bytes read.
type CReader = CString -> IO CInt

-- | Represents an output stream as a function.  Receives a character
-- | buffer and a number of bytes to read.  The bytes always represent
-- | an integer number of UTF-8 characters.
type CWriter = CString -> CInt -> IO ()

-- | Lifts a string tranformer to a filter on C streams.  The first
-- | parameter is the size of the buffer in bytes.  The number of
-- | bytes returned by the reader and passed to the writer should not
-- | exceed the buffer size.
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
    result = reader buf >>= loop . decodeInt
    loop 0 = return []
    loop n = do
      s <- peekCStringLen (buf, n)
      k <- reader buf
      fmap (Utf8.decodeString s ++) (loop (decodeInt k))

writeStream :: Buffer -> CWriter -> String -> IO ()
writeStream (Buffer (buf, size)) writer text = loop text where
    buffer = castPtr buf
    loop text = do
      let (head, tail) = splitAt (div size 4) text
          bytes        = Utf8.encode head
      pokeArray buffer bytes
      writer buffer (encodeInt (length bytes))
      case tail of
        [] -> return ()
        _  -> loop tail

decodeInt :: CInt -> Int
decodeInt x = fromInteger (toInteger x)

encodeInt :: Int -> CInt
encodeInt x = fromInteger (toInteger x)
