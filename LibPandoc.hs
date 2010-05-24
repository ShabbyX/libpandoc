{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}

-- | Provides FFI interface to Pandoc.
module LibPandoc (pandoc, LibPandocSettings(..), defaultLibPandocSettings) where

import Data.Data
import Data.DeriveTH
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe
import Text.Pandoc
import Text.Pandoc.Shared
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Generics.Rep as Rep
import qualified Text.XML.Light as Xml
import qualified Text.XML.Light.Generic as XG

-- | Represents an input stream as a function. 
type CReader = CWString -> CInt -> CInt -> IO CInt

-- | Represents an output stream as a function.
type CWriter = CWString -> CInt -> CInt -> IO ()

-- | The type of the main entry point.
type CPandoc = CWString -> CWString -> CWString 
             -> FunPtr CReader -> FunPtr CWriter
             -> IO CWString

foreign export ccall "pandoc"  pandoc     :: CPandoc
foreign import ccall "dynamic" peekReader :: FunPtr CReader -> CReader
foreign import ccall "dynamic" peekWriter :: FunPtr CWriter -> CWriter

readXml :: ParserState -> String -> Pandoc
readXml state xml = 
    let failed = readMarkdown state "Failed to parse XML." in
    case Xml.onlyElems (Xml.parseXML xml) of
      (elem : _) ->
          case XG.ofXml elem of
            Just pandoc -> pandoc
            Nothing     -> failed
      _ -> failed

writeXml :: WriterOptions -> Pandoc -> String
writeXml options pandoc = Xml.ppElement (XG.toXml pandoc)

getInputFormat :: String -> Maybe (ParserState -> String -> Pandoc)
getInputFormat x = 
    case map Char.toLower x of
      "html"     -> Just readHtml
      "latex"    -> Just readLaTeX
      "markdown" -> Just readMarkdown
      "rst"      -> Just readRST
      "xml"      -> Just readXml
      _          -> Nothing

getOutputFormat :: String -> Maybe (WriterOptions -> Pandoc -> String)
getOutputFormat x =
    case map Char.toLower x of
      "context"      -> Just writeConTeXt
      "docbook"      -> Just writeDocbook
      "html"         -> Just writeHtmlString
      "latex"        -> Just writeLaTeX
      "man"          -> Just writeMan
      "markdown"     -> Just writeMarkdown
      "mediawiki"    -> Just writeMediaWiki
      "opendocument" -> Just writeOpenDocument
      "plain"        -> Just writePlain
      "rst"          -> Just writeRST
      "rtf"          -> Just writeRTF
      "s5"           -> Just writeS5String
      "texinfo"      -> Just writeTexinfo
      "xml"          -> Just writeXml
      _              -> Nothing

data LibPandocSettings =
    LibPandocSettings { writerOptions :: WriterOptions
                      , parserState   :: ParserState
                      }

defaultLibPandocSettings :: LibPandocSettings
defaultLibPandocSettings = 
    LibPandocSettings defaultWriterOptions defaultParserState

joinRep :: Rep.ValueRep -> Rep.ValueRep -> Rep.ValueRep
joinRep (Rep.ValueRep name (Left x)) (Rep.ValueRep _ (Left y)) =
    Rep.ValueRep name (Left (Map.toList um)) where
        xm = Map.fromList x
        ym = Map.fromList y
        um = Map.unionWith joinRep xm ym
joinRep (Rep.ValueRep name (Right x)) (Rep.ValueRep _ (Right y)) =
    Rep.ValueRep name (Right (zipWith joinRep x y))
joinRep (Rep.TupleRep x) (Rep.TupleRep y) = 
    Rep.TupleRep (zipWith joinRep x y)
joinRep (Rep.ListRep x) (Rep.ListRep y) = 
    Rep.ListRep (zipWith joinRep x y)
joinRep x _ = x

getSettings :: CWString -> IO LibPandocSettings
getSettings settings
    | settings == nullPtr = 
        return defaultLibPandocSettings
    | otherwise =
        do let dS = defaultLibPandocSettings
           s <- peekCWString settings
           case Xml.onlyElems (Xml.parseXML s) of
             (e:_) ->
                 case XG.decodeXml e of
                   Nothing  -> return dS
                   Just rep ->
                       let r = Rep.toRep dS `joinRep` rep in
                       return $ maybe dS id (Rep.ofRep r) 
             _ -> return dS
           
pandoc :: CPandoc
pandoc input output settings reader writer = do
  let r = peekReader reader
      w = peekWriter writer
  i <- peekCWString input
  o <- peekCWString output
  s <- getSettings settings
  case (getInputFormat i, getOutputFormat o) of
    (Nothing, _)            -> newCWString "Invalid input format."
    (_, Nothing)            -> newCWString "Invalid output format."
    (Just read, Just write) ->
        do let run = write (writerOptions s) . read (parserState s)
           transform run r w
           return nullPtr

transform :: (String -> String) -> CReader -> CWriter -> IO ()
transform t reader writer = main where    
    size :: Num a => a
    size = 8 * 1024
    withBuffer action = withCWString (take size $ repeat ' ') action    
    main = withBuffer $ \rbuf ->
           withBuffer $ \wbuf ->
           process rbuf wbuf
    process rbuf wbuf = do
      s <- read rbuf
      write wbuf (t s)
    read buf = unsafeInterleaveIO r 
        where
          r      = reader buf 0 size >>= loop
          loop 0 = return []
          loop n = do
            s <- peekCWStringLen (buf, fromInteger (toInteger n))
            fmap (s ++) $ read buf
    write buf text = do
      let (head, tail) = splitAt size text
      writeString buf head
      writer buf 0 $ fromInteger (toInteger (length head))
      case tail of
        [] -> return ()
        _  -> write buf tail
    writeString buf = pokeArray buf . encodeString
    encodeString = foldr enc [] . map Char.ord
        where
          enc c wcs
              | c < 0x10000 = fromIntegral c : wcs
              | otherwise   = 
                  let c' = c - 0x10000 in
                  fromIntegral (c' `div` 0x400 + 0xd800) :
                  fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs

$( derive makeTypeable ''LibPandocSettings )
$( derive makeData ''LibPandocSettings )
$( derive makeTypeable ''ParserState )
$( derive makeData ''ParserState )
$( derive makeTypeable ''WriterOptions )
$( derive makeData ''WriterOptions )
$( derive makeTypeable ''ParserContext )
$( derive makeData ''ParserContext )
$( derive makeTypeable ''QuoteContext )
$( derive makeData ''QuoteContext )
$( derive makeTypeable ''HeaderType )
$( derive makeData ''HeaderType )
$( derive makeTypeable ''HTMLMathMethod )
$( derive makeData ''HTMLMathMethod )
$( derive makeTypeable ''ObfuscationMethod )
$( derive makeData ''ObfuscationMethod )

