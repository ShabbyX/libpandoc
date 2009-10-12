{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module LibPandoc where

import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO
import System.IO.Unsafe
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Printf

#include "pandoc.h"

type CReader    = CString -> CInt -> IO CInt
type CWriter    = CString -> CInt -> IO ()
type CReaderPtr = FunPtr CReader
type CWriterPtr = FunPtr CWriter
type CPandoc    = CInt -> CInt -> CInt -> CString ->
                  CReaderPtr -> CWriterPtr -> IO CString

foreign export ccall "pandoc"  pandoc     :: CPandoc
foreign import ccall "dynamic" peekReader :: CReaderPtr -> CReader
foreign import ccall "dynamic" peekWriter :: CWriterPtr -> CWriter

data Format = ConTeXt
            | Docbook
            | Html
            | Html_LHS
            | LaTeX
            | LaTeX_LHS
            | Man
            | Markdown
            | Markdown_LHS
            | MediaWiki
            | Native
            | ODT
            | OpenDocument
            | RST
            | RST_LHS
            | RTF
            | S5
            | Texinfo
              deriving Show

readFlags :: CInt -> (ParserState, WriterOptions)
readFlags flags = foldl' fold defaults rules where
    defaults = (defaultParserState, defaultWriterOptions)
    fold x (mask, g) | flags .&. mask == 0 = x
                     | otherwise           = g x
    rules = 
        [((#const PANDOC_FLAG_EMAIL_OBFUSCATION_NONE),
          (\(r, w) -> (r, w { writerEmailObfuscation = NoObfuscation })))
        ,((#const PANDOC_FLAG_EMAIL_OBFUSCATION_JAVASCRIPT),
          (\(r, w) ->
           (r, w { writerEmailObfuscation = JavascriptObfuscation })))
        ,((#const PANDOC_FLAG_EMAIL_OBFUSCATION_REFERENCES),
          (\(r, w) -> (r, w { writerEmailObfuscation = ReferenceObfuscation })))
        ,((#const PANDOC_FLAG_INCREMENTAL),
          (\(r, w) -> (r, w { writerIncremental = True })))
        ,((#const PANDOC_FLAG_MATH_ASCIIMATHML),
          (\(r, w) -> (r, w { writerHTMLMathMethod = PlainMath })))
        ,((#const PANDOC_FLAG_MATH_GLADTEX),
          (\(r, w) -> (r, w { writerHTMLMathMethod = GladTeX })))
        ,((#const PANDOC_FLAG_MATH_JSMATH),
          (\(r, w) -> (r, w { writerHTMLMathMethod = JsMath Nothing })))
        ,((#const PANDOC_FLAG_MATH_LATEXMATHML),
          (\(r, w) -> (r, w { writerHTMLMathMethod = LaTeXMathML Nothing })))
        ,((#const PANDOC_FLAG_MATH_MIMETEX),
          (\(r, w) -> 
           (r, w { writerHTMLMathMethod = MimeTeX "/cgi-bin/mimetex.cgi" })))
        ,((#const PANDOC_FLAG_NO_WRAP),
          (\(r, w) -> (r, w { writerWrapText = False })))
        ,((#const PANDOC_FLAG_NUMBER_SECTIONS),
          (\(r, w) -> (r, w { writerNumberSections = True })))
        ,((#const PANDOC_FLAG_PARSE_RAW),
          (\(r, w) -> (r { stateParseRaw = True }, w)))
        ,((#const PANDOC_FLAG_PRESERVE_TABS),
          (\(r, w) -> (r { stateTabStop = 0 }, w)))
        ,((#const PANDOC_FLAG_REFERENCE_LINKS),
          (\(r, w) -> (r, w { writerReferenceLinks = True })))
        ,((#const PANDOC_FLAG_SANITIZE_HTML),
          (\(r, w) -> (r { stateSanitizeHTML = True }, w)))
        ,((#const PANDOC_FLAG_SMART),
          (\(r, w) -> (r { stateSmart = True }, w)))
        ,((#const PANDOC_FLAG_STANDALONE),
          (\(r, w) -> (r { stateStandalone = True }, w)))
        ,((#const PANDOC_FLAG_STRICT),
          (\(r, w) -> (r { stateStrict = True }, w)))
        ,((#const PANDOC_FLAG_TOC),
          (\(r, w) -> (r, w { writerTableOfContents = True })))
        ]

type PandocError = String

type E m a = ErrorT PandocError m a

throw :: Monad m => String -> E m a
throw = throwError

getFormat :: Monad m => CInt -> E m Format
getFormat (#const PANDOC_FMT_CONTEXT)      = return ConTeXt
getFormat (#const PANDOC_FMT_DOCBOOK)      = return Docbook
getFormat (#const PANDOC_FMT_HTML)         = return Html
getFormat (#const PANDOC_FMT_HTML_LHS)     = return Html_LHS
getFormat (#const PANDOC_FMT_LATEX)        = return LaTeX
getFormat (#const PANDOC_FMT_LATEX_LHS)    = return LaTeX_LHS
getFormat (#const PANDOC_FMT_MAN)          = return Man
getFormat (#const PANDOC_FMT_MARKDOWN)     = return Markdown
getFormat (#const PANDOC_FMT_MARKDOWN_LHS) = return Markdown_LHS
getFormat (#const PANDOC_FMT_MEDIAWIKI)    = return MediaWiki
getFormat (#const PANDOC_FMT_NATIVE)       = return Native
getFormat (#const PANDOC_FMT_ODT)          = return ODT
getFormat (#const PANDOC_FMT_OPENDOCUMENT) = return OpenDocument
getFormat (#const PANDOC_FMT_RST)          = return RST
getFormat (#const PANDOC_FMT_RST_LHS)      = return RST_LHS
getFormat (#const PANDOC_FMT_RTF)          = return RTF
getFormat (#const PANDOC_FMT_S5)           = return S5
getFormat (#const PANDOC_FMT_TEXINFO)      = return Texinfo
getFormat k =
    let n = conv k :: Int in
    throw $ printf "Pandoc: unknown format code - %i" n

writerWithHeader :: String -> Writer -> Writer
writerWithHeader h w o | writerHeader o == "" = w $ o { writerHeader = h }
                       | otherwise            = w o

type Writer = WriterOptions -> Pandoc -> String       
type Reader = ParserState   -> String -> Pandoc

writerLHS :: Writer -> Writer
writerLHS w o = w $ o { writerLiterateHaskell = True }

writerByFormat :: Format -> Writer
writerByFormat ConTeXt      = writerWithHeader defaultConTeXtHeader writeConTeXt
writerByFormat Docbook      = writerWithHeader defaultDocbookHeader writeDocbook
writerByFormat Html         = writeHtmlString
writerByFormat Html_LHS     = writerLHS writeHtmlString
writerByFormat LaTeX        = writerWithHeader defaultLaTeXHeader writeLaTeX
writerByFormat LaTeX_LHS    = let w = writerLHS writeLaTeX in
                              writerWithHeader defaultLaTeXHeader w
writerByFormat Man          = writeMan
writerByFormat Markdown     = writeMarkdown
writerByFormat Markdown_LHS = writerLHS writeMarkdown
writerByFormat MediaWiki    = writeMediaWiki
writerByFormat Native       = const prettyPandoc
writerByFormat ODT          = let h = defaultOpenDocumentHeader in 
                              writerWithHeader h writeOpenDocument
writerByFormat OpenDocument = let h = defaultOpenDocumentHeader in
                              writerWithHeader h writeOpenDocument
writerByFormat RST          = writeRST
writerByFormat RST_LHS      = writerLHS writeRST
writerByFormat RTF          = writerWithHeader defaultRTFHeader writeRTF
writerByFormat S5           = writerWithHeader defaultS5Header writeS5String
writerByFormat Texinfo      = writeTexinfo

readerByFormat :: Monad m => Format -> E m Reader
readerByFormat Html     = return readHtml
readerByFormat Markdown = return readMarkdown
readerByFormat LaTeX    = return readLaTeX
readerByFormat RST      = return readRST
readerByFormat fmt      = let s = show fmt
                              e = printf "Pandoc: %s reader is not supported" s
                          in
                            throw e
pandoc :: CPandoc
pandoc inputFormat outputFormat flags options readerPtr writerPtr = run where
    main = do inpFmt <- getFormat inputFormat
              outFmt <- getFormat outputFormat
              reader <- readerByFormat inpFmt
              let writer               = writerByFormat outFmt
                  cReader              = peekReader readerPtr
                  cWriter              = peekWriter writerPtr                              
                  (rOptions, wOptions) = readFlags flags
              let transform = writer wOptions . reader rOptions
              let process buf = do
                    c <- readWithBuffer buf cReader
                    let chunks = chunked bufSz $ transform c
                    sequence_ $ map (writeTo cWriter) chunks
              let bufData = take bufSz $ repeat ' '
              liftIO $ withCString bufData $ \x -> liftIO $ process x
    run = runErrorT main >>= handle where
      handle (Left e)   = newCString e
      handle (Right ()) = return nullPtr

readWithBuffer :: CString -> CReader -> IO String
readWithBuffer buf reader = unsafeInterleaveIO read where
    read   = reader buf bufSz >>= loop
    loop 0 = return []
    loop n = do
      s <- peekCStringLen (buf, conv n)
      fmap (s ++) $ readWithBuffer buf reader

writeTo :: CWriter -> String -> IO ()
writeTo writer text = mapM_ w chunks where
    w chunk = withCStringLen chunk $ \(ptr, len) -> writer ptr (conv len)
    chunks  = chunked bufSz text

chunked :: Int -> [a] -> [[a]]
chunked n list = f $ splitAt n list where
    f (h, []) = [h]
    f (h, t)  = h : chunked n t

bufSz :: Num a => a
bufSz = 8 * 1024

conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger
