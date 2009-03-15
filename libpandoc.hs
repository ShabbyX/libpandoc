{-# LANGUAGE ForeignFunctionInterface #-}

module LibPandoc (pandoc) where

import Text.Pandoc 
import Text.Pandoc.Shared
import Text.JSON
import Foreign.C.Types
import Foreign.C.String

pandoc_fmt_MARKDOWN     = 0
pandoc_fmt_RST          = 1
pandoc_fmt_HTML         = 2
pandoc_fmt_LATEX        = 3
pandoc_fmt_S5           = 4
pandoc_fmt_DOCBOOK      = 5
pandoc_fmt_ODT          = 6
pandoc_fmt_CONTEXT      = 7
pandoc_fmt_TEXINFO      = 8
pandoc_fmt_MAN          = 9
pandoc_fmt_MEDIAWIKI    = 10
pandoc_fmt_RTF          = 11

reader x | x == pandoc_fmt_MARKDOWN = Just readMarkdown
         | x == pandoc_fmt_HTML     = Just readHtml
         | x == pandoc_fmt_LATEX    = Just readLaTeX
         | x == pandoc_fmt_RST      = Just readRST
         | otherwise                = Nothing

writer x | x == pandoc_fmt_MARKDOWN     = Just writeMarkdown
         | x == pandoc_fmt_RST          = Just writeRST
         | x == pandoc_fmt_HTML         = Just writeHtmlString
         | x == pandoc_fmt_LATEX        = Just writeLaTeX
         | x == pandoc_fmt_S5           = Just writeS5String
         | x == pandoc_fmt_DOCBOOK      = Just writeDocbook
         | x == pandoc_fmt_ODT          = Just writeOpenDocument
         | x == pandoc_fmt_CONTEXT      = Just writeConTeXt
         | x == pandoc_fmt_TEXINFO      = Just writeTexinfo
         | x == pandoc_fmt_MAN          = Just writeMan
         | x == pandoc_fmt_MEDIAWIKI    = Just writeMediaWiki
         | x == pandoc_fmt_RTF          = Just writeRTF
         | otherwise                    = Nothing

defWO          = defaultWriterOptions
defMath        = writerHTMLMathMethod defWO
defObfuscation = writerEmailObfuscation defWO

instance JSON ParserContext where
    showJSON _ = undefined
    readJSON (JSString s)  = 
        return $
        case fromJSString s of
          "ListItemState" -> ListItemState
          "NullState"     -> NullState
          _               -> NullState

instance JSON QuoteContext where
    showJSON = undefined
    readJSON (JSString s) = 
        return $
        case fromJSString s of
          "InSingleQuote" -> InSingleQuote
          "InDoubleQuote" -> InDoubleQuote
          "NoQuote"       -> NoQuote
          _               -> NoQuote

instance JSON ParserState where
    showJSON = undefined
    readJSON (JSObject j) = 
        do parseRaw        <- val (stateParseRaw def) "ParseRaw"
           parserContext   <- val (stateParserContext def) "ParserContext"
           quoteContext    <- val (stateQuoteContext def) "QuoteContext"
           sanitizeHTML    <- val (stateSanitizeHTML def) "SanitizeHTML"
           keys            <- return $ stateKeys def
           notes           <- return $ stateNotes def
           tabStop         <- val (stateTabStop def) "TabStop"
           standalone      <- val (stateStandalone def) "Standalone"
           title           <- return $ stateTitle def
           authors         <- val (stateAuthors def) "Authors"
           date            <- val (stateDate def) "Date"
           strict          <- val (stateStrict def) "Strict"
           smart           <- val (stateSmart def) "Smart"
           literateHaskell <- val (stateLiterateHaskell def) "LiterateHaskell"
           columns         <- val (stateColumns def) "Columns"
           headerTable     <- return $ stateHeaderTable def
           return (ParserState parseRaw
                               parserContext
                               quoteContext
                               sanitizeHTML
                               keys
                               notes
                               tabStop
                               standalone
                               title
                               authors
                               date
                               strict
                               smart
                               literateHaskell
                               columns
                               headerTable)
               where
                 def     = defaultParserState
                 val a n = case valFromObj n j of
                             Ok x    -> Ok x
                             Error _ -> Ok a
    readJSON _ = return defaultParserState

instance JSON HTMLMathMethod where
    readJSON (JSString s) =  
        return $
        case fromJSString s of
          "PlainMath"   -> PlainMath
          "GladTeX"     -> GladTeX
          "LaTeXMathML" -> LaTeXMathML Nothing
          "JsMath"      -> JsMath Nothing
          _             -> defMath
    readJSON (JSObject j) = 
        return $ 
        case fromJSObject j of
          [("MimeTeX", JSString s)]     -> MimeTeX (fromJSString s)
          [("LaTeXMathML", JSString s)] -> LaTeXMathML (Just (fromJSString s))
          [("JsMath", JSString s)]      -> JsMath (Just (fromJSString s))
          _                             -> defMath
        
    readJSON _ = return defMath
    showJSON   = undefined

instance JSON ObfuscationMethod where
    readJSON (JSString s) = 
        return $
        case fromJSString s of
          "NoObfuscation"         -> NoObfuscation
          "ReferenceObfuscation"  -> ReferenceObfuscation
          "JavascriptObfuscation" -> JavascriptObfuscation
          _ -> defObfuscation
    readJSON _ = return defObfuscation
    showJSON   = undefined

instance JSON WriterOptions where
    readJSON (JSObject j) =     
        do standalone       <- val (writerStandalone def) "Standalone"
           header           <- val (writerHeader def) "Header"
           titlePrefix      <- val (writerTitlePrefix def) "TitlePrefix"
           tabStop          <- val (writerTabStop def) "TabStop"
           tableOfContents  <- val (writerTableOfContents def) "TableOfContents"
           s5               <- val (writerS5 def) "S5"
           htmlMathMethod   <- val (writerHTMLMathMethod def) "HTMLMathMethod"
           ignoreNotes      <- val (writerIgnoreNotes def) "IgnoreNotes"
           incremental      <- val (writerIncremental def) "Incremental"
           numberSections   <- val (writerNumberSections def) "NumberSections"
           includeBefore    <- val (writerIncludeBefore def) "IncludeBefore"
           includeAfter     <- val (writerIncludeAfter def) "IncludeAfter"
           strictMarkdown   <- val (writerStrictMarkdown def) "StrictMarkdown"
           wrapText         <- val (writerWrapText def) "WrapText"
           referenceLinks   <- val (writerReferenceLinks def) "ReferenceLinks"
           literateHaskell  <- val (writerLiterateHaskell def) "LiterateHaskell"
           emailObfuscation <- 
               val (writerEmailObfuscation def) "EmailObfuscation"
           return $ (WriterOptions standalone
                                   header
                                   titlePrefix
                                   tabStop
                                   tableOfContents
                                   s5
                                   htmlMathMethod
                                   ignoreNotes
                                   incremental
                                   numberSections
                                   includeBefore
                                   includeAfter
                                   strictMarkdown
                                   referenceLinks
                                   wrapText
                                   literateHaskell
                                   emailObfuscation)
               where
                 def     = defWO
                 val a n = case valFromObj n j of
                             Ok x    -> Ok x
                             Error _ -> Ok a
    readJSON _  = return defWO
    showJSON    = undefined

pandoc :: CInt -> CString -> CInt -> CString -> CString -> IO CString
pandoc i ps o wo t = do
  ps <- fmap decode $ peekCString ps
  wo <- fmap decode $ peekCString wo
  t  <- peekCString t
  newCString $ case (reader i, writer o, wo, ps) of
                 (Just i, Just o, Ok wo, Ok ps) -> o wo $ i ps t
                 _ -> "pandoc error: invalid options"

foreign export ccall pandoc :: 
    CInt    -> -- input format code
    CString -> -- JSON-encoded extra ParserState options
    CInt    -> -- output format code
    CString -> -- JSON-encoded extra WriterOptions
    CString -> -- input text 
    IO CString -- output tex

