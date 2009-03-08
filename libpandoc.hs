{-# LANGUAGE ForeignFunctionInterface #-}

module LibPandoc (pandoc) where

import Text.Pandoc 
import Foreign.C.Types
import Foreign.C.String

convert :: String -> String -> String -> String
convert src tgt text =
    case reader src of
      Left e  -> e 
      Right r -> writer tgt (r text)

dps = defaultParserState

reader "markdown" = Right $ readMarkdown dps
reader "html"     = Right $ readHtml     dps
reader "latex"    = Right $ readLaTeX    dps
reader "rst"      = Right $ readRST      dps
reader x          = Left  $ unwords ["unknown reader:", x, ";",
                                     "available readers are:",
                                     "markdown, html, latex, rst"]

wo = defaultWriterOptions

writer "context"      = writeConTeXt wo
writer "docbook"      = writeDocbook wo
writer "html"         = writeHtmlString wo
writer "latex"        = writeLaTeX wo
writer "man"          = writeMan wo
writer "markdown"     = writeMarkdown wo
writer "mediawiki"    = writeMediaWiki wo
writer "opendocument" = writeOpenDocument wo
writer "rst"          = writeRST wo
writer "rtf"          = writeRTF wo
writer "s5"           = writeS5String wo
writer "texinfo"      = writeTexinfo wo
writer x = \_ ->
           unwords ["unknown writer:", x, ";",
                    "available writers are:",
                    "context, docbook, html,",
                    "latex, man, markdown, mediawiki,",
                    "opendocument, rst, rtf, s5, texinfo"]           

pandoc :: CString -> CString -> CString -> IO CString
pandoc src tgt text = do
  src' <- peekCString src
  tgt' <- peekCString tgt
  txt' <- peekCString text  
  newCString $ convert src' tgt' txt'

foreign export ccall pandoc :: CString -> CString -> CString -> IO CString

