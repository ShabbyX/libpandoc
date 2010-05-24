{-# LANGUAGE TemplateHaskell #-}

module LibPandoc.Instances where

import Data.Data
import Data.DeriveTH
import Text.Pandoc
import Text.Pandoc.Shared

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

