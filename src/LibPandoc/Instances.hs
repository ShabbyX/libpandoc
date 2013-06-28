{-# LANGUAGE TemplateHaskell #-}

module LibPandoc.Instances where

import Data.Data
import Data.DeriveTH
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Highlighting.Kate.Types

$( derive makeTypeable ''ReaderOptions )
$( derive makeData ''ReaderOptions )
$( derive makeTypeable ''WriterOptions )
$( derive makeData ''WriterOptions )
$( derive makeTypeable ''Extension )
$( derive makeData ''Extension )
$( derive makeTypeable ''HTMLSlideVariant )
$( derive makeData ''HTMLSlideVariant )
$( derive makeTypeable ''CiteMethod )
$( derive makeData ''CiteMethod )
$( derive makeTypeable ''EPUBVersion )
$( derive makeData ''EPUBVersion )
$( derive makeTypeable ''Style )
$( derive makeData ''Style )
$( derive makeTypeable ''HTMLMathMethod )
$( derive makeData ''HTMLMathMethod )
$( derive makeTypeable ''ObfuscationMethod )
$( derive makeData ''ObfuscationMethod )
$( derive makeTypeable ''TokenType )
$( derive makeData ''TokenType )
$( derive makeTypeable ''TokenStyle )
$( derive makeData ''TokenStyle )
$( derive makeTypeable ''Color )
$( derive makeData ''Color )

