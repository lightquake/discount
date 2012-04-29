{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Discount.Internal where

import Data.Bits
import Foreign.C

newtype DiscountOption = DiscountOption { unDiscountOption :: CInt } deriving (Eq, Show)

#include <mkdio.h>
#{enum DiscountOption, DiscountOption
 , noSuperscript     = MKD_NOSUPERSCRIPT
 , noLinks           = MKD_NOLINKS
 , noImages          = MKD_NOIMAGE
 , noHeader          = MKD_NOHEADER
 , noHtml            = MKD_NOHTML
 , noStrikethrough   = MKD_NOSTRIKETHROUGH
 , noTables          = MKD_NOTABLES
 , noSmartypants     = MKD_NOPANTS
 , noExt             = MKD_NO_EXT
 , noDivQuote        = MKD_NODIVQUOTE
 , noAlphaList       = MKD_NOALPHALIST
 , noDefinitionList  = MKD_NODLIST
 , autolink          = MKD_AUTOLINK
 , tagText           = MKD_TAGTEXT
 , cdata             = MKD_CDATA
 , strict            = MKD_STRICT
 , footnotes         = MKD_EXTRA_FOOTNOTE
 , tableOfContents   = MKD_TOC
 , safeLink          = MKD_SAFELINK
 , tabstop           = MKD_TABSTOP
 }

-- |INTERNAL USE ONLY. Combine a list of 'DiscountOption' values into
-- a single flag.
combineOptions :: [DiscountOption] -> CInt
combineOptions = foldr ((.|.) . unDiscountOption) 0