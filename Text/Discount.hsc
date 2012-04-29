{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Discount where
import Data.ByteString
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)


newtype DiscountOption = DiscountOption { unDiscountOption :: CInt } deriving (Eq, Show)

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
#include <mkdio.h>

data MMIOT = MMIOT
type MMIOPtr = Ptr MMIOT

foreign import ccall unsafe "mkdio.h mkd_string" mkd_string :: CString -> CInt -> CInt -> IO MMIOPtr
foreign import ccall unsafe "mkdio.h mkd_compile" mkd_compile :: MMIOPtr -> CInt -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_document" mkd_document :: MMIOPtr -> Ptr CString -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_cleanup" mkd_cleanup :: MMIOPtr -> IO ()

parseMarkdown :: ByteString -> ByteString
parseMarkdown markdown = unsafePerformIO . alloca $ \out_buf -> useAsCStringLen markdown $ \(markdown_c, len) -> do
  mmioptr <- mkd_string markdown_c (toEnum len) 0
  mkd_compile mmioptr 0
  mkd_document mmioptr out_buf
  result <- peek out_buf >>= packCString
  mkd_cleanup mmioptr
  return result