{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Discount where
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)


#{enum DiscountOption, DiscountOption
 , noSuperscript     = MKD_NOSUPERSCRIPT
 , noSmartypants     = MKD_NOPANTS
 , noExt             = MKD_NO_EXT
 , noDivQuote        = MKD_NODIVQUOTE
 , noAlphaList       = MKD_NOALPHALIST
 , noDefinitionList  = MKD_NODLIST
 , autolink          = MKD_AUTOLINK
 , strict            = MKD_STRICT
 , footnotes         = MKD_EXTRA_FOOTNOTE
 }
#include <mkdio.h>

data MMIOT = MMIOT
type MMIOPtr = Ptr MMIOT

foreign import ccall unsafe "mkdio.h mkd_string" mkd_string :: CString -> CInt -> CInt -> IO MMIOPtr
foreign import ccall unsafe "mkdio.h mkd_compile" mkd_compile :: MMIOPtr -> CInt -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_document" mkd_document :: MMIOPtr -> Ptr CString -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_cleanup" mkd_cleanup :: MMIOPtr -> IO ()

parse :: String -> String
parse markdown = unsafePerformIO . alloca $ \out_buf -> do
  (markdown_c, len) <- newCStringLen markdown
  mmioptr <- mkd_string markdown_c (toEnum len) 0
  mkd_compile mmioptr 0
  mkd_document mmioptr out_buf
  result <- peek out_buf >>= peekCString
  mkd_cleanup mmioptr
  return result