{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Discount (
    parseMarkdown
  , module Text.Discount.Internal
  ) where

import Data.ByteString
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Text.Discount.Internal hiding (unDiscountOption)

data MMIOT = MMIOT
type MMIOPtr = Ptr MMIOT

foreign import ccall unsafe "mkdio.h mkd_string" mkd_string :: CString -> CInt -> CInt -> IO MMIOPtr
foreign import ccall unsafe "mkdio.h mkd_compile" mkd_compile :: MMIOPtr -> CInt -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_document" mkd_document :: MMIOPtr -> Ptr CString -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_cleanup" mkd_cleanup :: MMIOPtr -> IO ()

parseMarkdown :: [DiscountOption] -> ByteString -> ByteString
parseMarkdown opts markdown = unsafePerformIO . alloca $ \out_buf -> useAsCStringLen markdown $ \(markdown_c, len) -> do
  mmioptr <- mkd_string markdown_c (toEnum len) flag
  mkd_compile mmioptr flag
  mkd_document mmioptr out_buf
  result <- peek out_buf >>= packCString
  mkd_cleanup mmioptr
  return result

  where flag = combineOptions opts