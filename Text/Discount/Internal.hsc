{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Discount.Internal where

import Data.Bits
import Foreign.C

newtype DiscountOption = DiscountOption { unDiscountOption :: CInt } deriving (Eq, Show)

-- |INTERNAL USE ONLY. Combine a list of 'DiscountOption' values into
-- a single flag.
combineOptions :: [DiscountOption] -> DiscountOption
combineOptions = DiscountOption . foldr ((.|.) . unDiscountOption) 0