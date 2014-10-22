{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.OpenEXR.RGBA where

import Data.Bits
import Data.Function (on)
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

#include <ImfCRgbaFile.h>

data RGBA = RGBA !Half !Half !Half !Half deriving (Eq,Show)

instance Storable RGBA where
  sizeOf _ = (#size ImgRgba)
  alignment = sizeOf
  peek p = do
    r <- (#peek ImgRgba, r) p
    g <- (#peek ImgRgba, g) p
    b <- (#peek ImgRgba, b) p
    a <- (#peek ImgRgba, a) p
    return $! RGBA r g b a
  poke p (RGBA r g b a) = do
    (#poke ImgRgba, r) p r
    (#poke ImgRgba, g) p g
    (#poke ImgRgba, b) p b
    (#poke ImgRgba, a) p a

instance Num RGBA where
  RGBA a b c d + RGBA a' b' c' d' = RGBA (a + a') (b + b') (c + c') (d + d')
  RGBA a b c d - RGBA a' b' c' d' = RGBA (a - a') (b - b') (c - c') (d - d')
  RGBA a b c d * RGBA a' b' c' d' = RGBA (a * a') (b * b') (c * c') (d * d')
  negate (RGBA a b c d) = RGBA (negate a) (negate b) (negate c) (negate d)
  abs (RGBA a b c d) = RGBA (abs a) (abs b) (abs c) (abs d)
  signum (RGBA a b c d) = RGBA (signum a) (signum b) (signum c) (signum d)
  fromInteger a = RGBA n n n n where n = fromInteger a
