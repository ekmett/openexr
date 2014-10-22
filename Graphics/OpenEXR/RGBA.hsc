{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.OpenEXR.RGBA where

import Foreign.Storable
import Graphics.OpenEXR.Half

#include <ImfCRgbaFile.h>

data RGBA = RGBA !Half !Half !Half !Half deriving (Eq,Ord,Show)

instance Storable RGBA where
  sizeOf _ = (#size ImfRgba)
  alignment = sizeOf
  peek p = do
    r <- (#peek ImfRgba, r) p
    g <- (#peek ImfRgba, g) p
    b <- (#peek ImfRgba, b) p
    a <- (#peek ImfRgba, a) p
    return $! RGBA r g b a
  poke p (RGBA r g b a) = do
    (#poke ImfRgba, r) p r
    (#poke ImfRgba, g) p g
    (#poke ImfRgba, b) p b
    (#poke ImfRgba, a) p a

instance Num RGBA where
  RGBA a b c d + RGBA a' b' c' d' = RGBA (a + a') (b + b') (c + c') (d + d')
  RGBA a b c d - RGBA a' b' c' d' = RGBA (a - a') (b - b') (c - c') (d - d')
  RGBA a b c d * RGBA a' b' c' d' = RGBA (a * a') (b * b') (c * c') (d * d')
  negate (RGBA a b c d) = RGBA (negate a) (negate b) (negate c) (negate d)
  abs (RGBA a b c d) = RGBA (abs a) (abs b) (abs c) (abs d)
  signum (RGBA a b c d) = RGBA (signum a) (signum b) (signum c) (signum d)
  fromInteger a = RGBA n n n n where n = fromInteger a
