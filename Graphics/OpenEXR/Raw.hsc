{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.OpenEXR.Raw where

import Data.Word
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <ImfCRgbaFile.h>

-- <https://github.com/openexr/openexr/issues/140>
#ifndef IMF_RANDOM_Y
#define IMF_RANDOM_Y IMF_RAMDOM_Y
#endif

type Half = (#type ImfHalf)

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfFloatToHalf" floatToHalf :: CFloat -> Ptr Half -> IO ()
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfFloatToHalfArray" floatToHalfArray :: CInt -> Ptr Float -> Ptr Half -> IO ()
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHalfToFloat" halfToFloat :: Half -> IO CFloat
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImgHalfToFloatArray" halfToFloatArray :: CInt -> Ptr Half -> Ptr Float -> IO ()

data Rgba = Rgba { rgba_r, rgba_g, rgba_b, rgba_a :: Half } deriving (Eq,Show)

instance Storable Rgba where
  sizeOf _ = (#size ImfRgba)
  alignment = sizeOf
  peek p = do
    r <- (#peek ImfRgba, r) p
    g <- (#peek ImfRgba, g) p
    b <- (#peek ImfRgba, b) p
    a <- (#peek ImfRgba, a) p
    return $! Rgba r g b a
  poke p (Rgba r g b a) = do
    (#poke ImfRgba, r) p r
    (#poke ImfRgba, g) p g
    (#poke ImfRgba, b) p b
    (#poke ImfRgba, a) p a

-- | Magic number; this must be the same as Imf::MAGIC
pattern MAGIC = (#const IMF_MAGIC)

-- | Version number; this must be the same as Imf::EXR_VERSION
pattern VERSION_NUMBER = (#const IMF_VERSION_NUMBER)

-- * Line order; values must the the same as in Imf::LineOrder.

type LineOrder = CInt

pattern INCREASING_Y = (#const IMF_INCREASING_Y) :: LineOrder
pattern DECREASING_Y = (#const IMF_DECREASING_Y) :: LineOrder
pattern RANDOM_Y     = (#const IMF_RANDOM_Y) :: LineOrder

-- * Compression types; values must be the same as in Imf::Compression.
type Compression = CInt

pattern NO_COMPRESSION    = (#const IMF_NO_COMPRESSION) :: Compression
pattern RLE_COMPRESSION   = (#const IMF_RLE_COMPRESSION) :: Compression
pattern ZIPS_COMPRESSION  = (#const IMF_ZIPS_COMPRESSION) :: Compression
pattern ZIP_COMPRESSION   = (#const IMF_ZIP_COMPRESSION) :: Compression
pattern PIZ_COMPRESSION   = (#const IMF_PIZ_COMPRESSION) :: Compression
pattern PXR24_COMPRESSION = (#const IMF_PXR24_COMPRESSION) :: Compression
pattern B44_COMPRESSION   = (#const IMF_B44_COMPRESSION) :: Compression
pattern B44A_COMPRESSION  = (#const IMF_B44A_COMPRESSION) :: Compression

-- * Channels; values must be the same as in Imf::RgbaChannels. 
pattern WRITE_R = (#const IMF_WRITE_R)
pattern WRITE_G = (#const IMF_WRITE_G)
pattern WRITE_B = (#const IMF_WRITE_B)
pattern WRITE_A = (#const IMF_WRITE_A)
pattern WRITE_Y = (#const IMF_WRITE_Y)
pattern WRITE_C = (#const IMF_WRITE_C)
pattern WRITE_RGB = (#const IMF_WRITE_RGB)
pattern WRITE_RGBA = (#const IMF_WRITE_RGBA)
pattern WRITE_YC = (#const IMF_WRITE_YC)
pattern WRITE_YA = (#const IMF_WRITE_YA)
pattern WRITE_YCA = (#const IMF_WRITE_YCA)

-- * Level modes; values must be the same as in Imf::LevelMode

pattern ONE_LEVEL = (#const IMF_ONE_LEVEL)
pattern MIPMAP_LEVELS = (#const IMF_MIPMAP_LEVELS)
pattern RIPMAP_LEVELS = (#const IMF_RIPMAP_LEVELS)

-- * Level rounding modes; values must be the same as in Imf::LevelRoundingMode

pattern ROUND_DOWN = (#const IMF_ROUND_DOWN)
pattern ROUND_UP   = (#const IMF_ROUND_UP)

-- * Headers

data OpaqueHeader

type Header = Ptr OpaqueHeader

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfNewHeader"     newHeader :: IO Header
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfDeleteHeader"  deleteHeader :: Header -> IO ()
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfCopyHeader"    copyHeader :: Header -> IO Header

-- | @headerSetDisplayWindow hdr xMin yMin xMax yMax@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetDisplayWindow"      headerSetDisplayWindow      :: Header -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | @headerDisplayWindow hdr xMin yMin xMax yMax@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderDisplayWindow"         headerDisplayWindow         :: Header -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | @headerSetDataWindow hdr xMin yMin xMax yMax@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetDataWindow"         headerSetDataWindow         :: Header -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | @headerDataWindow hdr xMin yMin xMax yMax@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderDataWindow"            headerDataWindow            :: Header -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | @headerSetPixelAspectRatio hdr pixelAspectRatio@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetPixelAspectRatio"   headerSetPixelAspectRatio   :: Header -> CFloat -> IO ()
-- | @headerPixelAspectRatio hdr@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderPixelAspectRatio"      headerPixelAspectRatio      :: Header -> IO CFloat

-- | @headerSetScreenWindowCenter hdr x y@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetScreenWindowCenter" headerSetScreenWindowCenter :: Header -> CFloat -> CFloat -> IO ()

-- | @headerScreenWindowCenter hdr x y@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderScreenWindowCenter"    headerScreenWindowCenter    :: Header -> Ptr CFloat -> Ptr CFloat -> IO ()

-- | @headerSetScreenWindowWidth hdr pixelAspectRatio@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetScreenWindowWidth"  headerSetScreenWindowWidth  :: Header -> CFloat -> IO ()
-- | @headerScreenWindowWidth hdr@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderScreenWindowWidth"     headerScreenWindowWidth     :: Header -> IO CFloat

-- | @headerSetLineOrder hdr pixelAspectRatio@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetLineOrder"          headerSetLineOrder          :: Header -> LineOrder -> IO ()

-- | @headerLineOrder hdr@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderLineOrder"             headerLineOrder             :: Header -> IO LineOrder

-- | @headerSetCompression hdr pixelAspectRatio@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetCompression"        headerSetCompression        :: Header -> Compression -> IO ()

-- | @headerCompression hdr@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderCompression"           headerCompression           :: Header -> IO Compression

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetIntAttribute"       headerSetIntAttribute       :: Header -> CString -> CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderIntAttribute"          headerIntAttribute          :: Header -> CString -> Ptr CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetFloatAttribute"     headerSetFloatAttribute     :: Header -> CString -> CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderFloatAttribute"        headerFloatAttribute        :: Header -> CString -> Ptr CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetDoubleAttribute"    headerSetDoubleAttribute    :: Header -> CString -> CDouble -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderDoubleAttribute"       headerDoubleAttribute       :: Header -> CString -> Ptr CDouble -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetStringAttribute"    headerSetStringAttribute    :: Header -> CString -> CString -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderStringAttribute"       headerStringAttribute       :: Header -> CString -> Ptr CString -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetBox2iAttribute"     headerSetBox2iAttribute     :: Header -> CString -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderBox2iAttribute"        headerBox2iAttribute        :: Header -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetBox2fAttribute"     headerSetBox2fAttribute     :: Header -> CString -> CFloat -> CFloat -> CFloat -> CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderBox2fAttribute"        headerBox2fAttribute        :: Header -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetV2iAttribute"       headerSetV2iAttribute       :: Header -> CString -> CInt -> CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderV2iAttribute"          headerV2iAttribute          :: Header -> CString -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetV2fAttribute"       headerSetV2fAttribute       :: Header -> CString -> CFloat -> CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderV2fAttribute"          headerV2fAttribute          :: Header -> CString -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetV3iAttribute"       headerSetV3iAttribute       :: Header -> CString -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderV3iAttribute"          headerV3iAttribute          :: Header -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetV3fAttribute"       headerSetV3fAttribute       :: Header -> CString -> CFloat -> CFloat -> CFloat -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderV3fAttribute"          headerV3fAttribute          :: Header -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt

-- | @float m[3][3]@
type M33 = Ptr CFloat

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetM33fAttribute"      headerSetM33fAttribute      :: Header -> CString -> M33 -> IO CInt 
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderM33fAttribute"         headerM33fAttribute         :: Header -> CString -> M33 -> IO CInt 

-- | @float m[4][4]@
type M44 = Ptr CFloat

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderSetM44fAttribute"      headerSetM44fAttribute      :: Header -> CString -> M44 -> IO CInt 
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfHeaderM44fAttribute"         headerM44fAttribute         :: Header -> CString -> M44 -> IO CInt 

-- * Output Files

data OpaqueOutputFile

type OutputFile = Ptr OpaqueOutputFile

-- | @openOutputFile name header channels@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfOpenOutputFile" openOutputFile :: CString -> Header -> CInt -> IO OutputFile
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfCloseOutputFile" closeOutputFile :: OutputFile -> IO CInt

-- | @outputSetFrameBuffer out base xStride yStride@
foreign import ccall "OpenEXR/ImCRgbaFile.h ImfOutputSetFrameBuffer" outputSetFrameBuffer :: OutputFile -> Ptr Rgba -> Ptr CSize -> Ptr CSize -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfOutputWritePixels" outputWritePixels :: OutputFile -> CInt -> IO CInt

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfOutputCurrentScanLine" outputCurrentScanLine :: OutputFile -> IO CInt

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfOutputHeader" outputHeader :: OutputFile -> IO Header

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfOutputChannels" outputChannels :: OutputFile -> IO CInt

-- TODO

-- * Input Files

data OpaqueInputFile

type InputFile = Ptr OpaqueInputFile

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfOpenInputFile" openInputFile :: CString -> IO InputFile
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfCloseInputFile" closeInputFile :: InputFile -> IO CInt
-- | @inputSetFrameBuffer inputFile base xStride yStride@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfInputSetFrameBuffer" inputSetFrameBuffer :: InputFile -> Ptr Rgba -> CSize -> CSize -> IO CInt
-- | @inputReadPixels input scanLine1 scanLine2@
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfInputReadPixels" inputReadPixels :: InputFile -> CInt -> CInt -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfInputHeader" inputHeader :: InputFile -> IO Header
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfInputChannels" inputChannels :: InputFile -> IO CInt
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfInputFileName" inputFileName :: InputFile -> IO CString

-- * TODO: Tiled Output Files
data OpaqueTiledOutputFile
type TiledOutputFile = Ptr OpaqueTiledOutputFile

-- * TODO: Tiled Input Files
data OpaqueTiledInputFile
type TiledInputFile = Ptr OpaqueTiledInputFile

-- * Lookup Tables

data OpaqueLut
type Lut = Ptr OpaqueLut

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfNewRound12logLut" newRound12logLut :: CInt -> IO Lut
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfNewRoundNBitLut" newRoundNBitLut :: CUInt -> CInt -> IO Lut
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfDeleteLut" deleteLut :: Lut -> IO ()
foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfApplyLut" applyLut :: Lut -> Ptr Rgba -> CInt -> CInt -> IO ()

foreign import ccall "OpenEXR/ImfCRgbaFile.h ImfErrorMessage" errorMessage :: IO CString
