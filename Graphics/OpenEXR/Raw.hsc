{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.OpenEXR.Raw where

import Graphics.OpenEXR.Half
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <ImfCRgbaFile.h>

-- <https://github.com/openexr/openexr/issues/140>
#ifndef IMF_RANDOM_Y
#define IMF_RANDOM_Y IMF_RAMDOM_Y
#endif

-- | Magic number
--
-- same as Imf::MAGIC
pattern MAGIC = (#const IMF_MAGIC)

-- | Version number
--
-- same as Imf::EXR_VERSION
pattern VERSION_NUMBER = (#const IMF_VERSION_NUMBER)

-- * Line order; values must the the same as in Imf::LineOrder.

type LineOrder = CInt

pattern INCREASING_Y = (#const IMF_INCREASING_Y) :: LineOrder
pattern DECREASING_Y = (#const IMF_DECREASING_Y) :: LineOrder
pattern RANDOM_Y     = (#const IMF_RANDOM_Y)     :: LineOrder

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
pattern WRITE_R    = (#const IMF_WRITE_R)
pattern WRITE_G    = (#const IMF_WRITE_G)
pattern WRITE_B    = (#const IMF_WRITE_B)
pattern WRITE_A    = (#const IMF_WRITE_A)
pattern WRITE_Y    = (#const IMF_WRITE_Y)
pattern WRITE_C    = (#const IMF_WRITE_C)
pattern WRITE_RGB  = (#const IMF_WRITE_RGB)
pattern WRITE_RGBA = (#const IMF_WRITE_RGBA)
pattern WRITE_YC   = (#const IMF_WRITE_YC)
pattern WRITE_YA   = (#const IMF_WRITE_YA)
pattern WRITE_YCA  = (#const IMF_WRITE_YCA)

-- * Level modes; values must be the same as in Imf::LevelMode

pattern ONE_LEVEL     = (#const IMF_ONE_LEVEL)
pattern MIPMAP_LEVELS = (#const IMF_MIPMAP_LEVELS)
pattern RIPMAP_LEVELS = (#const IMF_RIPMAP_LEVELS)

-- * Level rounding modes; values must be the same as in Imf::LevelRoundingMode

pattern ROUND_DOWN = (#const IMF_ROUND_DOWN)
pattern ROUND_UP   = (#const IMF_ROUND_UP)

-- * Headers

data OpaqueHeader

type Header = Ptr OpaqueHeader

foreign import ccall unsafe "ImfNewHeader"     newHeader :: IO Header
foreign import ccall unsafe "ImfDeleteHeader"  deleteHeader :: Header -> IO ()
foreign import ccall unsafe "ImfCopyHeader"    copyHeader :: Header -> IO Header

-- | @headerSetDisplayWindow hdr xMin yMin xMax yMax@
foreign import ccall unsafe "ImfHeaderSetDisplayWindow"      headerSetDisplayWindow      :: Header -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | @headerDisplayWindow hdr xMin yMin xMax yMax@
foreign import ccall unsafe "ImfHeaderDisplayWindow"         headerDisplayWindow         :: Header -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | @headerSetDataWindow hdr xMin yMin xMax yMax@
foreign import ccall unsafe "ImfHeaderSetDataWindow"         headerSetDataWindow         :: Header -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | @headerDataWindow hdr xMin yMin xMax yMax@
foreign import ccall unsafe "ImfHeaderDataWindow"            headerDataWindow            :: Header -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | @headerSetPixelAspectRatio hdr pixelAspectRatio@
foreign import ccall unsafe "ImfHeaderSetPixelAspectRatio"   headerSetPixelAspectRatio   :: Header -> CFloat -> IO ()
-- | @headerPixelAspectRatio hdr@
foreign import ccall unsafe "ImfHeaderPixelAspectRatio"      headerPixelAspectRatio      :: Header -> IO CFloat

-- | @headerSetScreenWindowCenter hdr x y@
foreign import ccall unsafe "ImfHeaderSetScreenWindowCenter" headerSetScreenWindowCenter :: Header -> CFloat -> CFloat -> IO ()

-- | @headerScreenWindowCenter hdr x y@
foreign import ccall unsafe "ImfHeaderScreenWindowCenter"    headerScreenWindowCenter    :: Header -> Ptr CFloat -> Ptr CFloat -> IO ()

-- | @headerSetScreenWindowWidth hdr pixelAspectRatio@
foreign import ccall unsafe "ImfHeaderSetScreenWindowWidth"  headerSetScreenWindowWidth  :: Header -> CFloat -> IO ()
-- | @headerScreenWindowWidth hdr@
foreign import ccall unsafe "ImfHeaderScreenWindowWidth"     headerScreenWindowWidth     :: Header -> IO CFloat

-- | @headerSetLineOrder hdr pixelAspectRatio@
foreign import ccall unsafe "ImfHeaderSetLineOrder"          headerSetLineOrder          :: Header -> LineOrder -> IO ()

-- | @headerLineOrder hdr@
foreign import ccall unsafe "ImfHeaderLineOrder"             headerLineOrder             :: Header -> IO LineOrder

-- | @headerSetCompression hdr pixelAspectRatio@
foreign import ccall unsafe "ImfHeaderSetCompression"        headerSetCompression        :: Header -> Compression -> IO ()

-- | @headerCompression hdr@
foreign import ccall unsafe "ImfHeaderCompression"           headerCompression           :: Header -> IO Compression

foreign import ccall unsafe "ImfHeaderSetIntAttribute"       headerSetIntAttribute       :: Header -> CString -> CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderIntAttribute"          headerIntAttribute          :: Header -> CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderSetFloatAttribute"     headerSetFloatAttribute     :: Header -> CString -> CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderFloatAttribute"        headerFloatAttribute        :: Header -> CString -> Ptr CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderSetDoubleAttribute"    headerSetDoubleAttribute    :: Header -> CString -> CDouble -> IO CInt
foreign import ccall unsafe "ImfHeaderDoubleAttribute"       headerDoubleAttribute       :: Header -> CString -> Ptr CDouble -> IO CInt
foreign import ccall unsafe "ImfHeaderSetStringAttribute"    headerSetStringAttribute    :: Header -> CString -> CString -> IO CInt
foreign import ccall unsafe "ImfHeaderStringAttribute"       headerStringAttribute       :: Header -> CString -> Ptr CString -> IO CInt
foreign import ccall unsafe "ImfHeaderSetBox2iAttribute"     headerSetBox2iAttribute     :: Header -> CString -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderBox2iAttribute"        headerBox2iAttribute        :: Header -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderSetBox2fAttribute"     headerSetBox2fAttribute     :: Header -> CString -> CFloat -> CFloat -> CFloat -> CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderBox2fAttribute"        headerBox2fAttribute        :: Header -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderSetV2iAttribute"       headerSetV2iAttribute       :: Header -> CString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderV2iAttribute"          headerV2iAttribute          :: Header -> CString -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderSetV2fAttribute"       headerSetV2fAttribute       :: Header -> CString -> CFloat -> CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderV2fAttribute"          headerV2fAttribute          :: Header -> CString -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderSetV3iAttribute"       headerSetV3iAttribute       :: Header -> CString -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderV3iAttribute"          headerV3iAttribute          :: Header -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ImfHeaderSetV3fAttribute"       headerSetV3fAttribute       :: Header -> CString -> CFloat -> CFloat -> CFloat -> IO CInt
foreign import ccall unsafe "ImfHeaderV3fAttribute"          headerV3fAttribute          :: Header -> CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CInt

-- | @float m[3][3]@
type M33 = Ptr CFloat

foreign import ccall unsafe "ImfHeaderSetM33fAttribute"      headerSetM33fAttribute      :: Header -> CString -> M33 -> IO CInt 
foreign import ccall unsafe "ImfHeaderM33fAttribute"         headerM33fAttribute         :: Header -> CString -> M33 -> IO CInt 

-- | @float m[4][4]@
type M44 = Ptr CFloat

foreign import ccall unsafe "ImfHeaderSetM44fAttribute"      headerSetM44fAttribute      :: Header -> CString -> M44 -> IO CInt 
foreign import ccall unsafe "ImfHeaderM44fAttribute"         headerM44fAttribute         :: Header -> CString -> M44 -> IO CInt 

-- * Output Files

data OpaqueOutputFile

type OutputFile = Ptr OpaqueOutputFile

-- | @openOutputFile name header channels@
foreign import ccall unsafe "ImfOpenOutputFile" openOutputFile :: CString -> Header -> CInt -> IO OutputFile
foreign import ccall unsafe "ImfCloseOutputFile" closeOutputFile :: OutputFile -> IO CInt

-- | @outputSetFrameBuffer out base xStride yStride@
foreign import ccall unsafe "ImfOutputSetFrameBuffer" outputSetFrameBuffer :: OutputFile -> Ptr RGBA -> Ptr CSize -> Ptr CSize -> IO CInt
foreign import ccall unsafe "ImfOutputWritePixels" outputWritePixels :: OutputFile -> CInt -> IO CInt

foreign import ccall unsafe "ImfOutputCurrentScanLine" outputCurrentScanLine :: OutputFile -> IO CInt

foreign import ccall unsafe "ImfOutputHeader" outputHeader :: OutputFile -> IO Header

foreign import ccall unsafe "ImfOutputChannels" outputChannels :: OutputFile -> IO CInt

-- TODO

-- * Input Files

data OpaqueInputFile

type InputFile = Ptr OpaqueInputFile

foreign import ccall unsafe "ImfOpenInputFile" openInputFile :: CString -> IO InputFile
foreign import ccall unsafe "ImfCloseInputFile" closeInputFile :: InputFile -> IO CInt
-- | @inputSetFrameBuffer inputFile base xStride yStride@
foreign import ccall unsafe "ImfInputSetFrameBuffer" inputSetFrameBuffer :: InputFile -> Ptr RGBA -> CSize -> CSize -> IO CInt
-- | @inputReadPixels input scanLine1 scanLine2@
foreign import ccall unsafe "ImfInputReadPixels" inputReadPixels :: InputFile -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ImfInputHeader" inputHeader :: InputFile -> IO Header
foreign import ccall unsafe "ImfInputChannels" inputChannels :: InputFile -> IO CInt
foreign import ccall unsafe "ImfInputFileName" inputFileName :: InputFile -> IO CString

-- * TODO: Tiled Output Files
data OpaqueTiledOutputFile
type TiledOutputFile = Ptr OpaqueTiledOutputFile

-- * TODO: Tiled Input Files
data OpaqueTiledInputFile
type TiledInputFile = Ptr OpaqueTiledInputFile

-- * Lookup Tables

data OpaqueLut
type Lut = Ptr OpaqueLut

foreign import ccall unsafe "ImfNewRound12logLut" newRound12logLut :: CInt -> IO Lut
foreign import ccall unsafe "ImfNewRoundNBitLut" newRoundNBitLut :: CUInt -> CInt -> IO Lut
foreign import ccall unsafe "ImfDeleteLut" deleteLut :: Lut -> IO ()
foreign import ccall unsafe "ImfApplyLut" applyLut :: Lut -> Ptr RGBA -> CInt -> CInt -> IO ()

foreign import ccall unsafe "ImfErrorMessage" errorMessage :: IO CString
