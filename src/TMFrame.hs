module TMFrame (
    TMFrame (..),
    TMFrameHeader (..),
    TMFrameMeta(..),
    tmFrameParser,
    tmFrameC
) where

import RIO
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.Binary
import Data.Bits
import Conduit
import Data.Conduit.Attoparsec
import Parser
import Data.Text.IO as T
import Verify

data TMFrameHeader = TMFrameHeader {
    frHdrVer :: Word8,
    frHdrScId :: Word16,
    frHdrVcId :: Word16,
    frHdrMCFC :: Word8,
    frHdrVCFC :: Word8,
    frHdrFHP :: Word16
} deriving (Show)

data TMFrame = TMFrame {
    frameHdr :: TMFrameHeader,
    frameData :: ByteString,
    frameOcf :: Maybe Word32,
    frameCRC :: Word16
} deriving (Show)

tmFrameHeaderParser :: Parser (Bool, TMFrameHeader)
tmFrameHeaderParser = do
    w1 <- anyWord16be
    mcfc <- anyWord8
    vcfc <- anyWord8
    w2 <- anyWord16be
    let version = fromIntegral $ (w1 .&. 0b1100_0000_0000_0000) `shiftR` 14
        scid = (w1 .&. 0b0011_1111_1111_0000) `shiftR` 4
        vcid = fromIntegral $ (w1 .&. 0b0000_0000_0000_1110) `shiftR` 2
        ocff = (w1 .&. 0b0000_0000_0000_0001) /= 0
        fhp = w2 .&. 0b0000_0111_1111_1111


    return (ocff, TMFrameHeader {
        frHdrVer = version,
        frHdrScId = scid,
        frHdrVcId = vcid,
        frHdrMCFC = mcfc,
        frHdrVCFC = vcfc,
        frHdrFHP = fhp
    })

tmFrameParser :: Parser TMFrame
tmFrameParser = do
    (ocff, hdr) <- tmFrameHeaderParser
    dat <- A.take (if ocff then 1115 - 6 - 2 - 4 else 1115 - 6 - 2)
    ocf <- if ocff then Just <$> anyWord32be else return Nothing
    crc <- anyWord16be
    return TMFrame{
        frameHdr = hdr,
        frameData = dat,
        frameOcf = ocf,
        frameCRC = crc
    }

data TMFrameMeta = TMFrameMeta {
    metaQuality :: Quality,
    metaFrame :: TMFrame
} deriving (Show)

tmFrameC :: (MonadIO m) => ConduitT Pack TMFrameMeta m ()
tmFrameC = awaitForever $ \pack -> do
    let frame = contentP pack
    if checkCRC frame
        then do
            case parseOnly tmFrameParser (contentP pack) of
                Left err -> liftIO $ T.putStrLn $ "Error!"
                Right frame -> do
                    yield TMFrameMeta {
                        metaQuality = qualityP pack,
                        metaFrame = frame
                    }
    else
        liftIO $ T.putStrLn "Error"