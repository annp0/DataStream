module Parser (
    Pack(..),
    Quality(..),
    StreamType(..),
    packParserC
) where

import RIO
import RIO.Text as T
import Data.Text.IO as T
import Time

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString as A
import Data.Conduit.Attoparsec
import Conduit

data Quality = Good | Bad 
    deriving (Show, Generic)

data StreamType = ONLT | ONLC | OFFL
    deriving (Show, Generic)

data DataStreamType =
    SLC StreamType
    | MC StreamType
    | VC StreamType
    | BadFrame StreamType
    deriving (Show, Generic)

data Pack = Pack {
    sizeP :: Word32,
    scIdP :: Word16,
    dsTypeP :: DataStreamType,
    vcIdP :: Word8,
    routeIdP :: Word16,
    ertP :: Time,
    seqP :: Word8,
    qualityP :: Quality,
    contentP :: ByteString
} deriving (Show, Generic)

streamTypeParser :: Parser DataStreamType
streamTypeParser = do
    b <- anyWord8
    case b of
        0 -> return $ SLC ONLT
        1 -> return $ MC ONLT
        2 -> return $ VC ONLT
        3 -> return $ BadFrame ONLT
        x -> fail $ show x

qualityParser :: Parser Quality
qualityParser = do
    b <- anyWord8
    case b of
        0 -> return Good
        1 -> return Bad
        x -> fail $ show x

packParser :: Parser Pack
packParser = do
    size <- anyWord32be
    scid <- anyWord16be
    streamType <- streamTypeParser
    vcid <- anyWord8
    route <- anyWord16be
    ert <- timeParser
    sequence <- anyWord8
    quality <- qualityParser
    dat <- A.take ((fromIntegral size) - 20)
    return Pack {
        sizeP = size,
        scIdP = scid,
        dsTypeP = streamType,
        vcIdP = vcid,
        routeIdP = route,
        ertP = ert,
        seqP = sequence,
        qualityP = quality,
        contentP = dat
}

packParserC :: (MonadIO m) => ConduitT ByteString Pack m ()
packParserC = conduitParserEither packParser .| sink
    where 
        sink = do
            x <- await
            case x of
                Just (Left err) ->
                    liftIO $ T.putStrLn $ (T.pack (show err))
                Just (Right (_, pack)) -> do
                    yield pack
                    sink
                Nothing -> return ()