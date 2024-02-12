module Time (
    Time(..),
    timeParser
) where

import RIO
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary

data Time = Time {
    daysT :: Word16,
    milli :: Word32,
    micro :: Word16
} deriving (Show)

timeParser :: Parser Time
timeParser = Time <$> anyWord16be <*> anyWord32be <*> anyWord16be