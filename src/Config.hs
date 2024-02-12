module Config (
    Config(..), 
    defaultConfig,
    writeConfig,
    readConfig
    ) where

import RIO
import qualified Data.Text.IO as T
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as BL
import Data.Aeson

data Config = Config {
    hostName :: Text,
    port :: Word16
} deriving (Show, Generic, ToJSON, FromJSON)

defaultConfig :: Config
defaultConfig = Config {
    hostName = "localhost",
    port = 2502
}

writeConfig :: Config -> FilePath -> IO()
writeConfig cfg path = BL.writeFile path (encode cfg)

readConfig :: FilePath -> IO (Either Text Config)
readConfig path = do
    response <- try $ BL.readFile path
    case response of
        Left (e :: IOException) -> return (Left $ T.pack (show e))
        Right content -> 
            case eitherDecode content of
                Left err -> return $ Left (T.pack err)
                Right cfg -> return $ Right cfg

