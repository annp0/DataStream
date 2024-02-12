module Main where

import RIO
import RIO.ByteString as BS

import Config
import Data.Conduit.Network
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Conduit
import Parser
import TMFrame
import Text.Show.Pretty

prettyShowC :: (MonadIO m, Show a) => ConduitT a Void m ()
prettyShowC = awaitForever $ \x -> liftIO $ pPrint x

connectClient :: Config -> IO()
connectClient cfg = do
    let settings = clientSettings (fromIntegral $ port cfg) (encodeUtf8 $ hostName cfg) 
    res <- try $ runGeneralTCPClient settings $ \appData -> do
            runConduitRes $ appSource appData .| packParserC .| tmFrameC .| prettyShowC
    case res of
        Left (e :: SomeException) -> do
            T.putStrLn "Reconnecting..."
            threadDelay 2_000_000
            connectClient cfg
        Right _ -> connectClient cfg

main :: IO ()
main = do
    connectClient defaultConfig

