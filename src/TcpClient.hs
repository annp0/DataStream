module TcpClient where

import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, sendAll)
import Data.Monoid (mempty, mappend)
-- Use Monoid because of Monoid
import Numeric (readHex)
import Data.Maybe

openConnection :: HostName -> ServiceName -> IO Socket
openConnection host port = do
        addrinfos <- getAddrInfo Nothing (Just host) (Just port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        return sock

withConnection :: HostName -> ServiceName -> (Socket -> IO b) -> IO b
withConnection host port consumer = do
    sock <- openConnection host port
    r <- consumer sock
    close sock
    return r

send :: Socket -> String -> IO ()
send sock msg = sendAll sock $ C.pack msg

recieve :: Socket -> IO C.ByteString
recieve sock = recieve' sock mempty
    where recieve' s acc = do
            maybeLine <- readPacketLine s
            case maybeLine of
                Just msg -> recieve' s (mappend acc msg)
                Nothing  -> return acc

readPacketLine :: Socket -> IO (Maybe C.ByteString)
readPacketLine sock = do
        len <- readAll mempty 4
        if C.null len then return Nothing else
            case readHex (C.unpack len) of
                ((l,_):_) | l > 4 -> do
                    line <- readAll mempty (l - 4)
                    return (Just line)
                _                 -> return Nothing
    where readAll acc expected = do
            line <- recv sock expected
            let len  = C.length line
                acc' = mappend acc line
                cont = (len /= expected) && not (C.null line)
            if cont then readAll acc' (expected - len) else return acc'

recieveWithSideBand :: Socket -> (C.ByteString -> IO a) -> IO C.ByteString
recieveWithSideBand sock f = rWSB mempty
    where rWSB acc = do
            maybeLine <- readPacketLine sock
            case maybeLine of
                Just line -> case C.unpack line of
                    "NAK\n" -> rWSB acc
                    _ -> case C.uncons line of
                        Just ('\1', rest) -> rWSB (mappend acc rest)
                        Just ('\2', rest) -> f (C.pack "remote: " `C.append` rest) >> rWSB acc
                        Just (_, rest) -> rWSB acc
                Nothing -> return acc
