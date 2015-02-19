-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Network
import System.Environment
import System.IO

getPort :: IO PortID
getPort = do
  portStr <- lookupEnv "CHAT_SERVER_PORT"
  let port = case portStr of
        Just str -> fromIntegral (read str :: Int)
        Nothing  -> 1617
  return (PortNumber port)

broadcast :: String -> IO ()
broadcast message = return ()

commandline :: Handle -> IO ()
commandline h = do
  message <- hGetLine h
  broadcast message
  commandline h

chatter :: Handle -> Chan -> IO ()
chatter h chan = do
  forkIO $ commandline h
  chatter h

-- | Chat server entry point.
chat :: IO ()
chat = do
  portNumber <- getPort
  chan <- newChan
  bracket (listenOn portNumber) sClose $ \s -> do $ loop s
where loop s chan = do
  (h, _, _) <- accept s
  forkIO $ finally (chatter h chan) (hClose h)
  loop s chan

