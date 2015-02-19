-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Control.Concurrent
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

talk :: Handle -> IO ()
talk h = do
  hPutStrLn h "Hi there, what's your name?"
  talk h
  -- name <- hGetLine h
  -- hPutStrLn h $ "What's up, " ++ name
  -- hClose h

loop :: Socket -> IO ()
loop s = do
  (h, _, _) <- accept s
  forkIO $ finally (talk h) (hClose h)
  loop s

-- | Chat server entry point.
chat :: IO ()
chat = do
  portNumber <- getPort
  bracket (listenOn portNumber) sClose $ \s -> do
    loop s
