-- | CS240h Lab 2 Chat Server
module Chat (chat) where
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

-- | Chat server entry point.
chat :: IO ()
chat = do
  portNumber <- getPort
  bracket (listenOn portNumber) sClose $ \s -> do
    bracket (accept s) (\(h, _, _) -> hClose h) $
      \(h, host, port) -> do
        putStrLn $ "Connection from host " ++ host
                  ++ " port " ++ show port
        hPutStrLn h "Hi there."

