module Chat (chat) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.Environment
import System.IO

type Name = Int
data Msg = Msg Name String deriving Show

getPort :: IO PortID
getPort = do
  portStr <- lookupEnv "CHAT_SERVER_PORT"
  let port = case portStr of
        Just str -> fromIntegral (read str :: Int)
        Nothing  -> 1617
  return (PortNumber port)

sendMessage :: Chan Msg -> Name -> String -> IO ()
sendMessage chan name msg = writeChan chan (Msg name msg)

messageSender :: Handle -> Chan Msg -> Name -> IO ()
messageSender h chan name = do
  message <- hGetLine h
  sendMessage chan name $ show name ++ ": " ++ message
  messageSender h chan name

messageListener :: Handle -> Chan Msg -> Name -> IO ()
messageListener h chan myName = do
  (Msg name msg) <- readChan chan
  when (name /= myName) $ hPutStrLn h msg
  messageListener h chan myName

chatStart :: Handle -> Chan Msg -> Name -> IO ()
chatStart h chan name = do
  sendMessage chan name $ show name ++ " has joined."
  tid <- myThreadId
  _ <- ($) forkIO $ finally (messageListener h chan name) (killThread tid)
  messageSender h chan name

chatEnd :: Handle -> Chan Msg -> Name -> IO ()
chatEnd h chan name = do
  sendMessage chan name $ show name ++ " has left."
  hClose h

loop :: Chan Msg -> MVar Name -> Socket -> IO ()
loop chan names s = do
  (h, _, _) <- accept s
  name <- modifyMVar names (\a -> return (a + 1, a))
  chan' <- dupChan chan
  _ <- ($) forkIO $ finally (chatStart h chan' name) (chatEnd h chan' name)
  loop chan names s

chat :: IO ()
chat = do
  portNumber <- getPort
  chan <- newChan
  names <- newMVar 0
  bracket (listenOn portNumber) sClose (loop chan names)

