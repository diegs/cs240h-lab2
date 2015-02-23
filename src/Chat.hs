module Chat (chat) where

import Control.Concurrent
import Control.Exception
import Network
import System.Environment
import System.IO

data Msg = Msg Int String

getPort :: IO PortID
getPort = do
  portStr <- lookupEnv "CHAT_SERVER_PORT"
  let port = case portStr of
        Just str -> fromIntegral (read str :: Int)
        Nothing  -> 1617
  return (PortNumber port)

broadcast :: Chan Msg -> Int -> String -> IO ()
broadcast chan name msg = do
  writeChan chan (Msg name msg)

commandline :: Handle -> Chan Msg -> Int -> IO ()
commandline h chan name = do
  message <- hGetLine h
  broadcast chan name $ (show name) ++ ": " ++ message
  commandline h chan name

broadcaster :: Handle -> Chan Msg -> Int -> IO ()
broadcaster h chan myName = do
  (Msg name msg) <- readChan chan
  if name /= myName then hPutStrLn h msg else return ()
  broadcaster h chan myName

chatStart :: Handle -> Chan Msg -> Int -> IO ()
chatStart h chan name = do
  broadcast chan name $ (show name) ++ " has joined."
  tid <- myThreadId
  _ <- ($) forkIO $ finally (broadcaster h chan name) (killThread tid)
  commandline h chan name

chatEnd :: Handle -> Chan Msg -> Int -> IO ()
chatEnd h chan name = do
  broadcast chan name $ (show name) ++ " has left."
  hClose h

loop :: Chan Msg -> MVar Int -> Socket -> IO ()
loop chan names s = do
  (h, _, _) <- accept s
  name <- modifyMVar names (\a -> return (a + 1, a))
  chan' <- dupChan chan
  _ <- ($) forkIO $ finally (chatStart h chan' name) (chatEnd h chan name)
  loop chan names s

chat :: IO ()
chat = do
  portNumber <- getPort
  chan <- newChan
  names <- newMVar 0
  bracket (listenOn portNumber) sClose (loop chan names)

