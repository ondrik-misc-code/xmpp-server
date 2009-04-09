{-----------------------------------------------------------------------------
 -
 -                FPR - Functional and Logic Programming
 -             Project 1: Lightweight XMPP server in Haskell
 -
 -                            Ondrej Lengal
 -                      xlenga00@stud.fit.vutbr.cz
 -
 -                   Faculty of Information Technology
 -                     Brno University of Technology
 -
 - This module contains the entry point of the program and all topmost
 - functions (including creation of thread etc.).
 -
 -----------------------------------------------------------------------------}

{-|
  This is the highest level module of the application. It contains the entry
  point and the functions that create and manage threads, as well as accept
  client connections.
 -}
module Main where

import Prelude
  hiding (catch)
import Network
  (listenOn, accept, sClose, Socket, withSocketsDo, PortID(..))
import System.IO
  (Handle, hGetContents, hClose, hSetBuffering, stderr, stdout, BufferMode(..))
import System.Environment
  (getArgs, getProgName)
import Control.Exception
  (finally, {--}catch{--})
import Control.Concurrent
  (forkIO)
import Control.Concurrent.STM
  (STM, TChan, newTChan, readTChan, writeTChan, atomically, orElse, retry)

import XMPPParser
  (parseXMPP)
import CommandEngine
  (processCommand)

import Global

{-
 - Part of the following code has been shamelessly exploited from
 - http://sequence.complete.org/node/258.
 -}

{-|
  The 'main' function is the entry point of the program.
 -}
main :: IO ()         -- ^ The return value
main = do
  settings
  withSocketsDo $ do
    args <- getArgs
    if (length args /= 1)
      then printHelp
      else do
        -- get the port number
        let port = fromIntegral (read (args!!0) :: Int)
        -- create listening socket on port
        servSock <- listenOn $ PortNumber port
        debugInfo $ "listening on: " ++ show port
        -- start the server
        startServer servSock `finally` sClose servSock


settings :: IO ()
settings = do
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout NoBuffering


{-|
  The 'printHelp' function prints help for the program.
 -}
printHelp :: IO ()    -- ^ The return value
printHelp = do
  -- get the program name
  programName <- getProgName
  -- print help
  putStrLn $ ""
  putStrLn $ "  XMPP-server in Haskell"
  putStrLn $ ""
  putStrLn $ "SYNOPSIS:"
  putStrLn $ "  " ++ programName ++ "  FILL IN!!"
  putStrLn $ ""
  putStrLn $ "AUTHORS:"
  putStrLn $ "  Ondrej Lengal (xlenga00@stud.fit.vutbr.cz)"
  putStrLn $ "  Faculty of Information Technology"
  putStrLn $ "  Brno University of Technology"
  putStrLn $ "  Czech Republic"
  putStrLn $ ""
  putStrLn $ "NOTES:"
  putStrLn $ "  Created as a project for Functional and Logic Programming, 2009."
  putStrLn $ "  Haskell rulez!"


{-|
  The 'startServer' function starts the server, i.e. it creates two threads:
  (i) the thread that listens for new incoming connections, and
  (ii) the thread that handles the program loop.
 -}
startServer :: Socket        -- ^ The socket the server will be listening on
            -> IO ()         -- ^ The return value
startServer servSock = do
  -- create a communication channel
  acceptChan <- atomically newTChan
  -- create a new thread for accepting connections
  forkIO $ acceptLoop servSock acceptChan
  -- go to the command processing loop
  commandLoop acceptChan []


{-|
  The 'acceptLoop' function runs in a loop and accepts new incoming
  connections.
 -}
acceptLoop :: Socket         -- ^ The socket the loop will be listening on
           -> TChan Client   -- ^ The channel that is used to send new client
                             --   connection parameters to the global handler
           -> IO ()          -- ^ The return value
acceptLoop servSock chan = do
  -- accept an incoming connection
  (cHandle, host, port) <- accept servSock
  -- disable buffering (SAX parser does not like it very much)
  hSetBuffering cHandle NoBuffering
  debugInfo $ show cHandle ++ " " ++ show host ++ " " ++ show port
  -- create a communication channel for a client
  cChan <- atomically newTChan
  -- create a new thread for processing the connected client
  forkIO $ processClient cHandle cChan
  -- send the client communication channel to the main loop
  atomically $ writeTChan chan $ initClient cChan cHandle
  -- start the loop that accepts new clients' connections
  acceptLoop servSock chan


{-|
  The 'processClient' function listens processes the data received from the client.
 -}
processClient :: Handle        -- ^ The socket handle the data will be read from
              -> TChan Command -- ^ The channel the client passes data to
              -> IO ()         -- ^ The return value
processClient handle chan = do
  contents <- hGetContents handle
  parseXMPP contents chan        -- start parsing the client stream
-- This code was commented out because it was responsible for closing the
-- socket before the server could respond. The socket should be closed
-- explicitly by CommandEngine
    `catch` (const $ return ())  -- in case of an exception, return
    `finally` hClose handle      -- when ending, close the socket 
  debugInfo $ "Client processing has finished"


--showBuffering NoBuffering = "NoBuffering"
--showBuffering LineBuffering = "LineBuffering"
--showBuffering (BlockBuffering a) =
--  case a of
--    Nothing -> label
--    (Just x) -> label ++ ": " ++ show x
--  where label = "Block buffering"


{-|
  The 'commandLoop' function receives commands from the client handling treads
  and executes proper actions depending on the commands.
 -}
commandLoop :: TChan Client  -- ^ The channel that the newly connected clients
                             --   information comes from
            -> [Client]      -- ^ The list of information about clients
                             --   ('Client' structures) connected to the
                             --   server, including a channel for every
                             --   client's connection
            -> IO ()         -- ^ The return value
commandLoop acceptChan clients = do
  -- try to read from the channel for newly connected clients' information,
  -- in case it cannot read, try reading from all connected clients' channels
  recv_data <- atomically $ (Left `fmap` readTChan acceptChan)
    `orElse`
    (Right `fmap` multipleSelect clients)
  -- choose according to what was read in the previous step
  case recv_data of
    -- if a new client connected
    Left new_client -> do
      debugInfo $ "new client at " ++ show (clientGetHandle new_client)
      commandLoop acceptChan $ new_client:clients
    -- if a command arrived from a client processing thread
    Right (handle, command) -> do
      new_clients <- processCommand clients handle command
      commandLoop acceptChan $ new_clients


{-|
  The 'multipleSelect' function tries to read data from a list of channels.
 -}
multipleSelect :: [Client]                 -- ^ A list of clients
               -> STM (Handle, Command)    -- ^ A software transactional
                                           --   memory value
multipleSelect =
  foldl orElse retry .
    map (\(channel, handle, _, _) -> ((,) handle) `fmap` readTChan channel)
--  foldl orElse retry . map (\(ch, ty) -> (flip (,) ty) `fmap` readTChan ch)
--  foldl orElse retry . map (\(channel, handle, _) -> readTChan channel >>= return . (\x -> (handle, x)))
