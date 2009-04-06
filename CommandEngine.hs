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
 - This module contains the functions for handling commands that are processed
 - in the command loop.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains functions for handling commands that are
  processed in the command loop.
 -}
module CommandEngine (processCommand) where

import Random
  (getStdRandom, random)
import System.Posix.Time
  (epochTime)
import Network.BSD
  (getHostName)
import System.IO
  (Handle, hPutStrLn, hFlush, hClose)
import Control.Monad
  (forM, when)

import Global


{-|
  The 'processCommand' function receives a list of clients, a command and
  a handle (to identify the thread that sent the command) and processes the
  command (sends messages, etc.) and returns an updated list of clients.
 -}
processCommand :: [Client]      -- ^ The list of clients
               -> Handle        -- ^ The handle of the command source
               -> Command       -- ^ The command that is being processed
               -> IO [Client]   -- ^ The updated list of clients
processCommand clients handle command = do
  let cl = clients `findByHandle` handle
  case cl of
    Nothing -> do
      debugInfo $ "Invalid client handle received!"
      --
      -- TODO: graceful degradation (maybe along the lines of assert(false)
      --
      return clients
    (Just sender) -> do
      debugInfo $ "data: " ++ (show command) ++ " handle: " ++ show handle
      case command of
        (OpenStream lang ver) -> openStream clients sender lang ver >>= return
--      hPutStr clientHandle "<stream:stream xmlns:stream=" ++ streamNamespace
--      clients' <- forM clients $
--        \(ch, h, state, jid) -> do
--          hPutStrLn h (show command)
--          hFlush h
--          return [(ch, h, state, jid)]
--          `catch` const (hClose h >> return [])
--      let dropped = length $ filter null clients'
--      when (dropped > 0) $
--        debugInfo $ "clients lost: " ++ show dropped
--      return $ concat clients'


openStream :: [Client]
           -> Client
           -> String
           -> String
           -> IO [Client]
openStream clients sender lang version = do
  hostname <- getHostName
  time <- epochTime
  randomPart <- (getStdRandom random) :: IO Int
  --
  -- TODO: digest the ID (SHA, MD5, CRC, ...)
  -- note: this needs a library "http://www.haskell.org/crypto/" to be
  -- installed, is it necessary?
  --
  let id = show time ++ (show . abs) randomPart
  sendToClient sender $ xmlDtd
  sendToClient sender $ serializeXmlNodeOpeningTag
    ("stream:stream",
    [
      ("xmlns", clientNamespace),
      ("xmlns:stream", streamNamespace),
      ("from", hostname),
      ("id", id),
      ("xml:lang", lang),
      ("version", version)
    ],
    [
      -- the content of the stream element will be added manually
    ])
  return clients


{-|
  The 'sendToClient' function is a wrapper function for sending data to
  a client through a socket.
 -}
sendToClient :: Client     -- ^ The target of the data
             -> String     -- ^ The data
             -> IO ()      -- ^ The return value
sendToClient client str = do
  debugInfo $ "Sending to client at (" ++ show (clientGetHandle client) ++  "): " ++ str
  hPutStrLn (clientGetHandle client) str
  hFlush (clientGetHandle client)


{-|
  This function searches a list of clients and returns a client with the given
  handle.
 -}
findByHandle :: [Client]       -- ^ The list of clients
             -> Handle         -- ^ The handle that we search
             -> Maybe Client   -- ^ The client with the handle (or Nothing)
findByHandle [] _ = Nothing
findByHandle (x:xs) handle =
  if (clientGetHandle x == handle)
    then Just x
    else findByHandle xs handle
