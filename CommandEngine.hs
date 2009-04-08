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


streamFeatures :: XmlNode
streamFeatures = ("stream:features", [], [{-mechanisms,-} auth])
  where mechanisms :: XmlContent
        mechanisms = XmlContentNode
          (
            "mechanisms",
            [("xmlns", saslNamespace)],
            [XmlContentNode ("mechanism", [], [XmlContentString "PLAIN"])]
          )
        auth :: XmlContent
        auth = XmlContentNode
          (
            "auth",
            [("xmlns", iqAuthNamespace)],
            []
          )


authenticationFields :: XmlNode
authenticationFields = (
    "query",
    [
      ("xmlns", authNamespace)
    ],
    [
      XmlContentNode ("username", [], []),
      XmlContentNode ("password", [], []),
      XmlContentNode ("resource", [], [])
    ]
  )

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
  hostname <- getHostName
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
        (OpenStream lang ver) -> do         -- opening stream
          openStream clients sender lang ver
        (Authenticate auths ident) -> do    -- trying to authenticate
          case auths of
            (Nothing, _, _) -> do
              invalidAuthQuery sender ident
              return clients
            (_, Nothing, _) -> do
              invalidAuthQuery sender ident
              return clients
            (_, _, Nothing) -> do
              invalidAuthQuery sender ident
              return clients
            validAuth@(Just username, Just password, Just resource) -> do
              authQuery username password resource sender ident
              return $ (clientAuthenticate sender validAuth hostname):
                (filter ((/= clientGetHandle sender) . clientGetHandle) clients)
        (UnknownIqNamespace namespace ident) -> do
          unknownIqNs sender namespace ident
          return clients
        (SendRoster ident) -> do
          sendClientRoster sender clients ident
          return clients
        --(Error str) -> sendToClient sender 
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
  -- send DTD
  sendToClient sender $ xmlDtd
  -- send stream opening tag
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
  -- send stream features
  sendToClient sender $ serializeXmlNode streamFeatures
  return clients


{-|
  This is the handler for invalid client authentication query.
 -}
invalidAuthQuery :: Client    -- ^ The sender of the authentication query
                 -> String    -- ^ The authentication query ID
                 -> IO ()     -- ^ The return value
invalidAuthQuery sender ident = sendToClient sender $
  serializeXmlNode $
    (
      "iq",
      [
        ("type", "result"),
        ("id", ident)
      ],
      [
        XmlContentNode authenticationFields
      ]
    )


authQuery :: String
          -> String
          -> String
          -> Client
          -> String
          -> IO ()
authQuery username password resource sender ident =
  sendToClient sender $ serializeXmlNode $
  (
    "iq",
    [
      ("type", "result"),
      ("id", ident)
    ],
    []
  )


unknownIqNs :: Client
            -> String
            -> String
            -> IO ()
unknownIqNs sender namespace ident =
  sendToClient sender $ serializeXmlNode
    (
      "iq",
      [
        ("type", "error"),
        ("id", ident)
      ],
      {-[
        XmlContentNode
          (
            "query",
            [
              ("xmlns", namespace),
              ("node", 
      -}
      []
    )


sendClientRoster :: Client
                 -> [Client]
                 -> String
                 -> IO ()
sendClientRoster sender clients ident = sendToClient sender $ serializeXmlNode
  (
    "iq",
    [
      ("type", "result"),
      ("id", ident)
    ],
    allClients
  )
  where allClients = map XmlContentNode $ foldr (\x z -> (clientToRosterItem x):z) [] clients


clientToRosterItem :: Client
                   -> XmlNode
clientToRosterItem client =
  (
    "item",
    [
      ("jid", showJIDNoResource (clientGetJID client)),
      ("subscription", "both")
    ],
    []
  )


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
