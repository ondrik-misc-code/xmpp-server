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

import Prelude
  hiding (catch)
import Random
  (getStdRandom, random)
import System.Posix.Time
  (epochTime)
import Network.BSD
  (getHostName)
import System.IO
  (Handle, hPutStrLn, hFlush, hClose)
import Control.Exception
  (catch)

import Global


streamFeatures :: XmlNode
streamFeatures = ("stream:features", [], [{-mechanisms,-} auth])
  where auth :: XmlContent
        auth = XmlContentNode
          (
            "auth",
            [("xmlns", iqAuthNamespace)],
            []
          )
--        mechanisms :: XmlContent
--        mechanisms = XmlContentNode
--          (
--            "mechanisms",
--            [("xmlns", saslNamespace)],
--            [XmlContentNode ("mechanism", [], [XmlContentString "PLAIN"])]
--          )


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
              let new_client = clientAuthenticate sender validAuth hostname
              sendClientPresence new_client clients $ Just "subscribed"
              sendPresenceOfClients clients new_client $ Just "subscribed"
              return $ new_client:
                (filter ((/= clientGetHandle sender) . clientGetHandle) clients)
        (UnknownIqNamespace namespace ident) -> do
          unknownIqNs sender namespace ident
          return clients
        (SendRoster ident) -> do
          sendClientRoster sender clients ident
          return clients
        (SendMessage message) -> do
          sendMessage sender clients message
          return clients
        (SendPresence typ) -> do
          debugInfo $ "Sending presence: " ++ show typ
          sendClientPresence sender clients typ
          return clients
        (AuthorizeSubscription target) -> do
          debugInfo $ "Authorizing subscription of " ++ target
          sender `sendAuthorizationOf` target
          return clients
        EndOfStream -> do
          debugInfo $ "Ending stream at handle " ++ (show $ clientGetHandle sender)
          sendToClient sender $ serializeXmlNodeClosingTag
            (
              "stream:stream",
              [],
              []
            )
          hClose $ clientGetHandle sender
          return (filter ((/= clientGetHandle sender) . clientGetHandle) clients)
        (Error str) -> do
          debugInfo $ "Detected error, ending stream at handle "
            ++ (show $ clientGetHandle sender) ++ ", message: " ++ str
          sendToClient sender $ serializeXmlNodeClosingTag
            (
              "stream:stream",
              [],
              []
            )
          hClose $ clientGetHandle sender
          return (filter ((/= clientGetHandle sender) . clientGetHandle) clients)


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
  -- TODO: hash the ID (SHA, MD5, CRC, ...)
  -- note: this needs a library "http://www.haskell.org/crypto/" to be
  -- installed, is it necessary?
  --
  let ident = show time ++ (show . abs) randomPart
  -- send DTD
  sendToClient sender $ xmlDtd
  -- send stream opening tag
  sendToClient sender $ serializeXmlNodeOpeningTag
    ("stream:stream",
    [
      ("xmlns", clientNamespace),
      ("xmlns:stream", streamNamespace),
      ("from", hostname),
      ("id", ident),
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


{-|
  The function for processing of authentication query.
 -}
authQuery :: String   -- ^ The client username
          -> String   -- ^ The client password
          -> String   -- ^ The client resource
          -> Client   -- ^ The sender
          -> String   -- ^ The identifier of the IQ stanza
          -> IO ()    -- ^ The return value
authQuery _ _ _ sender ident =
  sendToClient sender $ serializeXmlNode $
  (
    "iq",
    [
      ("type", "result"),
      ("id", ident)
    ],
    []
  )


{-|
  This function handles unknown IQ stanza namespaces.
 -}
unknownIqNs :: Client   -- ^ The sender of the IQ stanza
            -> String   -- ^ The namespace name
            -> String   -- ^ The identifier of the IQ stanza
            -> IO ()    -- ^ The return value
unknownIqNs sender namespace ident =
  sendToClient sender $ serializeXmlNode
    (
      "iq",
      [
        ("type", "error"),
        ("id", ident)
      ],
      [
        XmlContentNode
          (
            "query",
            [
              ("xmlns", namespace),
              ("node", commandNamespace)
            ],
            []
          ),
        XmlContentNode
          (
            "error",
            [
              ("type", "cancel")
            ],
            [
              XmlContentNode
                (
                  "feature-not-implemented",
                  [
                    ("xmlns", errorNamespace)
                  ],
                  []
                )
            ]
          )
      ]
    )


{-|
  This function sends a roster to a client.
 -}
sendClientRoster :: Client     -- ^ The receiver of the roster
                 -> [Client]   -- ^ List of connected clients
                 -> String     -- ^ The identifier of the query IQ stanza
                 -> IO ()      -- ^ The return value
sendClientRoster sender clients ident = sendToClient sender $ serializeXmlNode
  (
    "iq",
    [
      ("type", "result"),
      ("id", ident)
    ],
    allClients
  )
  where allClients = map XmlContentNode $
          foldr (\x z -> (clientToRosterItem x):z) [] clients


{-|
  This function sends the client presence to all requested clients.
 -}
sendClientPresence :: Client        -- ^ The input client
                   -> [Client]      -- ^ The list of target clients of the
                                    --   presence message
                   -> Maybe String  -- ^ The type of presence (or Nothing)
                   -> IO ()         -- ^ The return value
sendClientPresence newClient clients typ =
  foldr (\x z -> sendToClient x (serializeXmlNode $
    createPresenceFor newClient x typ) >>= return z) (return ()) authClients
  where authClients = filter clientIsAuth clients


{-|
  The function to create a presence XML node from a source client to a target
  client.
 -}
createPresenceFor :: Client       -- ^ The source client
                  -> Client       -- ^ The target client
                  -> Maybe String -- ^ The type of presence
                  -> XmlNode      -- ^ The presence XML node
createPresenceFor sender target typ =
  (
    "presence",
    [
      ("from", showJID $ clientGetJID sender),
      ("to", showJIDNoResource $ clientGetJID target)
    ] ++ typAttr,
    []
  )
  where typAttr = case typ of
          Nothing -> []
          (Just typp) -> [("type", typp)]


{-|
  This function sends to a client presence of a list of clients of given type.
 -}
sendPresenceOfClients :: [Client]       -- ^ The list of clients
                      -> Client         -- ^ The target
                      -> Maybe String   -- ^ The type of presence of Nothing
                      -> IO ()          -- ^ The return value
sendPresenceOfClients clients target typ =
  foldr (\x _ -> sendToClient target $ serializeXmlNode x) (return ()) $
    map (\x -> createPresenceFor x target typ) clients


{-|
  This function sends an authorization message of a node to a client.
 -}
sendAuthorizationOf :: Client        -- ^ The authorization requester
                    -> String        -- ^ The node to be authorized
                    -> IO ()         -- ^ The return value
sendAuthorizationOf sender target = sendToClient sender $ serializeXmlNode
  (
    "presence",
    [
      ("from", target),
      ("type", "subscribed")
    ],
    []
  )


{-|
  This function sends given message from a client.
 -}
sendMessage :: Client     -- ^ The sender
            -> [Client]   -- ^ A list of clients
            -> Message    -- ^ The message
            -> IO ()      -- ^ The return value
sendMessage sender clients msg = case messageGetTarget msg of
  Nothing -> return ()
  (Just targetJID) ->
    case clients `findClientByJID` targetJID of
      Nothing -> do
        debugInfo $ "Clients: " ++ (show $ map (showJID . clientGetJID) clients)
        debugInfo $ "Client not found!"
        return ()
      (Just target) -> do
        debugInfo $ "Client found!"
        sendToClient target $ serializeXmlNode (messageToElement msg sender)


{-|
  The 'sendToClient' function is a wrapper function for sending data to
  a client through a socket.
 -}
sendToClient :: Client     -- ^ The target of the data
             -> String     -- ^ The data
             -> IO ()      -- ^ The return value
sendToClient client str =
  (sendToClientNotHandled client str) `catch` (const $ return ())
  where sendToClientNotHandled cl s = do
          debugInfo $ "Sending to client at ("
            ++ show (clientGetHandle cl) ++  "): " ++ s
          hPutStrLn (clientGetHandle cl) s
          hFlush (clientGetHandle cl)


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
