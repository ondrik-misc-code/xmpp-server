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
 - This module contains the parser of presence stanza XMPP messages.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains the parser of presence stanza XMPP messages. See RFC
  3921 for details.
 -}
module PresenceStanzaParser (processPresence, processPresenceEmpty) where

import Control.Concurrent.STM
  (TChan)

import Text.XML.HaXml.SAX
  (SaxElement(..))
import Text.XML.HaXml
  (Attribute)

import Global
import ParserGlobal


{-|
  This function processes XMPP presence stanza. For details, see RFC 3921.
 -}
processPresence :: String             -- ^ The stream namespace prefix
                -> [Attribute]        -- ^ List of XML node attributes
                -> TChan Command      -- ^ Channel for sending commands
                -> [Maybe SaxElement] -- ^ List of SAX events
                -> IO (Maybe [Maybe SaxElement])
                -- ^ The return value
processPresence prefix attrs chan elements = do
  case getValueOfAttribute attrs "type" of
    Nothing -> do
      rem_elements <- processPresenceWithType prefix chan elements Nothing
      return rem_elements
    (Just typ) ->
      if (typ == "subscribe") then do
          -- consume everything else
          rem_elements <- consumeTagsUpTo prefix chan elements "presence"
          return rem_elements
        else do
          -- process as usual
          rem_elements <- processPresenceWithType prefix chan elements $
            Just typ
          return rem_elements
  where
    processPresenceWithType :: String
                            -> TChan Command
                            -> [Maybe SaxElement]
                            -> Maybe String
                            -> IO (Maybe [Maybe SaxElement])
    processPresenceWithType pref ch elems typ =
      safely ch elems $
        (\x xs -> case x of
          (SaxElementOpen name _) -> do           -- opening tag
             remains <- consumeTagsUpTo pref ch xs name
             case remains of
               Nothing -> return Nothing
               (Just xss) -> processPresenceWithType pref ch xss typ
          (SaxElementTag _ _) -> do               -- empty tag
            processPresenceWithType pref ch xs typ
          (SaxElementClose name) ->               -- closing tag
            if (matchesStringForPrefix name "presence" pref) then do
                sendCommand chan $ SendPresence typ
                return $ Just xs
              else do
                sendCommand chan $
                  Error "Invalid closing tag, </presence> expected!"
                return Nothing
          (SaxCharData _) -> do                   -- ignore CDATA
            processPresenceWithType pref ch xs typ
          _ -> do                                 -- other
            sendCommand chan $
              Error "Invalid tag in presence processing!"
            return Nothing
        )


{-|
  This function processes empty XMPP presence stanza. For details, see RFC 3921.
 -}
processPresenceEmpty :: [Attribute]         -- ^ List of node attributes
                     -> TChan Command       -- ^ Channel for sending commands
                     -> IO ()               -- ^ The return value
processPresenceEmpty attrs chan = do
  case attrs `getValueOfAttribute` "type" of
    Nothing -> do
      sendPresenceEmpty Nothing
    (Just typ) ->
      if (typ == "subscribe") then
          case attrs `getValueOfAttribute` "to" of
            Nothing -> sendPresenceEmpty Nothing
            (Just target) -> sendCommand chan $ AuthorizeSubscription target
        else do
          -- process as usual
          sendPresenceEmpty $ Just typ
  where sendPresenceEmpty t = sendCommand chan $ SendPresence t

