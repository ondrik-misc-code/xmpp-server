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
processPresence prefix attrs chan elements =
  safely chan elements $
    (\x xs -> case x of
      (SaxElementOpen name _) ->            -- opening tag
        if (matchesStringForPrefix name "show" prefix) then do
            remains <- consumeTagsUpTo prefix chan xs "show"
            case remains of
              Nothing -> return Nothing
              (Just xss) -> processPresence prefix attrs chan xss
--          else if (matchesStringForPrefix name "status" prefix) then do
--            remains <- consumeTagsUpTo prefix chan xs "status"
--            case remains of
--              Nothing -> return Nothing
--              (Just xss) -> processPresence prefix attrs chan xss
--          else if (matchesStringForPrefix name "priority" prefix) then do
--            remains <- consumeTagsUpTo prefix chan xs "priority"
--            case remains of
--              Nothing -> return Nothing
--              (Just xss) -> processPresence prefix attrs chan xss
          else do
            remains <- consumeTagsUpTo prefix chan xs name
            case remains of
              Nothing -> return Nothing
              (Just xss) -> processPresence prefix attrs chan xss
      (SaxElementTag _ _) -> do               -- empty tag
        processPresence prefix attrs chan xs
      (SaxElementClose name) ->               -- closing tag
        if (matchesStringForPrefix name "presence" prefix) then
            return $ Just xs
          else do
            sendCommand chan $
              Error "Invalid closing tag, </presence> expected!"
            return Nothing
      (SaxCharData _) -> do                   -- ignore CDATA
        processPresence prefix attrs chan xs
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
                     -> IO (Maybe ())       -- ^ The return value
processPresenceEmpty attrs chan = return $ Just ()
