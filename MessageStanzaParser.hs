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
 - This module contains the parser of message stanza XMPP messages.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains the parser of message stanza XMPP messages. See RFC
  3921 for details.
 -}
module MessageStanzaParser (processMessage) where

import Control.Concurrent.STM
  (TChan)

import Text.XML.HaXml.SAX
  (SaxElement(..))
import Text.XML.HaXml
  (Attribute)

import Global
import ParserGlobal



{-|
  This function processes XMPP message stanza. For details, see RFC 3921.
 -}
processMessage :: String              -- ^ The prefix of the stream namespace
               -> [Attribute]         -- ^ List of attributes of the node
               -> TChan Command       -- ^ Channel for sending commands
               -> [Maybe SaxElement]  -- ^ List of SAX events
               -> IO (Maybe [Maybe SaxElement])
               -- ^ The return value
processMessage prefix attrs chan elements =
  processMessageWithMsg prefix chan elements initMessage
  where
    processMessageWithMsg :: String
                          -> TChan Command
                          -> [Maybe SaxElement]
                          -> Message
                          -> IO (Maybe [Maybe SaxElement])
    processMessageWithMsg pref ch elems msg =
      safely chan elements $
        (\x xs -> case x of
          (SaxElementOpen name n_attrs) ->                      -- opening tag
            if (matchesStringForPrefix name "subject" prefix) then do
                --neco
                return $ Just xs
              else if (matchesStringForPrefix name "body" prefix) then do
                --neco
                return $ Just xs
              else if (matchesStringForPrefix name "thread" prefix) then do
                --neco
                return $ Just xs
              else do
                remains <- consumeTagsUpTo prefix chan xs name
                return remains
          (SaxElementTag name n_attrs) ->                       -- empty tag
            processMessage prefix attrs chan xs
          (SaxElementClose name) ->                             -- closing tag
            if (matchesStringForPrefix name "message" prefix) then do
                sendCommand chan $ SendMessage msg
                return $ Just xs
              else do
                sendCommand chan $ Error $ "Invalid closing tag: " ++ name
                return Nothing
          (SaxCharData _) ->                                    -- ignore CDATA
            processMessage prefix attrs chan xs
          _ -> do                                               -- other
            sendCommand chan $ Error "Invalid tag at message processing!"
            return Nothing
        )

