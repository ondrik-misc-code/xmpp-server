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
  case getValueOfAttribute attrs "to" of
    Nothing -> do
      rem_elements <- processMessageWithMsg prefix chan elements initMessage
      return rem_elements
    (Just target) -> do
      rem_elements <- processMessageWithMsg prefix chan elements $
        messageSetTarget initMessage target
      return rem_elements
  where
    processMessageWithMsg :: String
                          -> TChan Command
                          -> [Maybe SaxElement]
                          -> Message
                          -> IO (Maybe [Maybe SaxElement])
    processMessageWithMsg pref ch elems msg =
      safely ch elems $
        (\x xs -> case x of
          (SaxElementOpen name n_attrs) ->                      -- opening tag
            if (matchesStringForPrefix name "subject" prefix) then do
                debugInfo $ "Subject"
                strList <- stringPlusList ch xs
                case (strList) of
                  Nothing -> do                     -- no CDATA contents
                    sendCommand ch $
                      Error "The subject tag has no CDATA contents!"
                    return Nothing
                  (Just (str, xss)) -> do           -- with CDATA contents
                    remaining <- consumeClosingTag pref ch xss "subject"
                    case remaining of
                      Nothing -> return Nothing
                      (Just xsss) -> do
                        rem_elements <- processMessageWithMsg pref ch xsss
                          (msg `messageSetSubject` str)
                        return rem_elements
              else if (matchesStringForPrefix name "body" prefix) then do
                debugInfo $ "Body"
                strList <- stringPlusList ch xs
                case (strList) of
                  Nothing -> do                     -- no CDATA contents
                    sendCommand ch $
                      Error "The body tag has no CDATA contents!"
                    return Nothing
                  (Just (str, xss)) -> do           -- with CDATA contents
                    remaining <- consumeClosingTag pref ch xss "body"
                    case remaining of
                      Nothing -> return Nothing
                      (Just xsss) -> do
                        rem_elements <- processMessageWithMsg pref ch xsss
                          (msg `messageSetBody` str)
                        return rem_elements
              else if (matchesStringForPrefix name "thread" prefix) then do
                debugInfo $ "Thread"
                strList <- stringPlusList ch xs
                case (strList) of
                  Nothing -> do                     -- no CDATA contents
                    sendCommand ch $
                      Error "The subject tag has no CDATA contents!"
                    return Nothing
                  (Just (str, xss)) -> do           -- with CDATA contents
                    remaining <- consumeClosingTag pref ch xss "thread"
                    case remaining of
                      Nothing -> return Nothing
                      (Just xsss) -> do
                        rem_elements <- processMessageWithMsg pref ch xsss
                          (msg `messageSetThread` str)
                        return rem_elements
              else do
                debugInfo $ "Unexpected tag: " ++ name
                remains <- consumeTagsUpTo prefix chan xs name
                case remains of
                  Nothing -> return Nothing
                  (Just xss) -> do
                    rem_elems <- processMessageWithMsg prefix chan xss msg
                    return rem_elems
          (SaxElementTag name n_attrs) ->                       -- empty tag
            processMessageWithMsg prefix chan xs msg
          (SaxElementClose name) ->                             -- closing tag
            if (matchesStringForPrefix name "message" prefix) then do
                debugInfo $ "Closing " ++ name ++ " tag!"
                sendCommand chan $ SendMessage msg
                return $ Just xs
              else do
                sendCommand chan $ Error $ "Invalid closing tag: " ++ name
                return Nothing
          (SaxCharData _) -> do                                 -- ignore CDATA
            debugInfo $ "ignoring CDATA"
            rem_elements <- processMessageWithMsg prefix chan xs msg
            return rem_elements
          _ -> do                                               -- other
            sendCommand chan $ Error "Invalid tag at message processing!"
            return Nothing
        )

