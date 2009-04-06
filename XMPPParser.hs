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
 - This module contains the parser of XMPP messages.
 -
 -----------------------------------------------------------------------------}


{-|
  This module contains the parser of XMPP messages. See RFC 3920 for details.
 -}
module XMPPParser (parseXMPP) where

import Control.Concurrent.STM
  (TChan, writeTChan, atomically)
import Data.List
  (isPrefixOf)
import Control.Monad
  (when)

import Text.XML.HaXml.SAX
  (SaxElement(..), saxParse)
import Text.XML.HaXml
  (Attribute, AttValue(..), Reference)

import Global
import ParserGlobal
import IQStanzaParser
  (processIq)
import MessageStanzaParser
  (processMessage)
import PresenceStanzaParser
  (processPresence)


{-|
  The 'parseXMPP' function parser the client XMPP input and sends commands
  according to the parsed data to the main loop.
 -}
parseXMPP :: String        -- ^ The string that is loaded from the client socket
          -> TChan Command -- ^ The channel for sending data to the command loop
          -> IO ()         -- ^ The return value
parseXMPP xmlstring chan = do
  debugInfo "Parsing..."
  processXMPP xmlstring chan


{-|
  This function processes XMPP protocol and sends proper commands to the
  command channel; these are served in the command processing engine.
 -}
processXMPP :: String          -- ^ The input XML string
            -> TChan Command   -- ^ The channel for sending commands
            -> IO ()           -- ^ The return value
processXMPP xmlstring chan = do
  let elements = getElements xmlstring
  processConnection chan elements


{-|
  The function that processes the list of SAX parser events.
 -}
processConnection :: TChan Command         -- ^ The channel for sending commands
                  -> [Maybe SaxElement]    -- ^ The list of SAX parser events (or
                                           --   Nothing in case of an error)
                  -> IO ()                 -- ^ The return value 
processConnection chan elements = do
  safely chan elements $
    (\x xs -> case x of
      (SaxElementOpen name attrs) -> do  -- an opening XML tag
        let prefix = extractStreamNamespacePrefix attrs
        case prefix of
          Nothing -> sendCommand chan $ Error "No stream namespace declared!"
          (Just pref) -> do
            debugInfo $ "Stream prefix: " ++ pref
            let tag = pref ++ ":stream"
            when (name /= tag) $
              sendCommand chan $ Error
                ("The opening tag should be <" ++ tag ++ ">!")
            --
            -- TODO: detect if the tag name is prefix:stream
            --
            let xmlns = case (attrs `getValueOfAttribute` "xmlns") of
                          Nothing -> ""
                          (Just ns) -> ns
            when (xmlns /= clientNamespace) $
              sendCommand chan $ Error
                ("The default namespace does not match " ++ clientNamespace)
            let xmllang = case (attrs `getValueOfAttribute` "xml:lang") of
                            Nothing -> defaultXMLLang
                            (Just lang) -> lang
            let version = case (attrs `getValueOfAttribute` "version") of
                            Nothing -> "0.0"
                            (Just ver) -> ver
            debugInfo $ "Version: " ++ version
            let isUnsupported = isUnsupportedVersion version
            when (isUnsupported) $
              sendCommand chan $ Error "Maximum version supported is 1.0!"
            sendCommand chan $ OpenStream xmllang version
            processStream pref chan xs
      (SaxCharData _) ->           -- ignore CDATA
        processConnection chan xs
      _ ->                         -- other than the opening XML tag or CDATA
        sendCommand chan $ Error "Opening <stream> tag expected!"
    )


processStream :: String
              -> TChan Command
              -> [Maybe SaxElement]
              -> IO ()
processStream prefix chan elements =
  safely chan elements $
    (\x xs -> case x of
      (SaxElementOpen name attrs) -> do  -- opening XML tag
        if (matchesStringForPrefix name "iq" prefix) then
            processIq prefix attrs chan xs
          else if (matchesStringForPrefix name "presence" prefix) then
            processPresence prefix attrs chan xs
          else if (matchesStringForPrefix name "message" prefix) then
            processMessage attrs prefix chan xs
          else
            sendCommand chan $ Error ("Invalid stream stanza: " ++ name)
      (SaxElementTag name attrs) -> do   -- empty XML tag
        debugInfo $ "Empty tag: " ++ showSax x
      (SaxCharData _) ->                 -- ignore CDATA
        processStream prefix chan xs
      _ -> do                            -- other
        debugInfo $ "Malformed stream!" ++ showSax x
        sendCommand chan $ Error "Malformed stream!"
    )


{-|
  This function is to be used when deciding about whether to continue
  processing list of SAX events (in case it is non-empty) or whether to stop
  processing. The continuation of processing is done by applying the function
  given by the contFunc parameter.
 -}
continue :: TChan Command                          -- ^ The command channel
         -> [Maybe SaxElement]                     -- ^ The list of SAX events   
         -> (TChan Command -> [Maybe SaxElement] -> IO ())
         -- ^ The function to apply when we want to continue processing
         -> IO ()                                  -- ^ The return value
continue chan elements contFunc = do
  if (null elements)
    then return ()
    else contFunc chan elements



{- this could maybe be used at some point... (when sending commands to the
 - main loop)
listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan =
  sequence_ (repeat (act >>= atomically . writeTChan chan))
-}


{-|
  This is a wrapper for HaXml 'saxParse' function that does not need the name
  of the input file.
 -}
saxParse' :: String                        -- ^ The input XML string
          -> ([SaxElement], Maybe String)  -- ^ The 'saxParse' result
saxParse' = saxParse ""


{-|
  The function that gets the list of SAX parser events from a XML string
 -}
getElements :: String               -- ^ The input XML string
            -> [Maybe SaxElement]   -- ^ The list of Just SaxElements or in
                                    --   case of an error, Nothing
getElements str = getNextElement $ saxParse' str
  where getNextElement :: ([SaxElement], Maybe String) -> [Maybe SaxElement]
        getNextElement ([], Nothing) = []         -- correct end of XML stream
        getNextElement ([], Just _) = [Nothing]   -- XML stream error
        getNextElement ((x:xs), y) = case x of
          (SaxProcessingInstruction _) -> getNextElement (xs, y) -- ignore
          (SaxComment _) -> getNextElement (xs, y)               -- ignore
          (SaxDocTypeDecl _) -> getNextElement (xs, y)           -- ignore
          (SaxReference _) -> [Nothing]       -- unsupported XML construct
          (SaxElementOpen _ _) -> (Just x):(getNextElement (xs, y))
          (SaxElementClose _) -> (Just x):(getNextElement (xs, y))
          (SaxElementTag _ _) -> (Just x):(getNextElement (xs, y))
          (SaxCharData _) -> (Just x):(getNextElement (xs, y))


{-|
  This function retrieves the XMPP stream XML namespace prefix from a list of
  attributes.
 -}
extractStreamNamespacePrefix :: [Attribute]       -- ^ The list of attributes
                             -> Maybe String      -- ^ The XMPP stream XML
                                                  --   namespace prefix
extractStreamNamespacePrefix [] = Nothing
extractStreamNamespacePrefix ((name, attr):xs) =
  if (("xmlns" `isPrefixOf` name) && namespaceMatches attr)
    then (Just (getPrefix name))
    else (extractStreamNamespacePrefix xs)
  where namespaceMatches :: AttValue -> Bool
        namespaceMatches namespace = case (getAttributeValue namespace) of
          Nothing -> False
          (Just value) -> value == streamNamespace
        getPrefix :: String -> String
        getPrefix str = takeWhile (/= '=') $ tail $ dropWhile (/= ':') str


{-|
  The 'isUnsupportedVersion' function determines whether the server supports
  (or rather does not support) the XMPP version declared by the client.
 -}
isUnsupportedVersion :: String    -- ^ A string with the client XMPP version
                     -> Bool      -- ^ Does the server support such version?
isUnsupportedVersion version =
  case versionMajorMinor of
    (Nothing, _) -> False
    (_, Nothing) -> False
    (Just maj, Just min) -> if (maj > 1) then True
                                         else ((maj == 1) && (min > 0))
  where versionMajorMinor = (\(x, y) -> f x (tail y)) $ span (/= '.') version
        f major minor = (strToInt major, strToInt minor)
        strToInt :: String
                 -> Maybe Int
        strToInt str = if (null (snd inputPair))
                         then Just $ fst inputPair
                         else Nothing
          where inputPair = head $ reads str

