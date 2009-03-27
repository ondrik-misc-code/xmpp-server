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

import Text.XML.HaXml.SAX
  (SaxElement(..), saxParse)
import Text.XML.HaXml
  (Attribute, AttValue(..), Reference)
import Control.Concurrent.STM
  (TChan, writeTChan, atomically)

import Global


{-|
  The 'parseXMPP' function parser the client XMPP input and sends commands
  according to the parsed data to the main loop.
 -}
parseXMPP :: String        -- ^ The string that is loaded from the client socket
          -> TChan Command -- ^ The channel for sending data to the command loop
          -> IO ()         -- ^ The return value
parseXMPP xmlstring chan = do
  debugInfo "Parsing..."
  atomically $ writeTChan chan $ Authenticate "ondra" "pokus"
  debugInfo $ "Input XML: " ++ xmlstring
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
                  -> [(Maybe SaxElement)]  -- ^ The list of SAX parser events (or
                                           --   Nothing in case of an error)
                  -> IO ()                 -- ^ The return value 
processConnection chan elements = do
  safely chan elements $
    (\x xs -> case x of
      -- an opening XML tag
      (SaxElementOpen name attrs) -> do
        sendCommand chan OpenStream
        processStream chan xs
      -- other than the opening XML tag
      _ -> sendCommand chan Error
    )


processStream chan elements = ((return ()) :: IO ())


{-|
  The 'safely' construction is to be used when processing SAX parser events.
  It either processes the function passed in the handler parameter or in case
  of an error, it sends the error message to the channel.
 -}
safely :: TChan Command                 -- ^ The channel for sending commands
       -> [(Maybe SaxElement)]          -- ^ The input list of SAX events 
       -> (SaxElement -> [(Maybe SaxElement)] -> IO ())
          -- ^ The function that handles correct SAX events
       -> IO ()                         -- ^ The return value
safely chan elements handler = do
  case elements of
    [] -> do                    -- end of stream
      debugInfo "End of stream!"
      sendCommand chan EndOfStream
    (Nothing:_) -> do           -- an error
      debugInfo "Error!"
      sendCommand chan Error
    ((Just x):xs) -> do         -- a valid SAX element
      debugInfo $ showSax x
      handler x xs


{-|
  This function is to be used when deciding about whether to continue
  processing list of SAX events (in case it is non-empty) or whether to stop
  processing. The continuation of processing is done by applying the function
  given by the contFunc parameter.
 -}
continue :: TChan Command                          -- ^ The command channel
         -> [(Maybe SaxElement)]                   -- ^ The list of SAX events   
         -> (TChan Command -> [(Maybe SaxElement)] -> IO ())
         -- ^ The function to apply when we want to continue processing
         -> IO ()                                  -- ^ The return value
continue chan elements contFunc = do
  if (null elements)
    then return ()
    else contFunc chan elements


{-|
  The function that sends a command to a command channel.
 -}
sendCommand :: TChan Command    -- ^ The command channel
            -> Command          -- ^ The command to be sent
            -> IO ()            -- ^ The return value
sendCommand chan = atomically . writeTChan chan


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
  The 'showSax' function is a debug function for transformation of an
  arbitrary SAX element into a string.
 -}
showSax :: SaxElement     -- ^ The SAX element
        -> String         -- ^ The string representation of the SAX element
showSax element =
  case element of
    (SaxComment comment) ->
      "Comment: " ++ comment ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"
    (SaxElementOpen open_tag_name attrs) ->
      "Element open: " ++ (cropQuotes $ show open_tag_name)
      ++ "\n  Attributes: " ++ (showAttributes attrs) ++ "\n"
    (SaxElementClose close_tag_name) ->
      "Element close: " ++ (cropQuotes $ show close_tag_name) ++ "\n"
    (SaxElementTag tag_name attrs) ->
      "Element empty: " ++ (cropQuotes $ show tag_name) ++ "\n  Attributes: "
      ++ (showAttributes attrs) ++ "\n"
    (SaxCharData char_data) ->
      "Character data: " ++ (cropQuotes $ show char_data) ++ "\n"
    (SaxDocTypeDecl _) ->
      "DocType: " ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"
    (SaxProcessingInstruction (target, str)) ->
      "Processing instruction: " ++ target ++ " --- " ++ str
      ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"
    (SaxReference _) ->
      "Reference: " ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"


{-|
  The 'showAttributes' function converts a list of XML attributes into
  a string.
 -}
showAttributes :: [Attribute]       -- ^ The string of XML attributes
               -> String            -- ^ The output string
showAttributes attrs = foldr ((++) . (++ " ") . showAttr ) "" attrs
  where showAttr (name, AttValue value) =
          name ++ "=\"" ++ (showAttrValue $ head value) ++ "\""


{-|
  This function shows the attribute value.
 -}
showAttrValue :: Either String Reference   -- ^ The attribute value: either
                                           --   a string or a reference
              -> String                    -- ^ The output attribute value
showAttrValue (Left str) = str
showAttrValue _ = error "XMPP does not support other XML attributes than strings!"


{-|
  This function crops leading and trailing double quote marks from a string.
 -}
cropQuotes :: String       -- ^ The input string
           -> String       -- ^ The output string (with leading and trailing
                           --   double quote marks removed)
cropQuotes     [] = []
cropQuotes (x:xs) = if (x == '\"') then cropEnd xs else x:(cropEnd xs)
  where cropEnd = foldr (\y z -> if (z == "\"") then [y] else (y:z)) ""
