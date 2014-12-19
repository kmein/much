{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Notmuch where

--import Language.Haskell.TH.Ppr (bytesToString)
import Data.Aeson
--import Data.List.Split
--import Data.Attoparsec.ByteString hiding (try)
import Data.Maybe
import Data.Monoid
import Data.String
--import Data.Traversable
import Data.Tree
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
--import Data.Version (Version(..), parseVersion)
import System.Process
--import System.IO
import qualified Data.Map as M

import Notmuch.SearchResult
import Notmuch.Message


import Control.Concurrent
--import Control.Concurrent.MVar
import Control.Exception
import System.IO
import Control.DeepSeq (rnf)


-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid




notmuch :: [String] -> IO LBS.ByteString
notmuch args = do
    (_, Just hout, _, ph) <- createProcess (proc "notmuch" args)
                                          { std_out = CreatePipe }
    output <- LBS.hGetContents hout


    withForkWait (evaluate $ rnf output) $ \waitOut -> do

      ---- now write any input
      --unless (null input) $
      --  ignoreSigPipe $ hPutStr inh input
      -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
      --ignoreSigPipe $ hClose inh

      -- wait on the output
      waitOut
      hClose hout

    -- wait on the process
    _ex <- waitForProcess ph
    --return (ex, output)

    --case ex of
    --    ExitSuccess   -> return output
    --    ExitFailure r -> processFailedException "readProcess" cmd args r

    return output



--notmuch' args = do
--    (_, Just hout, _, _) <- createProcess (proc "notmuch" args)
--                                          { std_out = CreatePipe }
--    BS.hGetContents hout


search :: String -> IO ()
search term = do
    c <- notmuch [ "search", "--format=json", "--format-version=2", term ]

    let results = case eitherDecode' c :: Either String [SearchResult] of
                      Left err -> error err
                      Right x -> x

    mapM_ (T.putStrLn . drawSearchResult) results


showThread :: String -> IO ()
showThread tid = do
    c' <- notmuch [ "show", "--format=json", "--format-version=2"
                   , "thread:" <> tid ]

    let threads = case eitherDecode' c' :: Either String [Thread] of
                      Left err -> error err
                      Right x -> x
        --threadsF = map threadForest threads
        ttt = head $ threadForest $ head $ threads

    --Prelude.putStrLn $ drawTree $ fmap drawMessage ttt
    Prelude.putStrLn $ showTree $ ttt


getThread :: String -> IO (Tree Message)
getThread tid = do
    c' <- notmuch [ "show", "--format=json", "--format-version=2"
                   , "thread:" <> tid ]

    let threads = case eitherDecode' c' :: Either String [Thread] of
                      Left err -> error err
                      Right x -> x
        --threadsF = map threadForest threads
        ttt = head $ threadForest $ head $ threads
    return ttt



setTag :: String -> String -> IO LBS.ByteString
setTag tag i = do
    notmuch [ "tag", "+" <> tag , "id:" <> i ]


unsetTag :: String -> String -> IO LBS.ByteString
unsetTag tag i = do
    notmuch [ "tag", "-" <> tag , "id:" <> i ]


openMessage :: String -> IO ()
openMessage i = do
    notmuch [ "tag", "+open" , "id:" <> i ] >> return ()


closeMessage :: String -> IO ()
closeMessage i = do
    notmuch [ "tag", "-open" , "id:" <> i ] >> return ()






---- | Neat 2-dimensional drawing of a tree.
--drawTree :: Tree String -> String
--drawTree  = unlines . draw
--
---- | Neat 2-dimensional drawing of a forest.
--drawForest :: Forest String -> String
--drawForest  = unlines . map drawTree
--
draw :: Tree Message -> [String]
draw (Node x ts0) = (show $ drawMessage x) : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        -- "|" : shift "`- " "   " (draw t)
        shift "`- " "   " (draw t)

    drawSubTrees (t:ts) =
        -- "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
        shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)


indentMessageInit :: [String] -> [String]
indentMessageInit [] = []
indentMessageInit (s:ss) = (red "─╴" ++ s) : map (red "  " ++) ss

indentMessageLast :: [String] -> [String]
indentMessageLast [] = []
indentMessageLast (s:ss) = (red "┬╴" ++ s) : map (red "│ " ++) ss

indentInit :: [String] -> [String]
indentInit [] = []
indentInit (s:ss) = (red "├╴" ++ s) : map (red "│ " ++) ss

indentLast :: [String] -> [String]
indentLast [] = []
indentLast (s:ss) = (red "└╴" ++ s) : map (red "  " ++) ss

indentChildren :: [[String]] -> [[String]]
indentChildren [] = []
indentChildren ns = map indentInit (init ns) ++ [indentLast (last ns)]

appLast :: [String] -> String -> [String]
appLast ss s = init ss ++ [last ss ++ s]

showTree' :: Tree Message -> [String]
showTree' (Node n ns) =
    -- (if length ns == 0
    --     then indentMessageInit $ drawMessage n
    --     else indentMessageLast $ drawMessage n)
    drawMessage n
    ++
    concat (indentChildren (map showTree' ns))

-- | Show a 'Tree' using Unicode art
showTree :: Tree Message -> String
showTree = unlines . showTree'






drawMessage :: Message -> [String]
drawMessage Message{..} =
    ----unMessageID messageId
    --show messageTime
    --  -- <> " " <> T.unpack messageDateRel
    --  <> "\n" <> show (fromJust $ M.lookup "From" messageHeaders)
    --  <> "\n" <> show (fromJust $ M.lookup "Subject" messageHeaders)
    [ gray (unMessageID messageId)
      <> " " <> T.unpack (fromJust $ M.lookup "From" messageHeaders)
      <> " " <> gray (show messageDateRel)
      <> " " <> T.unpack (T.intercalate ", " $ map magenta messageTags)
    -- , T.unpack $ fromJust $ M.lookup "Subject" messageHeaders
    ]
    ++
    (if "open" `elem` messageTags
        then concat (map (map green . lines . drawMessagePart) messageBody)
        else [])
    --map drawMessagePart messageBody

drawMessagePart :: MessagePart -> String
drawMessagePart p = drawPartContent (partContent p)

drawPartContent :: MessageContent -> String
drawPartContent (ContentText t) = T.unpack t
--drawPartContent (ContentText t) = "ContentText"
drawPartContent (ContentMultipart _) = "ContentMultipart"
drawPartContent (ContentMsgRFC822 _) = "ContentMsgRFC822"

-- otherAuthors are non-matched authors in the same thread
drawSearchResult :: SearchResult -> T.Text
drawSearchResult SearchResult{..} = do
    let (matchedAuthors, otherAuthors) =
            case T.splitOn "| " searchAuthors of
                [a,b] -> (T.splitOn ", " a, T.splitOn ", " b)
                [a] -> (T.splitOn ", " a, [])

        a' = map green matchedAuthors
        b' = map red otherAuthors
        --qa = maybe [] (T.splitOn " ") (searchQuery !! 0)
        --qb = maybe [] (T.splitOn " ") (searchQuery !! 1)
        ThreadID tid = searchThread

    (T.pack tid)
        -- <> " " <> (T.pack $ show $ searchTime)
        <> " " <> "[" <> (T.pack $ show searchMatched) <> "/"
                      <> (T.pack $ show searchTotal) <> "]"
        <> " " <> searchDateRel
        <> " " <> searchSubject
        <> " " <> T.intercalate ", " (a' <> b')
        <> " " <> T.intercalate ", " (map magenta searchTags)




red, green, magenta, gray :: (Monoid m, IsString m) => m -> m
red = ("\ESC[31m"<>) . (<>"\ESC[m")
green = ("\ESC[32m"<>) . (<>"\ESC[m")
magenta = ("\ESC[35m"<>) . (<>"\ESC[m")
gray = ("\ESC[30;1m"<>) . (<>"\ESC[39;22m")




    --case fromJSON c of
    --    Error e -> error e
    --    Success x -> return x

--    c <- hGetContents hout
--
--    let v = 
--
--
--    putStrLn $ show c

--    let fixTags :: Char -> Char
--        fixTags '+' = '-'
--        fixTags '~' = '-'
--        fixTags c   = c
--    let vStr = map fixTags $ words out !! 1
--    let vs = filter (\(_,r) -> r == "") $ readP_to_S parseVersion vStr
--    case vs of
--        ((v,_):_) -> return v
--        _ -> throw $ NotmuchError $ "Unable to parse version: " ++ vStr



-- | The version of notmuch
--notmuchVersion :: MonadIO m => m Version
--notmuchVersion = do
--    out <- liftIO $ readProcess "notmuch" ["--version"] ""
--    let fixTags :: Char -> Char
--        fixTags '+' = '-'
--        fixTags '~' = '-'
--        fixTags c   = c
--    let vStr = map fixTags $ words out !! 1
--    let vs = filter (\(_,r) -> r == "") $ readP_to_S parseVersion vStr
--    case vs of
--        ((v,_):_) -> return v
--        _ -> throw $ NotmuchError $ "Unable to parse version: " ++ vStr
--
--
-- r <- createProcess (proc "ls" [])
--
--proc 
