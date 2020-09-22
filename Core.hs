{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Core where

import Action
import Blessings.String (Blessings(Plain,SGR),pp)
import Control.Concurrent
import Control.Monad
import Data.Time
import Event
import RenderTreeView (renderTreeView)
import Scanner (scan,Scan(..))
import Screen
import State
import System.Console.Docopt.NoTH (getArgWithDefault, parseArgsOrExit, parseUsageOrExit, shortOption)
import System.Environment
import System.IO
import System.Posix.Signals
import TreeSearch
import TreeView
import Utils
import qualified Blessings.Internal as Blessings
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Notmuch
import qualified System.Console.Terminal.Size as Term



emptyState :: State
emptyState = State
    { cursor = Z.fromTree (Tree.Node (TVSearch "<emptyState>") [])
    , xoffset = 0
    , yoffset = 0
    , flashMessage = "Welcome to much; quit with ^C"
    , screenWidth = 0
    , screenHeight = 0
    , headBuffer = []
    , treeBuffer = []
    , now = UTCTime (fromGregorian 1984 5 23) 49062
    , signalHandlers = []
    , keymap = displayKey
    , mousemap = displayMouse
    }

withQuery :: String -> State -> IO State
withQuery query q = do
  r_ <- either error id <$> Notmuch.search
                                [ "--offset=0"
                                , "--limit=100"
                                , query
                                ]

  return q { cursor = Z.fromTree $ fromSearchResults query r_ }

mainWithState :: State -> IO ()
mainWithState state = mainWithStateAndArgs state =<< getArgs

mainWithStateAndArgs :: State -> [String] -> IO ()
mainWithStateAndArgs state args = do
    usage' <- parseUsageOrExit usage
    args' <- parseArgsOrExit usage' args
    let query = getArgWithDefault args' defaultSearch (shortOption 'q')
    withScreen s0 (\_-> withQuery query state >>= runState)
  where
    usage = unlines
      [ "Command-line MUA using notmuch."
      , ""
      , "Usage:"
      , "  much [-q <search-term>]"
      , ""
      , "Options:"
      , "  -q <search-term>, --query=<search-term>"
      , "        Open specific search, defaults to " ++ (show defaultSearch)
      ]
    defaultSearch = "tag:inbox AND NOT tag:killed"

    s0 = Screen False NoBuffering (BlockBuffering $ Just 4096)
            [ 1000 -- X & Y on button press and release
            , 1005 -- UTF-8 mouse mode
            , 1047 -- use alternate screen buffer
            ]
            [   25 -- hide cursor
            ]

runState :: State -> IO ()
runState q0 = do

    -- load-env hack
    maybe (return ()) (setEnv "HOME") =<< lookupEnv "OLDHOME"

    (putEvent, getEvent) <- do
        v <- newEmptyMVar
        return (putMVar v, takeMVar v)

    let q1 = q0 { signalHandlers =
                    [ (sigINT, putEvent EShutdown)
                    , (28, winchHandler putEvent)
                    ] }

    installHandlers (signalHandlers q1)

    threadIds <- mapM forkIO
        [ forever $ scan stdin >>= putEvent . EScan
        ]

    winchHandler putEvent

    run getEvent q1
    mapM_ killThread threadIds


installHandlers :: [(Signal, IO ())] -> IO ()
installHandlers =
    mapM_ (\(s, h) -> installHandler s (Catch h) Nothing)

uninstallHandlers :: [(Signal, IO ())] -> IO ()
uninstallHandlers =
    mapM_ (\(s, _) -> installHandler s Ignore Nothing)


winchHandler :: (Event -> IO ()) -> IO ()
winchHandler putEvent =
    Term.size >>= \case
        Just (Term.Window {Term.width = w, Term.height = h}) ->
            putEvent $ EResize w h
        Nothing ->
            return ()

run :: IO Event -> State -> IO ()
run getEvent = rec . Right where
    rec = \case
        Right q -> rec =<< do
            t <- getCurrentTime
            let q' = render q { now = t }
            redraw q' >> getEvent >>= processEvent q'
        Left _q -> return ()


processEvent :: State -> Event -> IO (Either State State)
processEvent q = \case
    EFlash t ->
        return $ Right q { flashMessage = t }
    EScan (ScanKey s) ->
        Right <$> keymap q s q
    EScan info@ScanMouse{..} ->
        Right <$> mousemap q info q
    EShutdown ->
        return $ Left q
    EResize w h ->
        return $ Right q
            { screenWidth = w, screenHeight = h
            , flashMessage = Plain $ "resize " <> show (w,h)
            }
    ev ->
        return $ Right q
            { flashMessage = SGR [31,1] $ Plain $ "unhandled event: " <> show ev
            }


render :: State -> State
render q@State{..} =
    q { treeBuffer = newTreeBuf
      , headBuffer = newHeadBuf
      }
  where
    newTreeBuf = renderTreeView now cursor (Z.root cursor)
    newHeadBuf =
        [ Plain (show screenWidth) <> "x" <> Plain (show screenHeight)
          <> " " <> Plain (show $ linearPos cursor - yoffset)
          <> " " <> Plain (show $ topOverrun q)
          <> " " <> Plain (show $ botOverrun q)
          <> " " <> flashMessage
          <> " " <> Plain (show (xoffset, yoffset))
        ]

render0 :: State -> [Blessings String]
render0 _q@State{..} = do
    let buffer =
            map (Blessings.take screenWidth . Blessings.drop xoffset) $
            take screenHeight $
            headBuffer ++ drop yoffset treeBuffer
    buffer ++ take (screenHeight - length buffer) (repeat "~")


redraw :: State -> IO ()
redraw q@State{..} = do
    hPutStr stdout $ map (sub '\t' ' ') $ "\ESC[H" ++ (pp $ mintercalate "\n" $ map eraseRight $ render0 q)
    hFlush stdout
  where
    sub x x' c = if c == x then x' else c
    eraseRight s =
        if Blessings.length s < screenWidth
            then s <> "\ESC[K"
            else s



