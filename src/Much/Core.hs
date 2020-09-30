{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Much.Core where

import Blessings.String (Blessings(Plain,SGR),pp)
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.Time
import Much.API
import Much.Action
import Much.Event
import Much.RenderTreeView (renderTreeView)
import Much.Screen
import Much.State
import Much.TreeSearch
import Much.TreeView
import Much.Utils
import Options.Applicative
import Scanner (scan,Scan(..))
import System.Environment
import System.Exit
import System.IO
import System.Posix.Signals
import qualified Blessings.Internal as Blessings
import qualified Data.Map as M
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Much.Config as Config
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
    , query = "tag:inbox AND NOT tag:killed"
    , keymap = displayKey
    , mousemap = displayMouse
    , colorConfig = ColorConfig
        { tagMap = M.fromList
            [ ("killed", SGR [38,5,088])
            , ("star", SGR [38,5,226])
            , ("draft", SGR [38,5,202])
            ]
        , alt = SGR [38,5,182]
        , search = SGR [38,5,162]
        , focus = SGR [38,5,160]
        , unprintableFocus = SGR [38,5,204]
        , unprintableNormal = SGR [35]
        , quote = SGR [38,5,242]
        , boring = SGR [38,5,240]
        , prefix = SGR [38,5,235]
        , date = SGR [38,5,071]
        , tags = SGR [38,5,036]
        , boringMessage = SGR [38,5,023]
        , unreadMessage = SGR [38,5,117]
        , unreadSearch = SGR [38,5,250]
        }
    , tagSymbols = M.empty
    , apiConfig = Much.API.emptyConfig
    }


importConfig :: Config.Config -> State -> State
importConfig config state = state
  { tagSymbols = fromMaybe (tagSymbols state) (Config.tagSymbols config)
  , query = fromMaybe (query state) (Config.query config)
  , colorConfig =
    let fromColorConfig key1 key2 = case Config.colorConfig config of
                                      Just colorC -> maybe (key1 (colorConfig state)) SGR (key2 colorC)
                                      Nothing -> key1 (colorConfig state)
    in ColorConfig
      { tagMap =
        case tagMap <$> Config.colorConfig config of
          Just tagMap' ->
            M.foldlWithKey
              (\previous k v -> maybe previous (\code -> M.insert k (SGR code) previous) v)
              (tagMap (colorConfig state))
              tagMap'
          Nothing -> tagMap (colorConfig state)
      , alt = fromColorConfig alt alt
      , search = fromColorConfig search search
      , focus = fromColorConfig focus focus
      , quote = fromColorConfig quote quote
      , unprintableFocus = fromColorConfig unprintableFocus unprintableFocus
      , unprintableNormal = fromColorConfig unprintableNormal unprintableNormal
      , boring = fromColorConfig boring boring
      , prefix = fromColorConfig prefix prefix
      , date = fromColorConfig date date
      , tags = fromColorConfig tags tags
      , boringMessage = fromColorConfig boringMessage boringMessage
      , unreadSearch = fromColorConfig unreadSearch unreadSearch
      , unreadMessage = fromColorConfig unreadMessage unreadMessage
      }
  }

notmuchSearch :: State -> IO State
notmuchSearch q@State{query} = do
  r_ <- either error id <$> Notmuch.search
                                [ "--offset=0"
                                , "--limit=100"
                                , query
                                ]

  return q { cursor = Z.fromTree $ fromSearchResults query r_ }


mainWithState :: State -> IO ()
mainWithState state@State{query = defaultSearch} = do
    (query, maybeConfigPath) <- execParser (options defaultSearch)
    newState <- case maybeConfigPath of
      Just configPath -> do
        eitherConfig <- eitherDecodeFileStrict configPath
        case eitherConfig of
          Left err -> do
            hPutStrLn stderr err
            exitFailure
          Right config -> return $ importConfig config state
      Nothing -> return state
    withScreen s0 (\_-> notmuchSearch newState { query = query } >>= runState)
  where
    options defaultQuery =
      info (muchArgs <**> helper) (fullDesc <> progDesc "Command-line MUA using notmuch.")
        where
          muchArgs =
            (,)
            <$> strOption (long "query" <> short 'q' <> metavar "QUERY" <> help "Open specific search" <> value defaultQuery <> showDefault)
            <*> optional (strOption (long "config" <> short 'c' <> metavar "PATH" <> help "Import settings from a JSON config"))

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
        , Much.API.main (apiConfig q1) putEvent
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
        Just Term.Window {Term.width = w, Term.height = h} ->
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
    EScan mouseInfo@ScanMouse{..} ->
        Right <$> mousemap q mouseInfo q
    EShutdown ->
        return $ Left q
    EResize w h ->
        return $ Right q
            { screenWidth = w, screenHeight = h
            , flashMessage = Plain $ "resize " <> show (w,h)
            }
    EStateGet f ->
        forkIO (f q) $> Right q
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
    newTreeBuf = renderTreeView q (Z.root cursor)
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
    buffer ++ replicate (screenHeight - length buffer) "~"


redraw :: State -> IO ()
redraw q@State{..} = do
    hPutStr stdout $ map (sub '\t' ' ') $ "\ESC[H" ++ pp (mintercalate "\n" $ map eraseRight $ render0 q)
    hFlush stdout
  where
    sub x x' c = if c == x then x' else c
    eraseRight s =
        if Blessings.length s < screenWidth
            then s <> "\ESC[K"
            else s
