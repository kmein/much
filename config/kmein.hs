{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Much.Action
import Much.Core
import Much.State
import Much.TreeView
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Data.ByteString.Lazy.Char8 as LBS8

import Blessings.String
import Control.Monad
import Data.Maybe
import Data.Time.Format
import Scanner
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Signals
import System.Process
import Text.Hyphenation
import Text.LineBreak
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z

{- notmuch's special tags are:

  synchonised to maildir: draft flagged passed replied unread
  automatic: attachment signed encrypted
  cli default tags: unread inbox deleted spam

  ref: https://notmuchmail.org/special-tags/
-}

attachmentDestination :: FilePath
attachmentDestination = "/tmp"

main :: IO ()
main =
    mainWithState emptyState
      { keymap = myKeymap
      , mousemap = myMousemap
      , colorConfig = (colorConfig emptyState)
        { boring = SGR [38,5,8]
        , alt = SGR [38,5,182]
        , search = SGR [38,5,13]
        , focus = SGR [38,5,4]
        , quote = SGR [38,5,7]
        , prefix = SGR [38,5,235]
        , date = SGR [38,5,1]
        , tags = SGR [38,5,14]
        , boringMessage = SGR [38,5,3]
        , unreadMessage = SGR [38,5,11]
        , unreadSearch = SGR [38,5,15]
        , tagMap = M.fromList
            [ ("deleted", SGR [38,5,088])
            , ("flagged", SGR [38,5,226])
            , ("draft", SGR [38,5,63])
            , ("spam", SGR [38,5,202])
            ]
        }
      , tagSymbols = M.fromList
          [ ("flagged", "🔖")
          , ("attachment", "📎")
          , ("signed", "🔒")
          ]
      , query = "tag:inbox"
      }


saveAttachment :: State -> IO (Maybe FilePath, State)
saveAttachment q
  | TVMessagePart message part <- Z.label (cursor q) = do
    let messageId = Notmuch.unMessageID (Notmuch.messageId message)
    Notmuch.notmuchShowPart messageId (Notmuch.partID part) >>= \case
      Right part' ->
        let
          destination
            | Just partFileName <- Notmuch.partContentFilename part = attachmentDestination </> T.unpack partFileName
            | otherwise = concat ["much_", formatTime defaultTimeLocale "%s" (Notmuch.messageTime message), "_", show (Notmuch.partID part)]
          q' = q { flashMessage = SGR [1] (Plain destination) <> Plain " saved" }
         in case Notmuch.partContent part' of
              Notmuch.ContentText text -> (Just destination, q') <$ T.writeFile destination text
              Notmuch.ContentRaw raw _ -> (Just destination, q') <$ LBS8.writeFile destination raw
              _ -> return (Nothing, q { flashMessage = "this part cannot be saved" })
      Left err -> return (Nothing, q { flashMessage = Plain err })
  | otherwise = return (Nothing, q { flashMessage = "cursor not on attachment" })


reply :: State -> IO State
reply q = q <$ spawnCommand "i3-sensible-terminal -e $EDITOR -c 'read !mail-reply'"

myKeymap :: String -> State -> IO State
myKeymap "h" = closeFold
myKeymap "l" = openFold
myKeymap " " = toggleFold

myKeymap "g" = moveCursorUp 150
myKeymap "G" = moveCursorDown 150
myKeymap "k" = moveCursorUp 1
myKeymap "j" = moveCursorDown 1
myKeymap "\ESC[A" = moveCursorDown 1
myKeymap "\ESC[B" = moveCursorUp 1
myKeymap "\ESC[C" = moveTreeLeft 10  -- left
myKeymap "\ESC[D" = moveTreeRight 10 -- right

myKeymap "r" = notmuchSearch

myKeymap "R" = reply
myKeymap "S" = fmap snd . saveAttachment
myKeymap "o" = saveAttachment >=> \case
  (Nothing, q') -> return q'
  (Just filePath, q') ->
    q' <$ callProcess "xdg-open" [filePath]

myKeymap "q" = \q -> q <$ raiseSignal sigINT

myKeymap "*" = toggleTagAtCursor "flagged"
myKeymap "a" = toggleTagAtCursor "inbox" -- mnemonic: Archive
myKeymap "s" = toggleTagAtCursor "unread" -- mnemonic: Seen
myKeymap "d" = toggleTagAtCursor "deleted"
myKeymap "!" = toggleTagAtCursor "spam"

myKeymap "N" = moveCursorUpToPrevUnread
myKeymap "n" = moveCursorDownToNextUnread

myKeymap "K" = moveTreeDown 1
myKeymap "J" = moveTreeUp 1
myKeymap "\ESC[a" = moveTreeDown 1
myKeymap "\ESC[b" = moveTreeUp 1
myKeymap "\ESC[5~" = \q -> moveTreeDown (screenHeight q `div` 2) q  -- PgUp
myKeymap "\ESC[6~" = \q -> moveTreeUp (screenHeight q `div` 2) q    -- PgDn
myKeymap "\ESC[Z" = moveCursorUpToPrevUnread -- S-Tab
myKeymap "\DEL" = moveToParent  -- backspace
myKeymap "=" = \q@State{..} ->
    let cursor' = case Z.label cursor of
            TVMessageLine a b c s ->
                wrap (TVMessageLine a b c) cursor s
            _ -> cursor
    in return q { cursor = cursor' }
  where

    --unwrap = error "WIP"
        -- 1. get current id (must be TVMessageLine)
        -- 2. find first adjoined TVMessageLine with same id
        -- 3. find last adjoined TVMessageLine with same id
        -- 4. join lines (with space?)

    wrap ctor loc s =
        fromMaybe (error "die hard") $
        Z.nextTree $
        foldr (insert . ctor)
              (Z.delete loc)
              $ hy s

    insert a =
        Z.prevSpace . Z.insert (Tree.Node a [])

    hy = breakStringLn bf
      where
        shy = '\173'
        hyp = Just german_1996
        bf = BreakFormat 80 8 shy hyp

-- <F1>
myKeymap "\ESC[11~" = \q@State{..} ->
    return q { flashMessage = Plain $ show $ treeViewId $ Z.label cursor }

-- <F2>
myKeymap "\ESC[12~" = \q@State{..} ->
    return q { flashMessage =
                  Plain $
                  show $
                  fmap Notmuch.messageFilename $
                  getMessage $
                  Z.label cursor
              }

-- TODO Stuff Vim sends after exit (also there is more...)
myKeymap "\ESC[2;2R" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }
myKeymap "\ESC[>85;95;0c" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }

myKeymap s = displayKey s


myMousemap :: Scan -> State -> IO State
myMousemap ScanMouse{mouseButton=1,mouseY=y} = defaultMouse1Click y
myMousemap ScanMouse{mouseButton=3,mouseY=y} = defaultMouse1Click y >=> toggleFold
myMousemap ScanMouse{mouseButton=4} = moveTreeDown 3
myMousemap ScanMouse{mouseButton=5} = moveTreeUp 3
myMousemap ScanMouse{mouseButton=0} = return
myMousemap info = displayMouse info
