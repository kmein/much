{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Much.Action
import Much.Core (mainWithState, notmuchSearch)
import Much.State (State(..), ColorConfig(..))
import Much.TreeView (TreeView(TVMessagePart), treeViewId, getMessage)
import Notmuch (notmuchShowPart)
import Notmuch.Message

import Blessings.String (Blessings(..))
import Control.Monad ((>=>), unless)
import Data.Default (Default(..))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Scanner (Scan(ScanMouse), mouseButton, mouseY)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Posix.Signals (raiseSignal, sigINT)
import System.Process (callProcess, spawnCommand)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (writeFile)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (writeFile)
import qualified Data.Tree.Zipper as Z (label)

{- notmuch's special tags are:

  synchonised to maildir: draft flagged passed replied unread
  automatic: attachment signed encrypted
  cli default tags: unread inbox deleted spam

  ref: https://notmuchmail.org/special-tags/
-}

main :: IO ()
main =
    mainWithState def
      { keymap = myKeymap
      , mousemap = myMousemap
      , colorConfig = def
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
        , tagMap =
            [ ("deleted", SGR [38,5,088])
            , ("flagged", SGR [38,5,226])
            , ("draft", SGR [38,5,63])
            , ("spam", SGR [38,5,202])
            ]
        }
      , tagSymbols =
          [ ("flagged", "🔖")
          , ("attachment", "📎")
          , ("signed", "🔒")
          ]
      , query = "tag:inbox"
      }

showCurrentMessagePart :: State -> IO (Maybe (Message, MessagePart))
showCurrentMessagePart q =
  case Z.label (cursor q) of
    TVMessagePart message part -> do
      let m_id = unMessageID (messageId message)
      partResult <- notmuchShowPart m_id (partID part)
      case partResult of
        Right part' -> return $ Just (message, part')
        Left _ -> return Nothing
    _ -> return Nothing

currentAttachmentPath :: State -> Message -> MessagePart -> FilePath
currentAttachmentPath q message part =
  attachmentDirectory q </> attachmentFileName q message part

saveAttachment :: State -> IO State
saveAttachment q =
  showCurrentMessagePart q >>= \case
    Nothing -> return q { flashMessage = "cursor not on attachment" }
    Just (message, part) -> do
      let destination = currentAttachmentPath q message part
      alreadyDownloaded <- doesFileExist destination
      if attachmentOverwrite q || not alreadyDownloaded
         then case partContent part of
                ContentText text ->
                  T.writeFile destination text $>
                    q { flashMessage = Plain destination }
                ContentRaw raw _ ->
                  LBS8.writeFile destination raw $>
                    q { flashMessage = Plain destination }
                _ -> return q { flashMessage = "this part cannot be saved" }
         else return q { flashMessage = "not overwriting attachment" }

openAttachment :: State -> IO State
openAttachment q =
  showCurrentMessagePart q >>= \case
    Nothing -> return q { flashMessage = "cursor not on attachment" }
    Just (message, part) -> do
      let destination = currentAttachmentPath q message part
      alreadyDownloaded <- doesFileExist destination
      unless alreadyDownloaded $ saveAttachment q $> ()
      callProcess "xdg-open" [destination] $> q

reply :: State -> IO State
reply q = spawnCommand "i3-sensible-terminal -e $EDITOR -c 'read !mail-reply'" $> q

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
myKeymap "S" = saveAttachment
myKeymap "o" = openAttachment

myKeymap "q" = \q -> raiseSignal sigINT $> q

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

-- <F1>
myKeymap "\ESC[11~" = \q@State{..} ->
    return q { flashMessage = Plain $ show $ treeViewId $ Z.label cursor }

-- <F2>
myKeymap "\ESC[12~" = \q@State{..} ->
    return q { flashMessage =
                  Plain $
                  show $
                  fmap messageFilename $
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
