{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Action
import Core
import State

import Blessings.String
import Control.Monad
import Data.Maybe
import Scanner
import System.Posix.Signals
import Text.Hyphenation
import Text.LineBreak
import TreeView
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Notmuch.Message as Notmuch

{- notmuch's special tags are:

  synchonised to maildir: draft flagged passed replied unread
  automatic: attachment signed encrypted
  cli default tags: unread inbox deleted spam

ref: https://notmuchmail.org/special-tags/
-}

main :: IO ()
main =
    mainWithState emptyState
      { keymap = myKeymap
      , mousemap = myMousemap
      , colorConfig = (colorConfig emptyState)
        { tagMap =
            [ ("deleted", SGR [38,5,088])
            , ("flagged", SGR [38,5,226])
            , ("draft", SGR [38,5,63])
            , ("spam", SGR [38,5,202])
            ]
        }
      , tagSymbols =
          [ ("flagged", "*")
          , ("attachment", "📎")
          ]
      }

myKeymap :: String -> State -> IO State
myKeymap "h" = closeFold
myKeymap "l" = openFold
myKeymap "\n" = toggleFold

myKeymap "g" = moveCursorUp 150
myKeymap "G" = moveCursorDown 150
myKeymap "k" = moveCursorUp 1
myKeymap "j" = moveCursorDown 1
myKeymap "\ESC[A" = moveCursorDown 1
myKeymap "\ESC[B" = moveCursorUp 1
myKeymap "\ESC[C" = moveTreeLeft 10  -- left
myKeymap "\ESC[D" = moveTreeRight 10 -- right

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
