{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Blessings.String
import Data.Default
import Data.Maybe
import Much.Action
import Much.Core
import Much.State
import Much.TreeView
import Scanner
import Text.Hyphenation
import Text.LineBreak
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Much.API
import qualified Notmuch.Message as Notmuch

main :: IO ()
main =
    mainWithState def
      { apiConfig = def
          { Much.API.socketPath = "/home/tv/tmp/much/warp.sock"
          }
      , keymap = myKeymap
      , mousemap = myMousemap
      }

myKeymap :: String -> State -> IO State

myKeymap "a" = toggleTagAtCursor "inbox"
myKeymap "s" = toggleTagAtCursor "unread"
myKeymap "&" = toggleTagAtCursor "killed"
myKeymap "*" = toggleTagAtCursor "star"
myKeymap "k" = moveCursorUp 1
myKeymap "j" = moveCursorDown 1
myKeymap "K" = moveTreeDown 1
myKeymap "J" = moveTreeUp 1
myKeymap "\ESC[A" = moveCursorUp 1
myKeymap "\ESC[B" = moveCursorDown 1
myKeymap "\ESC[a" = moveTreeDown 1
myKeymap "\ESC[b" = moveTreeUp 1
myKeymap "\ESC[c" = moveTreeLeft 1  -- S-Right
myKeymap "\ESC[d" = moveTreeRight 1 -- S-Left
myKeymap "\ESC[5~" = \q -> moveTreeDown (screenHeight q `div` 2) q  -- PgUp
myKeymap "\ESC[6~" = \q -> moveTreeUp (screenHeight q `div` 2) q    -- PgDn
myKeymap "\n" = toggleFold
myKeymap "\ESC[Z" = moveCursorUpToPrevUnread -- S-Tab
myKeymap "\t" = moveCursorDownToNextUnread
myKeymap "\DEL" = moveToParent  -- backspace

-- TODO wrap/unwrap to separate module
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

    hy s =
        breakStringLn bf s
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
                  maybe Nothing (Just . Notmuch.messageFilename) $
                  getMessage $
                  Z.label cursor
              }

-- TODO Stuff Vim sends after exit (also there is more...)
myKeymap "\ESC[2;2R" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }
myKeymap "\ESC[>85;95;0c" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }

myKeymap s = displayKey s


myMousemap :: Scan -> State -> IO State
myMousemap ScanMouse{mouseButton=1,mouseY=y} = defaultMouse1Click y
myMousemap ScanMouse{mouseButton=3,mouseY=y} = \q -> defaultMouse1Click y q >>= toggleFold
myMousemap ScanMouse{mouseButton=4} = moveTreeDown 3
myMousemap ScanMouse{mouseButton=5} = moveTreeUp 3
myMousemap ScanMouse{mouseButton=0} = return
myMousemap info = displayMouse info
