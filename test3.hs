{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


import Data.Default
import Graphics.Vty

--import Data.List

--import Language.Haskell.TH.Ppr (bytesToString)
--import Data.Aeson
--import Data.List.Split
--import Data.Attoparsec.ByteString hiding (string)
import Data.Maybe
--import Data.Monoid
--import Data.String
--import Data.Traversable
import Data.Tree
import qualified Data.Tree.Zipper as Z
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
--import qualified Data.Text.IO as T
--import Data.Version (Version(..), parseVersion)
--import System.Process
--import System.IO
--import qualified Data.Map as M

import Notmuch
import Notmuch.Message
import Notmuch.SearchResult
--import Safe

import Control.Exception

import TreeView

import TreeSearch


data State = State
    { vty :: Vty
    , cursor :: Z.TreePos Z.Full TreeView
    , xoffset :: Int
    , yoffset :: Int
    }


toggleTag :: T.Text -> Message -> IO ()
toggleTag tag m = do
    _ <- if tag `elem` messageTags m
        then
            unsetTag tagString (unMessageID $ messageId m)
        else
            setTag tagString (unMessageID $ messageId m)
    return ()
  where
    tagString = T.unpack tag


tagMessage :: T.Text -> Message -> IO LBS.ByteString
tagMessage tag m =
  setTag (T.unpack tag) (unMessageID $ messageId m)


untagMessage :: T.Text -> Message -> IO LBS.ByteString
untagMessage tag m =
  unsetTag (T.unpack tag) (unMessageID $ messageId m)


main :: IO ()
main =
    main' "tag:inbox AND NOT tag:killed"

main' :: String -> IO ()
main' query =
    bracket (mkVty def) finit run
  where

    finit vty = do
        shutdown vty

    run vty0 = do
        --XXX show a single thread
        --t_ <- getThread "0000000000000862"
        --let v = fromMessageTree t_
        --let c = findMessage (MessageID "87egtmvj0n.fsf@write-only.cryp.to") v
        --rec vty 0 c v

        Right r_ <- search query
        rec State
            { vty = vty0
            , cursor = Z.fromTree $ fromSearchResults query r_
            , xoffset = 0
            , yoffset = 0
            }

    rec :: State -> IO ()
    rec q@State{..} = do
        let
            img =
                --string def (show i) <->
                --string def (maybe "Nothing" describe (focusPrev v cursor)) <->
                --string def (maybe "Nothing" describe cursor) <->
                --string def (maybe "Nothing" describe (focusNext v cursor)) <->
                --string def (maybe "Nothing" describe (focusPrev v cursor)) <->
                --string def (describe $ Z.label cursor) <->
                --string def (maybe "Nothing" describe (focusNext v cursor)) <->
                treeImage (Just $ Z.label cursor) (Z.toTree cursor)
                --renderTree q
            pic = picForImage $ translate xoffset yoffset img
            --v = Z.root cursor
        update vty pic
        nextEvent vty >>= \e -> case e of
            EvKey (KChar 'k') [] ->
                rec q { cursor = fromMaybe (Z.root cursor) $ findPrev cursor }
            EvKey (KChar 'j') [] ->
                rec q { cursor = fromMaybe (Z.root cursor) $ findNext cursor }
            EvKey KEnter [] ->
                onEnter cursor

            EvKey (KChar 'H') [] -> rec q { xoffset = xoffset - 1 }
            EvKey (KChar 'L') [] -> rec q { xoffset = xoffset + 1 }
            EvKey (KChar 'J') [] -> rec q { yoffset = yoffset - 1 }
            EvKey (KChar 'K') [] -> rec q { yoffset = yoffset + 1 }

            EvResize _w _h ->
                rec q

            _ -> do
                error $ "Last event was: " ++ show e
      where
        onEnter c_ = case Z.label c_ of
            TVMessage m -> do
                toggleTag "open" m

                let loc = cursor
                    Just sr = findParent isTVSearchResult loc
                    --Just sr0 = Z.firstChild sr -- TODO can there be only one (thread per sr)?
                    TVSearchResult the_sr = Z.label sr
                    ThreadID tid = searchThread the_sr

                t_ <- return . (:[]) . fromMessageTree =<< getThread tid

                let loc' = Z.modifyTree (\(Node l _) -> Node l t_) sr
                rec q { cursor = select (==Z.label cursor) loc' }

            TVSearchResult sr -> do
                --let Just loc = findTree (==c_) $ Z.fromTree v
                let loc = cursor

                let open = not $ null $ subForest $ Z.tree loc
                let ThreadID tid = searchThread sr

                t_ <-
                    if open
                        then return []
                        else return . (:[]) . fromMessageTree =<< getThread tid

                let loc' = Z.modifyTree (\(Node l _) -> Node l t_) loc
                rec q { cursor = select (==Z.label cursor) loc' }

            _ ->
                -- TODO make some noise
                rec q


select :: (a -> Bool) -> Z.TreePos Z.Full a -> Z.TreePos Z.Full a
select p loc = fromMaybe (error "cannot select") $ findTree p $ Z.root loc


treeImage :: Maybe TreeView -> Tree TreeView -> Image
treeImage c (Node n ns) =
    treeViewImage (c == Just n) n <->
    translateX 2 (vertCat $ map (treeImage c) ns)
