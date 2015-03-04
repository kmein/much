{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--module Main (main) where

import           Control.Applicative
import qualified Data.Text.Lazy as LT
import qualified Data.Tree as Tree
import           MBox (MBox)
import qualified MBox
import           System.Environment
import           System.Process


notmuchShowMBox :: String -> IO MBox
notmuchShowMBox searchTerm =
    MBox.parseMBox . LT.pack <$> readProcess
        "notmuch"
        [ "show"
        , "--format=mbox"
        , "--entire-thread=true"
        , searchTerm
        ]
        ""


renderMessage :: MBox.Message -> String
renderMessage msg =
    LT.unpack (MBox.getMessageId $ MBox.headers msg)
    ++ " "
    ++ drop (length ("From " :: String)) (LT.unpack $ MBox.fromLine msg)


main :: IO ()
main = do
    -- load-env hack
    maybe (return ()) (setEnv "HOME") =<< lookupEnv "OLDHOME"

    notmuchShowMBox "tree1" >>=
        putStrLn . Tree.drawTree .
        Tree.Node "subject:tree-test" .
        map (fmap renderMessage) .
        MBox.toForest
