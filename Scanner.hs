{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Scanner
    ( getKey
    ) where

import Prelude hiding ((/))
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.Char
import System.IO


-- high level interface
getKey :: IO String
getKey = do
    _ <- hLookAhead stdin -- wait for input
    ((_, raw_s), _) <- runScanner scan
    return $ map toChar raw_s


type P = C
type I = C
type F = C


data Token
    = CS [P] [I] F
    | Chr C
  deriving (Show)


type ScanLog = [C]


type ScanError = String


data ScanState = ScanState
    { _result :: Maybe Token -- TODO underscore supresses warning, rename before usage..
    , buffer :: [C]
    }


emptyScanState :: ScanState
emptyScanState = ScanState Nothing []


newtype Scanner m a = Scanner
    (ErrorT ScanError (WriterT ScanLog (StateT ScanState m)) a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadState ScanState
    , MonadError ScanError
    , MonadWriter ScanLog
    )


runScanner :: Scanner m a -> m ((Either ScanError a, ScanLog), ScanState)
runScanner (Scanner a) =
    runStateT (runWriterT (runErrorT a)) emptyScanState


-- TODO max timeout
timeout :: Int
timeout = 1


scan, scanESC, scanCS ::
    ( Monad m
    , MonadIO m
    , MonadError ScanError m
    , MonadState ScanState m
    , MonadWriter ScanLog m
    ) => m ()


scan = do
    c <- liftIO $ hGetC stdin
    tell [c]
    case () of _
                | c == 01/11 -> scanESC
                | otherwise -> return ()


scanESC = do
    mb_c <- liftIO $ hWaitGetC timeout stdin
    whenJust mb_c $ \ c -> do
        tell [c]
        case () of _
                    | c == 05/11 ->
                        -- CSI
                        scanCS

                    | c == 01/11 ->
                        -- XXX M-F1 and other crazy chords may cause
                        -- \ESC\ESC... on wu, so we just recurse here...
                        scanESC

                    | c == 04/15 ->
                        -- XXX Non-CSI SS3
                        one $ between (04/00) (07/14)

                    | otherwise -> return ()


scanCS = do
    zeroOrMore $ between (03/00) (03/15)    -- parameter bytes
    zeroOrMore $ between (02/00) (02/15)    -- intermediate bytes
    one $ between (04/00) (07/14)           -- final byte


between :: C -> C -> (C -> Bool)
between lo hi = \ x -> lo <= x && x <= hi


zeroOrMore, one ::
    ( Monad m
    , MonadIO m
    , MonadError ScanError m
    , MonadState ScanState m
    , MonadWriter ScanLog m
    ) => (C -> Bool) -> m ()


zeroOrMore p = do
    mb_c <- liftIO $ hWaitLookAheadC timeout stdin
    whenJust mb_c $ \ c ->
        when (p c) $ do
            _ <- liftIO $ hGetC stdin -- drop
            tell [c]
            modify $ \q -> q { buffer = buffer q ++ [c] }
            zeroOrMore p


one p = do
    mb_c <- liftIO $ hWaitLookAheadC timeout stdin
    whenJust mb_c $ \ c -> do
        if p c
            then do
                _ <- liftIO getChar
                tell [c]
                modify $ \q -> q { buffer = buffer q ++ [c] }
            else do
                throwError "expected one TODO"








whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mb f =
    case mb of
        Just a -> f a
        Nothing -> return ()



(/) :: Int -> Int -> C
c / r = C c r


data C = C { column :: Int, row :: Int }
    deriving (Eq)


instance Show C where
  show C{..} = 
      (padl 2 '0' $ show column) ++ "/" ++ (padl 2 '0' $ show row)
    where
      padl n c s
        | length s < n = padl n c (c : s)
        | otherwise = s


instance Ord C where
    compare (C c1 r1) (C c2 r2) =
        case compare c1 c2 of
            EQ -> compare r1 r2
            x -> x


fromChar :: Char -> C
fromChar c = let i = ord c in C ((shift i (-4)) .&. 0xf) (i .&. 0xf)

toChar :: C -> Char
toChar (C col row) = chr $ (shift col 4) .|. row


--


hGetC :: Handle -> IO C
hGetC h = hGetChar h >>= return . fromChar


hWaitGetChar :: Int -> Handle -> IO (Maybe Char)
hWaitGetChar t h = do
    ready <- hWaitForInput h t
    if ready
        then hGetChar h >>= return . Just
        else return Nothing


hWaitGetC :: Int -> Handle -> IO (Maybe C)
hWaitGetC t h = do
    mb_ch <- hWaitGetChar t h
    case mb_ch of
        Nothing -> return Nothing
        Just ch -> return $ Just $ fromChar $ ch


hWaitLookAheadC :: Int -> Handle -> IO (Maybe C)
hWaitLookAheadC t h = do
    ready <- hWaitForInput h t
    if ready
        then hLookAhead h >>= return . Just . fromChar
        else return Nothing
