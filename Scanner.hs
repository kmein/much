{-# LANGUAGE LambdaCase #-}

module Scanner
    ( scan
    ) where

import Data.Bits ((.&.), testBit)
import Data.Char (ord)
import Data.Word (Word8)
import Event
import System.IO (Handle, hGetChar, hLookAhead, hWaitForInput)


timeout :: Int
timeout = 1


scan :: Handle -> IO Event
scan h =
    hGetChar h >>= \case
        '\ESC' -> scanESC h
        c -> return $ EKey [c]



scanESC :: Handle -> IO Event
scanESC h =
    hWaitGetChar timeout h >>= \case
        Nothing -> return $ EKey "\ESC"
        Just c
            | c == '[' -> -- 05/11
                scanCS h
            | c == '\ESC' -> -- 01/11
                -- XXX M-F1 and other crazy chords may cause
                -- \ESC\ESC... on wu, so we just recurse here...
                scanESC h
            | c == 'O' -> -- 04/15
                -- XXX Non-CSI SS3
                -- XXX finalByte is maybe calles somehow else here, but it's
                -- the same range
                one h finalByte ['O','\ESC'] >>=
                return . EKey . reverse
            | otherwise ->
                return $ EKey ['\ESC',c]


scanCS :: Handle -> IO Event
scanCS h =
    hWaitLookAhead timeout h >>= \case
        Nothing -> return $ EKey "\ESC" -- TODO move this to scanESC
        Just c
            | c == 'M' -> do
                _ <- hGetChar h -- drop 'M'
                b <- hGetChar h
                x <- hGetChar h
                y <- hGetChar h
                return $ parseNormalButton b x y
            | otherwise ->
                zeroOrMore h parameterByte ['[', '\ESC'] >>=
                zeroOrMore h intermediateByte >>=
                one h finalByte >>=
                return . EKey . reverse



zeroOrMore :: Handle -> (Char -> Bool) -> [Char] -> IO [Char]
zeroOrMore h p buf =
    hWaitLookAhead timeout h >>= \case
        Nothing -> return buf
        Just c
            | p c ->
                hGetChar h {-drop c-} >> zeroOrMore h p (c:buf)
            | otherwise ->
                return buf


one :: Handle -> (Char -> Bool) -> [Char] -> IO [Char]
one h p buf =
    hWaitLookAhead timeout h >>= \case
        Nothing -> return buf -- TODO error?
        Just c
            | p c -> do
                _ <- hGetChar h -- drop c
                return (c:buf)
            | otherwise ->
                error "expected one TODO"


parameterByte :: Char -> Bool
parameterByte = between '0' '?'     -- 03/00 03/15

intermediateByte :: Char -> Bool
intermediateByte = between ' ' '/'  -- 02/00 02/15

finalByte :: Char -> Bool
finalByte = between '@' '~'         -- 04/00 07/14


between :: Ord a => a -> a -> (a -> Bool)
between lo hi = \ x -> lo <= x && x <= hi


hWaitGetChar :: Int -> Handle -> IO (Maybe Char)
hWaitGetChar t h = do
    ready <- hWaitForInput h t
    if ready
        then hGetChar h >>= return . Just
        else return Nothing


hWaitLookAhead :: Int -> Handle -> IO (Maybe Char)
hWaitLookAhead t h = do
    ready <- hWaitForInput h t
    if ready
        then hLookAhead h >>= return . Just
        else return Nothing


parseNormalButton :: Char -> Char -> Char -> Event
parseNormalButton cb cx cy = do
    let b = fromIntegral $ ord cb :: Word8
        x = ord cx - 32
        y = ord cy - 32
        button = case (b .&. 3) + (b .&. 64) of
                0 -> 1
                1 -> 2
                2 -> 3
                3 -> 0 -- release
                64 -> 4 -- wheel up
                65 -> 5 -- wheel down
                66 -> 6 -- wheel left
                67 -> 7 -- wheel right
                _ -> error "TODO proper parseNormalButton error"
    EMouse $ MouseInfo
        { mouseButton  = button
        , mouseShift   = testBit b 2
        , mouseMeta    = testBit b 3
        , mouseControl = testBit b 4
        , mouseX       = x
        , mouseY       = y
        }
