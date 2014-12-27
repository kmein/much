{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Trammel where

import Control.Applicative
import Data.List
import Data.String
import Data.Monoid
import Data.Ix (inRange)

type Ps = Int
type Pm = [Ps]

data Trammel a
    = Plain a
    | SGR Pm (Trammel a)
    | Append (Trammel a) (Trammel a)
    | Empty
  deriving (Eq, Show)


instance Monoid (Trammel a) where
    mappend = Append
    mempty = Empty


instance IsString a => IsString (Trammel a) where
    fromString = Plain . fromString


class IsPm a where
    toPm :: a -> Pm
    fromPm :: Pm -> Maybe a


data FColor = ECMA48FColor Ps   -- ECMA-48 / ISO 6429 / ANSI X3.64
            | Xterm256FColor Ps
            | ISO8613_3FColor Ps Ps Ps
  deriving (Eq, Show)

instance IsPm FColor where
    toPm (ECMA48FColor i) = [i]
    toPm (Xterm256FColor i) = [38,5,i]
    toPm (ISO8613_3FColor r g b) = [38,2,r,g,b]
    fromPm = fromSGRPm SGRPm
               { def8Ps = 39
               , extPs = 38
               , lo8Ps = 30
               , hi8Ps = 37
               , makeECMA48Color = ECMA48FColor
               , makeXterm256Color = Xterm256FColor
               , makeISO8613_3Color = ISO8613_3FColor
               }
           . filterPm sgrBColor


data BColor = ECMA48BColor Ps
            | Xterm256BColor Ps
            | ISO8613_3BColor Ps Ps Ps
  deriving (Eq, Show)


instance IsPm BColor where
    toPm (ECMA48BColor i) = [i]
    toPm (Xterm256BColor i) = [48,5,i]
    toPm (ISO8613_3BColor r g b) = [48,2,r,g,b]
    fromPm = fromSGRPm SGRPm
                 { def8Ps = 49
                 , extPs = 48
                 , lo8Ps = 40
                 , hi8Ps = 47
                 , makeECMA48Color = ECMA48BColor
                 , makeXterm256Color = Xterm256BColor
                 , makeISO8613_3Color = ISO8613_3BColor
               }
           . filterPm sgrFColor


data Bold = Bold | NoBold
  deriving (Eq, Show)

instance IsPm Bold where
    toPm Bold = [1]
    toPm NoBold = [22]
    fromPm = rec . filterPm sgrColor
      where
        rec xs = case filter (`elem`[1,22]) xs of
            [] -> Nothing
            xs' -> case last xs' of
                1 -> Just Bold
                22 -> Just NoBold
                _ -> error "filter broken in fromPm :: Pm -> Maybe Bold"


data Underline = Underline | NoUnderline
  deriving (Eq, Show)

instance IsPm Underline where
    toPm Underline = [4]
    toPm NoUnderline = [24]
    fromPm = rec . filterPm sgrColor
      where
        rec xs = case filter (`elem`[4,24]) xs of
            [] -> Nothing
            xs' -> case last xs' of
                1 -> Just Underline
                22 -> Just NoUnderline
                _ -> error "filter broken in fromPm :: Pm -> Maybe Underline"


data SGRPm c = SGRPm
    { def8Ps :: Ps
    , extPs :: Ps
    , lo8Ps :: Ps
    , hi8Ps :: Ps
    , makeECMA48Color :: Ps -> c
    , makeXterm256Color :: Ps -> c
    , makeISO8613_3Color :: Ps -> Ps -> Ps -> c
    }


fromSGRPm :: IsPm c => SGRPm c -> Pm -> Maybe c
fromSGRPm SGRPm{..} = rec Nothing
  where
    rec mb_c (x:xs)
        | x == extPs = case xs of
            (2:r:g:b:xs') -> rec (Just $ makeISO8613_3Color r g b) xs'
            (5:i:xs')     -> rec (Just $ makeXterm256Color i) xs'
            _             -> rec mb_c xs
        | x == def8Ps = rec (Just $ makeECMA48Color def8Ps) xs
        | inRange (lo8Ps, hi8Ps) x = rec (Just $ makeECMA48Color x) xs
        | otherwise = rec mb_c xs
    rec mb_c _ = mb_c


-- filterPm is used to preprocess Pm before searching with fromPm in
-- order to remove (longer) sequences that could contain subsequences
-- that look like the (shorter) sequences we're searching.
-- E.g. we could find [1] (bold) in any extended color sequence.
-- TODO Can we combine this whole from*Pm with Scanner?
filterPm :: (Pm -> Maybe Int) -> Pm -> Pm
filterPm f = rec []
  where
    rec ys xs@(xhead:xtail) = maybe (rec (ys ++ [xhead]) xtail)
                                    (rec ys . flip drop xs)
                                    (f xs)
    rec ys _ = ys

sgrColor, sgrFColor, sgrBColor :: Pm -> Maybe Int

sgrColor xs = sgrFColor xs <|> sgrBColor xs

sgrFColor (38:5:_) = Just 3
sgrFColor (38:2:_) = Just 5
sgrFColor _ = Nothing

sgrBColor (48:5:_) = Just 3
sgrBColor (48:2:_) = Just 5
sgrBColor _ = Nothing


type RenderState = [(FColor, BColor, Bold, Underline)]


emptyRenderState :: RenderState
emptyRenderState = [(ECMA48FColor 39, ECMA48BColor 49, NoBold, NoUnderline)]

renderString :: RenderState -> Trammel String -> String -> String

renderString _ (Plain s) y = s ++ y

-- TODO merge successive sequences: \ESC[32m\ESC[1m -> \ESC[31;1m
renderString rs@((fc, bc, b, u):_) (SGR c t) y =
    renderSGR bra ++ renderString rs' t (renderSGR ket ++ y)
  where
    fc' = maybe fc id $ fromPm c
    bc' = maybe bc id $ fromPm c
    b'  = maybe  b id $ fromPm c
    u'  = maybe  u id $ fromPm c
    rs' = (fc', bc', b', u') : rs
    bra = braket >>= fst
    ket = braket >>= snd
    braket =
        (if fc' /= fc then (toPm fc', toPm fc) else ([],[])) :
        (if bc' /= bc then (toPm bc', toPm bc) else ([],[])) :
        (if b'  /=  b then (toPm  b', toPm  b) else ([],[])) :
        (if u'  /=  u then (toPm  u', toPm  u) else ([],[])) : []

renderString _ (SGR _ _) _ =
    error "renderString called w/o proper initial state"
    -- where a proper initial state is s.th. like emptyRenderState

renderString r (Append t1 t2) y =
    renderString r t1 $ renderString r t2 y

renderString _ Empty y = y


len :: Trammel String -> Int
len (Plain x) = length x
len (SGR _ x) = len x
len (Append t1 t2) = len t1 + len t2
len Empty = 0


pp :: Trammel String -> String
pp t = renderString emptyRenderState t ""


renderSGR :: Pm -> String
renderSGR [] = []
renderSGR xs = ("\ESC["++) . (++"m") . intercalate ";" $ map show xs
