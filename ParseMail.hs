{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseMail (readMail) where

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Network.Email.Header.Parser as P
import qualified Network.Email.Header.Types as H
import qualified Network.Mail.Mime as M
import Codec.MIME.Parse
import Codec.MIME.Type
import Control.Applicative
import Data.Monoid
import Data.Char



-- TODO eventually we want our completely own Address, i.e. w/o M.Address
data Address = Mailbox M.Address | Group T.Text [M.Address]
  deriving (Show)



readMail :: FilePath -> IO M.Mail
readMail p =
    fromMIMEValue . parseMIMEMessage <$> T.readFile p


fromMIMEValue :: MIMEValue -> M.Mail
fromMIMEValue val =
    let m = foldr f (M.emptyMail $ M.Address Nothing "anonymous@localhost")
              $ fromMIMEParams
              $ mime_val_headers val
    in m { M.mailParts = [[part]] }
  where

    part =
        case mime_val_content val of
            Single content ->
                M.Part
                    -- TODO actually check if we're utf-8 or ascii(?)
                    { M.partType = "text/plain; charset=utf-8"
                    , M.partEncoding = M.QuotedPrintableText
                    , M.partFilename = Nothing
                    , M.partHeaders = []
                    , M.partContent = LT.encodeUtf8 $ LT.fromStrict content
                    }
            _ -> error ("meh: " ++ show val)

    f :: H.Header -> M.Mail -> M.Mail
    f (k, v) m = case k of
        "from" ->
            m { M.mailFrom =
                    (\case
                        Mailbox a -> a
                        Group _ _ ->
                            error "cannot use group in from header"
                    ) $
                    either error id $
                    parseAddress $
                    LBS.toStrict v
                }
        "to" ->
            m { M.mailTo =
                  mconcat $
                      map (\case
                            Mailbox a -> [a]
                            Group _ as -> as
                        ) $
                      either error id $
                      parseAddresses $
                      LBS.toStrict v
              }
        "cc" ->
            m { M.mailCc =
                  mconcat $
                      map (\case
                            Mailbox a -> [a]
                            Group _ as -> as
                        ) $
                      either error id $
                      parseAddresses $
                      LBS.toStrict v
              }
        "bcc" ->
            m { M.mailBcc =
                  mconcat $
                      map (\case
                            Mailbox a -> [a]
                            Group _ as -> as
                        ) $
                      either error id $
                      parseAddresses $
                      LBS.toStrict v
              }
        _ ->
            m { M.mailHeaders =
                  ( CI.original k
                  , either
                      (const "I am made of stupid")
                      LT.toStrict
                      (LT.decodeUtf8' v)
                  ) :
                  M.mailHeaders m
              }


parseAddress :: BS.ByteString -> Either String Address
parseAddress =
    A8.parseOnly (P.cfws *> address <* A8.endOfInput)


parseAddresses :: BS.ByteString -> Either String [Address]
parseAddresses =
    A8.parseOnly (P.cfws *> address `A8.sepBy1` A8.char ',' <* A8.endOfInput)


fromMIMEParams :: [MIMEParam] -> H.Headers
fromMIMEParams =
    map $ \(MIMEParam k v) ->
        (CI.mk $ T.encodeUtf8 k, LT.encodeUtf8 $ LT.fromStrict v)


-- TODO we should probably use email-header


-- address     =  mailbox                      ; one addressee
--             /  group                        ; named list
address :: A8.Parser Address
address =
    (A8.<?> "address") $
    Mailbox <$> mailbox
    <|>
    group


-- group       =  phrase ":" [#mailbox] ";"
group :: A8.Parser Address
group =
    (A8.<?> "group") $
    Group
        <$> T.intercalate "," <$> phrase
        <* A8.char ':'
        <*> mailbox `A8.sepBy` A8.many1 (A8.char ',')
        <* A8.char ';'


-- mailbox     =  addr-spec                    ; simple address
--             /  phrase route-addr            ; name & addr-spec
mailbox :: A8.Parser M.Address
mailbox =
    (A8.<?> "mailbox") $
    M.Address Nothing <$> addrSpec <|>
    M.Address . Just . T.intercalate " " <$> A8.option [] phrase <*> routeAddr


-- route-addr  =  "<" [route] addr-spec ">"
routeAddr :: A8.Parser T.Text
routeAddr =
    (A8.<?> "routeAddr") $
    P.cfws *>
    A8.char '<' *>
    -- TODO A8.option [] route <*>
    addrSpec <*
    A8.char '>'


---- route       =  1#("@" domain) ":"           ; path-relative
--route :: A8.Parser [T.Text]
--route =
--    (A8.<?> "route") $
--    A8.many1 (A8.char '@' *> domain) <* A8.char ':'


-- addr-spec   =  local-part "@" domain        ; global address
addrSpec :: A8.Parser T.Text
addrSpec =
    (A8.<?> "addrSpec") $ do
        a <- localPart
        b <- T.singleton <$> A8.char '@'
        c <- domain
        return $ a <> b <> c

-- local-part  =  word *("." word)             ; uninterpreted
--                                             ; case-preserved
localPart :: A8.Parser T.Text
localPart =
    (A8.<?> "localPart") $
     T.intercalate "." <$> (word `A8.sepBy1` A8.char '.')


-- domain      =  sub-domain *("." sub-domain)
domain :: A8.Parser T.Text
domain =
    (A8.<?> "domain") $
    T.intercalate "." <$> (subDomain `A8.sepBy1` A8.char '.')

-- sub-domain  =  domain-ref / domain-literal
subDomain :: A8.Parser T.Text
subDomain =
    (A8.<?> "subDomain") $
    domainRef <|> domainLiteral

-- domain-ref  =  atom                         ; symbolic reference
domainRef :: A8.Parser T.Text
domainRef =
    (A8.<?> "domainRef") $
    atom


-- atom        =  1*<any CHAR except specials, SPACE and CTLs>
atom :: A8.Parser T.Text
atom =
    (A8.<?> "atom") $
    P.cfws *>
    (T.pack <$> A8.many1 (A8.satisfy $ A8.notInClass atomClass))


-- domain-literal =  "[" *(dtext / quoted-pair) "]"
domainLiteral :: A8.Parser T.Text
domainLiteral =
    (A8.<?> "domainLiteral") $
    T.pack <$>
        (A8.char '[' *> A8.many' (dtext <|> quotedPair) <* A8.char ']')


-- dtext       =  <any CHAR excluding "[",     ; => may be folded
--                 "]", "\" & CR, & including
--                 linear-white-space>
dtext :: A8.Parser Char
dtext =
    (A8.<?> "dtext") $
    A8.satisfy (A8.notInClass "[]\\\CR")


-- phrase      =  1*word
phrase :: A8.Parser [T.Text]
phrase =
    (A8.<?> "phrase") $
    A8.many1 word


-- qtext       =  <any CHAR excepting <">,     ; => may be folded
--                 "\" & CR, and including
--                 linear-white-space>
qtext :: A8.Parser Char
qtext =
    (A8.<?> "qtext") $
    A8.satisfy (A8.notInClass "\"\\\CR")


-- quoted-pair =  "\" CHAR
quotedPair :: A8.Parser Char
quotedPair =
    (A8.<?> "quotedPair") $
    A8.char '\\' *> A8.anyChar


-- quoted-string = <"> *(qtext/quoted-pair) <">; Regular qtext or
--                                             ;   quoted chars.
quotedString :: A8.Parser T.Text
quotedString =
    (A8.<?> "quotedString") $
    T.pack <$> (A8.char '"' *> A8.many' (qtext <|> quotedPair) <* A8.char '"')


-- word        =  atom / quoted-string
word :: A8.Parser T.Text
word =
    (A8.<?> "word") $
    atom <|> quotedString


atomClass :: [Char]
atomClass = specialClass ++ spaceClass ++ ctlClass


specialClass :: [Char]
specialClass = "()<>@,;:\\\".[]"


spaceClass :: [Char]
spaceClass = " "


ctlClass :: [Char]
ctlClass = map chr $ [0..31] ++ [127]
