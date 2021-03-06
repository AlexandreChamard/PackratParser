module HTTPParser (parsHTTP) where

-- HTTP        =   Header "\n" Body ;
-- Header      =   CMD " " URI " " Version "\n" { HeaderList } ;
-- CMD         =   Word ;
-- URI         =   Word ;
-- Version     =   Word ;
-- HeaderList  =   HeaderName ": " Data ;
-- HeaderName  =   Word ;
-- Data        =   [^'\n']* ;
-- Body        =   [^EOF]* ;
-- Word        =   [^' ']* ;

import Data.Char
import Data.List
import Debug.Trace

data Result t = Parsed t Derivs | NoParsed
data HTTP = HTTP Header Body
data Header = Header Cmd Uri Version -- HeaderList
data Cmd = Cmd String
data Uri = Uri String
data Version = Version String
data HeaderList = HeaderList String String HeaderList | EndOfHeaderList
data Data = Data String
data Body = Body String


data Derivs = Derivs {
    dvHTTP          :: Result HTTP,
    dvFrac          :: Result Float,
    dvNumber        :: Result Int,          -- use to pars numbers
    dvUNumber       :: Result Int,          -- use to pars usigned numbers
    dvDecimal       :: Result Int,          -- use to pars digits
    dvWord          :: Result String,
    dvQuotedString  :: Result String,       -- use to pars quoted string
    dvEscapedChar   :: Result Char,         -- use to pars escaped characters
    dvChar          :: Result Char,         -- use to pars characters
    dvEnd           :: Result String
}

parse :: String -> Derivs
parse s = d where
    d   = Derivs http f nb unb dec word qstr echr chr end
    http = pHTTP d
    f = pFrac d
    nb  = pNumber d
    unb = pUNumber d
    dec = pDecimal d
    word = pWord d
    qstr = pQuotedString d
    echr = pEscapedChar d
    chr = case s of
        (c:s') -> Parsed c (parse s')
        [] -> NoParsed
    end = Parsed s (parse "")

pFrac :: Derivs -> Result Float
pFrac d = c1 where
    c1 = case dvNumber d of
        Parsed n d' -> case dvChar d' of
            Parsed '.' d'' -> case dvUNumber d'' of
                Parsed m d'' -> Parsed (read (show n ++ "." ++ show m) :: Float) d''
                _ -> Parsed (fromIntegral n) d''
            _ -> Parsed (fromIntegral n) d'
        _ -> c2

    c2 = case dvChar d of
        Parsed '.' d' -> case dvUNumber d' of
            Parsed n d' -> Parsed (read ("0." ++ show n) :: Float) d'
            _ -> NoParsed
        _ -> NoParsed


pNumber :: Derivs -> Result Int
pNumber d = c1 where
    c1 = case dvChar d of
        Parsed '-' d' -> case pUNumber d' of
            Parsed n d'' -> Parsed (-n) d''
            _ -> NoParsed
        _ -> c2

    c2 = pUNumber d

pUNumber :: Derivs -> Result Int
pUNumber d = case pMany pDecimal d of
    Parsed [] d' -> NoParsed
    Parsed arr d' -> Parsed (foldl (\num digit -> num * 10 + digit) 0 arr) d'

pDecimal :: Derivs -> Result Int
pDecimal d = case dvChar d of
    Parsed '0' d' -> Parsed 0 d'
    Parsed '1' d' -> Parsed 1 d'
    Parsed '2' d' -> Parsed 2 d'
    Parsed '3' d' -> Parsed 3 d'
    Parsed '4' d' -> Parsed 4 d'
    Parsed '5' d' -> Parsed 5 d'
    Parsed '6' d' -> Parsed 6 d'
    Parsed '7' d' -> Parsed 7 d'
    Parsed '8' d' -> Parsed 8 d'
    Parsed '9' d' -> Parsed 9 d'
    _ -> NoParsed

pWord :: Derivs -> Result String
pWord d = case pMany (pIsNot pChar isSpace) d of
    Parsed "" d' -> NoParsed
    other -> other

pQuotedString :: Derivs -> Result String
pQuotedString d = c1 where
    c1 = case dvChar d of
        Parsed '\"' d' -> c2 d'
        _ -> NoParsed

    c2 d' = case dvEscapedChar d' of
        Parsed c d'' -> case c2 d'' of
            Parsed str d_' -> Parsed (c:str) d_'
            _ -> NoParsed
        _ -> c3 d'
        
    c3 d' = case dvChar d' of
        Parsed '\"' d_ -> case pSpaces d_ of
            Parsed _ d_ -> case dvChar d_ of
                Parsed '\"' d'' -> case pQuotedString d_ of
                    Parsed str d_' -> Parsed str d_'
                    _ -> NoParsed
                _ -> Parsed "" d_
        Parsed c d_ -> case c2 d_ of
            Parsed str d'' -> Parsed (c:str) d''
            _ -> NoParsed
        _ -> NoParsed

pEscapedChar :: Derivs -> Result Char
pEscapedChar d = case dvChar d of
    Parsed '\\' d' -> case dvChar d' of
        Parsed '\\' d'' -> Parsed '\\' d''
        Parsed '\'' d'' -> Parsed '\'' d''
        Parsed '\"' d'' -> Parsed '\"' d''
        Parsed 'a' d'' -> Parsed '\a' d''
        Parsed 'b' d'' -> Parsed '\b' d''
        Parsed 't' d'' -> Parsed '\t' d''
        Parsed 'n' d'' -> Parsed '\n' d''
        Parsed 'v' d'' -> Parsed '\v' d''
        Parsed 'f' d'' -> Parsed '\f' d''
        Parsed 'r' d'' -> Parsed '\r' d''
        _ -> NoParsed
    _ -> NoParsed


pChar :: Derivs -> Result Char
pChar d = dvChar d


pIs func verif d = case func d of
    Parsed a d' -> case verif a of
        True -> Parsed a d'
        _ -> NoParsed
    _ -> NoParsed

pIsNot func verif d = case func d of
    Parsed a d' -> case verif a of
        False -> Parsed a d'
        _ -> NoParsed
    _ -> NoParsed
    
-- pMany :: (Derivs -> Result a) -> Derivs -> Result [a]
pMany func d = case func d of
    Parsed a d' -> case pMany func d' of
        Parsed b d'' -> Parsed (a:b) d''
        _ -> Parsed (a:[]) d'
    _ -> Parsed [] d

-- pOr :: (Derivs -> Result a) -> (Derivs -> Resultat a)-> Derivs -> Result a
pOr f1 f2 d = case f1 d of
    Parsed a d' -> Parsed a d'
    _ -> f2 d

-- pAnd :: (Derivs -> Result a) -> (Derivs -> Resultat b)-> Derivs -> Result b
pAnd f1 f2 d = case f1 d of
    Parsed a d' -> case f2 d' of
        Parsed b d'' -> Parsed b d''
        _ -> NoParsed
    _ -> NoParsed

-- pSpace :: Derivs -> Resulat a
pSpace d = pIs pChar isSpace d
-- pSpaces :: Derivs -> Resulat [a]
pSpaces d = pMany pSpace d


pHTTP :: Derivs -> Result HTTP
pHTTP d = Parsed (HTTP (Header (Cmd "cmd") (Uri "uri") (Version "version")) (Body "body")) d

getInfoNeeded :: HTTP -> String -> String
getInfoNeeded (HTTP (Header (Cmd cmd) _ _) _) toGet = cmd

parsHTTP :: String -> String -> IO String
parsHTTP file toGet = do
    allLines <- lines <$> readFile file
    case pHTTP $ parse (intercalate "\n" allLines) of
        Parsed http d -> return $ getInfoNeeded http toGet
        _ -> return $ "Parsed Error"