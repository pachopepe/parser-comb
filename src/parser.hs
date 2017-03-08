{-# LANGUAGE GADTs #-}

{- Most of the code was taken or adapted from Languages and Compilers,
   Swierstra, and Jeuring, 2011.
   The Json data type was taken from Real World Haskell Applications,
   O'Sullivan, Stewart, and Goerzen.
-}

import Data.Char
import Control.Applicative

newtype Parser s a = Parser { runParse :: ([s] -> [(a,[s])]) }

get :: Parser s s
get = Parser get'
  where get'     [] = [] -- No hay elementos en la entrada
        get' (x:xs) = [(x,xs)]

put :: a -> Parser s a
put v = Parser (\xs -> [(v,xs)])

succeed :: a -> Parser s a
succeed = put

failp :: Parser s a
failp = Parser (\xs -> [])

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser satisfy'
  where satisfy' (x:xs) | p x = [(x,xs)]
        satisfy' _ = []

symbol :: Eq s => s -> Parser s s -- el siguiente elemento en
symbol x = satisfy (== x)    -- la entrada es x

digit :: Parser Char Char    -- dígito
digit = satisfy isDigit

space :: Parser Char Char    -- ' ','\t','\n','\r','\f','\v'
space = satisfy isSpace

letter :: Parser Char Char   -- letra
letter = satisfy isLetter

alphaNum :: Parser Char Char   -- alfanumérico
alphaNum = satisfy isAlphaNum

epsilon :: Parser s ()       -- secuencia vacía
epsilon = succeed ()

end :: Parser s ()           -- Fin de la entrada
end = Parser end'
  where end' [] = [((),[])]
        end' _  = []

instance Functor (Parser s) where
  fmap f p = Parser (\ e -> [ (f ai,si) | (ai,si) <- runParse p e ])

instance Applicative (Parser s) where
  pure = put
  p <*> q = Parser (\e -> [ (f x,s') | (f,s) <- runParse p e
                                     , (x,s') <- runParse q s ])

instance Alternative (Parser s) where
  empty = failp
  p <|> q = Parser (\e -> runParse p e ++ runParse q e)

digits :: Parser Char String   -- secuencia de dígitos
digits = some digit

identifier :: Parser Char String  -- identificador
identifier = (:) <$> letter <*> many alphaNum

spaces :: Parser Char () -- espacios
spaces = () <$ many space

between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between open close p = open *> p <* close

accept :: Eq s => [s] -> Parser s [s]
accept [] = succeed []
accept (x:xs) = (:) <$> symbol x <*> accept xs

sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)
            <|> succeed []

option :: a -> Parser s a -> Parser s a
option x p = p <|> succeed x

string :: Parser Char String
string = between (symbol '"') (symbol '"')
                 (many ('"' <$ accept "\\\"" <|> satisfy (/= '"')))

number :: Parser Char Double  -- numero
number = read <$> pnum
   where neg  = "-" <$ symbol '-'
         sig  = "+" <$ symbol '+' <|> "-" <$ symbol '-'
         frac = (:) <$> symbol '.' <*> digits
         exp  = con4 <$> letE <*> sig <*> digits <*> put ""
         letE = "e" <$ (symbol 'e' <|> symbol 'E')
         nat  = accept "0"
              <|> (:) <$> satisfy (\d -> '0' < d && d <= '9') <*> many digit
         pnum = con4 <$> option "" neg <*> nat <*> option "" frac
                <*> option "" exp
         con4 x1 x2 x3 x4 = concat [x1,x2,x3,x4]

data JValue where
     JBool :: Bool -> JValue
     JNull :: JValue
     JString :: String -> JValue
     JNumber :: Double -> JValue
     JObject :: [(String, JValue)] -> JValue
     JArray  :: [JValue] -> JValue
     deriving (Eq, Ord, Show)

jbool :: Parser Char JValue
jbool = JBool True <$ accept "true"
      <|> JBool False <$ accept "false"

jnull :: Parser Char JValue
jnull = JNull <$ accept "null"

jstring :: Parser Char JValue
jstring = JString <$> string

jnumber :: Parser Char JValue
jnumber = JNumber <$> number

jarray :: Parser Char JValue
jarray = JArray <$> between (symbol '[') (symbol ']')
                    (sepBy json (symbol ','))

jobject :: Parser Char JValue
jobject = JObject <$> between (symbol '{') (symbol '}') jprops

jprops :: Parser Char [(String,JValue)]
jprops = sepBy jprop (symbol ',')

jprop :: Parser Char (String,JValue)
jprop = (,) <$ spaces <*> string
            <* spaces <* (symbol ':') <*> json

json :: Parser Char JValue
json = spaces *> (jbool <|> jnull <|> jstring <|> jnumber
                  <|> jobject <|> jarray) <* spaces

bind :: (a -> Parser s b) -> Parser s a -> Parser s b
bind f p = Parser (\e -> [ r | (x,s) <- runParse p e, r <- runParse (f x) s])

instance Monad (Parser s) where
  return = put
  p >>= f = bind f p

anbncn :: Parser Char [Char]
anbncn = do
    an <- many (symbol 'a')
    let n = length an
    bn <- accept (replicate n 'b')
    cn <- accept (replicate n 'c')
    end
    return (an ++ bn ++ cn)

ex1 = "{ \"nombre\" : \"Jose\", \"edad\" : 23, \"casado\" : true, \"intereses\" : [\"Programacion funcional\",\"Calculo lambda\"] }"

ex2 = "aaabbbccc" -- Ok

ex3 = "aaabbbbccc" -- a3b4c3 fails

ex4 = "aaaabbbccc" -- a4b3c3 fails

ex5 = "aaabbbcccc" -- a3b3c4 fails

