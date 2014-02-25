module Language.STL.Lex (
    lex
  , Result(..)
  , TokenStream
  , Token(..)
  , PlainTok(..)
  , Punctuation(..)
  , Keyword(..)
  , Literal(..)
) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Data.Char
import Data.Function
import Data.List
import Data.Monoid
import Data.Text (Text, pack)
import Prelude hiding (lex, until)
import Text.Trifecta
import Text.Trifecta.Delta

data Keyword = K_Void | K_Choose | K_If | K_Else deriving (Show, Eq, Enum, Bounded)

data Punctuation = P_Cons | P_Colon | P_Force | P_Equals
                 | P_LBracket | P_RBracket
                 | P_LBrace | P_RBrace
                 | P_LParen | P_RParen
                 | P_Comma | P_Dot
                 | P_Arrow
                 deriving (Show, Eq, Enum, Bounded)

data Comm = Single Text
          | Multiline Text
          deriving (Show, Eq)

data Literal = L_Str Text
             | L_Integer Integer
             deriving (Show, Eq)

data PlainTok = Ident Text
              | Comment Comm
              | Separator
              | Punct Punctuation
              | Lit Literal
              | KW Keyword deriving (Show, Eq)

data Token = Token
           { tTok :: PlainTok
           , tPos :: (Delta,Delta)
           } deriving (Show, Eq)

type TokenStream = [Token]

until :: Alternative f => f b -> f a -> f [a]
until p m = [] <$ p <|> liftA2 (:) m (until p m)

ptok :: Parser PlainTok
ptok = (Separator <$ (char '\n' <|> symbolic ';') <?> "separator")

   <|> runUnlined
           (fmap Comment (Multiline . pack
                         <$> (string "###" *> until (string "###") anyChar)
                      <|> Single . pack <$> (char '#' *> many (notChar '\n'))
                      <?> "comment"
                         )

        <|> runUnlined (fmap Punct (P_Cons <$ (symbol "::" <|> symbol "∷")
                    <|> P_Arrow <$ (symbol "->" <|> symbol "→")
                    <|> P_Force <$ symbolic '!'
                    <|> P_Colon <$ symbolic ':'
                    <|> P_Dot <$ symbolic '.'
                    <|> P_Equals <$ symbolic '='
                    <|> P_LBracket <$ symbolic '['
                    <|> P_RBracket <$ symbolic ']'
                    <|> P_LParen <$ symbolic '('
                    <|> P_RParen <$ symbolic ')'
                    <|> P_LBrace <$ symbolic '{'
                    <|> P_RBrace <$ symbolic '}'
                    <|> P_Comma <$ symbolic ','
                    <?> "punctuation"))

        <|> fmap KW (K_Choose <$ symbol "choose"
                 <|> K_Void <$ symbol "void"
                 <|> K_If <$ symbol "if"
                 <|> K_Else <$ symbol "else"
                 <?> "keyword"
                 )

        <|> token (Ident . pack <$>
                liftA2 (:)
                    (lower <|> char '_')
                    (many $ alphaNum <|> char '_')
                <?> "ident"
                  )

        <|> fmap Lit (L_Str . pack . concat <$>
                          token (between (char '"') (char '"' <?> "end of string")
                              (many (esc <|> fmap return (noneOf "\"\n"))))
                  <|> L_Integer <$> integer
                  <?> "literal"
                     ))
    where esc = do
            c <- char '\\' <?> "backslash"
            m <- anyChar
            return [c,m]

tok :: Parser Token
tok = do
    before <- position
    m <- ptok
    after <- position
    return $ Token m (before,after)

lex :: ByteString -> Result TokenStream
lex = parseByteString (some tok <* eof) mempty

--test :: IO ()
--test = -- do
--    quickCheck $ \x -> length (collapseSeps x) <= length x
--
--instance Arbitrary Token where
--    arbitrary = Token <$> arbitrary <*> pure mempty
--
--instance Arbitrary PlainTok where
--    arbitrary = oneof [genIdent, pure Separator, genPunc, genLit, genKW, genComm] where
--        genIdent = do
--            start <- elements $ '_' : ['a'..'z']
--            chars <- vectorOf 10 . elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
--            return . Ident . pack $ start : chars
--
--        genComm = Comment <$> oneof [Single <$> arbitrary, Multiline <$> arbitrary]
--
--        genPunc = Punct <$> elements [minBound..maxBound]
--
--        genLit = Lit <$> oneof [L_Str <$> arbitrary]
--
--        genKW = KW <$> elements [minBound..maxBound]
