import Control.Applicative
import Data.Char
import Data.HashSet (fromList)
import Data.Text (Text, pack)
import Prelude hiding (lex, until)
import System.IO
import Text.Parser.Token.Highlight hiding (Comment)
import Text.Trifecta hiding (symbol, symbolic, token)

data Keyword = K_Choose | K_If | K_Else deriving Show

data Punctuation = P_Colon | P_Force | P_Equals
                 | P_Lbracket | P_Rbracket
                 | P_Lbrace | P_Rbrace
                 | P_Lparen | P_Rparen
                 | P_Comma
                 | P_Arrow
                 deriving Show

data Comm = Single Text
          | Multiline Text
          deriving Show

data Literal = L_Str Text
             deriving Show

data Tok = Ident Text
         | Comment Comm
         | Separator
         | Punct Punctuation
         | Lit Literal
         | KW Keyword deriving Show

symbolic c = token (char c)

symbol c = token (string c)

realSpace = many $ satisfy (\x -> x /= '\n' && isSpace x)

token m = m <* realSpace

until p m = [] <$ p <|> liftA2 (:) m (until p m)

lex = Separator <$ (char '\n' <|> char ';' <?> "separator") <* realSpace

  <|> token (Ident . pack <$>
          liftA2 (:)
              (lower <|> char '_')
              (many $ alphaNum <|> char '_' <|> char '.')
          <?> "ident"
            )

  <|> fmap Comment (Multiline . pack
                        <$> (string "###" *> (until (string "###") anyChar))
                <|> Single . pack <$> (char '#' *> many (notChar '\n'))
                   )

  <|> fmap Punct (P_Colon <$ symbolic ':'
              <|> P_Force <$ symbolic '!'
              <|> P_Equals <$ symbolic '='
              <|> P_Lbracket <$ symbolic '['
              <|> P_Rbracket <$ symbolic ']'
              <|> P_Lparen <$ symbolic '('
              <|> P_Rparen <$ symbolic ')'
              <|> P_Lbrace <$ symbolic '{'
              <|> P_Rbrace <$ symbolic '}'
              <|> P_Comma <$ symbolic ','
              <|> P_Arrow <$ symbol "->"
              <?> "punctuation")

  <|> fmap KW (K_Choose <$ symbol "choose"
           <|> K_If <$ symbol "if"
           <|> K_Else <$ symbol "else"
           <?> "keyword"
           )

  <|> fmap Lit (L_Str . pack . concat <$>
                    token (between (char '"') (char '"')
                        (many (esc <|> fmap return (notChar '"'))))
            <?> "literal"
               )
    where esc = do
            c <- char '\\'
            m <- anyChar
            return [c,m]

main = withFile "main.stl" ReadMode $ \h -> parseTest (some lex <* eof) =<< hGetContents h
