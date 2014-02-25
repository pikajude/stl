module Language.STL.Parse where

import Control.Applicative hiding (many, (<|>))
import Data.Functor.Identity
import Data.Text (Text)
import qualified Language.STL.Lex as L
import Text.Parsec
import Text.Parsec.Pos
import Text.Trifecta.Delta (Delta(..))

type AST = [Dec]

data Dec = TypeDec Ident Type deriving Show

data Type = TyVar Ident | TyApp Type Type | TyVoid deriving Show

data Ident = Ident Text deriving Show

type TokenParser = ParsecT L.TokenStream [L.Token] Identity

parse :: L.TokenStream -> Either ParseError AST
parse = runP stlParser [] "file" where
    stlParser = tyDec `sepBy` sepT where
        stlToken f = token show locToSourcePos (f . L.tTok)

        sepT = stlToken $ \x -> case x of L.Separator -> Just (); _ -> Nothing

        tyDec = do
            i <- identT
            tyColon
            ty <- typeT
            return $ TypeDec i ty

        typeNonAppT = TyVoid <$ tyVoidT
                  <|> TyVar <$> identT

        typeT = typeAppT
            <|> typeNonAppT

        typeAppT = try $ do
            ty1 <- typeNonAppT
            tyArrowT
            ty2 <- typeT
            return $ TyApp ty1 ty2

        tyArrowT = stlToken $ \x -> case x of L.Punct L.P_Arrow -> Just (); _ -> Nothing

        identT = stlToken $ \x -> case x of
                     L.Ident t -> Just $ Ident t
                     _ -> Nothing

        tyVoidT = stlToken $ \x -> case x of
                      L.KW L.K_Void -> Just TyVoid
                      _ -> Nothing

        tyColon = stlToken $ \x -> case x of
                      L.Punct L.P_Colon -> Just ()
                      _ -> Nothing

locToSourcePos :: L.Token -> SourcePos
locToSourcePos (L.Token _ tp) = case tp of
    (Columns a _, _) -> newPos "unknown" 1 (fromIntegral a + 1)
    (Lines a b _ _, _) -> newPos "unknown" (fromIntegral a + 1) (fromIntegral b + 1)
    x -> error $ show x
