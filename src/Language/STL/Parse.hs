{-# LANGUAGE FlexibleContexts #-}

module Language.STL.Parse where

import Control.Applicative hiding (many, (<|>))
-- import Control.Lens (over, _Left)
import Data.Functor.Identity
import Data.Text (Text)
import qualified Language.STL.Lex as L
import Text.Parsec
import Text.Parsec.Pos
import Text.Trifecta.Delta (Delta(..))

type AST = [Dec]

data Dec = TypeDec Ident Type | NakedExpr Expr deriving Show

data Expr = App Expr Expr | Var Ident | Lit L.Literal deriving Show

data Type = TyVar Ident | TyApp Type Type | TyVoid deriving Show

data Ident = Ident Text deriving Show

type TokenParser = ParsecT L.TokenStream [L.Token] Identity

parse :: Stream s Identity L.Token
      => String -> s -> Either ParseError AST
parse src = runP stlParser [] src where
    stlParser = dec `endBy` sepT <* eof where

        dec = try tyDec <|> fmap NakedExpr nakedExpr

        nonAppE = varE

        nakedExpr = appE <|> nonAppE

        varE = Var <$> identT

        appE = App <$> nonAppE <*> appE

        stlToken f = token (show . L.tTok) (locToSourcePos src) (f . L.tTok)

        sepT = stlToken $ \x -> case x of L.Separator -> Just (); _ -> Nothing

        tyDec = do
            i <- identT
            tyColon
            ty <- typeT <?> "type"
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

locToSourcePos :: SourceName -> L.Token -> SourcePos
locToSourcePos src (L.Token _ tp) = case tp of
    (Columns a _, _) -> newPos src 1 (fromIntegral a + 1)
    (Lines a b _ _, _) -> newPos src (fromIntegral a + 1) (fromIntegral b + 1)
    x -> error $ show x
