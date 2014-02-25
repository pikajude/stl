{-# LANGUAGE FlexibleContexts #-}

module Language.STL.Parse where

import Control.Applicative hiding (many, (<|>))
import Data.Functor.Identity
import Data.List
import Data.Text (Text)
import qualified Language.STL.Lex as L
import Text.Parsec
import Text.Parsec.Pos
import Text.Trifecta.Delta (Delta(..))

type AST = [Dec]

type Body = Expr

data Dec = TypeDec { tdId :: Ident, tdType :: Type }
         | FunDec { fdId :: Ident, fdPatterns :: [([Pat],Body)] }
         deriving Show

data Pat = PatVar Ident deriving Show

data Expr = App { appHead :: Expr, appBody :: [Expr] }
          | Seq [Expr]
          | Var { unVar :: Ident }
          | Assign Ident Expr
          | Lit { unLit :: L.Literal }
          deriving Show

data Type = TyVar { tvId :: Ident }
          | TyApp Type Type
          | TyVoid
          deriving Show

data Ident = Ident Text deriving Show

type TokenParser = ParsecT L.TokenStream [L.Token] Identity

parse :: Stream s Identity L.Token
      => String -> s -> Either ParseError AST
parse src = runP stlParser [] src where
    stlParser = fmap combineFunDecs $ dec `endBy` sepT <* eof where

        dec = try tyDec <|> funDec

        funDec = FunDec <$> identT <*> fmap return patBodyT

        patBodyT = (,) <$> patSigT <*> (pEqualsT *> expT)

        seqExpT = fmap Seq . braces $ many sepT *> expT `sepEndBy` sepT

        patSigT = parens $ patT `sepBy` commaT

        patT = PatVar <$> identT

        expT = try seqExpT
           <|> try appT
           <|> try assignT
           <|> try varE
           <|> litE

        appStartT = try varE <|> parens expT

        litE = fmap Lit $ try litStrT
                      <|> litIntT

        parens e = lParenT *> e <* rParenT
        braces e = lBraceT *> e <* rBraceT

        appT = do
            m <- appStartT
            ns <- parens $ expT `sepBy` commaT
            return $ App m ns

        assignT = Assign <$> identT <*> (pEqualsT *> expT)

        varE = Var <$> identT

        stlToken f = token (show . L.tTok) (locToSourcePos src) (f . L.tTok)

        sepT = stlToken $ \x -> case x of L.Separator -> Just (); _ -> Nothing

        pEqualsT = stlToken $ \x -> case x of L.Punct L.P_Equals -> Just ()
                                              _ -> Nothing

        lParenT = stlToken $ \x -> case x of L.Punct L.P_LParen -> Just ()
                                             _ -> Nothing

        rParenT = stlToken $ \x -> case x of L.Punct L.P_RParen -> Just ();
                                             _ -> Nothing

        lBraceT = stlToken $ \x -> case x of L.Punct L.P_LBrace -> Just ()
                                             _ -> Nothing

        rBraceT = stlToken $ \x -> case x of L.Punct L.P_RBrace -> Just ();
                                             _ -> Nothing

        commaT = stlToken $ \x -> case x of L.Punct L.P_Comma -> Just ();
                                            _ -> Nothing

        litStrT = stlToken $ \x -> case x of L.Lit l@(L.L_Str _) -> Just l
                                             _ -> Nothing

        litIntT = stlToken $ \x -> case x of L.Lit i@(L.L_Integer _) -> Just i
                                             _ -> Nothing

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

combineFunDecs = map (\ xs@(x:_) -> if length xs == 1
                                        then x
                                        else FunDec (fdId x) (concatMap fdPatterns xs))
    . groupBy (\a b -> case (a, b) of
        (FunDec (Ident t) _, FunDec (Ident t1) _) -> t == t1
        _ -> False)

locToSourcePos :: SourceName -> L.Token -> SourcePos
locToSourcePos src (L.Token _ tp) = case tp of
    (Columns a _, _) -> newPos src 1 (fromIntegral a + 1)
    (Lines a b _ _, _) -> newPos src (fromIntegral a + 1) (fromIntegral b + 1)
    x -> error $ show x
