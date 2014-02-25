module Language.STL.Lex.Normalize (normalize) where

import Data.Function
import Data.List
import Language.STL.Lex

type TokenTrans = TokenStream -> TokenStream

normalize :: TokenTrans
normalize = collapseSeps
          . dropWhile ((== Separator) . tTok)
          . filter (\x -> case tTok x of Comment _ -> False; _ -> True)

collapseSeps :: TokenTrans
collapseSeps = (=<<) (\xs -> if tTok (head xs) == Separator then take 1 xs else xs)
             . groupBy ((==) `on` tTok)
