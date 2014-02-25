module Main where

import qualified Data.ByteString as B
import Language.STL.Lex
import Language.STL.Lex.Normalize
import Language.STL.Parse
import Prelude hiding (lex)
import System.IO

main :: IO ()
main = withFile "main.stl" ReadMode $ \h -> do
    m <- fmap lex (B.hGetContents h)
    case m of
        Success ts -> do
            let ns = normalize ts
            print $ parse "main.stl" ns
        e -> error $ show e
