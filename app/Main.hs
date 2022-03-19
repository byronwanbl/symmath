module Main where

import Control.Category ((>>>))
import Data.Functor (fmap)
import Data.Maybe (maybe)
import Symmath.Lib (parseExpr, simplify)
import Prelude
import Foreign.C (CString, peekCString, newCString)

main :: IO ()
main = getLine >>= (simplify' >>> putStrLn) >> main

simplify' :: String -> String
simplify' = parseExpr >>> fmap simplify >>> maybe "Simplify failed!" show

simplifyC :: CString -> IO CString
simplifyC x = peekCString x >>= (simplify' >>> newCString)

foreign export ccall simplifyC:: CString -> IO CString