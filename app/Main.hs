module Main where

import Control.Category ((>>>))
import Data.Functor (fmap)
import Data.Maybe (maybe)
import Foreign.C (CString, newCString, peekCString)
import Symmath.Lib (parseExpr, simplify)
import Prelude

main :: IO ()
main = do
  putStrLn "Symmath CLI"
  putStrLn "Copyright 2022 ByronWan. All rights reserved."
  putStrLn ""
  putStrLn "Input expressions below to simplify them:"
  main'

main' :: IO ()
main' = do
  s <- getLine
  let s' = simplify' s
  putStrLn s'
  main'

simplify' :: String -> String
simplify' = parseExpr >>> fmap simplify >>> maybe "Simplify failed!" show

simplifyC :: CString -> IO CString
simplifyC x = peekCString x >>= (simplify' >>> newCString)

foreign export ccall simplifyC :: CString -> IO CString