module Main where

import qualified Data.ByteString as BS
import Sei


printValue (Left err) = print err
printValue (Right v) = print v

main :: IO ()
main = BS.getContents >>= printValue  . fmap (interp . desugar) . runParserLexeme sexp
