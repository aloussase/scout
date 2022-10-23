{-# LANGUAGE LambdaCase #-}
module Main where

import           System.IO (hPutStrLn, stderr)

import           Scout

main :: IO ()
main = do
  getOptions >>=
    \case
      Right opts -> run opts
      Left usage -> hPutStrLn stderr usage
