{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Server
import           Universum
import qualified Universum.Unsafe                        as Unsafe

main :: IO ()
main = Server.start
