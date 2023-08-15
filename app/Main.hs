module Main (main) where

import Lib
import Parser
import Options.Applicative

main :: IO ()
main = listf =<< execParser opts 