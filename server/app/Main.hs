{-# LANGUAGE LambdaCase #-}

module Main where

import MoviesServer ( moviesApp )
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)


main :: IO ()
main = initialize >> run 7143 moviesApp


initialize :: IO ()
initialize = doesFileExist "db.json" >>=
    
    \case
    True -> pure ()
    False -> writeFile "db.json" ""