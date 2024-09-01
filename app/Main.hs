module Main where

import           State

data Env = Env {os, dist, kernel :: String} deriving (Show)

env = Env {os = "linux", dist = "arch", kernel = "6.10.2"}

uname :: State Env String
uname = do
  modify (\s -> s {dist = "debian"})
  s <- get
  let os' = os s
  dist' <- gets dist
  kern' <- gets kernel
  return $ "uname: " <> os' <> " " <> dist' <> " " <> kern'

main :: IO ()
main = do
  putStrLn "Reader Writer State"
