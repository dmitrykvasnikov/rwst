module Main where

import           Option
import           Result
import           StateT

-- import           StateT

-- data Env = Env {os, dist, kernel :: String} deriving (Show)
--
-- env :: Env
-- env = Env {os = "linux", dist = "arch", kernel = "6.10.2"}

type Env = [(String, String)]

env :: Env
env = [("os", "linux"), ("dist", "arch"), ("kernel", "6.10.7")]

getData :: String -> StateT Env (Either String) String
getData key = do
  e <- get
  case lookup key e of
    Nothing    -> StateT $ \_ -> Left $ "Error: no value for key " <> key
    Just value -> return value

uname :: StateT Env (Either String) String
uname = do
  os <- getData "os"
  dist <- getData "dist"
  return $ "uname: " <> os <> " " <> dist

main :: IO ()
main = do
  putStrLn "Reader Writer State Transformers examples"
