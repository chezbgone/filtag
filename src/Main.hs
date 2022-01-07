{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Monad
import qualified Data.ByteString as B
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize
import Data.Time
import System.Directory
import System.Environment
import GHC.Generics
import Control.Monad.Except

type TagMap = Map FilePath (Set String)

data MapReadError = UnreadableError | NonexistentError
  deriving Generic
instance Serialize MapReadError

tagFile :: FilePath
tagFile = ".tags"

addFile :: FilePath -> TagMap -> Maybe TagMap
addFile file m = do
  guard $ Map.member file m
  pure $ Map.insert file Set.empty m

getTagMap :: IO (Either MapReadError TagMap)
getTagMap = undefined

getTagMap' :: ExceptT MapReadError IO TagMap
getTagMap' = do
  exists <- lift $ doesFileExist tagFile
  pure _

writeTagMap :: TagMap -> IO ()
writeTagMap tm = undefined

main :: IO ()
main = do
  args <- getArgs
  tagMap <- getTagMap
  case args of
    ["add", file] -> do
      putStrLn $ unwords ["adding", file]
    "tag" : file : tags -> do
      putStrLn $ unwords $ ["tagging", file, "with"] ++ tags
      putStrLn "not supported yet"
    ["list"] -> do
      putStrLn "listing"
      putStrLn "not supported yet"
    ["find", tag] -> do
      putStrLn $ unwords ["finding", tag]
      putStrLn "not supported yet"
    _ -> putStrLn "command not recognized"
  print args
