{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize
import Data.Time
import PyF
import System.Directory
import System.Environment
import GHC.Generics
import Control.Monad.Except

type Tag = String
type TagMap = Map Tag (Set FilePath)

data MapReadError = UnreadableError | NonexistentError
  deriving (Generic, Show)
instance Serialize MapReadError

tagFile :: FilePath
tagFile = ".tags"

addTags :: Set Tag -> FilePath -> TagMap -> TagMap
addTags tags file tagMap = Set.foldr' upd tagMap tags
  where upd :: Tag -> TagMap -> TagMap
        upd tag tm = Map.alter add tag tm
        add Nothing = Just $ Set.singleton file
        add (Just files) = Just $ Set.insert file files

readTags :: ExceptT MapReadError IO TagMap
readTags = do
  exists <- lift $ doesFileExist tagFile
  if exists then do
    bs <- lift $ B.readFile tagFile
    liftEither $ first (const UnreadableError) $ decode bs
  else throwError NonexistentError

writeTags :: TagMap -> IO ()
writeTags tm = do
  exists <- doesFileExist tagFile
  when exists $ do
    writePerms <- writable <$> getPermissions tagFile
    guard writePerms
  B.writeFile tagFile (encode tm)

moveOldTags :: IO ()
moveOldTags = do
  timeStr <- formatTime defaultTimeLocale "%Y%m%d%H%M%S"
             <$> (utcToLocalZonedTime =<< getCurrentTime)
  putStrLn [fmt|Moving old {tagFile} file to {tagFile}{timeStr}|]
  renameFile tagFile (tagFile ++ timeStr)

newTags :: IO TagMap
newTags = do
  putStrLn [fmt|Creating new {tagFile} file|]
  pure Map.empty

help :: String
help = "filtag - file tagging cli\n" ++
       "------\n" ++
       "filtag list [verbose]         Lists available tags\n" ++
       "filtag find {tag}             Lists files associated with [tag]\n" ++
       "filtag tag {file} {tags}      Add [tags] to [file]\n" ++
       "filtag remove {file} {tags}   Add [tags] to [file]"

main :: IO ()
main = do
  args <- getArgs
  tm <- runExceptT readTags
  tagMap <- case tm of
    Left UnreadableError  -> moveOldTags >> pure Map.empty
    Left NonexistentError -> pure Map.empty
    Right tm              -> pure tm
  tagMap' <- case args of
    "tag" : file : tags -> do
      pure $ Just $ addTags (Set.fromList tags) file tagMap
    ["list"] -> do
      let tags = map (second Set.size) $ Map.toList tagMap
      mapM_ (putStrLn . \(t, n) -> [fmt|{t} ({n})|]) tags
      pure Nothing
    ["find", tag] -> do
      case Map.lookup tag tagMap of
        Nothing     -> pure ()
        Just files' -> mapM_ putStrLn files'
      pure Nothing
    ["remove", file] -> do
      pure $ Just $ Map.mapMaybe upd tagMap
        where upd fs = let fs' = Set.delete file fs
                       in if Set.null fs' then Nothing else Just fs'
    "remove" : file : tags -> do
      pure $ Just $ Map.mapMaybeWithKey upd tagMap
        where upd t fs
                | t `elem` tags = let fs' = Set.delete file fs
                    in if Set.null fs' then Nothing else Just fs'
                | otherwise     = Just fs
    ["list", "verbose"] -> do
      forM_ (Map.toList tagMap) $ \(tag, fs) -> do
        putStrLn [fmt|{tag} |]
        forM_ (Set.toList fs) $ \file ->
          putStrLn [fmt|  {file}|]
      pure Nothing
    [] -> do
      putStrLn help
      pure Nothing
    ["help"] -> do
      putStrLn help
      pure Nothing
    args' -> do
      putStrLn [fmt|unrecognized command "{unwords args}"|]
      pure Nothing
  case tagMap' of
    Just tm -> writeTags tm
    Nothing -> pure ()
