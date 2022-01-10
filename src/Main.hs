{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Control.Monad.Except
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

type Tag = String
type TagMap = Map Tag (Set FilePath)

data MapReadError = UnreadableError | NonexistentError
  deriving (Show)

-----------------------
-- TagMap Operations --
-----------------------

tagFile :: FilePath
tagFile = ".tags"

addTags :: Set Tag -> FilePath -> TagMap -> TagMap
addTags tags file tagMap = Set.foldr' upd tagMap tags
  where upd :: Tag -> TagMap -> TagMap
        upd tag tm = Map.alter add tag tm
        add Nothing = Just $ Set.singleton file
        add (Just files) = Just $ Set.insert file files

listTags :: TagMap -> [(Tag, Int)]
listTags tagMap = map (second Set.size) $ Map.toList tagMap

listTagsVerbose :: TagMap -> [(Tag, [FilePath])]
listTagsVerbose tagMap = map (second Set.toList) $ Map.toList tagMap

findTag :: Tag -> TagMap -> [FilePath]
findTag tag tagMap =
  Set.toList $ Set.unions $ maybeToList $ Map.lookup tag tagMap

mapRemoveFile :: FilePath -> TagMap -> TagMap
mapRemoveFile file = Map.mapMaybe upd
  where upd fs = let fs' = Set.delete file fs
                  in if Set.null fs' then Nothing else Just fs'

removeTags :: FilePath -> [Tag] -> TagMap -> TagMap
removeTags file tags = Map.mapMaybeWithKey upd
  where upd t fs
          | t `elem` tags = let fs' = Set.delete file fs
                             in if Set.null fs' then Nothing else Just fs'
          | otherwise     = Just fs

---------------
-- TagMap IO --
---------------

readTags :: ExceptT MapReadError IO TagMap
readTags = do
  exists <- lift $ doesFileExist tagFile
  unless exists $ throwError NonexistentError
  bs <- lift $ B.readFile tagFile
  liftEither $ first (const UnreadableError) $ decode bs

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


--------------------
-- Script Actions --
--------------------

type MapAction = IO (Maybe TagMap)

doAddTags :: [Tag] -> FilePath -> TagMap -> MapAction
doAddTags ts f tm = pure $ Just $ addTags (Set.fromList ts) f tm

doListTags :: TagMap -> MapAction
doListTags tm = do
  mapM_ (putStrLn . \(t, n) -> [fmt|{t} ({n})|]) (listTags tm)
  pure Nothing

doListTagsVerbose :: TagMap -> MapAction
doListTagsVerbose tm = do
  forM_ (listTagsVerbose tm) $ \(tag, fs) -> do
    putStrLn [fmt|{tag} |]
    forM_ fs $ \file ->
      putStrLn [fmt|  {file}|]
  pure Nothing

doFindTag :: Tag -> TagMap -> MapAction
doFindTag tag tm = do
  mapM_ putStrLn $ findTag tag tm
  pure Nothing

doRemoveFile :: FilePath -> TagMap -> MapAction
doRemoveFile f tm = pure $ Just $ mapRemoveFile f tm

doRemoveTags :: FilePath -> [Tag] -> TagMap -> MapAction
doRemoveTags f ts tm = pure $ Just $ removeTags f ts tm

doPrintHelp :: MapAction
doPrintHelp = putStrLn help >> pure Nothing
  where
    help = "filtag - file tagging utility\n" ++
           "------\n" ++
           "filtag list [verbose]         Lists available tags\n" ++
           "filtag find {tag}             Lists files associated with {tag}\n" ++
           "filtag tag {file} {tags}      Add {tags} to {file}\n" ++
           "filtag remove {file} {tags}   Add {tags} to {file}"

doUnsupported :: [String] -> MapAction
doUnsupported args = do
  putStrLn [fmt|unrecognized command "{unwords args}"|]
  pure Nothing

----------
-- Main --
----------

main :: IO ()
main = do
  args <- getArgs
  tm <- runExceptT readTags
  tagMap <- case tm of
    Left UnreadableError  -> moveOldTags >> pure Map.empty
    Left NonexistentError -> pure Map.empty
    Right tm              -> pure tm
  tagMap' <- case args of
    "tag" : file : tags    -> doAddTags tags file tagMap
    ["list"]               -> doListTags tagMap
    ["list", "verbose"]    -> doListTagsVerbose tagMap
    ["find", tag]          -> doFindTag tag tagMap
    ["remove", file]       -> doRemoveFile file tagMap
    "remove" : file : tags -> doRemoveTags file tags tagMap
    []                     -> doPrintHelp
    ["help"]               -> doPrintHelp
    _                  -> doUnsupported args
  mapM_ writeTags tagMap'
