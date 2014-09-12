{-# LANGUAGE
  TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables #-}


module DB (
  DBLocation(..), existsDB, readDB, writeDB, backupDB )
  where


import ClassyPrelude
import Prelude (read)
import Data.UUID
import System.Directory (
  getAppUserDataDirectory, doesFileExist, createDirectoryIfMissing)
import qualified Data.Aeson as A
import           Data.Aeson ((.:), (.=))
import Filesystem (copyFile)
import Text.Printf

import ClassesLens hiding ((<.>), (.:), (.=))
import Helpers
import Notes


data DBLocation =
  NoDB |
  DBFile FilePath |
  DefaultDBFile
  deriving (Show)

instance A.FromJSON NID where
  parseJSON = A.withText "NID" $ maybe mempty pure . readMay

instance A.ToJSON NID where
  toJSON = A.toJSON . show

instance A.FromJSON Note where
  parseJSON (A.Object v) =
    Note
      <$> v .: "nid"
      <*> v .: "subs"
      <*> v .: "title"
      <*> v .: "created"
      <*> v .: "lastModified"
  parseJSON _            = mempty

instance A.ToJSON Note where
  toJSON n = A.object [
    "nid"          .= (n ^. nid),
    "subs"         .= (n ^. subs),
    "title"        .= (n ^. title),
    "created"      .= (n ^. created),
    "lastModified" .= (n ^. lastModified) ]

instance A.FromJSON NStore where
  parseJSON = A.withArray "NStore" 
    (fmap (mapFromList . unpack .  map (_nid &&& id)) . mapM A.parseJSON)

instance A.ToJSON NStore where
  toJSON = A.Array . map A.toJSON . repack

instance A.FromJSON DBState where
  parseJSON (A.Object v) = DBState <$>
                             v A..: "notes" <*>
                             v A..: "root"
  parseJSON _            = mempty

instance A.ToJSON DBState where
  toJSON db = A.object ["notes" A..= (db ^. notes),
                        "root"  A..= (db ^. root)]

-- | By default the database is stored in system-specific directory for
-- application files (different for each user), in a file named “Jane.db”.
defaultDBPath :: IO_ FilePath
defaultDBPath = liftIO' $ do
  d ← getAppUserDataDirectory "Jane"
  return (fpFromString d </> "Jane.db")

existsDB :: DBLocation → IO_ Bool
existsDB NoDB          = return True
existsDB DefaultDBFile = existsDB /$/ DBFile $/ defaultDBPath
existsDB (DBFile file) = liftIO' $ doesFileExist $ fpToString file

-- | Fails if the database doesn't exist.
readDB :: DBLocation → IO_ (Maybe DBState)
readDB NoDB          = return Nothing
readDB DefaultDBFile = readDB /$/ DBFile $/ defaultDBPath
readDB (DBFile file) = liftIO' $ A.decode $/ readFile file

writeDB :: DBLocation → DBState → IO_ ()
writeDB NoDB _           = return ()
writeDB DefaultDBFile db = do
  file ← defaultDBPath
  writeDB (DBFile file) db
writeDB (DBFile file) db = liftIO' $ do
  createDirectoryIfMissing True $ fpToString $ directory file
  writeFile file (A.encode db)

backupDB :: DBLocation → IO_ ()
backupDB NoDB          = return ()
backupDB DefaultDBFile = backupDB /$/ DBFile $/ defaultDBPath
backupDB (DBFile file) = liftIO' $ copyFile file (file <.> "backup")
