{-# LANGUAGE
  TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables #-}

module DB where

import ClassyPrelude
import Control.Lens
import Data.UUID
import System.Directory (
  getAppUserDataDirectory, doesFileExist, createDirectoryIfMissing)
import System.Posix (getFileStatus, fileSize)
import qualified Data.Aeson as A
import Filesystem (copyFile)
import Text.Printf

import Notes

instance A.FromJSON NID where
  parseJSON = A.withText "NID" $ maybe mempty pure . readMay

instance A.ToJSON NID where
  toJSON = A.toJSON . show

instance A.FromJSON Note where
  parseJSON (A.Object v) = Note <$>
                             v A..: "nid" <*>
                             v A..: "subs" <*>
                             v A..: "title"
  parseJSON _            = mempty

instance A.ToJSON Note where
  toJSON n = A.object ["nid"   A..= _nid n, 
                       "subs"  A..= _subs n,
                       "title" A..= _title n]


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

data NS = NS {
  _db :: DBState,
  _prevStates, _undoneStates :: [DBState],
  _prevOps,    _undoneOps    :: [Text] }
  deriving (Show)

makeLenses ''NS

dbPath :: IO FilePath
dbPath = do
  d <- getAppUserDataDirectory "Jane"
  return (fpFromString d </> "Jane.db")

existsDb :: IO Bool
existsDb = doesFileExist =<< fpToString <$> dbPath

-- fails if db file doesn’t exist
readDb :: IO (Maybe DBState)
readDb = fmap A.decode . readFile =<< dbPath

-- creates file if it doesn’t exist
writeDb :: DBState -> IO ()
writeDb db = do
  path <- dbPath
  createDirectoryIfMissing True $ fpToString $ directory path
  writeFile path (A.encode db)

getDbSize :: IO Int64
getDbSize = fromIntegral . fileSize <$> 
            (getFileStatus =<< fpToString <$> dbPath)

backupDb :: IO ()
backupDb = do
  path <- dbPath
  copyFile path (directory path </> "Jane.db~")
