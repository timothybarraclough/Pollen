{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Config
import Control.Monad.Reader
import Data.Aeson           (FromJSON, ToJSON)
import Data.Text            (Text)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics         (Generic)

newtype TestNotification =
  TestNotification { deviceToken :: Text } deriving Generic

instance FromJSON TestNotification
instance ToJSON TestNotification

data SoundedNotification =
  SoundedNotification { token :: Text, soundFile :: Text } deriving Generic

instance FromJSON SoundedNotification
instance ToJSON SoundedNotification

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Device json
    uuid Text
    deriving Show Eq

Notification json
    uuid Text
    title Text
    body Text
    sound Text Maybe
    badge Int default=0
    queuedAt UTCTime default=now()
    deliveredAt UTCTime Maybe
    deriving Show Eq
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
