{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module DeviceAPI where

import Config                      (App (..), Config (..))
import Control.Monad.Except
import Control.Monad.Reader        (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Data.Aeson                  (FromJSON, ToJSON)
import Data.Int                    (Int64)
import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, selectFirst, selectList, (==.))
import GHC.Generics                (Generic)
import Models
import Network.PushNotify.APN
import Network.Wai                 (Application)
import Servant
import Data.Time


newtype TestNotification =
  TestNotification { deviceToken :: Text } deriving Generic

instance FromJSON TestNotification
instance ToJSON TestNotification

data SoundedNotification =
  SoundedNotification { deviceToken :: Text, soundFile :: Text } deriving Generic

instance FromJSON SoundedNotification
instance ToJSON SoundedNotification


type DeviceAPI =
         "devices" :> Get '[JSON] [Entity Device]
    :<|> "devices" :> Capture "uuid" Text :> Get '[JSON] (Entity Device)
    :<|> "devices" :> ReqBody '[JSON] Device :> Post '[JSON] Int64
    :<|> "notifications" :> ReqBody '[JSON] TestNotification :> Post '[JSON] String
    :<|> "playSound" :> ReqBody '[JSON] SoundedNotification :> Post '[JSON] String


-- | The server that runs the DeviceAPI
deviceServer :: ServerT DeviceAPI App
deviceServer =
         allDevices
    :<|> singleDevice
    :<|> createDevice
    :<|> queueTestNotification
    :<|> queueSoundedNotification

-- | Returns all devices in the database.
allDevices :: App [Entity Device]
allDevices =
    runDb (selectList [] [])


-- | Returns a device by uuid or throws a 404 error.
singleDevice :: Text -> App (Entity Device)
singleDevice str = do
    maybeDevice <- runDb (selectFirst [DeviceUuid ==. str] [])
    case maybeDevice of
         Nothing ->
            throwError err404
         Just device ->
            return device


-- | Creates a devices in the database.
createDevice :: Device -> App Int64
createDevice p = do
    maybeDevice <- runDb (selectFirst [DeviceUuid ==. deviceUuid p] [])
    case maybeDevice of
      Nothing -> do
          newDevice <- runDb (insert p)
          return $ fromSqlKey newDevice
      Just device ->
          return 0


queueTestNotification :: TestNotification -> App String
queueTestNotification notification = do
  time <- liftIO getCurrentTime

  let notificationEntity =
        Notification
          (deviceToken (notification :: TestNotification))
          "Test Notification"  -- title
          "Yay, it's working!" -- body
          Nothing              -- sound
          0                    -- badge
          (Just time)          -- queuedAt
          Nothing              -- deliveredAt

  queuedNotification <- runDb $ insert notificationEntity
  return ""


queueSoundedNotification :: SoundedNotification -> App String
queueSoundedNotification notification = do
  time <- liftIO getCurrentTime

  let notificationEntity =
        Notification
          (deviceToken (notification :: SoundedNotification))
          "Spooky Notification"           -- title
          "Boo!"                          -- body
          (Just $ soundFile notification) -- sound
          0                               -- badge
          (Just time)                     -- queuedAt
          Nothing                         -- deliveredAt

  queuedNotification <- runDb $ insert notificationEntity
  return ""
