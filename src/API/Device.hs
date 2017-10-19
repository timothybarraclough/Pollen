{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api.Device where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models
import           Data.Text (Text)

import           Network.PushNotify.APN

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
    :<|> sendTestNotification
    :<|> sendSoundedNotification

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

sendTestNotification :: TestNotification -> App String
sendTestNotification notification = do
    let sandbox = True -- Production environment
        timeout = 10   -- Minutes to keep the connection open

    session <- liftIO $ newSession "Pollen.key" "Pollen.crt" "pushCA.pem" sandbox timeout "com.floracreative.PollenPush"

    let payload        = alertMessage "Let's give it a go!" "Hello From Haskell"
        soundedPayload = setSound "sound.wav" payload
        message        = newMessage soundedPayload
        token          = hexEncodedToken $ deviceToken notification

    result <- liftIO $ sendMessage session token message
    return $ show result

sendSoundedNotification :: SoundedNotification -> App String
sendSoundedNotification notification = do
    let sandbox = True -- Production environment
        timeout = 10   -- Minutes to keep the connection open

    session <- liftIO $ newSession "Pollen.key" "Pollen.crt" "pushCA.pem" sandbox timeout "com.floracreative.PollenPush"

    let payload        = alertMessage "Let's give it a go!" "Hello From Haskell"
        soundedPayload = setSound (soundFile notification) payload
        message        = newMessage soundedPayload
        encodedToken   = hexEncodedToken $ token notification

    result <- liftIO $ sendMessage session encodedToken message
    return $ show result
