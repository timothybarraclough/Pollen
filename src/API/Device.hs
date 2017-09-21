{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

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

type DeviceAPI =
         "devices" :> Get '[JSON] [Entity Device]
    :<|> "devices" :> Capture "uuid" Text :> Get '[JSON] (Entity Device)
    :<|> "devices" :> ReqBody '[JSON] Device :> Post '[JSON] Int64

-- | The server that runs the DeviceAPI
deviceServer :: ServerT DeviceAPI App
deviceServer =
         allDevices
    :<|> singleDevice
    :<|> createDevice

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
    newDevice <- runDb (insert p)
    return $ fromSqlKey newDevice
