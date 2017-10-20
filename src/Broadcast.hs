{-# LANGUAGE OverloadedStrings #-}

module Broadcast where

import Control.Concurrent          (threadDelay)
import Control.Monad
import Control.Monad.Except
import Data.ByteString
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.Time
import Database.Persist.Postgresql (ConnectionPool, Entity, entityKey, entityVal, fromSqlKey,
                                    runSqlPool, selectList, update, (=.), (==.))
import Debug.Trace
import Network.PushNotify.APN

import Config
import Models


startBroadcaster :: IO ()
startBroadcaster = do
  session <- liftIO $ newSession "Pollen.key" "Pollen.crt" "pushCA.pem" sandbox timeout "com.floracreative.PollenPush"
  pool <- makePool Production
  forever $ processNotification session pool
  where
    sandbox = True -- Production environment
    timeout = 10   -- Minutes to keep the connection open


processNotification :: ApnSession -> ConnectionPool -> IO ()
processNotification session pool = do
  Control.Concurrent.threadDelay 300000
  let query = selectList [ NotificationDeliveredAt ==. Nothing ] []
  queuedNotifications <- runSqlPool query pool
  mapM_ (send session pool) queuedNotifications


send :: ApnSession -> ConnectionPool -> Entity Notification -> IO ()
send session pool notificationEntity = do
  let notification   = entityVal notificationEntity
      payload        = alertMessage (notificationTitle notification) (notificationBody notification)
      soundedPayload = setSound (soundOrDefault notification) payload
      message        = newMessage soundedPayload
      token          = hexEncodedToken $ notificationDeviceToken notification

  result <- sendMessage session token (Debug.Trace.trace "Sending message... " message)

  case result of
    ApnMessageResultOk -> do
      time <- getCurrentTime
      let query = update (entityKey notificationEntity) [ NotificationDeliveredAt =. Just time ]
      runSqlPool query pool
    failure -> do
      let _ = Debug.Trace.trace "Failed to send message: " failure
      return ()


soundOrDefault :: Notification -> Text
soundOrDefault notification =
   fromMaybe "sound.wav" $ notificationSound notification
