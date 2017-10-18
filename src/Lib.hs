{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Models                      (doMigrations)
import           Safe                        (readMay)
import           Network.PushNotify.APN


-- | The 'main' function gathers the required environment information and
-- initializes the application.
startApp :: Int -> IO ()
startApp port = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" port
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

testNotification :: IO()
testNotification = do
    let sandbox = True -- Production environment
    let timeout = 10   -- Minutes to keep the connection open
    session <- newSession "Pollen.key" "Pollen.crt" "pushCA.pem" sandbox timeout "com.floracreative.PollenPush"
    let payload = alertMessage "Let's give it a go!" "Hello From Haskell"
    let message = newMessage payload
    let token   = hexEncodedToken "99ebc5deab93d51067d6ccdbcf08ab89608ba382126eb79ef1ecbbb4a59d6f4a"
    success <- sendMessage session token message
    print success
