module Main where

import           Lib
import           Network.PushNotify.APN

main :: IO ()
main = startApp 8080


main :: IO ()
main = do
    let sandbox = True -- Development environment
    let timeout = 10   -- Minutes to keep the connection open
    session <- newSession "my.key" "my.crt"
    "/etc/ssl/ca_certificates.txt" sandbox
    timeout "my.bundle.id"
    let payload = alertMessage "Title" "Hello From Haskell"
    let message = newMessage payload
    let token   = base16EncodedToken "the-token"
    success <- sendMessage session token payload
    print success
