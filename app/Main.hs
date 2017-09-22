module Main where



import           Control.Concurrent
import           Data.Default
import qualified Data.HashSet            as HS
import           Data.Text               (pack)
import           Lib
import           Network.PushNotify.Apns
import           Network.TLS.Extra       (fileReadCertificate,
                                          fileReadPrivateKey)

main :: IO ()
-- main = startApp 8080
main = do
             cert <- fileReadCertificate "public-cert.pem"
             key  <- fileReadPrivateKey  "private-key.pem"
             let confg = def{
                             apnsCertificate = cert
                         ,   apnsPrivateKey  = key
                         ,   environment     = Local }

             putStrLn "Let's send a notification:"

             putStrLn "A device token (hexadecimal): "
             dtoken  <- getLine

             putStrLn "An alert message: "
             alertMsg <- getLine

             let msg = def { deviceTokens = HS.singleton $ pack dtoken
                           , alert = Left $ pack alertMsg }
             manager <- startAPNS confg
             res     <- sendAPNS manager msg
             putStrLn ("Result: " ++ show res)
             closeAPNS manager

             putStrLn "\nLet's connect to the Feedback Service:"
             fres    <- feedBackAPNS confg
             putStrLn ("Result: " ++ show fres)
