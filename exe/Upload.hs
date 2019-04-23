{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Data.Text (unpack)
import Network.Socket (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Multipart
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as LBS

-- Our API, which consists in a single POST endpoint at /
-- that takes a multipart/form-data request body and
-- pretty-prints the data it got to stdout before returning 0.
type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

-- The handler for our single endpoint.
-- Its concrete type is:
--   MultipartData -> Handler Integer
--
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: Server API
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Writing file " ++ show (fdFileName file)
      LBS.writeFile
        ("/srv/http/files" ++ "/" ++ unpack (fdFileName file)) content
  return 0

startServer :: IO ()
startServer = run 8082 $ simpleCors (serve api upload)

main :: IO ()
main = do
  putStrLn "starting server"
  withSocketsDo startServer
