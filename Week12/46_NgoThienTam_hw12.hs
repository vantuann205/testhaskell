{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-

This is a example program to introduce an useful data type and some functions. What it does:
+ Encodes an object to json request (in ByteString)
+ Sends the json request over HTTP to a public echo server
+ Receives the json response (in ByteString)
+ Parse the json response to an object

Data type:
1. ByteString: a data type that is well optimized for high performance usage (like networking)
https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html

Functions:

2. Data.Aeson.encode: to convert a object to ByteString
encode :: ToJSON a => a -> ByteString

3. Data.Aeson.decode: to parse ByteString back to a object
decode :: FromJSON a => ByteString -> Maybe a

4. Network.HTTP.Simple.parseRequest: to parse a String to HTTP request
parseRequest :: MonadThrow m => String -> m Request

5. Network.HTTP.Simple.httpLBS: to send a HTTP request
httpLBS :: MonadIO m => Request -> m (Response ByteString)

6. Some helper functions to extract values from response
+ Network.HTTP.Simple.getResponseStatus
+ Network.HTTP.Simple.getResponseStatusCode
+ Network.HTTP.Simple.getResponseHeader
+ Network.HTTP.Simple.getResponseHeaders
+ Network.HTTP.Simple.getResponseBody
-}

module Main where 

-- use "qualified" for all imports to show clearly which function belongs to which package
import qualified Data.ByteString.Lazy.Char8
import qualified Network.HTTP.Simple
import qualified Data.Aeson
import qualified GHC.Generics

data MyRequest = MyRequest {
      requestId :: Int,
      requestContent  :: String
    } deriving (GHC.Generics.Generic, Show)

data MyResponse = MyResponse {
      method :: String,
      protocol  :: String,
      host  :: String,
      path  :: String,
      ip  :: String
    } deriving (GHC.Generics.Generic, Show)

instance Data.Aeson.ToJSON MyRequest where
instance Data.Aeson.FromJSON MyResponse where

main :: IO ()
main = do
    -- prepare request body, from object to ByteString
    let requestBody = Data.Aeson.encode (MyRequest {requestId = 100, requestContent = "this is content"})
    putStrLn "--------------------------------------------------"
    putStrLn "Request body: "
    Data.ByteString.Lazy.Char8.putStrLn requestBody

    -- prepare http request, add url (a public echo server) and request body
    rawRequest <- Network.HTTP.Simple.parseRequest "POST https://echo.free.beeceptor.com"
    let request = Network.HTTP.Simple.setRequestBodyLBS requestBody rawRequest

    -- send http request
    putStrLn "Sending request..."
    response <- Network.HTTP.Simple.httpLBS request

    -- receive raw http response
    let responseBody = Network.HTTP.Simple.getResponseBody response
    putStrLn "--------------------------------------------------"
    putStrLn $ "Received response, response code: " ++ show (Network.HTTP.Simple.getResponseStatusCode response)
    putStrLn "Raw response:"
    Data.ByteString.Lazy.Char8.putStrLn responseBody

    putStrLn "--------------------------------------------------"
    putStrLn "Parsed response:"
    -- parse http response body to data type MyResponse
    let myResponse = Data.Aeson.decode responseBody :: Maybe MyResponse
    case myResponse of 
        Nothing -> putStrLn "Failed to parse json responses"
        Just res -> do
                        putStrLn $ "method: " ++ method res
                        putStrLn $ "protocol: " ++ protocol res
                        putStrLn $ "host: " ++ host res
                        putStrLn $ "origin ip: " ++ ip res