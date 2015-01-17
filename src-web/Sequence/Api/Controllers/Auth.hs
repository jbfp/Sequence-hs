{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Controllers.Auth where

import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding (json)
import Data.List
import Data.Text hiding (find)
import Data.Time.Clock.POSIX
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics
import Network.HTTP.Types
import Prelude hiding (exp) -- exp is used for expiration of a JWT from Web.JWT.
import Sequence.Api.Error
import Web.JWT
import Web.Scotty.Trans

type UserList = MVar [User]

data User = User
    { unUserId :: UUID
    , unUserName :: String
    , unEmailAddress :: String
    , unPassword :: String } -- TODO: Use hashing + salt!

data RegisterUserRequest = RegisterUserRequest
    { desiredUserName :: String
    , emailAddress :: String
    , password :: String
    , confirmPassword :: String
    } deriving (Generic)
    
instance FromJSON RegisterUserRequest

data TokenRequest = TokenRequest
    { userName :: String
    , pwd :: String
    } deriving (Generic)

instance FromJSON TokenRequest

data JWTResponse = JWTResponse
    { accessToken :: Text    
    , expiresIn ::  Int }

instance ToJSON JWTResponse where
    toJSON jwt =
        object [ "access_token" .= accessToken jwt
               , "token_type"   .= pack "bearer"
               , "expires_in"   .= (show . expiresIn) jwt ]

auth :: UserList -> ScottyT ErrorResult IO ()
auth users = do
    post "/register" $ register users
    post "/token" $ getToken users        

register :: UserList -> ActionT ErrorResult IO ()
register users = do
    requestBodyJson <- jsonData
    registerUserRequest <-
        if password requestBodyJson /= confirmPassword requestBodyJson
        then raise $ BadRequest "The passwords don't match."
        else return requestBodyJson
    userId <- liftIO nextRandom 
    let user = User { unUserId = userId
                    , unUserName = desiredUserName registerUserRequest
                    , unEmailAddress = emailAddress registerUserRequest
                    , unPassword = password registerUserRequest }
    liftIO $ modifyMVar_ users (\us -> return $ user : us)
    status status201
    
getToken :: UserList -> ActionT ErrorResult IO ()
getToken users = do
    -- Verify user exists.
    tokenRequest <- jsonData
    let name = userName tokenRequest
    user <- do
        maybeUser <- liftIO $ withMVar users (return . find (\usr -> name == unUserName usr))
        case maybeUser of
            Nothing -> raise Unauthorized
            Just u -> return u
            
    -- Confirm password is correct.
    -- TODO: Compare hashes, not plain-text.    
    if unPassword user /= pwd tokenRequest
    then raise Unauthorized
    else do 
        now <- liftIO getPOSIXTime
        let issuedAt = now
        let expires = issuedAt + 2629743 -- Expires in 1 month (30.44 days) 
        let cs = def {
            iss = stringOrURI "Foo",
            iat = intDate issuedAt,
            exp = intDate expires,
            sub = (stringOrURI . pack . unUserName) user }
        let token = encodeUnsigned cs
        let jwt = JWTResponse { accessToken = token, expiresIn = round (expires - issuedAt) }
        json jwt