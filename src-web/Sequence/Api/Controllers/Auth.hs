{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Controllers.Auth where

import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Crypto.PasswordStore
import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import Data.List
import Data.Text hiding (find)
import qualified Data.Text.Encoding as TE
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
    , unUserName :: Text
    , unEmailAddress :: Text
    , unPassword :: BS.ByteString }

data RegisterUserRequest = RegisterUserRequest
    { desiredUserName :: Text
    , emailAddress :: Text
    , password :: Text
    , confirmPassword :: Text
    } deriving (Generic)
    
instance FromJSON RegisterUserRequest

data TokenRequest = TokenRequest
    { userName :: Text
    , pwd :: Text
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

auth :: Text -> UserList -> ScottyT ErrorResult IO ()
auth secret users = do
    post "/register" $ register users
    post "/token" $ getToken secret users        

register :: UserList -> ActionT ErrorResult IO ()
register users = do
    requestBodyJson <- jsonData
    registerUserRequest <-
        if password requestBodyJson /= confirmPassword requestBodyJson
        then raise $ BadRequest "The passwords don't match."
        else return requestBodyJson
    userId <- liftIO nextRandom
    let strength = 12
    hashedSaltedPassword <- liftIO $ makePassword ((TE.encodeUtf8 . password) registerUserRequest) strength 
    let user = User { unUserId = userId
                    , unUserName = desiredUserName registerUserRequest
                    , unEmailAddress = emailAddress registerUserRequest
                    , unPassword = hashedSaltedPassword }
    liftIO $ modifyMVar_ users (\us -> return $ user : us)
    status status201
    
getToken :: Text -> UserList -> ActionT ErrorResult IO ()
getToken secretKey users = do
    -- Verify user exists.
    tokenRequest <- jsonData
    let name = userName tokenRequest
    user <- do
        maybeUser <- liftIO $ withMVar users (return . find (\usr -> name == unUserName usr))
        case maybeUser of
            Nothing -> raise Unauthorized
            Just u -> return u
            
    -- Confirm password is correct.
    if verifyPassword (unPassword user) ((TE.encodeUtf8 . pwd) tokenRequest)
    then raise Unauthorized
    else do 
        let key = secret secretKey
        now <- liftIO getPOSIXTime
        let issuedAt = now
        let expires = issuedAt + 2629743 -- Expires in 1 month (30.44 days) 
        let cs = def {
            iss = stringOrURI "Foo",
            iat = intDate issuedAt,
            exp = intDate expires,
            sub = (stringOrURI . unUserName) user }
        let token = encodeSigned HS256 key cs
        let jwt = JWTResponse { accessToken = token, expiresIn = round (expires - issuedAt) }
        json jwt