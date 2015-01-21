{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Controllers.Auth where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Crypto.PasswordStore
import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Text as DT hiding (find)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Word8 as W8
import GHC.Generics
import Network.HTTP.Types
import Prelude hiding (exp) -- exp is used for expiration of a JWT from Web.JWT.
import Sequence.Api.Error
import Web.JWT hiding (header)
import Web.Scotty.Trans

type UserList = MVar [User]
type Authorizer = ActionT ErrorResult IO User

data User = User
    { unUserId :: UUID
    , unUserName :: Text
    , unEmailAddress :: Text
    , unPassword :: BS.ByteString }

data RegisterUserRequest = RegisterUserRequest
    { desiredUserName :: Text
    , emailAddress :: Text
    , password :: Text
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

auth :: Secret -> UserList -> ScottyT ErrorResult IO ()
auth key users = do
    post "/register" $ register users
    post "/token" $ getToken key users        

register :: UserList -> ActionT ErrorResult IO ()
register users = do
    -- TODO: Validate email address.
    -- TODO: Validate user does not exist.
    -- TODO: Validate password requirements.
    requestBodyJson <- jsonData
    registerUserRequest <-
        if (DT.null . desiredUserName) requestBodyJson
        then raise $ BadRequest "User name is empty."
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
    
getToken :: Secret -> UserList -> ActionT ErrorResult IO ()
getToken key users = do
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
        
authorize :: Secret -> UserList -> ActionT ErrorResult IO User
authorize key users = do
    -- Verify authorization header exists.
    tl <- do    
        authorizationHeader <- header "Authorization" 
        case authorizationHeader of
            Nothing -> raise $ UnauthorizedMessage "Authorization header is required."
            Just tl -> return tl
    
    -- Verify authorization scheme is bearer, while also fighting
    -- with Haskell string types.
    let bs = (BS.concat . BSL.toChunks . TLE.encodeUtf8) tl
    let (x, y) = BS.break W8.isSpace bs
    if BS.map W8.toLower x /= "bearer"
    then raise $ UnauthorizedMessage "Authorization schema must be Bearer."
    else do
        -- Verify JWT signature.
        let txt = strip $ TE.decodeUtf8 y
        verifiedJwt <- case decodeAndVerifySignature key txt of
            Nothing -> raise $ UnauthorizedMessage "Invalid JWT."
            Just jwt -> return jwt
                
        let cs = claims verifiedJwt
        now <- liftIO getPOSIXTime
        
        -- Verify "iat" (issued at) claim. If iat is later than now, it should be rejected.        
        issuedAt <- case iat cs of
            Nothing -> raise $ UnauthorizedMessage "Missing iat from JWT."
            Just i -> return $ secondsSinceEpoch i        
        when (issuedAt > now) (raise Unauthorized)
        
        -- Verify "exp" (expiration time) claim. If exp is earlier than now, 
        -- it has expired and should be rejected.
        expiration <- case exp cs of
            Nothing -> raise $ UnauthorizedMessage "Missing exp from JWT."
            Just e -> return $ secondsSinceEpoch e
        when (expiration < now) (raise $ UnauthorizedMessage "JWT has expired.")        
        
        -- Verify user exists.
        subject <- case sub cs of        
             Nothing -> raise $ UnauthorizedMessage "Missing sub from JWT."
             Just s -> (return . pack . show) s        
        maybeUser <- liftIO $ withMVar users (return . find (\usr -> subject == unUserName usr))
        case maybeUser of
            Nothing -> raise Unauthorized
            Just u -> return u