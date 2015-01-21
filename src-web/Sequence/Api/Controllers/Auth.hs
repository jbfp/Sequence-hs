{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Controllers.Auth where

import Control.Applicative
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Crypto.PasswordStore
import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe (isJust, isNothing)
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
import Sequence.Api.Models.User
import Web.JWT hiding (header)
import Web.Scotty.Trans

type UserList = MVar [User]
type Authorizer = ActionT ErrorResult IO User

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
    post "/register" $ register key users
    post "/token" $ getToken key users        

register :: Secret -> UserList -> ActionT ErrorResult IO ()
register key users = do    
    (RegisterUserRequest uname ea pass) <- jsonData
    
    maybeUser <- liftIO $ getUserByNameIO uname users
    when (isJust maybeUser) (raise $ BadRequest "User already exists.")
    
    userId <- liftIO nextRandom
    salt <- liftIO genSaltIO
    user <- case mkUser userId uname ea salt pass of
        Left err -> (raise . BadRequest . show) err 
        Right u -> return u                 
    liftIO $ modifyMVar_ users (\us -> return $ user : us)
    status status201
    liftIO (mkJWT key user) >>= json

getToken :: Secret -> UserList -> ActionT ErrorResult IO ()
getToken key users = do
    -- Verify user exists.
    tokenRequest <- jsonData
    let name = userName tokenRequest
    user <- getUserByNameOrUnauthorized name users
            
    -- Confirm password is correct.
    if verifyPassword ((TE.encodeUtf8 . pwd) tokenRequest) (unPassword user)
    then liftIO (mkJWT key user) >>= json
    else raise $ UnauthorizedMessage "Password is incorrect."
       
mkJWT :: Secret -> User -> IO JWTResponse
mkJWT key user = do
    now <- liftIO getPOSIXTime
    let issuedAt = now - 60 -- Avoid rounding errors, sometimes requests following the "getToken" request would be denied because they were too "early".
    let expires = issuedAt + 2629743 -- Expires in 1 month (30.44 days) 
    let cs = def {
        iss = stringOrURI "Foo",
        iat = intDate issuedAt,
        exp = intDate expires,
        sub = (stringOrURI . unUserName) user }
    let token = encodeSigned HS256 key cs
    return JWTResponse { accessToken = token, expiresIn = round (expires - issuedAt) }
        
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
        getUserByNameOrUnauthorized subject users
        
getUserByNameOrUnauthorized :: Text -> UserList -> ActionT ErrorResult IO User
getUserByNameOrUnauthorized name users = do
    maybeUser <- liftIO $ getUserByNameIO name users
    case maybeUser of
        Nothing -> raise Unauthorized
        Just u -> return u

getUserByNameIO :: Text -> UserList -> IO (Maybe User)
getUserByNameIO name users =
    liftIO $ withMVar users (return . getUserByName name)    

getUserByName :: Text -> [User] -> Maybe User
getUserByName name = find (\usr -> name == unUserName usr)