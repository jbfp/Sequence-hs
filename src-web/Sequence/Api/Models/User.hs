{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Models.User
( User
, unUserId
, unUserName
, unEmailAddress
, unPassword
, UserError (..)
, mkUser
) where

import Control.Applicative ((<$>), (<*>))
import Crypto.PasswordStore (makePasswordSalt, Salt)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.UUID (UUID)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString as BS

data User = UserT
    { unUserId :: UUID
    , unUserName :: DT.Text
    , unEmailAddress :: DT.Text
    , unPassword :: BS.ByteString }

instance ToJSON User where
    toJSON (UserT uid uname ea _) =
        object [ "id" .= show uid
               , "userName" .= uname
               , "emailAddress" .= ea ]

data UserError = EmptyUserName
               | InvalidEmailAddress
               | PasswordTooShort

instance Show UserError where
    show EmptyUserName = "User name is empty."
    show InvalidEmailAddress = "The given email address is not valid."
    show PasswordTooShort = "The password is too short."

mkUser :: UUID -> DT.Text -> DT.Text -> Salt -> DT.Text -> Either UserError User
mkUser userId userName emailAddress salt password =        
    UserT userId
    <$> validateUserName userName
    <*> validateEmailAddress emailAddress
    <*> (fmap (hashAndSalt salt) . validatePassword) password
    
validateUserName :: DT.Text -> Either UserError DT.Text
validateUserName name
    | DT.null name = Left EmptyUserName
    | otherwise = Right name
    
validateEmailAddress :: DT.Text -> Either UserError DT.Text
validateEmailAddress ea
    | '@' `notElem` DT.unpack ea = Left InvalidEmailAddress
    | otherwise = Right ea
    
validatePassword :: DT.Text -> Either UserError DT.Text
validatePassword password
    | DT.length password < 6 = Left PasswordTooShort
    | otherwise = Right password
    
hashAndSalt :: Salt -> DT.Text -> BS.ByteString
hashAndSalt salt password =
    let strength = 12
        encoded = DTE.encodeUtf8 password
    in makePasswordSalt encoded salt strength