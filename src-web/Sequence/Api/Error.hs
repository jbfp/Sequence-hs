{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Error where

import Data.Aeson hiding (json)
import Data.String (IsString)
import Data.Text.Lazy
import Network.HTTP.Types
import Web.Scotty.Trans

data ErrorResponse = ErrorResponse String

instance ToJSON ErrorResponse where
    toJSON (ErrorResponse err) = object [ "errorMessage" .= err ] 

data ErrorResult = BadRequest String
                 | Unauthorized
                 | UnauthorizedMessage String
                 | NotFound
                 | InternalServerError String
                 deriving (Show, Eq)

instance ScottyError ErrorResult where
    stringError = InternalServerError
    showError = pack . show
    
toJsonError :: Monad m => String -> ActionT ErrorResult m ()
toJsonError = json . ErrorResponse

handleErrorResult :: Monad m => ErrorResult -> ActionT ErrorResult m ()
handleErrorResult (BadRequest err)          = do status status400; toJsonError err
handleErrorResult Unauthorized              = status status401
handleErrorResult (UnauthorizedMessage err) = do status status401; toJsonError err
handleErrorResult  NotFound                 = status status404
handleErrorResult (InternalServerError err) = do status status500; toJsonError err