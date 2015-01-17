{-# LANGUAGE OverloadedStrings #-}

module Sequence.Api.Error where

import Data.Text.Lazy
import Network.HTTP.Types
import Web.Scotty.Trans

data ErrorResult = BadRequest String
                 | Unauthorized
                 | NotFound
                 | InternalServerError String
                 deriving (Show, Eq)

instance ScottyError ErrorResult where
    stringError = InternalServerError
    showError = pack . show

handleErrorResult :: Monad m => ErrorResult -> ActionT ErrorResult m ()
handleErrorResult (BadRequest err)          = do status status400; json err
handleErrorResult  Unauthorized             = status status401
handleErrorResult  NotFound                 = status status404
handleErrorResult (InternalServerError err) = do status status500; json err