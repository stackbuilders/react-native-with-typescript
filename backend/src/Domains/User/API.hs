{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Domains.User.API
       ( UsersRoutes(..)
       , UserRoutes(..)
       , UserAPI
       ) where

import           Domains.User.Types  (AddUser, UpdateUser, User)
import           Numeric.Natural     (Natural)
import           Servant
import           Servant.API.Generic

newtype UsersRoutes route = UsersRoutes
  { getUsers :: route :- Get '[JSON] [User] }
  deriving Generic

data UserRoutes route = UserRoutes
  { getUser    :: route :- UserId :> Get '[JSON] User
  , addUser    :: route :- ReqBody '[JSON] AddUser :> Post '[JSON] NoContent
  , updateUser :: route
      :- UserId :> ReqBody '[JSON] UpdateUser :> Post '[JSON] NoContent
  , deleteUser :: route :- UserId :> Delete '[JSON] NoContent
  } deriving Generic

type UserId = Capture "user-id" Natural

type UserAPI =
        "users" :> ToServantApi UsersRoutes
  :<|>  "user" :> ToServantApi UserRoutes
