{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Domains.User.Handlers
       ( usersHandlers
       , userHandlers
       ) where

import           App.Types                          (App, AppState (..))
import           Control.Monad                      (void)
import           Control.Monad.Except               (liftIO, throwError)
import           Control.Monad.Reader               (ask)
import           Data.List                          (find)
import           Data.Maybe                         (maybe)
import qualified Domains.User.API                   as UAPI
import           Domains.User.Types                 (AddUser (..),
                                                     UpdateUser (..), User (..))
import qualified Domains.User.Types                 as User
import           GHC.Conc                           (atomically, readTVar,
                                                     readTVarIO, writeTVar)
import           GHC.Natural                        (intToNatural)
import           Numeric.Natural                    (Natural)
import           Servant.API
import           Servant.Server.Generic             (AsServerT)
import           Servant.Server.Internal.ServantErr (err404, err409, errBody)

getAllUsers :: App [User]
getAllUsers = do
  AppState users <- ask
  liftIO . readTVarIO $ users

getUser :: Natural -> App User
getUser userId =
  getSingleUser userId
    >>= maybe (throwError err404 { errBody = "User not found" }) pure

deleteUser :: Natural -> App NoContent
deleteUser userId = do
  void $ getUser userId
  updateUsersReference (filter ((== userId) . User.uId))

getSingleUser :: Natural -> App (Maybe User)
getSingleUser userId = find ((== userId) . User.uId) <$> getAllUsers

addUser :: AddUser -> App NoContent
addUser AddUser{..} = do
  appUsers <- getAllUsers
  let mUser = find ((== auName) . uName) appUsers
  case mUser of
    Nothing -> do
      let newUserId = intToNatural $ length appUsers + 1
          newUser = User newUserId auName auAge auHobbies auType
      updateUsersReference (newUser :)
    Just _  -> throwError err409 { errBody = "User already exists." }

updateUser :: Natural -> UpdateUser -> App NoContent
updateUser userId UpdateUser{..} = do
  void $ getUser userId
  let update u@(User id' name _ _ type') =
        if id' == userId
          then User id' name uuAge uuHobbies type'
          else u
  updateUsersReference (fmap update)

updateUsersReference:: ([User] -> [User]) -> App NoContent
updateUsersReference f = do
  AppState users <- ask
  liftIO . atomically $ readTVar users >>= writeTVar users . f
  pure NoContent

usersHandlers :: UAPI.UsersRoutes (AsServerT App)
usersHandlers = UAPI.UsersRoutes { getUsers = getAllUsers }

userHandlers :: UAPI.UserRoutes (AsServerT App)
userHandlers = UAPI.UserRoutes
  { UAPI.getUser    = getUser
  , UAPI.addUser    = addUser
  , UAPI.updateUser = updateUser
  , UAPI.deleteUser = deleteUser
  }
