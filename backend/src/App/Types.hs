{-# LANGUAGE TypeOperators #-}

module App.Types (App, AppState(..), API) where

import           Control.Monad.Except               (ExceptT)
import           Control.Monad.Reader               (ReaderT)
import           Docs.API                           (DocsAPI)
import           Domains.User.API                   (UserAPI)
import           Domains.User.Types                 (User)
import           GHC.Conc                           (TVar)
import           Servant                            ((:<|>))
import           Servant.Server.Internal.ServantErr (ServantErr)

newtype AppState = AppState { users :: TVar [User] }

type App = ReaderT AppState (ExceptT ServantErr IO)

type API = DocsAPI :<|> UserAPI
