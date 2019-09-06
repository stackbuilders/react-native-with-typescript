{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs.Types (docsApiProxy) where

import           Domains.User.API
import           Domains.User.Types
import           Numeric.Natural    (Natural)
import           Servant
import           Servant.Docs       hiding (Normal)

instance ToSample User where
  toSamples _ = samples users
    where users =
            [ User 1 "Esteban" 27 [Baseball] Normal
            , User 2 "Xavier" 50 [] Admin
            ]

instance ToSample AddUser where
  toSamples _ = singleSample u
    where u = AddUser "Francisco" 20 [Soccer] Normal

instance ToSample UpdateUser where
  toSamples _ = singleSample u
    where u = UpdateUser 25 []

instance ToCapture (Capture "user-id" Natural) where
  toCapture _ = DocCapture "user-id" "Id of the user to search"

userApiProxy :: Proxy UserAPI
userApiProxy = Proxy

docsApiProxy :: API
docsApiProxy = docs . pretty $ userApiProxy
