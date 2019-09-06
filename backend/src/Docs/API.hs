{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Docs.API
       ( DocsRoutes(..)
       , DocsAPI
       ) where

import           Data.Text                    (Text)
import           GHC.Generics
import           Servant
import           Servant.API.Generic

newtype DocsRoutes route = DocsRoutes
  { getDocs :: route :- Get '[PlainText] Text}
  deriving Generic

type DocsAPI = "docs" :> ToServantApi DocsRoutes
