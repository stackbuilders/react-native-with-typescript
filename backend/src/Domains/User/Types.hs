{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Domains.User.Types
       ( User(..)
       , AddUser(..)
       , UpdateUser(..)
       , Hobby(..)
       , UserType(..)
       ) where

import           Data.Aeson            (FromJSON (..), ToJSON (..),
                                        Value (String), withText)
import           Data.Aeson.TH         (deriveJSON)
import           Data.Text             (Text)
import           Domains.Serialization (options)
import           Numeric.Natural       (Natural)

data User = User
  { uId      :: !Natural
  , uName    :: !Text
  , uAge     :: !Natural
  , uHobbies :: ![Hobby]
  , uType    :: !UserType
  }

data Hobby
  = Soccer
  | Basketball
  | Baseball
  | Other Text

data UserType
  = Admin
  | Normal

data AddUser = AddUser
  { auName    :: !Text
  , auAge     :: !Natural
  , auHobbies :: ![Hobby]
  , auType    :: !UserType
  }

data UpdateUser = UpdateUser
  { uuAge     :: !Natural
  , uuHobbies :: ![Hobby]
  }

-- | AESON instances
$(deriveJSON (options 1) ''User)
$(deriveJSON (options 2) ''AddUser)
$(deriveJSON (options 2) ''UpdateUser)

instance FromJSON UserType where
  parseJSON = withText "user type" $ \case
    "admin"  -> pure Admin
    "normal" -> pure Normal
    _        -> fail "User type not recognized"

instance ToJSON UserType where
  toJSON = \case
    Admin  -> String "admin"
    Normal -> String "normal"

instance FromJSON Hobby where
  parseJSON = withText "hobby" $ \case
    "soccer"     -> pure Soccer
    "basketball" -> pure Basketball
    "baseball"   -> pure Baseball
    other        -> pure $ Other other

instance ToJSON Hobby where
  toJSON = \case
    Soccer      -> String "soccer"
    Basketball  -> String "basketball"
    Baseball    -> String "baseball"
    Other other -> String other
