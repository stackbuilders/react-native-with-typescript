{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Lib (Expr) where

import           Data.Extensible.Sum1

data Add a = Add a a

data Prod a = Prod a a

data Boolean a  = Boolean Bool a

type Expr = Add :||: Prod :||: Boolean
