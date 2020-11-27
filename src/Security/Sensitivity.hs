{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.Sensitivity where

import           GHC.TypeLits
import           Data.Proxy
import           Security.Sing

data Sensitivity = Public | Secret

data instance Sing (a :: Sensitivity) where
  PublicSing :: Sing Public
  SecretSing :: Sing Secret

instance SingI Public where
  sing = PublicSing

instance SingI Secret where
  sing = SecretSing

instance SingKind Sensitivity where
  type Demote Sensitivity = Sensitivity

  fromSing PublicSing = Public
  fromSing SecretSing = Secret

  toSing Public = SomeSing PublicSing
  toSing Secret = SomeSing SecretSing

  singInstance PublicSing = SingInstance
  singInstance SecretSing = SingInstance


type family Max sA sB where
  Max Public Public = Public
  Max sA     sB     = Secret

type family Min sA sB where
  Min Secret Secret = Secret
  Min sA     sB     = Public

type family (:<=) sA sB :: Bool where
  (:<=) Public sB     = 'True
  (:<=) Secret Secret = 'True
  (:<=) Secret Public = 'False


