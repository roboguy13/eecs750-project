{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Security.Sensitivity where

import           GHC.TypeLits

data Sensitivity = Public | Secret

data family Sing (a :: k)

class SingI a where
  sing :: Sing a

data instance Sing Public = PublicSing
data instance Sing Secret = SecretSing

instance SingI Public where sing = PublicSing
instance SingI Secret where sing = SecretSing

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


