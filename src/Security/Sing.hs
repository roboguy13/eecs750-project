-- Lightweight singletons, since we don't need the full power of the
-- 'singletons' package (which would be a significantly large dependency).
--
-- Based on "Dependently Typed Programming with
-- Singletons" by Richard A. Eisenberg and Stephanie Weirich

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Security.Sing
  where

import           Data.Proxy

data family Sing (a :: k)

class SingI (a :: k) where
  sing :: Sing a

withSing :: SingKind k => Sing (a :: k) -> (SingI (a :: k) => r) -> r
withSing s x =
  case singInstance s of
    SingInstance -> x

class SingKind k where
  type Demote k
  fromSing :: Sing (a :: k) -> Demote k
  toSing :: Demote k -> SomeSing k
  singInstance :: Sing (a :: k) -> SingInstance a
  -- singInstance :: Proxy (a :: k) -> SingInstance a

data SingInstance (a :: k) where
  SingInstance :: SingI a => SingInstance a

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

