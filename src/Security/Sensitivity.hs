module Security.Sensitivity where

data Sensitivity = Public | Secret

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

