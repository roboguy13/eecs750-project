{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Security.Repr where

import           Security.Sensitivity
import           Data.Proxy

data ArraySize = StaticSize Int | DynamicSize Int

data Ptr a
data Array a

data CType a where
  CInt :: CType Int
  CPtr :: forall a. CType a -> CType (Ptr a)
  CArray :: forall a. ArraySize -> CType a -> CType (Array a)

data MarkedType s a where
  SecretType :: forall a. CType a -> MarkedType Secret a
  PublicType :: forall a. CType a -> MarkedType Public a

unMarkType :: MarkedType s a -> CType a
unMarkType (SecretType ty) = ty
unMarkType (PublicType ty) = ty

markAttr :: MarkedType s a -> String
markAttr (SecretType {}) = "__attribute__((nospec))__"
markAttr (PublicType {}) = ""


class Repr a where
  typeReprSplit :: CType a -> (String, String)


instance Repr Int where
  typeReprSplit m = ("int", "")

instance Repr a => Repr (Ptr a) where
  typeReprSplit (CPtr ty) =
    let (pre, post) = typeReprSplit ty
    in
    (pre <> "*", post)

instance Repr a => Repr (Array a) where
  typeReprSplit (CArray (StaticSize n) ty) =
    let (pre, post) = typeReprSplit ty
    in
    (pre, post <> "[" ++ show n ++ "]")

  typeReprSplit (CArray (DynamicSize _) ty) =
    typeReprSplit (CPtr ty)

