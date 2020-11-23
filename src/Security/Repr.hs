{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}

module Security.Repr where

import           Security.Sensitivity
import           Security.CodeGen.Types
import           Data.Proxy

data Ptr a

data CType a where
  CInt :: CType Int
  CBool :: CType Bool
  CPtr :: forall a. CType a -> CType (Ptr a)

class Repr a where
  typeRepr :: CType a -> String
  ctype :: CType a
  lit :: a -> String


instance Repr Int where
  typeRepr m = "int"
  ctype = CInt
  lit = show

instance Repr a => Repr (Ptr a) where
  typeRepr (CPtr ty) = typeRepr ty <> "*"
  ctype = CPtr ctype


instance Repr Bool where
  typeRepr CBool = "bool"
  ctype = CBool
  lit True = "true"
  lit False = "false"

instance Repr () where
  typeRepr x = case x of {}

class GenDecl s where
  genDecl :: (Repr a) => CType a -> Name s a -> String

instance GenDecl Public where
  genDecl ty n = unwords [typeRepr ty, emitName n]

instance GenDecl Secret where
  genDecl ty n = unwords [typeRepr ty, emitName n, "__attribute__((nospec))__"]

