{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Security.Repr where

import           Security.Sensitivity
import           Security.CodeGen.Types
import           Security.Sing

import           Data.Proxy

data Ptr a

data CType a where
  CInt :: CType Int
  CBool :: CType Bool
  CPtr :: forall a. CType a -> CType (Ptr a)

deriving instance Show (CType a)

class Repr a where
  typeRepr :: CType a -> String
  ctype :: CType a
  lit :: a -> String

ctypeProxy :: Repr a => proxy a -> CType a
ctypeProxy _ = ctype

ptrTypeRepr :: Repr a => CType (Ptr a) -> String
ptrTypeRepr (CPtr ty) = typeRepr ty

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

instance Repr a => Repr (Name s a) where

genDecl :: forall (s :: Sensitivity) a. (Repr a) => CType a -> Name s a -> String
genDecl ty n =
  case nameSens n of
    PublicSing -> leftPart
    SecretSing -> unwords [leftPart, "__attribute__((nospec))__"]
  where
    leftPart = unwords [typeRepr ty, emitName n]


  -- case sing :: Sing s of
  -- case toSing @s undefined of
  --   SomeSing someSing -> undefined
      -- case fromSing (sing :: Sing s) of
      --   _ -> undefined

-- class GenDecl s where
--   genDecl :: (Repr a) => CType a -> Name s a -> String

-- instance GenDecl Public where
--   genDecl ty n = unwords [typeRepr ty, emitName n]

-- instance GenDecl Secret where
--   genDecl ty n = unwords [typeRepr ty, emitName n, "__attribute__((nospec))__"]

