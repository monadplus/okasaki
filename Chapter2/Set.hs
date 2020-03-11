{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chapter2.Set (
    Set(..)
  ) where

import Data.Kind

class Set (t :: Type -> Type) (a :: Type) where
  empty  :: t a
  insert :: a    -> t a -> t a
  member :: a    -> t a -> Bool
