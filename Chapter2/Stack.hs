{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chapter2.Stack (
    module Chapter2.Stack
  ) where

import Data.Kind

class Stack (f :: Type -> Type) (a :: Type) where
  empty   :: f a
  isEmpty :: f a -> Bool
  cons    :: a       -> f a -> f a
  head    :: f a -> Maybe a
  tail    :: f a -> f a

instance Stack [] a where
  empty = []
  isEmpty = null
  cons = (:)
  head = headMaybe
  tail = Prelude.tail

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (a:_) = Just a

--class Stack (a :: Type) where
  --data T a :: Type
  --empty   :: T a
  --isEmpty :: T a -> Bool
  --cons    :: a   -> T a -> T a
  --head    :: T a -> Maybe a
  --tail    :: T a -> T a

--instance Stack a where
  --data T a = List ([] a)
  --empty = List []
  --isEmpty (List xs) = null xs
  --cons a (List as) = List (a : as)
  --head (List xs) = headMaybe xs
  --tail (List xs) = List (Prelude.tail xs)

--example :: T Int
--example =
  --cons 1 $
  --cons 2 $
  --cons 3 $
  --cons 4 $
  --empty
