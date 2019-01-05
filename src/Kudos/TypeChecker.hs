-- TypeChecker.hs ---
-- Copyright (C) 2018 Hussein Ait-Lahcen
-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.
module Kudos.TypeChecker
  ( justice
  , typecheck
  ) where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.HashMap.Strict      as H

import           Kudos.Config
import           Kudos.Error
import           Kudos.Normalizer
import           Kudos.Syntax             as S

justice :: System -> System -> Bool
justice x y = normalize x == normalize y

liftUniverse :: Universe -> Universe
liftUniverse = (+ 1)

typeUniverse :: Universe
typeUniverse = 1

kindUniverse :: Universe
kindUniverse = liftUniverse typeUniverse

bindIdentifier :: HasBindings t => Quantifier -> System -> t -> t
bindIdentifier q s =
  case q of
    (Abstraction n)    -> inject n
    (Product (Just n)) -> inject n
    _                  -> id
  where
    inject n =
      bindings %~ Bindings . fmap (shift n 1) . H.insert (n, 0) s . unBindings

typecheck ::
     ( HasConfiguration env
     , HasEnvironment env
     , HasBindings env
     , AsTypeError err
     , MonadReader env m
     , MonadError err m
     , Show env
     )
  => System
  -> m System
typecheck (SStar u) = pure $ SStar $ liftUniverse u
typecheck (SVar n i) = do
  binds <- view bindings
  case H.lookup (n, i) (unBindings binds) of
    Just s  -> pure s
    Nothing -> throwing _UnknowBinding (n, binds)
typecheck (SQuant q@(Abstraction n) l r) = go =<< typecheck l
  where
    go (SStar _) = do
      let lnorm = normalize l
      r' <- local (bindIdentifier q lnorm) (typecheck r)
      pure $ SQuant (Product (Just n)) lnorm r'
    go _ = throwing _InvalidAbstraction l
typecheck (SQuant q@(Product _) l r) = do
  l' <- typecheck l
  r' <- local (bindIdentifier q (normalize l)) (typecheck r)
  h <- view hierarchy
  go l' r' h
  where
    go (SStar u) (SStar v) Predicative = pure $ SStar $ max u v
    go (SStar u) (SStar v) Impredicative
      -- Impredicative only for low levels to avoid paradoxes
      | u <= kindUniverse && v <= kindUniverse = pure $ SStar v
      | otherwise = pure $ SStar $ max u v
    go _ _ _ = throwing _InvalidProduct (l, r)
typecheck (SApp f x) = go =<< typecheck f
  where
    go (SQuant (Product mn) l r) = do
      x' <- typecheck x
      if justice l x'
        then let shiftNamed n = shift n (-1) . substitute n 0 (shift n 1 x)
              in pure $ normalize $ maybe id shiftNamed mn r
        else throwing _Unfairness (normalize l, normalize x')
    go f' = throwing _InvalidApplication (f, f', x)
