-- Normalizer.hs ---
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
{-# LANGUAGE OverloadedStrings #-}

module Kudos.Normalizer
  ( shift
  , substitute
  , normalize
  ) where

import           Kudos.Syntax

shift :: Identifier -> Index -> System -> System
shift n' k = go 0
  where
    go i' v@(SVar n i)
      | n == n' && i >= i' = SVar n (i + k)
      | otherwise = v
    go i' (SQuant q l r) =
      let i'' =
            case q of
              (Abstraction n)
                | n == n' -> i' + 1
              (Product (Just n))
                | n == n' -> i' + 1
              _ -> i'
       in SQuant q (go i' l) (go i'' r)
    go i' (SApp f a) = SApp (go i' f) (go i' a)
    go _ x = x

substitute :: Identifier -> Index -> System -> System -> System
substitute n' i' v' v@(SVar n i)
  | n == n' && i == i' = v'
  | otherwise = v
substitute n' i' v' (SQuant q l r) =
  case q of
    (Abstraction n) -> go (Just n)
    (Product mn)    -> go mn
  where
    go (Just n) =
      let i'' =
            if n == n'
              then i' + 1
              else i'
          v'' = shift n 1 v'
       in SQuant q (substitute n' i' v' l) (substitute n' i'' v'' r)
    go Nothing = SQuant q (substitute n' i' v' l) (substitute n' i' v' r)
substitute n' i' v' (SApp f x) =
  SApp (substitute n' i' v' f) (substitute n' i' v' x)
substitute _ _ _ x = x

normalize :: System -> System
normalize (SQuant q l r) = SQuant q (normalize l) (normalize r)
normalize (SApp f x) =
  let x' = normalize x
   in case normalize f of
        (SQuant (Abstraction n) _ r) ->
          normalize $ shift n (-1) $ substitute n 0 (shift n 1 x') r
        f' -> SApp f' x'
normalize x = x
