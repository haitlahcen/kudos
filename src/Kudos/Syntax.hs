-- Syntax.hs ---
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
module Kudos.Syntax
  ( Universe
  , Index
  , Identifier
  , Quantifier(..)
  , System(..)
  ) where

import           Data.Text

type Universe = Int

type Index = Int

type Identifier = Text

data Quantifier
  = Abstraction Identifier
  | Product (Maybe Identifier)
  deriving (Show)

data System
  = SVar Identifier
         Index
  | SStar Universe
  | SApp System
         System
  | SQuant Quantifier
           System
           System
  deriving (Show)

instance Eq System where
  SVar x y == SVar x' y' = x == x' && y == y'
  SStar u == SStar u' = u == u'
  SApp f a == SApp f' a' = f == f' && a == a'
  SQuant (Abstraction _) l r == SQuant (Abstraction _) l' r' =
    l == l' && r == r'
  SQuant (Product _) l r == SQuant (Product _) l' r' = l == l' && r == r'
  _ == _ = False
