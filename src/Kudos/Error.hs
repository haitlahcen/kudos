-- Error.hs ---
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
{-# LANGUAGE TemplateHaskell #-}

module Kudos.Error
  ( TypeError(..)
  , AsTypeError(..)
  ) where

import           Control.Lens

import           Data.Text
import           Kudos.Config
import           Kudos.Pretty
import           Kudos.Syntax

data TypeError
  = Unfairness System
               System
  | InvalidApplication System
                       System
                       System
  | InvalidAbstraction System
  | InvalidProduct System
                   System
  | UnknowBinding Identifier
                  Bindings

instance Show TypeError where
  show (Unfairness x y) =
    "Unfairness:\nExpected: " <> unpack (pretty x) <> "\nCurrent: " <> unpack (pretty y)
  show (InvalidApplication f f' x) =
    "InvalidApplication:"
    <> "\nRaw Function: " <> unpack (pretty f)
    <> "\nTyped Function: " <> unpack (pretty f')
    <> "\nRaw Input: " <> unpack (pretty x)
  show (InvalidProduct x y) =
    "InvalidProduct :\n" <> unpack (pretty x) <> "\n" <> unpack (pretty y)
  show (UnknowBinding i b) =
    "UnknownBinding :\n" <> unpack i <> "\n" <> show b
  show (InvalidAbstraction x) =
    "InvalidProduct :\n" <> unpack (pretty x)

makeClassyPrisms ''TypeError
