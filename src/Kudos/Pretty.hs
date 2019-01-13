-- Pretty.hs ---
-- Copyright (C) 2019 Hussein Ait-Lahcen
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
module Kudos.Pretty
  ( pretty
  ) where

import           Data.Text    (Text, pack, unpack)
import           Kudos.Syntax

pretty :: System -> Text
pretty = pack . go
  where
    go (SVar n 0) = unpack n
    go (SVar n i) = unpack n <> "@" <> show i
    go (SLit (LNat x)) = show x
    go (SType TNat) = "Nat"
    go (SStar 1) = "✶"
    go (SStar 2) = "☐"
    go (SStar x) = replicate x '↑'
    go (SApp f@(SQuant (Abstraction _) _ _) x) = "(" <> go f <> ") " <> go x
    go (SApp f x@(SQuant (Product _) _ _)) = "(" <> go f <> ") " <> go x
    go (SApp f x@(SQuant (Abstraction _) _ _)) = go f <> "\n(" <> go x <> ")"
    go (SApp f x@(SApp _ _)) = go f <> " (" <> go x <> ")"
    go (SApp f x) = go f <> " " <> go x
    go (SQuant (Product Nothing) l@(SQuant (Product _) _ _) r) =
      "(" <> go l <> ") → " <> go r
    go (SQuant (Product Nothing) l r) = go l <> " → " <> go r
    go (SQuant (Product (Just n)) l r) =
      "Π(" <> unpack n <> ":" <> go l <> ") → " <> go r
    go (SQuant (Abstraction n) l r) =
      "λ(" <> unpack n <> ":" <> go l <> ") → " <> go r
