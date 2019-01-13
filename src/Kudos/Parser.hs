-- Parser.hs ---
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
{-# LANGUAGE RankNTypes        #-}

module Kudos.Parser
  ( parseSystem
  , module Text.Trifecta
  , module Text.Trifecta.Result
  , module Text.Trifecta.Combinators
  , module Text.Parser.Token.Highlight
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import qualified Data.HashSet                as H
import           Text.Parser.Token.Highlight
import           Text.Trifecta
import           Text.Trifecta.Combinators
import           Text.Trifecta.Result

import           Kudos.Syntax

type MonadParsing m a
   = (Monad m, TokenParsing m, DeltaParsing m) =>
       Unspaced m a

style :: CharParsing m => IdentifierStyle m
style =
  IdentifierStyle
    { _styleName = "Identifier"
    , _styleStart = letter
    , _styleLetter = alphaNum
    , _styleReserved = H.fromList ["Lam", "Pi", "let", "in"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

parseVar :: MonadParsing m System
parseVar =
  (do n <- highlight Identifier (ident style)
      i <-
        highlight
          Number
          (reserveText style "@" *> (fromIntegral <$!> natural) <|> pure 0)
      pure (SVar n i)) <?>
  "Variable: x(@i)"

parseLit :: MonadParsing m System
parseLit = (SLit <$!> highlight Number (LNat <$!> natural)) <?> "Literal"

parseType :: MonadParsing m System
parseType = (SType <$!> (reserveText style "Nat" $> TNat)) <?> "Type"

parseStar :: MonadParsing m System
parseStar =
  SStar <$!> highlight Symbol (length <$!> some (reserveText style "*")) <?>
  "Universe"

{-
 let a = b in
 let c = a in
 a + c

-}
parseLet :: MonadParsing m System
parseLet =
  (do _ <- reserveText style "let"
      _ <- spaces
      binds <- parseBinding `sepBy` (spaces *> symbolic ',' <* spaces)
      _ <- spaces
      _ <- reserveText style "in"
      _ <- spaces
      body <- parseSystem
      _ <- spaces
      pure $ foldl goApp (foldr goLam body binds) binds) <?>
  "Binding: Let x = y, a = b in z"
    -- lambda are nested
  where
    goLam (n, l, _) s = SQuant (Abstraction n) l s
    -- application are sequenced
    goApp s (_, _, r) = SApp s r

parseBinding :: MonadParsing m (Identifier, System, System)
parseBinding =
  (do n <- highlight Identifier (ident style)
      _ <- spaces
      _ <- highlight ReservedConstructorOperator colon
      _ <- spaces
      l <- parseSystem
      _ <- spaces
      _ <- highlight ReservedConstructorOperator (symbolic '=')
      _ <- spaces
      r <- parseSystem
      _ <- spaces
      pure (n, l, r)) <?>
  "Binding: x = y"

parseLam :: MonadParsing m System
parseLam =
  (do _ <- reserveText style "Lam"
      _ <- spaces
      n <- highlight Identifier (ident style)
      _ <- spaces
      _ <- highlight ReservedConstructorOperator colon
      _ <- spaces
      l <- parseSystem
      _ <- spaces
      _ <- highlight ReservedConstructorOperator dot
      _ <- spaces
      r <- parseSystem
      pure (SQuant (Abstraction n) l r)) <?>
  "Lambda: λx:T.y"

parsePi :: MonadParsing m System
parsePi =
  (do _ <- reserveText style "Pi"
      _ <- spaces
      n <- highlight Identifier (ident style)
      _ <- spaces
      _ <- highlight ReservedConstructorOperator colon
      _ <- spaces
      l <- parseSystem
      _ <- spaces
      _ <- highlight ReservedConstructorOperator dot
      _ <- spaces
      r <- parseSystem
      pure (SQuant (Product (Just n)) l r)) <?>
  "Product: Πx:T.y"

parseArrow :: MonadParsing m System
parseArrow =
  chainr1
    (spaces *> parseSubSystem)
    (space *> reserveText style "->" $> SQuant (Product Nothing)) <?>
  "Product arrow: T -> T"

parseApp :: MonadParsing m System
parseApp =
  chainl1 (spaces *> parseSubSystem) (spaces $> SApp) <?> "Application: f x"

parseSystem :: MonadParsing m System
parseSystem = try parseApp <|> try parseArrow <|> parseSubSystem

parseSubSystem :: MonadParsing m System
parseSubSystem =
  parens parseSystem <|> parseLet <|> parsePi <|> parseLam <|> parseType <|>
  parseLit <|>
  parseStar <|>
  parseVar
