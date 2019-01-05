-- Main.hs ---
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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

import           Kudos.Config
import           Kudos.Error
import           Kudos.Normalizer
import           Kudos.Parser
import           Kudos.Pretty
import           Kudos.Syntax
import           Kudos.TypeChecker

import qualified Data.Text.IO           as TIO

main :: IO ()
main = do
  expression <-
    parseFromFileEx (runUnspaced parseSystem <* spaces <* eof) "./expression1.k"
  case expression of
    (Success system) ->
      let typeChecked =
            runIdentity $
            runExceptT (runReaderT (typecheck system) defaultEnvironment) :: Either TypeError System
       in do putStrLn "Base system:"
             TIO.putStrLn $ pretty system
             putStrLn ""
             case typeChecked of
               (Right typedSystem) -> do
                 putStrLn "Type of the system:"
                 TIO.putStrLn $ pretty $ normalize typedSystem
               (Left typeError) -> do
                 putStrLn "Type error:"
                 print typeError
             putStrLn ""
             putStrLn "Normalized system:"
             TIO.putStrLn $ pretty $ normalize system
    (Failure e) -> print e
