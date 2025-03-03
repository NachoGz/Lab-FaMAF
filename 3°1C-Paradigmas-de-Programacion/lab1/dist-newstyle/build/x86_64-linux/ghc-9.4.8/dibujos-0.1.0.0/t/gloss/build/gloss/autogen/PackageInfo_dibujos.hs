{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_dibujos (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "dibujos"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Laboratorio de Programaci\243n Funcional de Paradigmas-FAMAF."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
