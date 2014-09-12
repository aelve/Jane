{-# LANGUAGE PackageImports, TemplateHaskell, RankNTypes,
             NoImplicitPrelude, OverloadedStrings,
             MultiWayIf, ScopedTypeVariables #-}

module Lex where

import ClassyPrelude

import Helpers

data Token =
 
lex :: Text -> [((Int, Int), Token)]
