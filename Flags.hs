{-# LANGUAGE
  DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}


module Flags (
  module Flagged,
  Quit(..) )
  where


import Flagged


makeFlags ["Quit"]
