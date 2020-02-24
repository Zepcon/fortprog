module SLD where

  import Type

  data SLDTree = SLDTree [Rule] Goal
   deriving Show
