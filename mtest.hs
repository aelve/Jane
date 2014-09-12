{-# LANGUAGE
  TypeFamilies, DataKinds, TypeOperators, ConstraintKinds #-}

import GHC.Exts

blah :: Is '[Read, Show, Num] a => (a -> Int) -> Int
blah f = f 3

type family Is (cs :: [* -> Constraint]) (x :: *) :: Constraint where
  Is (c ': cs) x = (c x, Is cs x)
  Is '[]       x = ()
