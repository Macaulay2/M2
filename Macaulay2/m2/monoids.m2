--		Copyright 1994 by Daniel R. Grayson

Monoid = new Type of Type

ZZ _ Monoid := (i,M) -> (
     if i === 1 then M#1
     else error "expected integer to be 1"
     )

baseName Symbol := identity

OrderedMonoid = new Type of Monoid
degreeLength OrderedMonoid := M -> M.degreeLength
     
