--		Copyright 1993-1999 by Daniel R. Grayson

odd  = x -> 1 === x%2

even = x -> 0 === x%2

zero = x -> x == 0					    -- we use == so this can apply to all types of things

numeric = method(Dispatch => Thing)
numeric Thing := identity
numeric VisibleList := x -> apply(x,numeric)
numeric Number := x -> x + 0.
numeric CC := identity
numeric RR := identity

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
