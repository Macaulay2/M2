--		Copyright 1993-1999 by Daniel R. Grayson

List ? List := (s,t) -> if class s === class t then toSequence s ? toSequence t else (class s) ? (class t)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
