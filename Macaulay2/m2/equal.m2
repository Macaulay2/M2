--		Copyright 1993-1999 by Daniel R. Grayson

-- this stuff should get into the kernel

List == List := (x,y) -> (
     # x === # y
     and
     class x === class y
     and
     all( 0 .. # x - 1, i -> x#i == y#i ))

Sequence == Sequence := (x,y) -> (
     # x === # y
     and
     all( 0 .. # x - 1, i -> x#i == y#i ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
