--		Copyright 1994 by Daniel R. Grayson

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
     class x === class y
     and
     all( 0 .. # x - 1, i -> x#i == y#i ))
