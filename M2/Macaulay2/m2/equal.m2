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

-- Thing == Thing := Boolean => (x,y) -> x === y			    
    -- this is a bit experimental, installed 7/15/99, deleted 3/20/2000
