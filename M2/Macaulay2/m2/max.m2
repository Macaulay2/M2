--		Copyright 1993-1999 by Daniel R. Grayson

InfiniteNumber = new Type of HashTable
InfiniteNumber.synonym = "infinite number"

sign := symbol sign

infinity = new InfiniteNumber from {
     symbol name => "infinity", 
     symbol symbol => infinity,
     sign => 1
     }

mathML InfiniteNumber := i -> (
     if i#sign === -1 
     then "<mrow><mo>-</mo><mi>&infin;</mi></mrow>"
     else "<mi>&infin;</mi>"
     )

IndeterminateNumber = new Type of HashTable
IndeterminateNumber.synonym = "indeterminate number"
indeterminate = new IndeterminateNumber from {
     symbol name => "indeterminate",
     symbol symbol => symbol indeterminate
     }

InfiniteNumber + ZZ := (i,j) -> i
ZZ + InfiniteNumber := (i,j) -> j
InfiniteNumber - ZZ := (i,j) -> i
ZZ - InfiniteNumber := (i,j) -> -j
InfiniteNumber * ZZ := (i,j) -> if j > 0 then i else if j < 0 then -i else indeterminate
ZZ * InfiniteNumber := (j,i) -> if j > 0 then i else if j < 0 then -i else indeterminate
InfiniteNumber + InfiniteNumber := (x,y) -> if x#sign === y#sign then x else indeterminate
InfiniteNumber - InfiniteNumber := (x,y) -> if x#sign =!= y#sign then x else indeterminate
InfiniteNumber * InfiniteNumber := (x,y) -> if x#sign === 1 then y else -y
InfiniteNumber / InfiniteNumber := (x,y) -> indeterminate

neginfinity := new InfiniteNumber from {
     symbol name => "-infinity", 
     sign => -1
     }

InfiniteNumber ? InfiniteNumber := (x,y) -> x#sign ? y#sign

Thing ? InfiniteNumber := (x,y) -> (
     if x === y then symbol ==
     else if y === infinity then symbol <
     else symbol >
     )

InfiniteNumber == Thing := (x,y) -> x === y
Thing == InfiniteNumber := (x,y) -> x === y

InfiniteNumber ? Thing := (x,y) -> (
     if x === y then symbol ==
     else if x === infinity then symbol >
     else symbol <
     )

- InfiniteNumber := x -> (
     if x === infinity then neginfinity
     else infinity)

max Sequence := max List := x -> (
     m := neginfinity;
     scan(x, n -> if n > m then m = n);
     m)

min Sequence := min List := x -> (
     m := infinity;
     scan(x, n -> if m > n then m = n);
     m)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
