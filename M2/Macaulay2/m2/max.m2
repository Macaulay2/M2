--		Copyright 1994 by Daniel R. Grayson

InfiniteNumber = new Type of HashTable
document { quote InfiniteNumber,
     TT "InfiniteNumber", " -- the class of all infinite numbers.",
     PARA,
     SEEALSO ( "infinity", "-infinity" )
     }

sign := quote sign

infinity = new InfiniteNumber from {quote name => quote infinity, sign => 1}
document { quote infinity,
     TT "infinity", " -- a representation of infinity.",
     SEEALSO ( "-infinity", "InfiniteNumber" )
     }

neginfinity := new InfiniteNumber from {
     quote name => "-infinity", 
     sign => -1
     }

document { "-infinity",
     TT "-infinity", " -- a representation of negative infinity.",
     SEEALSO ( "infinity", "InfiniteNumber" )
     }

InfiniteNumber ? InfiniteNumber := (x,y) -> x#sign ? y#sign

Thing ? InfiniteNumber := (x,y) -> (
     if x === y then quote ==
     else if y === infinity then quote <
     else quote >
     )

InfiniteNumber == Thing := (x,y) -> x === y
Thing == InfiniteNumber := (x,y) -> x === y

InfiniteNumber ? Thing := (x,y) -> (
     if x === y then quote ==
     else if x === infinity then quote >
     else quote <
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

document { quote max,
     TT "max x", " -- yields the maximum of the elements in the list or sequence x."
     }

document { quote min,
     TT "min x", " -- yields the minimum of the elements in the list or sequence x."
     }

TEST ///
assert(max{4,5,6} === 6)
assert(min{4,5,6} === 4)
assert(max(4,5,6) === 6)
assert(min(4,5,6) === 4)
///
