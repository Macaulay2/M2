--		Copyright 1993-1999 by Daniel R. Grayson

odd  = x -> 1 === x%2

even = x -> 0 === x%2

numeric = x -> (
     if basictype x === ZZ or basictype x === QQ
     then 0. + x
     else if basictype x === BasicList or basictype x === Sequence
     then apply(x,numeric)
     else x);

pi=3.1415926535897932384626433832795028841971693993

