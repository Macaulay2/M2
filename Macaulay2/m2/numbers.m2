--		Copyright 1994 by Daniel R. Grayson

odd  = x -> 1 === x%2

document { quote odd,
     TT "odd x", " -- returns true or false, tells whether x is an odd integer.",
     PARA,
     "See also ", TO "even", "."
     }

even = x -> 0 === x%2

document { quote even,
     TT "even x", " -- returns true or false, tells whether x is an even integer.",
     PARA,
     "See also ", TO "odd", "."
     }

Numeric = x -> (
     if basictype x === ZZ or basictype x === QQ
     then 0. + x
     else if basictype x === BasicList or basictype x === Sequence
     then apply(x,Numeric)
     else x);

document { quote Numeric,
     TT "Numeric x", " -- yields the expression obtained from x by converting the 
     integers and rational numbers within to double precision floating 
     point numbers.",
     PARA,
     EXAMPLE "Numeric {1,2,3}",
     PARA,
     "See also ", TO "RR", "."
     }

pi=3.1415926535897932384626433832795028841971693993

document { quote pi,
     TT "pi", " -- the numerical value of the arithmetic quantity pi."
     }
