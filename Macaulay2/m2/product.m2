--		Copyright 1994 by Daniel R. Grayson

ProductRing = new Type of Ring
document { quote ProductRing,
     TT "ProductRing", " -- the class of all product rings.",
     PARA,
     "If R and S are rings, then R * S denotes their product ring.
     If r and s are elements of R and S respectively, then an element
     of the product is provided by ", 
     PRE "          new R*S from {r,s}",
     "This has to be rethought!"     
     }
name ProductRing := (RS) -> name RS#0 | " * " | name RS#1;
Ring * Ring := (R,S) -> (
     RS := new ProductRing of BasicList;
     RS#0 = new RS from {R#0,S#0};
     RS#1 = new RS from {R#1,S#1};
     RS == RS := (x,y) -> x#0 == y#0 and x#1 == y#1;
     RS == ZZ := (x,i) -> x#0 == i and x#1 == i;
     ZZ == RS := (i,x) -> x#0 == i and x#1 == i;
     name RS := x -> "(" | name x#0 | "," | name x#1 | ")";
     - RS := x -> new RS from {-x#0,-x#1};
     RS + RS := (x,y) -> new RS from {x#0+y#0,x#1+y#1};
     RS - RS := (x,y) -> new RS from {x#0-y#0,x#1-y#1};
     RS * RS := (x,y) -> new RS from {x#0 y#0,x#1 y#1};
     RS / RS := (x,y) -> new RS from {x#0/y#0,x#1/y#1};
     InverseMethod RS := x -> new RS from {x#0^-1,x#1^-1};
     ZZ + RS := (x,y) -> new RS from {x + y#0,x + y#1};
     ZZ - RS := (x,y) -> new RS from {x - y#0,x - y#1};
     ZZ * RS := (x,y) -> new RS from {x * y#0,x * y#1};
     ZZ / RS := (x,y) -> new RS from {x / y#0,x / y#1};
     RS + ZZ := (x,y) -> new RS from {x#0 + y,x#1 + y};
     RS - ZZ := (x,y) -> new RS from {x#0 - y,x#1 - y};
     RS * ZZ := (x,y) -> new RS from {x#0 * y,x#1 * y};
     RS / ZZ := (x,y) -> new RS from {x#0 / y,x#1 / y};
     --check RS;
     RS
     )

TEST "
-- test product
R = ZZ/101
S = ZZ/103
RS = R*S
r = 23_R
s = 47_S
x = new RS from {r,s}
assert( 2*x == new RS from {2*r,2*s} )
assert( x + 1_RS == new RS from {r+1, s+1} )
"
