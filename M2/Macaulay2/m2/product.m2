--		Copyright 1993-1999 by Daniel R. Grayson

ProductRing = new Type of Ring

toString ProductRing := RS -> if RS.?name then RS.name else toString RS.baseRings#0 | " * " | toString RS.baseRings#1
net ProductRing := (RS) -> net RS.baseRings#0 | " * " | net RS.baseRings#1
Ring * Ring := ProductRing => (R,S) -> (
     RS := new ProductRing of BasicList;
     RS.baseRings = {R,S};
     RS#0 = new RS from {R#0,S#0};
     RS#1 = new RS from {R#1,S#1};
     RS == RS := (x,y) -> x#0 == y#0 and x#1 == y#1;
     RS == ZZ := (x,i) -> x#0 == i and x#1 == i;
     ZZ == RS := (i,x) -> x#0 == i and x#1 == i;
     toString RS := x -> "(" | toString x#0 | "," | toString x#1 | ")";
     - RS := x -> new RS from {-x#0,-x#1};
     RS + RS := (x,y) -> new RS from {x#0+y#0,x#1+y#1};
     RS - RS := (x,y) -> new RS from {x#0-y#0,x#1-y#1};
     RS * RS := (x,y) -> new RS from {x#0 y#0,x#1 y#1};
     RS / RS := (x,y) -> new RS from {x#0/y#0,x#1/y#1};
     RS.InverseMethod = x -> new RS from {x#0^-1,x#1^-1};
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
