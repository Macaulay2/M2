Steps to make NCalgebra arithmetic in the engine

. make a rawNCFreeAlgebra(coeff ring, NCmonoid?):
    NCAlgebra.m2 will call this function when making a ring
    in d/interface.dd: write a "hook" for the e dir routine
    in e/engine.h: write a declaration for this function
    in e/NCAlgebra.{hpp,cpp}, we include
      a type NCFreeAlgebra
      the class NCFreeAlgebra does the arithmetic.
        R->add(f,g) --> h ("raw pointers")

restart
debug Core    
kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"))
1_R
a = R_0
b = R_1
c = R_2
-a
a*b*a*b*b*a*a*a
a > b
a < b
a >= b
a <= b
a == b -- not sure why this is returning true
a*b*a*b*b*a*a*c > a*b*a*b*b*a*a*b
a*b*b*a*a*c > a*b*a*b*b*a*a*b
a*b*a*b*b*a*a*c > a*b*b*a*a*b
f = a+b+c
-- this thing takes up a lot of memory... 3^12 many operations.
time(f*f*f*f*f*f*f*f*f*f*f*f*f);
time(f6 = f*f*f*f*f*f);
time(f6*f6);
g = a-b-c
f*g-g*f
f*f

restart
R = QQ[a,b,c]
M = basis(1040,R);
f = sum flatten entries M;

