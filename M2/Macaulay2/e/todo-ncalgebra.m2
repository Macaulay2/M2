Steps to make NCalgebra arithmetic in the engine

. make a rawNCFreeAlgebra(coeff ring, NCmonoid?):
    NCAlgebra.m2 will call this function when making a ring
    in d/interface.dd: write a "hook" for the e dir routine
    in e/engine.h: write a declaration for this function
    in e/NCAlgebra.{hpp,cpp}, we include
      a type NCFreeAlgebra
      the class NCFreeAlgebra does the arithmetic.
        R->add(f,g) --> h ("raw pointers")

debug Core    
kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"))
1_R
a = R_0
b = R_1
c = R_2
-R_0
