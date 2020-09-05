-- FrobeniusMultiplicities Macaulay 2 package
-- Copyright (C) 2009 Jason G McCullough 

--This program is free software; you can redistribute it and/or
--modify it under the terms of the GNU General Public License version 2
--as published by the Free Software Foundation.

--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.


--=========================================================================--
--=========================================================================--
--=========================================================================--

newPackage(
     "FrobeniusMultiplicities",
     Version => "0.7", 
     Date => "May 27, 2009",
     Authors => {
	  {Name => "Jason McCullough", Email => "jmccullo@math.ucr.edu", HomePage => "http://www.math.ucr.edu/~jmccullo"}
	  },
     Headline => "successive approximations of Hilbert-Kunz and related
                  multiplicities in characteristic p > 0; the Frobenius action on
		  modules and free complexes is implemented as well",
     DebuggingMode => false
     )

--=========================================================================--
     
export{"frob","tensorF","torF","hkSeq","tiSeq","frobeniusPower"} 
        
--=========================================================================--


--*************************************************************************--
-- Computes F^n(M) where M is a module and F^n is the nth iteration of
-- the Frobenius functor.
-- Usage: frob(M,n)

frob = method()
frob(Module,ZZ) := Module => (M,n) -> (
     N := minimalPresentation M;
     T := relations N;
     L := entries T;
     q := (char ring M)^n;
     LL := applyTable(L,x->x^q);
     coker matrix LL
     )


--*************************************************************************--
-- Computes F^n(C) where F^n is the nth Frobenius functor and C is a
-- minimal free resolution of a module M or a complex of free modules.
-- The first integer i input gives the homological degree to which M2
-- should compute C.  
-- Usage: tensorF(M,i,n) or tensorF(C,n)


tensorF = method()
tensorF(Module,ZZ,ZZ) := ChainComplex => (M,i,n) -> (
     C := res(M,LengthLimit=>(i+1));
     tensorF(C,n)    
     )

tensorF(ChainComplex,ZZ) := ChainComplex => (C,n) -> (
     for i from 0 to length C do if not isFreeModule C_i then error "Expected complex of free modules";
     A := ring C;
     p := char A;
     if p == 0 then error "Expect complex over ring of positive characteristic";
     q := p ^ n;
     len := length C;
     mats := for i from 1 to len list matrix applyTable( entries C.dd_i, x->x^(q));  --apply(entries(C.dd_i),l->apply(l,x->x^(p^n)));
     degs := for i from 0 to len list (- q*(degrees C_i));
     maps := for i from 1 to len list map(A^(degs#(i-1)),A^(degs#i),mats#(i-1));
     chainComplex maps
     )
--*************************************************************************--
-- Computes tor_i(M,fnA) where M is an A-module and fnA is the bimodule
-- with left action given by f^n.  
-- Note: This is the same as HH_i(F^n(C)) where C is a free resolution of M.
-- Usage: torF(M,i,n)


torF = method()
torF(Module,ZZ,ZZ) := Module => (M,i,n) ->(
     HH_i(tensorF(M,i,n))
     )

-----------------------------------------------------------------------------
TEST ///
A = ZZ/2[x,y,z];
k = module A / ideal vars A;
assert zero torF(k,1,1);
///
-----------------------------------------------------------------------------


--*************************************************************************--
-- Computes I^[p^n]
-- Usage: frobeniusPower(I,n)

frobeniusPower = method()
frobeniusPower(Ideal,ZZ) := Ideal =>(I,n) ->(
     p := char ring I;
     if not isPrime p then error "Expected ideal from a ring of positive characteristic";
     if not isField coefficientRing ring I then error "Expected ideal from a quotient of a polynomial ring";
     Ipngens := for i in I_* list i^(p^n);
     ideal Ipngens
     )

--*************************************************************************--
-- Computes the first n elements in the sequence defining the Hilbert-Kunz
-- Multiplicity of M.  In other words, this function outputs the list
-- length(
-- with left action given by f^n.  
-- Note: This is the same as HH_i(F^n(C)) where C is a free resolution of M.
-- Usage: torF(M,i,n)


hkSeq = method()
hkSeq(Module,ZZ) := List =>(M,n) ->(
     p := char ring  M;
     if not isPrime p then error "Expected module over ring of positive prime characteristic";
     d := dim ring  M;
     A := ring M;
     m := ideal vars A;
     mpn := frobeniusPower(m,n);
     for i from 1 to n list (degree ((module A / frobeniusPower(m,i)) ** M))/(p^(i*d))
     )


--*************************************************************************--
-- Computes hkSeq of the residue field, usually written e_HK(R).
-- Usage: hkSeq(A,n)

hkSeq(Ring,ZZ) := List =>(A,n) ->(
     p := char A;
     if not isPrime p then error "Expected ring of positive prime characteristic";
     d := dim A;
     k := module A / ideal vars A;
     for i from 1 to n list (degree torF(k,0,i))/(p^(i*d))
     )

--*************************************************************************--
-- Computes by default the first 8 terms of hkSeq for a ring.
-- Usage: hkSeq(A)

hkSeq(Ring) := List =>(A) ->(
     hkSeq(A,8)
     )

--*************************************************************************--
-- Computes by default the first 8 terms of hkSeq for a module.
-- Usage: hkSeq(M)

hkSeq(Module) := List =>(M) ->(
     hkSeq(M,8)
     )


--*************************************************************************--
-- Computes the sequence of numbers defining the higher derived Hilbert-Kunz
-- multiplicities defined.  
-- Usage: hkSeq(A)

tiSeq = method()
tiSeq(Module,ZZ,ZZ) := List =>(M,i,n) ->(    
     p := char ring M;
     if not isPrime p then error "Expected module over ring of positive prime characteristic";
     d := dim ring M;
     for j from 1 to n list (degree torF(M,i,j))/(p^(j*d))
     )


tiSeq(Ring,ZZ,ZZ) := List =>(A,i,n) ->(
     k = module A / ideal vars A;
     tiSeq(k,i,n)
     )

tiSeq(Ring,ZZ) := List =>(A,i) ->(
     tiSeq(A,i,8)
     )

tiSeq(Module,ZZ) := List =>(M,i) ->(
     tiSeq(M,i,8)
     )




beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

document { 
     Key => FrobeniusMultiplicities, 
     Headline => "Computations in characteristic p>0 related to the Frobenius
                  and Hilbert-Kunz multiplicities",      
     EM "FrobeniusMultiplicities", " is a package which will hopefully help users
     make computations involving the frobenius endomorphism, the Hilbert-Kunz
     Multiplicity and the higher derived Hilbert-Kunz multiplicites.  In particular
     we add functions", TO (frob,Module,ZZ), ", ", TO (hkSeq,Module,ZZ), ", ", TO (tiSeq, Module, ZZ, ZZ), ", ",
     TO (frobeniusPower, Ideal, ZZ), ", ", TO (torF, Module, ZZ, ZZ), " and ", 
     TO (tensorF, Module, ZZ, ZZ), "."
      }

-----------------------------------------------------------------------------

document {
     Key => {frob,(frob, Module, ZZ)},
     Headline => "computes the module F^n(M) where F^n is the nth iteration of the Frobenius functor",
     Usage => "frob(M,n)",
     Inputs => {
	  "M" => Module,
	  "n" => ZZ,
	  },
     Outputs => {
	  Module => {"the module F^n(M)"}
	  },
          Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     "The function ", TT "frob(M,n)", ", computes the module ", TT "F^n(M)",
     ". It does this by finding a minimal presentation of M and applying the nth Frobenius
     to the elements in the defining matrix and then taking the cokernel.",
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     M = module A / ideal vars A;
     frob(M,3)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

-----------------------------------------------------------------------------

document {
     Key => {frobeniusPower,(frobeniusPower, Ideal, ZZ)},
     Headline => "computes the ideal generated by the (p^n)th powers of the generators of an ideal where p is the characteristic of the ambient ring",
     Usage => "frobeniusPower(I,n)",
     Inputs => {
	  "I" => Ideal,
	  "n" => ZZ
	  },
     Outputs => {
	  Ideal => {"the ideal I^[p^n] generated by the (p^n)th powers of the generators of I"}
	  },
          Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     "The function ", TT "frobeniusPower(I,n)", ", computes the ideal ", TT "I^[p^n}",
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     I = ideal vars A;
     frobeniusPower(I,3)
     frobeniusPower(I,4)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }

-----------------------------------------------------------------------------

document {
     Key => {tensorF,
	  (tensorF, ChainComplex, ZZ)
	  },
     Headline => "computes the complex F^n(C) where C is a chain complex of free modules and where F^n is the nth iteration of the Frobenius functor",
     Usage => "tensorF(C,n)",
     Inputs => {
	  "C" => ChainComplex,
	  "n" => ZZ
	  },
     Outputs => {
	  ChainComplex => {"the chain complex F^n(C)"}
	  },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     "The function ", TT "tensorF(C,n)", ", computes the chain complex ", TT "F^n(C)",
     ". It does this by applying the nth Frobenius to the elements in the matrices defining the maps in the chain complex.",
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     M = module A / ideal vars A;
     C = res M
     C.dd
     D = tensorF(C,3)
     D.dd
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

-----------------------------------------------------------------------------

document {
     Key => {tensorF,
	  (tensorF, Module, ZZ, ZZ)
	  },
     Headline => "computes the complex F^n(C) where C is the free resoltuion of M (with LengthLimit set to i+1) and where F^n is the nth iteration of the Frobenius functor",
     Usage => "tensorF(M,i,n)",
     Inputs => {
	  "M" => Module,
	  "i" => ZZ,
	  "n" => ZZ
	  },
     Outputs => {
	  ChainComplex => {"the chain complex F^n(C) where C is the free resolution of M"}
	  },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     "The function ", TT "tensorF(M,i,n)", ", computes the chain complex ", TT "F^n(C)",
     " where C is the free resolution of M. It does this by applying the nth Frobenius to the elements in the matrices defining the maps in the chain complex.",
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     M = module A / ideal vars A;
     C = res M
     C.dd
     D = tensorF(C,3)
     D.dd
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

-----------------------------------------------------------------------------

document {
     Key => {torF,
	  (torF, Module, ZZ, ZZ)
	  },
     Headline => "computes the module Tor_i(M,fnA) where fnA is the module A^1 with A-action defined by x.a = x^(p^n)a.",
     Usage => "torF(M,i,n)",
     Inputs => {
	  "M" => Module,
	  "i" => ZZ,
	  "n" => ZZ
	  },
     Outputs => {
	  Module => {"the module Tor_i(M,fnA where fnA is the module A^1 with A-action defined by the nth Frobenius"}
	  },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     "The function ", TT "torF(M,i,n)", ", computes the module ", TT "Tor_i(M,fnA)",
     " where M is a module over a characteristic p>0 ring A and fnA is A-module A^1 with A-action defined by the nth Frobenius x->x^(p^n). This function calls tensorF(C,i,n) and takes homology.",
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     M = module A / ideal vars A;
     torF(M,0,1)
     torF(M,0,2)
     torF(M,1,2)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

 -----------------------------------------------------------------------------

document {
     Key => {hkSeq,
	  (hkSeq, Module, ZZ),
	  (hkSeq, Ring, ZZ)
	  },
     Headline => "computes the first n approximations of the Hilbert-Kunz multiplicity of a ring of positive prime characteristic or a module over such a ring.",
     Usage => "hkSeq(A,n)",
     Inputs => {
	  "A" => {"a ", TO "Ring", " or ", TO "Module"},
	  "n" => ZZ
	  },
     Outputs => {
	  List =>{"The function ", TT "hkSeq(A,n)", ", computes the first n approximations of the Hilbert-Kunz multiplicity of a ring or module; that is, the function computes
      the length of modules A/m^[p^i]A divided by p^(nd) for i from 1 to n where d = dim A.  The limit defined by this sequence of numbers exists and is defined to be the Hilbert-Kunz multiplicity of A.  This function 
      calls the function torF(M,i,n)."}
      },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     hkSeq(A,6)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	

-----------------------------------------------------------------------------

document {
     Key => {
	  (hkSeq, Module),
	  (hkSeq, Ring)
	  },
     Headline => "computes the first 8 approximations of the Hilbert-Kunz multiplicity of a ring of positive prime characteristic or a module over such a ring.",
     Usage => "hkSeq(A)",
     Inputs => {
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {
	  List => {"The function ", TT "hkSeq(A)", ", computes the first 8 approximations of the Hilbert-Kunz multiplicity of a ring or module.  It calls by default ", TT "hkSeq(A,8)", "." }
	  },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     hkSeq(A)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

-----------------------------------------------------------------------------

document {
     Key => {tiSeq,
	  (tiSeq, Module, ZZ, ZZ),
	  (tiSeq, Ring, ZZ, ZZ)
	  },
     Headline => "computes the first n approximations of the ith derived Hilbert-Kunz multiplicity of a ring of positive prime characteristic or a module over such a ring.",
     Usage => "tiSeq(A,n)",
     Inputs => {
	  "A" => {"a ", TO "Ring", " or ", TO "Module"},
	  "i" => ZZ,
	  "n" => ZZ
	  },
     Outputs => {
	  List =>{"the first n approximations of the ith derived Hilbert-Kunz multiplicity"}
	  },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic ", TT "p>0", "."},
     "The function ", TT "tiSeq(A,n)", ", computes the first n approximations of the ith derived Hilbert-Kunz multiplicity of a ring or module; that is, the function computes
      the length of modules ", TT "Tor_i(A,fjA)", " divided by ", TT "p^(jd)", " for j from 1 to n where ", TT "d = dim A", " and ", TT "fjA ", " is the A-module A with action defined by the jth Frobenius.  The limit defined by this sequence of numbers exists and is defined to be the Hilbert-Kunz multiplicity of A.  This function 
      calls the function ", TT "torF(M,i,n)", ".",
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     tiSeq(A,1,6)
     tiSeq(A,2,6)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

-----------------------------------------------------------------------------

document {
     Key => {
	  (tiSeq, Module, ZZ),
	  (tiSeq, Ring, ZZ)
	  },
     Headline => "computes the first 8 approximations of the ith derived Hilbert-Kunz multiplicity of a ring of positive prime characteristic or a module over such a ring.",
     Usage => "tiSeq(A)",
     Inputs => {
	  "A" => {"a ", TO "Ring", " or ", TO "Module"},
	  "i" => ZZ
	  },
     Outputs => {
	  List => {"The function ", TT "tiSeq(A,i)", ", computes the first 8 approximations of the ith derived Hilbert-Kunz multiplicity of a ring or module.  It calls by default ", TT "tiSeq(A,i,8)", "." }
	  },
     Caveat => {"The ambient ring is assumed to a quotient of a polynomial ring over a field of characteristic p>0."},
     EXAMPLE lines ///
     A = ZZ/2[x,y,z]/ideal(x^2, x*y);
     tiSeq(A,1)
     tiSeq(A,2)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO FrobeniusMultiplicities, "."
     	  }
     }	   

-----------------------------------------------------------------------------
