-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {installHilbertFunction,
	  (installHilbertFunction,Ideal,RingElement),
	  (installHilbertFunction,Module,RingElement),
	  (installHilbertFunction,Matrix,RingElement)},
     Headline => "install a Hilbert function without computation",
     Usage => "installHilbertFunction(M,hf)",
     Inputs => { "M" => ofClass{Ideal,Module,Matrix},
	  "hf" },
     Consequences => {
	  {"The poincare polynomial ", TT "hf", " is used as the poincare polynomial
	  for ", TT "M"}
	  },
     "If M is a module, then hf should be the poincare polynomial of M.
     If M is an ideal, then hf should be the poincare polynomial of comodule M.
     If M is a matrix, then hf should be the poincare polynomial of cokernel M.",
     PARA {
	  "An installed Hilbert function will be used by Gröbner basis 
	  computations when possible."},
     PARA {
	  "Sometimes you know or are very sure that you know the Hilbert function.
	  For example, in the following example, the Hilbert function of 3 random polynomials
	  should be the same as the Hilbert function for a complete intersection."
	  },
     EXAMPLE lines ///
     	  R = ZZ/101[a..g];
	  I = ideal random(R^1, R^{3:-3});
	  hf = poincare ideal(a^3,b^3,c^3)
	  installHilbertFunction(I, hf)
	  gbTrace=3
	  time poincare I
	  time gens gb I;
          ///,
     "In this case, the savings is minimal, but often it can be dramatic.",
     PARA {
	  "Another important situation is
	  to compute a Gröbner basis using a different monomial order.
	  In the example below", 
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  I = ideal random(R^1, R^{3:-3});
	  time hf = poincare I
	  S = QQ[a..d,MonomialOrder=>Eliminate 2]
	  J = substitute(I,S)
	  installHilbertFunction(J, hf)
	  gbTrace=3
	  time gens gb J;
	  selectInSubring(1,gens gb J)
	  ///,
     SeeAlso =>{selectInSubring, "Elimination::Elimination"}
     }
