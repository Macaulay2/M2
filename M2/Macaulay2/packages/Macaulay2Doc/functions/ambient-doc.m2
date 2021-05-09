--- status: DRAFT
--- author(s): MES
--- notes: what should 'ambient Ring' do??
---        what about 'ambient Ideal'? ideal 1_R seems suitable (Mahrud)

-*
-- TODO
ambient(AffineVariety)
ambient(CoherentSheaf)
ambient(GradedModule)
ambient(ProjectiveVariety)
*-

document { 
     Key => ambient,
     Headline => "ambient free module of a subquotient, or ambient ring",
     "Each module M in Macaulay2 is a quotient, submodule or subquotient of a free
     module, called the ambient free module of M.",
     PARA{},
     "The ambient ring of a quotient ring R/I is the polynomial ring of that it is a
     quotient.  The ambient ring of a Galois field is the quotient ring that it was
     constructed from.",
     PARA{},
     SeeAlso => {GF,cover,super}
     }
document { 
     Key => {(ambient,Ring),(ambient,PolynomialRing),(ambient,QuotientRing)},
     Headline => "ambient polynomial ring",
     Usage => "ambient R",
     Inputs => {
	  "R" => {"a polynomial ring or a quotient of a polynomial ring"}
	  },
     Outputs => {
	  Ring => {"the polynomial ring of which this ring is a quotient"}
	  },
     EXAMPLE {
	  "A = ZZ[a..d];",
     	  "B = A/(3*a^2-1);",
	  "C = B/(a*b-3);",
	  "describe C",
	  "ambient C"
	  },
     "If R is not a quotient of a polynomial ring, an error is given.",
     Caveat => {"If the ring is a ", TO GaloisField, ", then the meaning is
	  different.  See ", TO (ambient,GaloisField), "."},
     SeeAlso => {}
     }
document { 
     Key => (ambient,GaloisField),
     Headline => "corresponding quotient ring",
     Usage => "ambient F",
     Inputs => {
	  "F"
	  },
     Outputs => {
	  Ring => "a quotient of a polynomial ring over the prime field"
	  },
     "description",
     EXAMPLE {
	  "F = GF(25,Variable=>a)",
	  "ambient F"
	  },
     SeeAlso => {}
     }
document { 
     Key => (ambient,Module),
     Headline => "ambient free module",
     Usage => "ambient M",
     Inputs => {
	  "M"
	  },
     Outputs => {
	  Module => "a free module"
	  },
     "If a module is a submodule or quotient of a free module F, or is a subquotient of F 
     	  (that is, a submodule of a quotient of F), then this routine yields the free
	  module F.",
     EXAMPLE {
	  "R = QQ[x_1 .. x_5]",
	  "N = image matrix{{x_1,x_2},{x_2,x_3}}",
	  "ambient N",
	  "ambient cokernel vars R",
	  "ambient kernel vars R",
	  "M = image vars R ++ cokernel vars R",
          "ambient M"
	  },
     "This module is always the common target free module of the 
     generator and relation matrices of M",
     EXAMPLE {
	  "ambient M == target generators M",
	  "ambient M == target relations M"
	  },
     Caveat => {},
     SeeAlso => {"subquotient modules",
	       (cover,Module), 
	       (super,Module), 
	       (generators,Module), 
	       (relations,Module)}
     }
document { 
     Key => (ambient,Matrix),
     Headline => "",
     Usage => "ambient f",
     Inputs => {
	  "f" => "M --> N, where M is a free module or quotient of a free module F.",
	  },
     Outputs => {
	  Matrix => "ambient M --> ambient N"
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "f = map(image vars R, coker matrix{{a,b},{c,d}}, transpose matrix{{a,b,c,d},{d,c,b,a}})",
	  "target f",
	  "source f",
	  "ambient f"
	  },
     Caveat => {},
     SeeAlso => {
	  -- Mike wanted this: "homomorphisms of modules",
	  (cover,Module),
	  (cover,Matrix),
	  (matrix,Matrix)}
     }
