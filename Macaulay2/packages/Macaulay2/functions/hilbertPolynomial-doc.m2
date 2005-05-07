--- status: DRAFT
--- author(s): L. Gold
--- notes: write caution: i is not a variable in option, show how to use
---        show ex of eventually = HF

document { 
     Key => hilbertPolynomial,
     Headline => "compute the Hilbert polynomial",
     "In a singly graded ambient ring, the ", 
     TO2(hilbertFunction, "Hilbert function"), " eventually is
     a polynomial called the Hilbert polynomial. By default this polynomal
     is written in terms of the Hilbert polynomials of projective
     spaces because it is a good form for extracting geometric
     information from the polynomial. The Hilbert
     polynomial of ", TT "P^i", " is ",  TT "z |--> binomial(z + i, i).",
     SeeAlso => {"degreesRing", "reduceHilbert", "poincare", "poincareN", 
	  "hilbertSeries", "hilbertFunction"}
     } 

document {
     Key => (hilbertPolynomial,Ring),
     Headline => "compute the Hilbert polynomial of the ring",
     Usage => "hilbertPolynomial R",
     Inputs => { 
	  "R" => Ring => ""
	  },
     Outputs => {
	  ProjectiveHilbertPolynomial => "unless the option Projective is false" 
	  },
     "We compute the ", TO2(hilbertPolynomial, "Hilbert polynomial"),
     " of a coordinate ring of the rational quartic curve in ", 
     TT "P^3.",
         EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "S = image map(R, R, {a^4, a^3*b, a*b^3, b^4});",
      	  "presentation S",
      	  "h =  hilbertPolynomial S",
     	  "hilbertPolynomial(S, Projective=>false)"
	  },
     "The rational quartic curve in ", TT "P^3", " is therefore 'like'
     4 copies of ", TT "P^1", ", with three points missing.  One can
     see this by noticing that there is a deformation of the rational
     quartic to the union of 4 lines, or 'sticks', which intersect in
     three successive points.",
     PARA,
     "These Hilbert polynomials can serve as Hilbert functions, too.",
     EXAMPLE {
	  "h(3)",
      	  "basis(3,S)",
      	  "rank source basis(3,S)",
	  }
     }

document { 
     Key => (hilbertPolynomial,Module),
     Headline => "compute the Hilbert polynomial of the module",
     Usage => "hilbertPolynomial M",
     Inputs => {
	  "M" => Module => ""
	  },
     Outputs => {
	  ProjectiveHilbertPolynomial => "unless the option Projective is false" 
	  },
     "We compute the ", TO2(hilbertPolynomial, "Hilbert polynomial"),
     " of a module.",
     EXAMPLE {
	  "R = QQ[a..h];",
	  "M = coker matrix {{a,c,d},{d,b,h}}",
	  "h = hilbertPolynomial M",
     	  "hilbertPolynomial(M, Projective=>false)"
	  	  },
     }

document { 
     Key => (hilbertPolynomial,CoherentSheaf),
     Headline => "compute the Hilbert polynomial of the coherent sheaf",
     Usage => "hilbertPolynomial S",
     Inputs => {
	  "S" => ""
	  },
     Outputs => {
	  ProjectiveHilbertPolynomial => "unless the option Projective is false" 
	  },
     "We compute the ", TO2(hilbertPolynomial, "Hilbert polynomial"),
     " of a coherent sheaf.",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_2];",
	  "V = Proj R;",
	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	  "h = hilbertPolynomial S",
     	  "hilbertPolynomial(S, Projective=>false)"
	  },
     }

document { 
     Key => (hilbertPolynomial,Ideal),
     Headline => "compute the Hilbert polynomial of the quotient of
     the ambient ring by the ideal",
     Usage => "hilbertPolynomial I",
     Inputs => {
	  "I" => Ideal => ""
	  },
     Outputs => {
	  ProjectiveHilbertPolynomial => "unless the option Projective is false" 
	  },
     "We compute the ", TO2(hilbertPolynomial, "Hilbert polynomial"),
     " of the quotient of the ambient ring by an ideal.",
     EXAMPLE {
	  "R = QQ[a..h];",
	  "I = ideal(a*b, c*d, e*f);",
	  "hilbertPolynomial I",
	  "hilbertPolynomial (R/I)",
	  "hilbertPolynomial(I, Projective=>false)"
	  },
     Caveat => {
	  "As is often the case, calling this function on an ideal ", 
	  TT "I", " actually computes it for ", TT "R/I", " where ", 
	  TT "R", " is the ring of ", TT "I", "."
	  }
     }

document { 
     Key => (hilbertPolynomial,ProjectiveVariety),
     Headline => "compute the Hilbert polynomial of the projective variety",
     Usage => "hilbertPolynomial V",
     Inputs => {
	  "V" => ProjectiveVariety => ""
	  },
     Outputs => {
	  ProjectiveHilbertPolynomial => "unless the option Projective is false" 
	  },
     "We compute an example of the ", 
     TO2(hilbertPolynomial, "Hilbert polynomial"), " of a projective
     Hilbert variety. This is the same as the Hilbert polynomial of
     its coordinate ring.",
     EXAMPLE {
	  "V = Proj(QQ[x_0..x_5]/(x_0^3+x_5^3))",
	  "h = hilbertPolynomial V",
     	  "hilbertPolynomial(V, Projective=>false)"
	  }
     }

document { 
     Key => [hilbertPolynomial, Projective],
     Headline => "choose how to display the Hilbert polynomial",
     Usage => "hilbertPolynomial(...,Projective => b",
     Inputs => {
	  "b" => Boolean => "either true or false"
	  },
     TT "Projective => true", " is an option to ", TO "hilbertPolynomial", 
     " which specifies that the Hilbert polynomial produced should be
     expressed in terms of the Hilbert polynomials of projective
     spaces. This is the default.",
     PARA,
     TT "Projective => false", " is an option to ", TO "hilbertPolynomial",
      " which specifies that the Hilbert polynomial produced should be
      expressed as a polynomial in the variable ", TT "i", ".",
      PARA,
     "We compute the ", TO2(hilbertPolynomial, "Hilbert polynomial"), 
     " of a coordinate ring of the  rational quartic curve in ", 
     TT "P^3.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "S = image map(R, R, {a^4, a^3*b, a*b^3, b^4});",
      	  "hilbertPolynomial S",
	  "hilbertPolynomial(S, Projective=>false)"
     	  },
     SeeAlso => "ProjectiveHilbertPolynomial"
     }

TEST "
scan(3, n -> (
     R = ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M = R^{-d};
	  h = hilbertPolynomial M;
	  scan(d .. d + 4, e -> assert(numgens source basis(e,M) == h e))))))
"
TEST "
scan(3, n -> (
     R = ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M = R^{-d};
	  h = hilbertPolynomial (M, Projective => false);
	  i = (ring h)_0;
	  scan(d .. d + 4, e -> (
		    r = numgens source basis(e,M);
		    s = substitute(h, { i => e/1 });
	       	    assert( r == s)))))))
"
