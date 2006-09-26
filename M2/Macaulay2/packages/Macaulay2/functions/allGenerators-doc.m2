--- status: DRAFT
--- author(s): MES
--- notes: 

-- this function has been replaced by the CoefficientRing option to "generators"

undocumented {
	  (allGenerators, PolynomialRing),
	  (allGenerators, FractionField),
	  (allGenerators, QuotientRing)}

document { 
     Key => {allGenerators, (allGenerators,Ring)},
     Headline => "list of all generators",
     Usage => "allGenerators R",
     Inputs => {
	  "R" => Ring
	  },
     Outputs => {
	  List => {"of every generator of ", TT "R", " over the prime field or the integers"}
	  },
     "Returns the list of all of the generators, as elements of R.",
     EXAMPLE {
	  "A = GF(9,Variable=>a)",
	  "B = A[r,s,t]",
	  "generators B",
	  "allGenerators B",
	  "C = B[x,y,z]/(x^2-a*x-r)",
	  "generators C",
	  "allGenerators C"
	  },
     "This same order is used when creating ", TO2(RingMap, "ring maps"), " from this ring.
     The following ring map from C --> A[u,v] sends ",
     TT "x|-->0, y|-->u, z|-->v, r|-->0, s-->av+1, t|-->1.",
     EXAMPLE {
	  "D = A[u,v];",
	  "F = map(D, C, {0, u, v,  0, a*v+1, 1})",
	  "F (x+s*y)"
	  },
     SeeAlso => {generators, vars, "substitution and maps between rings"}
     }

TEST ///
A = GF(9,Variable=>a)
B = A[r,s,t]
generators B
allGenerators B
C = B[x,y,z]/(x^2-a*x-r)
generators C
v = allGenerators C
assert (#v == 7)
///
