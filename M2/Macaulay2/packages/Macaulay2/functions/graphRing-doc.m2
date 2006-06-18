document { 
     Key => {(graphRing, RingMap),graphRing,[graphRing,MonomialOrder],[graphRing,MonomialSize],[graphRing,VariableBaseName]},
     Headline => "the coordinate ring of the graph of the regular map corresponding to a ring map",
     Usage => "graphRing f",
     Inputs => {
	  "f",
	  MonomialOrder => { "a monomial ordering, see ", TO "monomial orderings" },
	  MonomialSize => ZZ => { "the monomial size, see ", TO MonomialSize },
	  VariableBaseName => Symbol => { "the variable base name, see ", TO VariableBaseName }
	  },
     Outputs => {{"the coordinate ring of the graph of regular map corresponding to ", TT "f"}},
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  S = QQ[s,t,u]
	  f = map(R,S,{x^2,x*y,y^2})
	  graphRing f
	  Spec oo
     ///,
     SeeAlso => {"graphIdeal"}
     }
