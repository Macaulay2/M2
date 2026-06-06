--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => genus,
     Headline => "arithmetic genus",
     SeeAlso => {genera, euler}
     }
document { 
     Key => {(genus,CoherentSheaf),(genus,Module),(genus, Ideal)},
     Usage => "genus F",
     Inputs => {"F" => {ofClass{CoherentSheaf,Module,Ideal} }},
     Outputs => {ZZ },
     "Computes the arithmetic genus of the coherent sheaf ", TT "F", " that is (-1)^dim-support * (chi(F) - 1)).  If ", TT "F", " is a module
     over a ring, then the genus of ", TT "F^~", " is computed.  If ", TT "I", " is an ideal in a ring ", TT "R", " then the genus of ", TT "(R/I)^~", " is
     computed.",
     EXAMPLE {
	  "V = Proj(QQ[x,y,z]/ideal(y^2*z-x^2*(x+z)))",
	  "genus OO_V^1"
	  },
     SeeAlso => {genera,euler}
     }
document { 
     Key => (genus,ProjectiveVariety),
     Usage => "genus V",
     Inputs => {"V"
	  },
     Outputs => {ZZ
	  },
     "Computes the arithmetic genus of the projective scheme ", TT "V",
     "A nodal plane cubic curve has arithmetic genus 1:",
     EXAMPLE {
	  "V = Proj(QQ[a,b,c]/ideal(b^2*c-a^2*(a+c)))",
	  "genus V"
	  },
     "The Fano model of a Reye type Enriques surface in projective fivespace:",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_5];",
	  "M = random(R^4, R^{4:-1});",
	  "I = minors(3, M+transpose(M));",
	  "V = Proj(R/I);",
	  "genus V"
	  },
     SeeAlso => {genera,euler}
     }
document { 
     Key => (genus,Ring),
     Usage => "genus R",
     Inputs => {"R"
	  },
     Outputs => {ZZ
	  },
     "Computes the arithmetic genus of the projective scheme ", TT "V",
     " with homogeneous coordinate ring ", TT "R",
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(y^2*z-x^3)",
	  "genus R"
	  },
     SeeAlso => {genera,euler}
     }
