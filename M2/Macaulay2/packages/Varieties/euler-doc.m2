--- status: draft
--- author(s): Sorin Popescu
--- notes: 



document {
     Key => euler,
     Headline => "Euler characteristic",
     SeeAlso => {eulers, genus}
     }

document {
     Key => (euler,ProjectiveVariety),
     Headline => "topological Euler characteristic of a (smooth) projective variety",
     Usage => "euler V",
     Inputs => {"V"
	  },
     Outputs => {ZZ =>"the topological Euler characteristics of the variety V"
	  },    
     "The command computes the topological Euler characteristic of the (smooth) projective 
     variety V as an alternated sum of its Hodge numbers. The Hodge numbers can be computed 
     directly using the command ", TO "hh", ".",
     PARA{},
     "A smooth plane quartic curve has genus 3 and topological Euler characteristic -4:",  
     EXAMPLE {
	  "Quartic = Proj(QQ[x_0..x_2]/ideal(x_0^4+x_1^4+x_2^4))",
	  "euler(Quartic)"
	  },
     PARA{},
     "The topological Euler characteristic of a smooth quintic hypersurface in
     projective fourspace is -200:",     
     EXAMPLE {
	  "Quintic = Proj(QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-101*x_0*x_1*x_2*x_3*x_4))",
	  "euler(Quintic)"
	  },
     Caveat  => {"No test is made to see if the projective variety is smooth"}, 
     SeeAlso => {Proj,genus,hh}
     }

document {
     Key => (euler,CoherentSheaf),
     Headline => "Euler characteristic of coherent sheaf",
     Usage => "euler F",
     Inputs => {"F"
	  },
     Outputs => {ZZ =>" the Euler characteristic of the cohomology of the sheaf"},
     "The command returns ", TT "chi(F)", " the Euler characteristic of the sheaf ",
      TT "F", " i.e. the alternated sum of the dimensions of its cohomology groups",
     PARA{},
      
     SeeAlso => {(eulers,CoherentSheaf),genus}
     }

document {
     Key => (euler,Module),
     "This needs to be documented.",
     SeeAlso => {(eulers,Module),genus}
     }

document {
     Key => (euler,Ring),
     "This needs to be documented.",
     SeeAlso => {(eulers,Ring),genus}
     }

document {
     Key => (euler,Ideal),
     "This needs to be documented.",
     SeeAlso => {(eulers,Ideal),genus}
     }

document { 
     Key => eulers,
     Headline => "list the sectional Euler characteristics",
     SeeAlso => {euler,genera,genus}
     }

document { 
     Key => {(eulers, CoherentSheaf),(eulers,Module)},
     Usage => "eulers E",
     Inputs => {"E"
	  },
     Outputs => {List =>"the successive sectional Euler characteristics of a coherent sheaf, or a module."
	  },
     "Computes a list of the successive sectional Euler characteristics of a coherent sheaf,
     the i-th entry on the list being the Euler characteristic of the i-th
     generic hyperplane restriction of ", TT "E",
     PARA{},
     "The Horrocks-Mumford bundle on the projective fourspace:",
     EXAMPLE {
	  "R = QQ[x_0..x_4];",
	  "a = {1,0,0,0,0}",
	  "b = {0,1,0,0,1}",
	  "c = {0,0,1,1,0}",
	  "M1 = matrix table(5,5, (i,j)-> x_((i+j)%5)*a_((i-j)%5))",
	  "M2 = matrix table(5,5, (i,j)-> x_((i+j)%5)*b_((i-j)%5))",
	  "M3 = matrix table(5,5, (i,j)-> x_((i+j)%5)*c_((i-j)%5))",
	  "M = M1 | M2 | M3;",
	  "betti (C=res coker M)",
	  "N = transpose submatrix(C.dd_3,{10..28},{2..36});",
	  "betti (D=res coker N)",
	  "Pfour = Proj(R)",
	  "HorrocksMumford = sheaf(coker D.dd_3);",
	  "HH^0(HorrocksMumford(1))",
	  "HH^0(HorrocksMumford(2))",
	  "eulers(HorrocksMumford(2))"
	  },
     SeeAlso => {genera,genus}
     }
document { 
     Key => (eulers,Ideal),
     Usage => "eulers I",
     Inputs => {"I"
	  },
     Outputs => {List =>"the successive sectional Euler 
	  characteristics of an ideal (sheaf)."
          },
     "Computes a list of the successive sectional Euler 
     characteristics of an ideal (sheaf), the i-th entry 
     in the list being the Euler characteristic of the i-th
     generic hyperplane restriction of ", TT "I",     
     EXAMPLE {
	  "R = ZZ/101[a,b,c];",
	  "I =ideal(a^3+b^3+c^3)", 
	  "eulers I"
	  },
     SeeAlso => {genera,genus}
     }
document { 
     Key => (eulers,Ring),
     Usage => "eulers R",
     Inputs => {"R"
	  },
     Outputs => {List =>"the successive sectional Euler 
	       characteristics of a (sheaf of) ring(s)."
	  },
     "Computes a list of the successive sectional Euler 
     characteristics of a ring (sheaf of), the i-th entry 
     in the list being the Euler characteristic of the i-th
     generic hyperplane restriction of ", TT "R",     
     EXAMPLE {
	  "S = ZZ/101[a,b,c];",
	  "I = ideal(a^3+b^3+c^3)", 
	  "R = S/I",
	  "eulers(R)",
	  "J = substitute(ideal(b,a+c),R)",
	  "eulers(R/J)"
	  },
     SeeAlso => {genera,genus},
     }

