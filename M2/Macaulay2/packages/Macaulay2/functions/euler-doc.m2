--- status: draft
--- author(s): Popescu
--- notes: 

document { 
     Key => euler,
     Headline => "list the sectional Euler characteristics",
     SeeAlso => {genera,genus}
     }
document { 
     Key => {(euler, CoherentSheaf),(euler,ProjectiveHilbertPolynomial),(euler,Module)},
     Usage => "euler E",
     Inputs => {"E" => ""
	  },
     Outputs => {List =>"the successive sectional Euler characteristics of a coherent sheaf, or a module."
	  },
     "Computes a list of the successive sectional Euler characteristics of a coherent sheaf,
     the i-th entry on the list being the Euler characteristic of the i-th
     generic hyperplane restriction of ", TT "E",
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
	  "euler(HorrocksMumford(2))"
	  },
     SeeAlso => {genera,genus}
     }
document { 
     Key => (euler,Ideal),
     Usage => "euler I",
     Inputs => {"I" => ""
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
	  "euler I"
	  },
     SeeAlso => {genera,genus}
     }
document { 
     Key => (euler,Ring),
     Usage => "euler R",
     Inputs => {"R" => ""
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
	  "euler(R)",
	  "J = substitute(ideal(b,a+c),R)",
	  "euler(R/J)"
	  },
     SeeAlso => {genera,genus},
     }

 -- doc6.m2:1132:     Key => euler,
