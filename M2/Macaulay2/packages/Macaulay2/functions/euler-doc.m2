--- status: draft
--- author(s): 
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
	  "HH^0(HorrocksMumford(2))",
	  "euler(HorrocksMumford(2))"
	  },
     SeeAlso => {genera,genus}
     }
document { 
     Key => (euler,Ideal),
     Usage => "euler I",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (euler,Ring),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc6.m2:1132:     Key => euler,
