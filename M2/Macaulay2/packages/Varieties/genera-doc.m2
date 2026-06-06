--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => genera,
     Headline => "list of the successive linear sectional arithmetic genera",
     SeeAlso => {genus,hilbertPolynomial,euler}
     }
document { 
     Key => (genera,Ring),
     Usage => "genera R",
     Inputs => {"R"
	  },
     Outputs => {List =>{"of the successive linear sectional
     arithmetic genera of projective scheme ", TT "V"," with homogeneous coordinate
     ring ", TT "R"}
	  },
     "Computes the list of successive linear sectional arithmetic genera, 
     where the i-th entry in the list is the arithmetic genus of the i-th
     successive generic hyperplane section of ", TT "V", " ", 
     "(= (-1)^dim-lin-section * (chi(OO_lin-section) - 1)).",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_4];",
	  "I = ideal random(R^1, R^{-2,-3});",
	  "genera(R/I)"  
	  },
     SeeAlso => {genus, euler}
     }
document { 
     Key => {(genera,CoherentSheaf), (genera,Module)},
     Usage => "genera M",
     Inputs => {"M"
	  },
     Outputs => {List =>{"of the successive linear sectional
     arithmetic genera of the successive generic hyperplane restrictions 
     of ", TT "M"}
	  },
     "Computes the list of successive generic linear sectional arithmetic genera, 
     where the i-th entry in the list is 
     (-1)^dim-support -i  * (chi(M ** OO_lin-section) - 1)).",
     EXAMPLE {
	  "V = Proj(ZZ/101[x_0..x_2]);",
	  "M = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",     
	  "genera M"
	  },
     SeeAlso => {euler}
     }
document { 
     Key => (genera,Ideal),
     Usage => "genera I",
     Inputs => {"I"
	  },
     Outputs => {List =>{"of the successive linear sectional
     arithmetic genera of ", TT "I"}
          }, 
     "Computes the list of successive linear sectional arithmetic genera, 
     where the i-th entry in the list is the arithmetic genus of the i-th
     successive generic hyperplane section of the zero-locus of ", TT "I", " ",  
     "(= (-1)^dim-lin-section * (chi(OO_lin-section) - 1)).",
     PARA{},
     "A complete intersection of type (2,3) in projective fourspace;
     its hyperplane section is a canonical curve of genus 4:",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_4];",
	  "I = ideal random(R^1, R^{-2,-3});",
	  "genera I"  
	  },
     SeeAlso => {euler, genus}
     }

document { 
     Key => {(genera,ProjectiveVariety)},
     Usage => "genera V",
     Inputs => {"V"
	  },
     Outputs => {List => {"of the successive linear sectional
     arithmetic genera of ", TT "V"}
	  },
     "Computes the list of successive linear sectional arithmetic genera, 
     where the i-th entry in the list is the arithmetic genus of the i-th
     successive generic hyperplane section of ", TT "V", " ", 
     "(= (-1)^dim-lin-section * (chi(OO_lin-section) - 1)).",
     PARA{},
     "A complete intersection of type (2,3) in projective fourspace;
     its hyperplane section is a canonical curve of genus 4:",
     EXAMPLE {
	  "R = ZZ/101[x_0..x_4];",
	  "V = Proj(R/(ideal random(R^1, R^{-2,-3})));",
	  "genera V"  
	  },
     SeeAlso => {euler,genus}
     }

 
