--- status: draft
--- author(s): Sorin Popescu, Burt Totaro
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
     Inputs => {"V"},
     Outputs => {ZZ =>"the topological Euler characteristic of the variety V"
	  },    
     "The command computes the topological Euler characteristic of the (smooth) projective 
     variety V as an alternating sum of its Hodge numbers. It also works for smooth subspaces
     of a weighted projective space as a stack, sometimes called quasi-smooth subvarieties
     of the coarse moduli space. If V is singular, this command is harder
     to interpret. But, for example, this command also gives the topological Euler characteristic
     if V has quotient singularities and the characteristic is zero.",
     PARA{},
     "The Hodge numbers can be computed directly using the command ",
     TO2{(hh,Sequence,ProjectiveVariety),"hh^(p,q)(X)"}, " although that is slower.",
     PARA{},
     "A smooth plane quartic curve has genus 3, hence topological Euler characteristic 2 - 2g = -4:",
     EXAMPLE {
	  "Quartic = Proj(QQ[x_0..x_2]/ideal(x_0^4+x_1^4+x_2^4));",
	  "euler(Quartic)"
	  },
     PARA{},
     "The topological Euler characteristic of a smooth quintic hypersurface in
     projective fourspace is -200:",     
     EXAMPLE {
	  "Quintic = Proj(QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-101*x_0*x_1*x_2*x_3*x_4));",
	  "euler(Quintic)"
	  },
     Caveat  => {"No test is made to see if the projective variety is smooth."},
     SeeAlso => {Proj,genus,(hh,Sequence,ProjectiveVariety),(hh,ZZ,CoherentSheaf)},
     }

 document {
     Key => (euler,Module),
     Headline => "Euler characteristic of a coherent sheaf",
     Usage => "euler M",
     Inputs => {"M" => Module},
     Outputs => {ZZ},
     "For ", TT "M", " a graded module over a ring R with generators in degree 1,
     return the Euler characteristic of the sheaf associated to ", TT "M", " on Proj(R).",
     EXAMPLE {
	 "R0 = QQ[x,y,z];",
	 "M1 = R0(3)",
	 "euler M1"
	 },
     SeeAlso => {(euler,CoherentSheaf),(eulers,CoherentSheaf),genus}
     }

document {
     Key => (euler,Ring),
     Headline => "Holomorphic Euler characteristic of projective scheme",
     Usage => "euler R",
     Inputs => {"R" => Ring},
     Outputs => {ZZ},
     "For ", TT "R", " a graded ring with generators in degree 1,
     return the Euler characteristic of the sheaf O_X on X = Proj(R).",
     PARA{},
     "For example, a quartic plane curve has genus 3, hence holomorphic Euler characteristic 1 - 3 = -2.",
     EXAMPLE {
	 "R = ZZ/101[x,y,z]/(x^4+y^4+z^4);",
	 "euler R"
	 },
     SeeAlso => {(euler,CoherentSheaf),(eulers,Ring),genus}
     }

document {
     Key => (euler,Ideal),
     Headline => "Holomorphic Euler characteristic of projective scheme",
     Usage => "euler I",
     Inputs => {"I" => Ideal},
     Outputs => {ZZ},
     "For ", TT "I", " a homogeneous ideal in a graded ring R with generators in degree 1,
     return the Euler characteristic of the sheaf O_X on X = Proj(R/I).",
     PARA{},
     "For example, a quintic plane curve has genus 6, hence holomorphic Euler characteristic 1 - 6 = -5.",
     EXAMPLE {
	 "R0 = ZZ/31991[x,y,z];",
	 "I1 = ideal(x^5+y^5+z^5);",
	 "euler I1"
	 },
     SeeAlso => {(euler,CoherentSheaf),(eulers,Ideal),genus}
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
     Outputs => {List =>"the successive sectional Euler characteristics of a coherent sheaf (or of a module)"
	  },
     "Computes a list of the successive sectional Euler characteristics of a coherent sheaf,
     the i-th entry on the list being the Euler characteristic with coefficients in the i-th
     generic hyperplane restriction of ", TT "E",".",
     PARA{},
     "The Horrocks-Mumford bundle on projective 4-space:",
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
	  characteristics of an ideal (sheaf)"
          },
     "Computes a list of the successive sectional Euler 
     characteristics of an ideal (sheaf), the i-th entry 
     in the list being the Euler characteristic with coefficients in the i-th
     generic hyperplane restriction of ", TT "I",".",
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
	       characteristics of a (sheaf of) ring(s)"
	  },
     "Computes a list of the successive sectional Euler 
     characteristics of a ring, the i-th entry 
     in the list being the Euler characteristic of the i-th
     generic hyperplane restriction of ", TT "Proj(R)",".",
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

doc ///
Node
  Key
    (euler, CoherentSheaf)
  Headline
    Euler characteristic of coherent sheaf
  Usage
    euler F
  Inputs
    F:CoherentSheaf
  Outputs
    :ZZ
  Description
    Text
      This function returns $\chi(X, F)$, the Euler characteristic of the sheaf @TT "F"@
      on a projective variety $X$,
      that is, the alternating sum of the dimensions of its cohomology groups.
      This is usually faster than computing the individual cohomology groups.
      It works for a coherent sheaf on a projective scheme over a field,
      or more generally on a closed subspace of a weighted projective space.
    Text
      The distinction between a weighted projective space as a stack $X$
      and its associated coarse moduli space, $s\colon X \to W$, does not matter
      for computing cohomology. Indeed, for a coherent sheaf @TT "F"@ on $X$,
      we have $H^i(X, F) \cong H^i(W, s_*(F))$ for every $i$.
    Example
      Cubic = Proj(ZZ/31991[x_0..x_2]/ideal(x_0^3+x_1^3+x_2^3));
      euler(OO_Cubic(1))
    Example
      Plane = Proj(QQ[y_0..y_2]);
      euler(cotangentSheaf Plane)
    Example
      WeightedPlane = Proj(ZZ/101[z_0..z_2,Degrees=>{1,2,3}]);
      euler(cotangentSheaf WeightedPlane)
    Example
      QuadricCone = Proj(QQ[s,t,u,v]/(s*t-u^2));
      euler(reflexiveDifferentials QuadricCone)
  SeeAlso
    (euler,CoherentSheaf,ZZ,ZZ)
    (eulers,CoherentSheaf)
    (hh,ZZ,SumOfTwists)
    genus

Node
  Key
    (euler, CoherentSheaf, ZZ, ZZ)
  Headline
    Euler characteristic of coherent sheaf with a range of twists
  Usage
    euler(F,a,b)
  Inputs
    F:CoherentSheaf
    a:ZZ
    b:ZZ
  Outputs
    :RingElement
      the Euler characteristic of the cohomology of the sheaf, with twists from $a$ to $b$
  Description
    Text
      This functions returns the Laurent polynomial $\sum_c \chi(X,F(c))T^c$,
      giving the Euler characteristic of the sheaf @TT "F(c)"@ (@TT "F"@ tensored
      with the line bundle O(c)),
      that is, the alternating sum of the dimensions of its cohomology groups,
      for all $c$ from $a$ to $b$.
      This is usually much faster than computing the individual cohomology groups.
      It works for a coherent sheaf @TT "F"@ on a projective scheme $X$ over a field,
      or more generally on a closed subspace of a weighted projective space.
    Text
      The distinction between a weighted projective space as a stack $X$
      and its associated coarse moduli space, $s\colon X \to W$, does not matter
      for this purpose. Indeed, for a coherent sheaf @TT "F"@ on $X$,
      we have $H^i(X, F) \cong H^i(W, s_*(F))$ for every $i$.
      The twists $F(c)$ are understood
      to take place on the stack $X$, where $O_X(1)$ is a line bundle.
    Example
      Cubic = Proj(ZZ/101[x_0..x_2]/ideal(x_0^3+x_1^3+x_2^3));
      euler(OO_Cubic,-5,5)
  SeeAlso
    (euler,CoherentSheaf)
    (hh,ZZ,CoherentSheaf,ZZ,ZZ)
    (hh,ZZ,SumOfTwists)
    genus
///

