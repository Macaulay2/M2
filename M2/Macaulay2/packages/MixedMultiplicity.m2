--------------------------------------------------------------------------------
-- PURPOSE : Compute the defining ideal of the multi-Rees algebra and 
--	     mixed multiplicities of ideals in a polynomial ring. 
--    	     Also to compute mixed volumes of lattice polytopes and 
--    	     sectional Milnor numbers of hypersurfaces with an 
--           isolated singularity. 
-- PROGRAMMERs : Mixed multiplicity code written by Kriti Goel and
--		 Sudeshna Roy under the guidance of J. K. Verma, IIT Bombay. 
--------------------------------------------------------------------------------

newPackage(
	"MixedMultiplicity",
	Version => "1.0",
	Date => "June 24, 2020",
	Authors => {
		{	Name => "Kriti Goel", 
                        Email => "kritigoel.maths@gmail.com", 
                        HomePage => "https://sites.google.com/view/kritigoel"
		},
		{	Name => "Sudeshna Roy", 
			Email => "sudeshnaroy@math.iitb.ac.in",
			HomePage => "https://sites.google.com/site/sudeshnaroy11"
		},
		{	Name => "J. K. Verma", 
			Email => "jkv@math.iitb.ac.in", 
			HomePage => "https://sites.google.com/site/profjkvermaiitbombay/"
		}
	},
	Headline => "Mixed Multiplicities",
	Keywords => {"Commutative Algebra"},
	DebuggingMode => false,
	Reload => true,
	Headline => "Mixed Multiplicities of ideals"
)
 
    
export {
    "multiReesIdeal",
    "mixedMultiplicity",
    "homIdealPolytope",
    "mixedVolume",
    "secMilnorNumbers"
    }
	
-- For information see documentation key "MixedMultiplicity" below.


---------------------------------------------------------------------------------
--          Defining equations of multi-Rees algebras of ideals          --
---------------------------------------------------------------------------------


-- PURPOSE: To compute the defining ideal of a multi-Rees algebra
-- INPUT: A list of ideals over a polynomial ring
-- OUTPUT: Defining ideal of the multi-Rees algebra of ideals
-- COMMENT: The algorithm uses a generalization of a result of
--	    D. Cox, K.-i. Lin and G. Sosa which gives an expression 
--	    of the defining ideal of the multi-Rees algebra of 
--	    monomial ideals over a polynomial ring.

multiReesIdeal = method(
    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  VariableBaseName => "X"
	  }
      )
multiReesIdeal List := Ideal => o -> W -> (
	R := ring W#0;
		
	-- to check if all ideals are from the same polynomial ring 
	for i from 1 to #W-1 do (
	    if ring W#i =!= R then error "Ambient ring of ideals is not same";
	);
	
	-- to check if $R$ is a polynomial ring
	if class R =!= PolynomialRing then error "Base ring is not a polynomial ring";
	
	n := #W;
	f := (i) -> sum(i+1, j -> numgens W#j);
	m := f(n-1);
	X := o.VariableBaseName;
    	if instance(X, String) then X = getSymbol X 
	else error "expected VariableBaseName option to provide a string or a symbol";
	S := R[X_1..X_m];
	G := (i) -> (first entries gens W#i);
	D := (i) -> (toList apply(f(i)+1..f(i+1), j -> S_(j-1)));  
	M := (i) -> (matrix{G(i), D(i-1)});
	L := sum(n, i -> minors(2, M(i))); 
	
	-- to check if all the ideals in input sequence are monomial ideals or not
	pointer := true;
	i := 0;
	while ( pointer == true and i < #W ) do ( 
		pointer  = isMonomialIdeal W#i;
		i = i+1;
	);
	
	-- If all ideals are monomial ideals, then we use result of Cox, Lin and Sosa
	-- If not, then we use the generalized result
	if pointer == true then b := product gens R
	else b = product(#W, i -> W#i_0);
	c := substitute(b, S);
	RI := trim saturate(L,c);
	RI	
)


---------------------------------------------------------------------------------
--      Computation of mixed multiplicities of multi-graded algebras          --
---------------------------------------------------------------------------------

----------------------
-- mixedMultiplicity
----------------------


-- INPUT: W = (W1,W2), where W1 = (I0,I1,..,Ir) is a sequence of ideals in a polynomial ring 
--        and W2 = (a0,a1,a2,..,ar) is a sequence consisting of the tuple at which
--        the mixed multiplicity will be calculated
-- OUTPUT: Mixed multiplicity e_W2

mixedMultiplicity = method(
    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null
	  }
      )

mixedMultiplicity (Sequence, Sequence) := ZZ => o -> (W1, W2) -> (
	R := ring W1#0;
		
	-- to check if all ideals are from the same polynomial ring 
	for i from 1 to #W1-1 do (
	    if ring W1#i =!= R then error "Ambient ring of ideals is not same";
	);
	
	-- to check if $R$ is a polynomial ring
	if class R =!= PolynomialRing then error "Base ring is not a polynomial ring"; 

	-- to check if the sequence W2 of natural numbers is correct
	if #W1 != #W2 then error "The sequence is incorrect";
	if sum(#W2, i -> W2_i) != dim R-1 then error "The sequence is incorrect";

	n := #W1;
	L1 := multiReesIdeal toList W1;
	V := (i) -> (
	    (1..n)/(j -> if j == i then 1 else 0)
	);
        Q := {};
	for i from 0 to n-1 do (
	    Q = flatten{Q, toList apply(1..(numgens W1#i), j -> toList V(i+1))};
	);
	S := newRing(ring L1, Degrees => Q);
	p := substitute(W1#0, S);
	L2 := substitute(L1, S/p);
	H := reduceHilbert hilbertSeries L2;
	B := toList H;
        T := getSymbol "T";
	A := QQ[T_0..T_(n-1)];
	B0 := substitute(B#0, A);
	B1 := substitute(value B#1, A);
	facB1 := factor B1;
	g := toList apply(n, i-> (facB1#i#1)-1);	
	b := product(n, i -> (g#i - W2#i)!); 
	r := sum(n, i -> g#i - W2#i);
	for i from 0 to n-1 do (
	    a := g#i - W2#i;
	    for j from 1 to a do (
	        B0 = diff(A_i,B0);
	    );
	);
	s := (-1)^r/b; 
	J := ideal(apply(n, i -> A_i-1));
	B2 := substitute(B0, vars (A/J));
	MM := s*B2;
	lift(MM, ZZ)
)


-----------------------------------------------------
--      Mixed volume of lattice polytopes          --
-----------------------------------------------------


--------------------
-- homIdealPolytope
--------------------


-- INPUT: W = {p1,p2,..,pr} a list of vertices of a lattice polytope in R^n
-- OUTPUT: A homogeneous ideal of a polynomial ring over QQ in n+1  
--         number of variables such that the polytope is the convex hull  
--         of the lattice points of its dehomogenized monomials in the 
--         polynomial subring without the last variable

homIdealPolytope = method(
	Options => {
	    CoefficientRing => QQ,
	    VariableBaseName => "X"
	    }
	)
homIdealPolytope List := Ideal => o -> W -> (
	n := #W_0;
        r := #W;
	
	-- to check if the vertices entered are in the same dimension
	for i from 1 to r-1 do (
	    if #W_i != n then error "Incorrect list of vertices entered";
	);
	
        X := o.VariableBaseName;
	if instance(X, String) then X = getSymbol X 
	else "expected VariableBaseName option to provide a string or a symbol";
	k := o.CoefficientRing;
        R := k[X_1..X_(n+1)];
	g := 0;
	for p from 0 to r-1 do (
	    h := W#p;
	    f := product(0..(n-1), i -> R_i^((h_i)));
	    g = g + f;
	);
	g1 := homogenize(g,R_n);    
	I := ideal(terms g1);
	I
)

----------------
-- mixedVolume
----------------


-- INPUT: A list of homogeneous ideals in a polynomial ring 
--        over the field of rational numbers, corresponding 
--        to the lattice polytopes.
-- OR
-- INPUT: A list consisting of lists of vertices of the lattice polytopes.
-- OUTPUT: Mixed volume of the lattice polytopes

mixedVolume = method(
    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  CoefficientRing => QQ,
	  VariableBaseName => "X"
	  }
      )
mixedVolume List := ZZ => o -> W -> (
	if class W#0 === Ideal then  mixedVolumeIdeal W
	else (
	-- to check if all vertices are in same R^n
	for i from 0 to #W-1 do (
	    for j from 0 to #(W#i)-1 do (
	        if #(W#0#0) != #(W#i#j) then error "Ambient space of all polytopes is not same";
	    )
	);				
		
	R := ring homIdealPolytope W#0;
	U := apply(#W, i-> substitute(homIdealPolytope W#i, R));
	mixedVolumeIdeal U
	)
)

-- It is a sub-function of the mixedVolume function. 
-- W = (I1,I2,..,In)
mixedVolumeIdeal = method(
    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  CoefficientRing => QQ,
	  VariableBaseName => "X"
	  }
      )
mixedVolumeIdeal List := ZZ => o -> W -> (
        R := ring W#0;
	-- to check if all ideals are from the same ring
	for i from 1 to #W-1 do (
	    if ring W#i =!= R then error "All ideals are not over the same ring";
	);
	
	-- to check number of ideals in the sequence
	if (#W != dim R - 1) then error "Incorrect sequence entered";
	
	m := ideal vars R;
	W = prepend(m,W);
	n := dim R;
	Q := {};
	for i from 0 to n-1 do (
		apply(1..numgens(W#i), j -> Q = append(Q, toList (1..n)/(j -> if j == (i+1) then 1 else 0)))
	);
	D := apply(0..(n-1), j -> W#j);
	L1 := multiReesIdeal toList D;
	S := newRing(ring L1, Degrees => Q);
	L2 := substitute(L1, S);
	H := reduceHilbert hilbertSeries L2;
	B := toList H;
        T := getSymbol "T";
	A := QQ[T_0..T_(n-1)];
	B0 := substitute(B#0, A);
	B1 := substitute(value B#1, A);
	facB1 := factor B1;
	g := toList apply(n, i-> (facB1#i#1)-1);	
	b := (g#0)!*product(1..(n-1), i -> (g#i-1)!); 
	r := g#0 + sum(1..(n-1), i -> g#i); 	
	B0 = diff(A_0^(g#0),B0);
	for i from 1 to n-1 do (
		B0 = diff(A_i^(g#i-1),B0);
	);
	c := r - n + 1;
	s := (-1)^c/b; 
	J := ideal(apply(0..(n-1), i -> A_i-1));
	B2 := substitute(B0, A/J);
	MV := s*B2;
	lift(MV, ZZ)
)


--------------------------------------------
--      Sectional Milnor numbers          --
--------------------------------------------

--------------------
-- secMilnorNumbers
--------------------

-- INPUT: a polynomial in $d$ variables
-- OUTPUT: e_0(m|J(f)),..., e_{d−1}(m|J(f)), where m is 
--         the maximal homogeneous ideal of the polynomial 
--         ring and J(f) is the Jacobian ideal

secMilnorNumbers = method(
    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null
	  }    
    ) 
secMilnorNumbers RingElement := HashTable => o -> f -> (
	R := ring f;
	d := dim R;
	m := ideal(apply(0..(d-1), i-> R_i));
--	J := ideal(apply(0..(d-1), i-> diff(R_i,f)));
        J := ideal jacobian ideal f;	
	D := toList apply(1..(2*d), i -> if i <= d then {1,0} else {0,1});
	RI := multiReesIdeal {m,J};
        A := newRing(ring RI, Degrees => D);
	M := substitute(m,A);
	P := substitute(RI, A/M);
	H := reduceHilbert hilbertSeries P;
	B := toList H;
    	T := getSymbol "T";
	A1 := QQ[T_0,T_1];          
	B0 := substitute(B#0, A1);
	B1 := substitute(value B#1, A1);
	facB1 := factor B1;
	g := toList apply(2, i -> (facB1#i#1)-1);
	c := g#0 + g#1 - d + 1; 
	h := (i,B0) -> (
	    T := ring B0;
	    b := (g#0-d+1+i)!*(g#1-i)!;
	    q := (-1)^c/b; 
	    for j from 1 to g#0-d+1+i do (
	        B0 = diff(A1_0,B0);
		);
	    for p from 1 to g#1-i do (
	        B0 = diff(A1_1,B0);
		); 
	    I := ideal(A1_0 - 1, A1_1 - 1);
	    B2 := substitute(B0, T/I);
	    w := q*B2;
	    lift(w, ZZ)
	);
	v := new HashTable from {}; 
	for i from 0 to d-1 do (
	    u := new HashTable from {i => h(i,B0)};
	    v = merge(v, u, plus);
	);
	v
)


-------------------------------------------
-----          DOCUMENTATION          -----
-------------------------------------------

beginDocumentation()


///
restart
uninstallPackage "MixedMultiplicity"
restart
installPackage "MixedMultiplicity"
viewHelp MixedMultiplicity
check "MixedMultiplicity"
///


doc ///
  Key
    MixedMultiplicity
  Headline
    Calculate mixed multiplicities, mixed volume and sectional Milnor numbers
  Description
    Text
      P. B. Bhattacharya and J. J. Risler - B. Teissier proved that if $I_0,I_1,...,I_r$ 
	  are $m$-primary ideals in a Noetherian local ring $(R,m)$ of dimension $d$, then 
	  the function $B(u_0,u_1,...,u_r) = l(R/I_0^{u_0}I_1^{u_1}...I_r^{u_r})$ is a polynomial 
	  function in $u_0,u_1,...,u_r$ of degree $d.$ The coefficients of the top degree
	  term are called the mixed multiplicities of the ideals $I_0,I_1,...,I_r.$ 
	  This result was generalized for ideals of positive height in the works of D. Katz, J. K. Verma 
	  and N. V. Trung. D. Rees studied these numbers using complete and joint reductions 
	  of ideals. 
	  
	  Our algorithm requires computation of the defining ideal of the multi-Rees algebra 
	  of ideals. An expression of the defining ideal of the multi-Rees algebra of 
	  monomial ideals over a polynomial ring was given by D. Cox, K.-i. Lin and G. Sosa in
	  (Multi-Rees algebras and toric dynamical systems. Proc. Amer. Math. Soc., 
	  147(11):4605-4616, 2019). We use a generalization of their result for ideals over a 
	  polynomial ring. 
	  
	  The computation of mixed multiplicities helps compute mixed volume of a collection of 
	  lattice polytopes and also the sectional Milnor numbers of hypersurfaces with an
	  isolated singularity.
	  
	  Let $Q_1,...,Q_n$ be a collection of lattice polytopes in $\mathbb{R}^n$ and 
	  $t_1,...,t_n \in \mathbb{R}_+$. Minkowski proved that the $n$-dimensional
	  volume, $vol_n(t_1Q_1 + ... + t_nQ_n)$ is a homogeneous polynomial of degree $n$ in
	  $t_1,...,t_n.$ The coefficient of $t_1...t_n$ is called the mixed volume of $Q_1,...,Q_n.$
	  N. V. Trung and J. K. Verma proved that the mixed volume of lattice polytopes in the 
	  above setup can be realized as a mixed multiplicity of the homogeneous ideals 
	  corresponding to the lattice polytopes.
	  
	  Let origin be an isolated singular point of a complex analytic hypersurface 
	  $H = V(f).$ The $\mathbb{C}$-vector space dimension of 
	  $\mathbb{C}\{x_0,...,x_n\}/(f_{x_0},...,f_{x_n})$ is called the Milnor 
	  number of the hypersurface $H$ at the origin. Let $(X, x)$ be a germ of a 
	  hypersurface in $\mathbb{C}^{n+1}$ with an isolated singularity. 
	  The Milnor number of $X \cap E$, where $E$ is a general linear subspace of dimension
	  $i$ passing through $x$ is called the $i$-th sectional Milnor number of $X$. B. Teissier 
	  identified the $i$-th sectional Milnor number with the $i$-th mixed multiplicity of
	  the maximal homogeneous ideal of the polynomial ring and the Jacobian ideal of $f.$
///   

doc ///
  Key
    multiReesIdeal
  Headline
    Compute the defining ideal of multi-Rees algebra of ideals
  Usage
    multiReesIdeal W
  Inputs
    W:List 
      list of ideals $I_1,...,I_n$ over a polynomial ring.
  Outputs
    :Ideal
      defining ideal of the multi-Rees algebra of $I_1,...,I_n$.
  Description
   Text
      The function computes the defining ideal of the multi-Rees algebra of a set of ideals 
      over a polynomial ring by computing the saturation of a binomial ideal with respect 
      to a polynomial. The technique is a generalization of a result of D. Cox, K.-i. Lin 
      and G. Sosa for monomial ideals over a polynomial ring.
   Example 
     S = QQ[x_0..x_3]
     C = trim monomialCurveIdeal(S,{2,3,5})
     multiReesIdeal {C}
     multiReesIdeal {C,C}
///


doc ///
  Key
    mixedMultiplicity
  Headline
    Compute a given mixed multiplicity of ideals in a polynomial ring.
  Usage
    mixedMultiplicity (W1, W2)
  Inputs
    W1:Sequence 
      sequence of ideals $I_0,...,I_r$ over a polynomial ring.
    W2:Sequence
      tuple $a=(a_0,...,a_r)$ at which the mixed multiplicity is calculated
  Outputs
    :ZZ
      mixed multiplicity $e_a$ of ideals $I_0,...,I_r$.
  Description
   Text
     Given the ideals $I_0,...,I_r$ in a polynomial ring $R$ and the tuple 
     $a = (a_0,...,a_r) \in \mathbb{N}^{r+1}$ such that $I_0$ is primary to the
     maximal homogeneous ideal of $R$, $I_1,...,I_r$ have positive height 
     and $a_0+...+a_r = dim R -1$, the command computes the mixed 
     multiplicity $e_a$ of the ideals. 
   Example 
     R = QQ[x,y,z,w]
     I = ideal(x*y*w^3,x^2*y*w^2,x*y^3*w,x*y*z^3)
     m = ideal vars R;
     mixedMultiplicity ((m,I,I,I),(0,1,1,1))
   Text
     The function computes the Hilbert polynomial of the graded ring 
     $\oplus I_0^{u_0}I_1^{u_1}...I_r^{u_r}/I_0^{u_0+1}I_1^{u_1}...I_r^{u_r}$ to
     calculate the mixed multiplicity. This setup enforces $a_0 \neq 0.$ 
     Due to the same reason, to compute the $(a_0+1, a_1,..., a_r)$-th
     mixed multiplicity, one needs to enter the sequence ${a_0,a_1,...,a_r}$ in 
     the function. The same is illustrated in the following example.
   Example
     R = QQ[x,y,z]
     m = ideal vars R
     f = z^5 + x*y^7 + x^15
     I = ideal(apply(0..2, i -> diff(R_i,f)))
     mixedMultiplicity ((m,I),(2,0))
     mixedMultiplicity ((m,I),(1,1))
///


doc ///
  Key
    homIdealPolytope
  Headline
    Compute the homogeneous ideal corresponding to the vertices of a lattice polytope in $\mathbb{R}^n$.
  Usage
    homIdealPolytope W
  Inputs
    W:List 
      list ${p_1,...,p_r}$, where $p_1,...,p_r$ are vertices of the polytope. 
  Outputs
    :Ideal
      A homogeneous ideal of $k[x_1,...,x_{n+1}].$ 
  Description
   Text
      Given a list of vertices of a lattice polytope, the command outputs a homogeneous 
      ideal of $k[x_1,...,x_{n+1}]$ such that the polytope is the convex hull of the
      lattice points of the dehomogenization of a set of monomials that generates the 
      ideal in $k[x_1,...,x_n]$.
      The following example computes the homogeneous ideal corresponding to a 2-cross polytope.
   Example 
	 I = homIdealPolytope {(0,1),(1,0),(2,1),(1,2)}
///


doc ///
  Key
    mixedVolume
  Headline
    Compute the mixed volume of a collection of lattice polytopes
  Usage
    mixedVolume W
  Inputs
    W:List 
      list of homogeneous ideals $I_1,...,I_n$ over a polynomial ring,
      or a list of lists of vertices of the polytopes
  Outputs
    :ZZ
      mixed volume
  Description
   Text
     Let $Q_1,...,Q_n$ be a collection of lattice polytopes in $\mathbb{R}^n$ 
     and let $I_1,...,I_n$ be homogeneous ideals in a polynomial ring over 
     the field of rational numbers, corresponding to the given polytopes. 
     These ideals can be obtained using the command @TO homIdealPolytope @. 
     The mixed volume is calculated by computing a mixed multiplicity of these ideals. 
	 
     The following example computes the mixed volume of three 3-cross polytopes.
   Example 
     I = homIdealPolytope {(0,1,1),(1,0,1),(1,1,0),(2,1,1),(1,2,1),(1,1,2)}
     mixedVolume {I,I,I}
   Text
     One can also compute the mixed volume of a collection of lattice polytopes 
     by directly entering the vertices of the polytopes. Mixed Volume in the above 
     example can also be computed as follows.
   Example 
     C = {(0,1,1),(1,0,1),(1,1,0),(2,1,1),(1,2,1),(1,1,2)}
     mixedVolume {C,C,C}
///


doc ///
  Key
    secMilnorNumbers
  Headline
    Compute the sectional Milnor numbers of a hypersurface with an isolated singularity
  Usage
    secMilnorNumbers f
  Inputs
    f:RingElement
      polynomial
  Outputs
    :HashTable
      First $d-1$ sectional Milnor numbers, where $d$ is the dimension of the polynomial ring
  Description
   Text
     Let $f$ be an element of a polynomial ring $R$ and let $d$ be the dimension of $R$. 
     The function computes the first $d-1$ sectional Milnor numbers by computing 
     the mixed multiplicities $e_0(m|J(f)),...,e_{d-1}(m|J(f))$, where $m$ is the 
     maximal homogeneous ideal of $R$ and $J(f)$ is the Jacobian ideal of $f$. 
   Example 
     k = frac(QQ[t])
     R = k[x,y,z]
     secMilnorNumbers(z^5 + t*y^6*z + x*y^7 + x^15)
     secMilnorNumbers(z^5 + x*y^7 + x^15)
///

doc ///
  Key 
    [multiReesIdeal, VariableBaseName]
    [homIdealPolytope, VariableBaseName]
    [mixedVolume, VariableBaseName]
  Headline
    Choose a base name for variables in the created ring
  Usage
    multiReesIdeal(..., VariableBaseName => X)
    homIdealPolytope(..., VariableBaseName => X)
    mixedVolume(..., VariableBaseName => X)
  Description
    Text
      Each of these functions creates a new ring of the form $R[X_0,\ldots, X_r]$
      or $k[X_0,\ldots, X_r]$, where $R$ is the ring of the input ideal and $k$ is 
      the coefficient ring of the output ideal. This option allows the user to 
      change the base names of the new variables in this ring. The default variable is X.
    Example
      S = QQ[x_0..x_3]
      C = trim monomialCurveIdeal(S,{2,3,5})
      multiReesIdeal ({C}, VariableBaseName => "T")
    Example
      homIdealPolytope ({(0,1),(1,0),(2,1),(1,2)}, VariableBaseName => "T")
///


doc ///
  Key 
    [homIdealPolytope, CoefficientRing]
    [mixedVolume, CoefficientRing]
  Headline
    Choose a coefficient ring of the (output) ideal
  Usage
     homIdealPolytope(..., CoefficientRing => QQ)
     mixedVolume(..., CoefficientRing => QQ)  
  Description
    Text
      The function @TO homIdealPolytope@ creates a new ring of the form 
      $k[X_0,\ldots, X_r]$, where $k$ is the coefficient ring of the output ideal. 
      This option allows the user to chose the coefficient ring $k.$ The default ring is QQ.
    Example
      I = homIdealPolytope ({(0,1),(1,0),(2,1),(1,2)}, CoefficientRing => ZZ/2)	 
///


doc ///
   Key
    [multiReesIdeal, Strategy]  
    [mixedMultiplicity, Strategy] 
    [mixedVolume, Strategy]	
    [secMilnorNumbers, Strategy]  
   Headline
     Choose a strategy for the saturation step
   Usage
     multiReesIdeal(..., Strategy => X)
     mixedMultiplicity(..., Strategy => X)
     mixedVolume(..., Strategy => X)
     secMilnorNumbers(..., Strategy => X)
   Description
    Text
     where X is one of @TO Iterate@, @TO Linear@, @TO Bayer@, @TO Eliminate@.
     These are described in the documentation node for @TO saturate@.
   SeeAlso
    multiReesIdeal
    mixedMultiplicity
    mixedVolume
    secMilnorNumbers
///


doc ///
   Key
    [multiReesIdeal, PairLimit]
    [mixedMultiplicity, PairLimit]
    [mixedVolume, PairLimit]
    [secMilnorNumbers, PairLimit]
   Headline
    Bound the number of s-pairs considered in the saturation step
   Usage
    multiReesIdeal(..., PairLimit => X)
    mixedMultiplicity(..., PairLimit => X)
    mixedVolume(..., PairLimit => X)
    secMilnorNumbers(..., PairLimit => X)
   Description
    Text
     Here X is a positive integer. The calculation of Rees ideal requires 
     a saturation step, and the optional argument causes the saturation
     process to stop after that number of s-pairs is found.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    multiReesIdeal
    mixedMultiplicity
    mixedVolume
    secMilnorNumbers
///


doc ///
   Key
    [multiReesIdeal, MinimalGenerators]
    [mixedMultiplicity, MinimalGenerators]
    [mixedVolume, MinimalGenerators]
    [secMilnorNumbers, MinimalGenerators]
   Headline
    Whether the saturation step returns minimal generators
   Usage
    multiReesIdeal(..., MinimalGenerators => X)
    mixedMultiplicity(..., MinimalGenerators => X)
    mixedVolume(..., MinimalGenerators => X)
    secMilnorNumbers(..., MinimalGenerators => X)
   Description
    Text
     Here X is of type boolean. Each of these functions involves the
     computation of a Rees ideal, which involves a saturation step.
     This optional argument determines whether or not 
     the output of the saturation step will be forced to have a minmimal generating set.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    multiReesIdeal
    mixedMultiplicity
    mixedVolume
    secMilnorNumbers
///


doc ///
   Key
    [multiReesIdeal, BasisElementLimit]
    [mixedMultiplicity, BasisElementLimit]
    [mixedVolume, BasisElementLimit]
    [secMilnorNumbers, BasisElementLimit]
   Headline
    Bound the number of Groebner basis elements to compute in the saturation step
   Usage
    multiReesIdeal(..., BasisElementLimit => X)
    mixedMultiplicity(..., BasisElementLimit => X)
    mixedVolume(..., BasisElementLimit => X)
    secMilnorNumbers(..., BasisElementLimit => X)
   Description
    Text
     Here X is a positive integer. Each of these functions computes the Rees
     ideal using a saturation step, and the optional argument causes the saturation
     process to stop after that number of s-pairs is found.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    multiReesIdeal
    mixedMultiplicity
    mixedVolume
    secMilnorNumbers
///


doc ///
   Key
    [multiReesIdeal, DegreeLimit]
    [mixedMultiplicity, DegreeLimit]
    [mixedVolume, DegreeLimit]
    [secMilnorNumbers, DegreeLimit]
   Headline
    Bound the degrees considered in the saturation step.
   Usage
    multiReesIdeal(..., DegreeLimit => X)
    mixedMultiplicity(..., DegreeLimit => X)
    mixedVolume(..., DegreeLimit => X)
    secMilnorNumbers(..., DegreeLimit => X)
   Description
    Text
     where X is a non-negative integer. Stop computation at degree X.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    multiReesIdeal
    mixedMultiplicity
    mixedVolume
    secMilnorNumbers
///


----Tests----

TEST ///
-- test for multiReesIdeal
  R = QQ[x,y]
  m = ideal vars R
  Rm = multiReesIdeal {m^2}
  use ring Rm
  assert(Rm == ideal(y*X_2-x*X_3, y*X_1-x*X_2, X_2^2-X_1*X_3))
  RI = multiReesIdeal {m^2,m^2}
  use ring RI
  assert(RI == ideal( X_5*y-X_6*x, X_4*y-X_5*x, X_2*y-X_3*x, X_1*y-X_2*x, X_5^2-X_4*X_6, X_3*X_5-X_2*X_6, X_2*X_5-X_1*X_6, X_3*X_4-X_1*X_6, X_2*X_4-X_1*X_5, X_2^2-X_1*X_3))
  J = ideal(x^3 + y^3, x^2*y + y^2)
  RIJ = multiReesIdeal {m^2,J}
  use ring RIJ
  assert(RIJ == ideal(X_2*y-X_3*x, X_1*y-X_2*x, X_2^2-X_1*X_3, X_2*X_4*x+X_3*X_4-X_1*X_5*x-X_3*X_5*y, X_4*x^2*y+X_4*y^2-X_5*x^3-X_5*y^3))
///

TEST ///
-- test for multiReesIdeal
-- Checking if different strategies yeild the same multi-Rees ideal
  S=ZZ/101[x_0..x_4]
  I=monomialCurveIdeal(S,{5,9,11})
  time M1 = gens gb multiReesIdeal {I}; 
  time M2 = gens gb multiReesIdeal({I}, Strategy => Bayer);
  M1 = substitute(M1, ring M2);
  assert(M2 == M1)
  time M3 = gens gb multiReesIdeal ({I,I}, Strategy => Bayer);
  time M4 = gens gb multiReesIdeal {I,I};
  M3 = substitute(M3, ring M4);
  assert(M3==M4)
///

TEST ///
-- test for mixedMultiplicity
  R = QQ[x,y,z]
  m = ideal vars R
  f = z^5 + x*y^7 + x^15
  I = ideal(apply(0..2, i -> diff(R_i,f)))
  E3 = mixedMultiplicity ((m,I),(2,0))
  E2 = mixedMultiplicity ((m,I),(1,1))
  assert(E3 == 1)
  assert(E2 == 4)
///

TEST ///
-- test for mixedMultiplicity  
  B = QQ[x,y,z]
  I = ideal(x^5*y, x*y^3*z^2, x*y*z^4)
  m = ideal vars B
  mm = mixedMultiplicity((m,I,I),(0,1,1))
  assert(mm == 8)
///

TEST ///
-- test for homIdealPolytope 
  I = homIdealPolytope {(0,1),(1,0),(2,1),(1,2)}
  use ring I
  assert(I == ideal(X_1^2*X_2, X_1*X_2^2, X_1*X_3^2, X_2*X_3^2))
///


TEST ///
-- test for mixedVolume
  I = homIdealPolytope {(0,1),(1,0),(2,1),(1,2)}
  MVI = mixedVolume {I,I}
  assert(MVI == 4)
///

TEST ///
-- test for mixedVolume
  C = {(0,1,1),(1,0,1),(1,1,0),(2,1,1),(1,2,1),(1,1,2)}
  MVC = mixedVolume {C,C,C}
  assert(MVC == 8)
///

TEST ///
-- test for secMilnorNumbers
  R = QQ[x,y,z]
  f = x^2*y + y^2*z + z^3
  SMN = secMilnorNumbers(f)
  assert(SMN === hashTable{0 => 1, 1 => 2, 2 => 4})
///

TEST ///
-- test for secMilnorNumbers
  R = QQ[x,y,z]
  SMN = secMilnorNumbers(z^5 + x*y^7 + x^15)
  assert(SMN === hashTable{0 => 1, 1 => 4, 2 => 28})
///

end--

restart
uninstallPackage "MixedMultiplicity"
restart
installPackage "MixedMultiplicity"
check "MixedMultiplicity"
