--------------------------------------------------------------------------------
-- PURPOSE : Compute the defining ideal of the multi-Rees algebra and 
--	     mixed multiplicities of ideals. Also to compute mixed
--    	     volumes of lattice polytopes and sectional Milnor numbers 
--    	     of hypersurfaces with an isolated singularity.
-- PROGRAMMERs : Mixed multiplicity code written by Kriti Goel, Vivek Mukundan 
--		 and Sudeshna Roy, under the guidance of J. K. Verma, IIT Bombay. 
--------------------------------------------------------------------------------

newPackage(
    "MixedMultiplicity",
    Version => "3.0",
    Date => "May, 2023",
    Authors => {
	{    Name => "Kriti Goel", 
	    Email => "kritigoel.maths@gmail.com", 
            HomePage => "https://sites.google.com/view/kritigoel"
	    },
	{    Name => "Vivek Mukundan",
	    Email => "vivekm85@gmail.com", 
	    HomePage => "https://web.iitd.ac.in/~vmukunda"
	    },
	{    Name => "Sudeshna Roy", 
	    Email => "sudeshnaroy@math.iitb.ac.in",
	    HomePage => "https://sites.google.com/site/sudeshnaroy11"
	    },
	{    Name => "J. K. Verma", 
	    Email => "jkv@math.iitb.ac.in", 
	    HomePage => "https://sites.google.com/site/profjkvermaiitbombay/"
	    }
	},
    Headline => "Mixed Multiplicities",
    Headline => "Mixed Multiplicities of ideals",
    PackageImports=>{"Divisor", "ReesAlgebra", "Depth", "Polyhedra"},
    Keywords => {"Commutative Algebra"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "Algorithms for computing mixed multiplicities, mixed volumes, and sectional Milnor numbers",
	"acceptance date" => "2023-06-06",
	"published article URI" => "https://msp.org/jsag/2023/13-1/p01.xhtml",
	"published article DOI" => "10.2140/jsag.2023.13.1",
	"published code URI" => "https://msp.org/jsag/2023/13-1/jsag-v13-n1-x01-MixedMultiplicity.m2",
	"release at publication" => "23c0f53e78831d43a978c61ccdc0cddef35efae7",
	"version at publication" => "3.0",
	"volume number" => "13",
	"volume URI" => "https://msp.org/jsag/2023/13-1/"
	}
)
 
    
export {
    "multiReesIdeal",
    "mixedMultiplicity",
    "homIdealPolytope",
    "mMixedVolume",
    "secMilnorNumbers"
    }
	
-- For information see documentation key "MixedMultiplicity" below.


---------------------------------------------------------------------------------
--          Defining equations of multi-Rees algebras of ideals          --
---------------------------------------------------------------------------------


-- PURPOSE: To compute the defining ideal of multi-Rees algebra of ideals
-- INPUT: A list of ideals or a list of ideals and a list of nonzerodivisors
-- OUTPUT: Defining ideal of the multi-Rees algebra of input ideals
-- COMMENT: When the ambient ring is a domain, the algorithm uses a 
--    	    generalization of a result of D. Cox, K.-i. Lin and 
--    	    G. Sosa to get an expression of the defining ideal 
--    	    of the multi-Rees algebra of ideals. Otherwise, the 
--    	    function uses a generalization of the algorithm of 
--    	    the function reesIdeal for multi-Rees algebra of ideals.

multiReesIdeal = method(
    Options => {VariableBaseName => "X"}
    )
  
multiReesIdeal List := Ideal => o -> W -> (
    R := ring W#0;
    numofIdeals := #W;
    
    for i from 0 to #W-1 do (
	-- to check if W is a list of ideals 
	if isIdeal W#i == false then error "All entries are not ideals";
	-- to check if all ideals are from the same ring 
	if ring W#i =!= R then error "Ambient ring of ideals is not same";
	-- to check for unit ideals
	if W#i == R then error "Unit ideal added";
	);
    --to remove zero entries, if any, from gen. set of ideals
    removeZeroes := Id -> ideal compress gens Id;
    
    --new list of input ideals with no zero entry in gen. set
    U := W/removeZeroes;
    
    X := o.VariableBaseName;
    if instance(X, String) then X = getSymbol X;
    
    if isDomain R then (
	f := i -> sum(i+1, j -> numgens U#j);
	m := f(numofIdeals-1);
	
	V := i -> ( (1..numofIdeals)/(j -> if j == i then 1 else 0) ); 
	Q := {};
	for i from 0 to numofIdeals-1 do (
	    Q = flatten{Q, toList apply(1..(numgens U#i), j -> flatten{toList V(i+1), degree U#i_(j-1)})};
	    );
	S := R[X_0..X_(m-1), Degrees => Q, Join=>false];
	G := i -> first entries gens U#i;
	D := i -> toList apply(f(i)+1..f(i+1), j -> S_(j-1));
	M := i -> matrix{G(i), D(i-1)};
	L := sum(numofIdeals, i -> minors(2, M(i)));
	
	-- to check if all the ideals in input sequence are monomial ideals or not
	monIdeal := true;
	i := 0;
	while ( monIdeal == true and i < #U ) do (
	    monIdeal = isMonomialIdeal U#i;
	    i = i+1;
	    );
	
	-- If all ideals are monomial ideals, then we use result of Cox, Lin and Sosa
	-- It is faster as the saturation now is with respect to product of variables
	if monIdeal == true then b := product gens R
	else b = product(#W, i -> U#i_0);
	c := substitute(b, S);
	return trim saturate(L,c)
	)
    else (
	--constructing source symmetric algebra 
	ni' := numgens U#0;
	SSA := symmetricAlgebra(source gens U#0, Variables => toList(X_0 .. X_(ni'-1)));  
	if numofIdeals > 1 then (
	    for i from 1 to (numofIdeals-1) do (
	    ni := numgens U#i;     -- computes the number of generators of i-th ideal 
	    VarNames := toList(X_(ni') .. X_(ni'+ni-1)); 
	    SSA = SSA**symmetricAlgebra(source gens U#i, Variables => VarNames); 
	    ni' = ni'+ni;              -- saving the number of gens in n_i-1st ideal 
	    );
	); 
 
	--constructing target symmetric algebra 
	T := getSymbol"T";
	TSA := symmetricAlgebra(target gens U#0, Variables => {T_0});
	if numofIdeals > 1 then (
	    for i from 1 to (numofIdeals-1) do ( 
	    TSA = TSA**symmetricAlgebra(target gens U#i, Variables => {T_i}); 
	    );
	); 
	matrixGens := sub(matrix{{}}, TSA); 
	t' := 0;
	for i from 0 to (numofIdeals-2) do ( 
	    t := numgens U#i; 
	    if (i==0) then (VarNames := toList(X_0 .. X_(t-1))) else (VarNames = toList(X_(t') .. X_(t'+t-1))); 
	    matrixGens = matrixGens|((sub(matrix(map(symmetricAlgebra(target gens U#i, Variables => {T_i}),symmetricAlgebra(source gens U#i, Variables => VarNames), gens(U#i))), TSA))_{0 .. (t-1)}); 
	    t' = t'+t; 
	    ); 
	nn := numofIdeals-1; 
	t := numgens U#nn; 
	VarNames := toList(X_(t') .. X_(t'+t-1)); 
	matrixGens = matrixGens|(sub(matrix(map(symmetricAlgebra(target gens U#nn, Variables => {T_nn}),symmetricAlgebra(source gens U#nn, Variables => VarNames), gens(U#nn))), TSA)); 
	return trim ker map(TSA, SSA, matrixGens) 
	)	
)

multiReesIdeal Ideal := Ideal => o -> W -> (
    multiReesIdeal ({W},o)
)  

multiReesIdeal (List, List) := Ideal => o -> (W1, W2) -> (
    R := ring W1#0;
    numIdeals := #W1;

    -- to check if the sequence W2 has correct length
    if #W1 != #W2 then error "The lists entered don't have the same length"; 
    
    for i from 0 to numIdeals-1 do (
	-- to check if W1 is a list of ideals 
	if isIdeal W1#i == false then error "All entries are not ideals";
	-- to check if all ideals are from the same ring 
	if ring W1#i =!= R then error "Ambient ring of ideals is not same";
	-- to check W2 is a list of nonzerodivisors
	if ann W2#i != sub(ideal(0),R) then error "The list contains a zerodivisor";		
	);
    
    --to remove zero entries, if any, from gen. set of ideals
    removeZeroes := Id -> ideal compress gens Id;
    
    --new list of input ideals with no zero entry in gen. set
    U := W1/removeZeroes;
	
        
    X := o.VariableBaseName;
    if instance(X, String) then X = getSymbol X;

    f := i -> sum(i+1, j -> numgens U#j);
    m := f(numIdeals-1);
    V := i -> ( (1..numIdeals)/(j -> if j == i then 1 else 0) ); 
    Q := {};
    for i from 0 to numIdeals-1 do (
	Q = flatten{Q, toList apply(1..(numgens U#i), j -> flatten{toList V(i+1), degree U#i_(j-1)})};
	);
    S := R[X_0..X_(m-1), Degrees => Q, Join=>false];
    G := i -> first entries gens U#i;
    D := i -> toList apply(f(i)+1..f(i+1), j -> S_(j-1));
    M := i -> matrix{G(i), D(i-1)};
    L := sum(numIdeals, i -> minors(2, M(i)));
    
    b := product(#W2, i -> W2#i);
    c := substitute(b, S);
    return trim saturate(L,c)
)

multiReesIdeal (Ideal, RingElement) := Ideal => o -> (W,f) -> (
    multiReesIdeal ({W},{f},o)
)


---------------------------------------------------------------------------------
--      Computation of mixed multiplicities of multi-graded algebras          --
---------------------------------------------------------------------------------

----------------------
-- mixedMultiplicity
----------------------


-- INPUT: W = (W1,W2), where W1 = (I0,I1,..,Ir) is a sequence of ideals 
--        and W2 = (a0,a1,a2,..,ar) is the mixed multiplicity that is to be calculated
-- OUTPUT: Mixed multiplicity e_W2

mixedMultiplicity = method()

mixedMultiplicity (Sequence, Sequence) := ZZ => (W1, W2) -> (
    R := ring W1#0;

    -- to check dimension of R
    if dim R == 0 then error "Ambient ring should have dimension at least one";
    
    -- to check if the sequence W2 of natural numbers is correct
    if #W1 != #W2 then error "The sequence is incorrect";     
    
    for i from 0 to #W1-1 do (
	-- to check if W1 is a sequence of ideals 
	if isIdeal W1#i == false then error "The sequence of ideals is incorrect";
	-- to check if all ideals are from the same ring 
	if ring W1#i =!= R then error "Ambient ring of ideals is not same";    	
	--check if input ideals have positive grade
	if depth(W1#i, R) == 0 then error "All input ideals don't have positive grade";
	-- to check if the sequence W2 of natural numbers is correct
	if (instance(W2_i,ZZ) == false or W2_i<0) then error "The sequence of integers is incorrect";
	);
    -- to check if the sequence W2 of natural numbers is correct
    if sum(#W2, i -> W2_i) != dim R-1 then error "The sequence of numbers is incorrect";
    
    n := #W1;
    L1 := multiReesIdeal (toList W1);
    V := i -> ( (1..n)/(j -> if j == i then 1 else 0) );
    Q := {};
    for i from 0 to n-1 do (
	Q = flatten{Q, toList apply(1..(rank source compress gens W1#i), j -> toList V(i+1))};
	);
    S := newRing(ring L1, Degrees => Q);
    p := substitute(W1#0, S);
    L2 := substitute(L1, S/p); 
    H := reduceHilbert hilbertSeries L2; 
    B := toList H;
    T := getSymbol "T"; 
    A := QQ (monoid [T_0..T_(n-1)]); 
    B0 := substitute(B#0, vars A); 
    B1 := substitute(B#1, vars A); 
    facB1 := factor value B1;
    g := toList apply(n, i -> (facB1#(n-1-i)#1)-1);
    diffList := apply(n, i -> g#i - W2#i);
    negCheck := 0;
    for i from 0 to n-1 do (
	if diffList#i < 0 then negCheck = 1;
	);
    if (negCheck == 0) then (
	b := product(n, i -> (diffList#i)!); 
	r := sum(n, i -> diffList#i); 
	for i from 0 to n-1 do ( 
	    for j from 1 to diffList#i do (
		B0 = diff(A_i,B0);
		);
	    );
	s := (-1)^r/b; 
	J := ideal(apply(n, i -> A_i-1)); 
	B2 := substitute(B0, vars (A/J)); 
	MM := s*B2; 
	return lift(MM, ZZ)
	)
    else return 0
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
    for i from 0 to r-1 do (
	if #W_i != n then error "Incorrect list of vertices entered";
	); 
    
    --to translate points    
    shiftList := splice new MutableList from {n:0}; 
    for i from 0 to r-1 do (
	apply(0..n-1, j -> if W#i#j < shiftList#j then shiftList#j = W#i#j);
	); 
    WNew := toList apply(0..r-1, j -> toSequence (toList W#j - toList shiftList));
         
    X := o.VariableBaseName; 
    if instance(X, String) then X = getSymbol X; 
    k := o.CoefficientRing;
    R := k[X_1..X_(n+1)];
    g := sum for p from 0 to r-1 list ( product(0..(n-1), i -> R_i^(((WNew#p)_i))) );
    g1 := homogenize(g,R_n); 
    I := ideal(terms g1)
)

----------------
-- mMixedVolume
----------------


-- INPUT: A list of homogeneous ideals in a polynomial ring 
--        over the field of rational numbers, corresponding 
--        to the lattice polytopes, or a list consisting of 
--    	  lists of vertices of the lattice polytopes.
-- OUTPUT: Mixed volume of the lattice polytopes

mMixedVolume = method()

mMixedVolume List := ZZ => W -> (
    if class W#0 === Ideal then  mixedVolumeIdeal (W) 
    else (
	-- to check if all vertices are in same R^n
	for i from 0 to #W-1 do (
	    for j from 0 to #(W#i)-1 do (
	        if #(W#0#0) != #(W#i#j) then error "Ambient space of all polytopes is not same";
		) 
	    );
	R := ring homIdealPolytope W#0;
	U := apply(#W, i-> substitute(homIdealPolytope W#i, R));
	mixedVolumeIdeal (U)
	)
)

-- It is a sub-function of the mixedVolume function. 
-- W = (I1,I2,..,In)
mixedVolumeIdeal = method()
mixedVolumeIdeal List := ZZ => W -> (
    R := ring W#0;
    
    -- to check if all ideals are from the same ring 
    for i from 0 to #W-1 do (
	if ring W#i =!= R then error "All ideals are not over the same ring";
	);
    
    -- to check number of ideals in the sequence
    if (#W != dim R - 1) then error "Incorrect sequence entered";
    
    m := ideal vars R;
    W = prepend(m,W);
    n := dim R;	
    L1 := multiReesIdeal (W);
    V := i -> ( (1..n)/(j -> if j == i then 1 else 0) );
    Q := {};
    for i from 0 to n-1 do (
	Q = flatten{Q, toList apply(1..(rank source compress gens W#i), j -> toList V(i+1))};
	);
    S := newRing(ring L1, Degrees => Q);
    L2 := substitute(L1, S/(sub(m,S)));
    H := reduceHilbert hilbertSeries L2;
    B := toList H;
    T := getSymbol "T";	
    A := QQ monoid([T_0..T_(n-1)]); 
    B0 := substitute(B#0, vars A);
    B1 := substitute(B#1, vars A); 
    facB1 := factor value B1;
    g := toList apply(n, i-> (facB1#(n-1-i)#1)-1);
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
-- OUTPUT: e_0(m|J(f)),..., e_{d}(m|J(f)), where m is 
--         the maximal homogeneous ideal of the polynomial 
--         ring and J(f) is the Jacobian ideal of height d.

secMilnorNumbers = method() 

secMilnorNumbers RingElement := HashTable => f -> ( 
    --to check if input is a nonzero polynomial
    if f == 0 then error "Zero polynomial entered";  
    
    R := ring f;
    --to check the characteristic of the ring
    if char R != 0 then error "Polynomial is not over a characteristic zero ring";  
        
    d := dim R; 
    m := ideal(apply(0..(d-1), i-> R_i));
    J := ideal jacobian ideal f;
    if codim J != d then error "The jacobian ideal is not a finite length ideal";
    
    RI := multiReesIdeal {m,J};
    Q := splice{numgens m : {1,0}, rank source compress gens J : {0,1}};
    A := newRing(ring RI, Degrees => Q);
    M := substitute(m,A); 
    P := substitute(RI, A/M); 
    H := reduceHilbert hilbertSeries P; 
    B := toList H;
    T := getSymbol "T"; 
    A1 := QQ[T_0,T_1];
    B0 := substitute(B#0, vars A1); 
    B1 := substitute(B#1, vars A1); 
    facB1 := factor value B1;
    g := toList apply(2, i -> (facB1#(1-i)#1)-1); 
    c := g#0 + g#1 - d + 1; 
    h := (i,B0) -> ( 
	T := ring B0; 
	b := (g#0-d+1+i)!*(g#1-i)!; 
	q := (-1)^c/b; 
	for j from 1 to g#0-d+1+i do ( B0 = diff(A1_0,B0););
	for p from 1 to g#1-i do ( B0 = diff(A1_1,B0);); 
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
    v1 := new HashTable from {d => degree J};
    v = merge(v, v1, plus);
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
      P. B. Bhattacharya and J. J. Risler - B. Teissier proved that if $I_0,I_1,\ldots,I_r$  
      are $m$-primary ideals in a Noetherian local ring $(R,m)$ of dimension $d$, then 
      the function $B(u_0,u_1,\ldots,u_r) = l(R/I_0^{u_0}I_1^{u_1} \cdots I_r^{u_r})$ is a polynomial 
      function in $u_0,u_1,\ldots,u_r$ of degree $d$ for large values of $u_0,u_1,\ldots,u_r.$ 
      The coefficients of the top degree term are called the mixed multiplicities of the 
      ideals $I_0,I_1,\ldots,I_r.$ This result was generalized for ideals of positive height in 
      the works of D. Katz, J. K. Verma and N. V. Trung. D. Rees studied these numbers 
      using complete and joint reductions of ideals. 
      
      Our algorithm to compute the mixed multiplicities requires computation of the defining 
      ideal of the multi-Rees algebra of ideals. An expression of the defining ideal of the 
      multi-Rees algebra of monomial ideals over a polynomial ring was given by D. Cox, 
      K.-i. Lin and G. Sosa in (Multi-Rees algebras and toric dynamical systems. Proc. Amer. 
      Math. Soc., 147(11):4605-4616, 2019). We use a generalization of their result to compute 
      the defining ideal of multi-Rees algebras of ideals over domains. Otherwise, we use 
      a generalization of the algorithm of the function reesIdeal to the multi-ideal case.  
      
      The computation of mixed multiplicities helps compute mixed volume of a collection of 
      lattice polytopes and also the sectional Milnor numbers of hypersurfaces with an 
      isolated singularity. 
      
      Let $Q_1,\ldots,Q_n$ be a collection of lattice polytopes in $\mathbb{R}^n$ and 
      $t_1,\ldots,t_n \in \mathbb{R}_+$. Minkowski proved that the $n$-dimensional 
      volume, $vol_n(t_1Q_1 + \cdots + t_nQ_n)$ is a homogeneous polynomial of degree $n$ in 
      $t_1,\ldots,t_n.$ The coefficient of $t_1 \cdots t_n$ is called the mixed volume of $Q_1,\ldots,Q_n.$ 
      N. V. Trung and J. K. Verma proved that the mixed volume of lattice polytopes in the 
      above setup can be realized as a mixed multiplicity of the homogeneous ideals 
      corresponding to the lattice polytopes.
      
      Let origin be an isolated singular point of a complex analytic hypersurface 
      $H = V(f).$ The $\mathbb{C}$-vector space dimension of 
      $\mathbb{C}\{x_0,\ldots,x_n\}/(f_{x_0},\ldots,f_{x_n})$ is called the Milnor 
      number of the hypersurface $H$ at the origin. Let $(X, 0)$ be a germ of a 
      hypersurface in $\mathbb{C}^{n+1}$ with an isolated singularity. The Milnor 
      number of $X \cap E$, where $E$ is a general linear subspace of dimension 
      $i$ passing through $0,$ is called the $i$-th sectional Milnor number of $X$. B. Teissier 
      identified the $i$-th sectional Milnor number with the $i$-th mixed multiplicity of 
      the maximal homogeneous ideal of the polynomial ring and the Jacobian ideal of $f.$      
///   

doc ///
  Key
    multiReesIdeal
    (multiReesIdeal,List)
    (multiReesIdeal,Ideal)
    (multiReesIdeal,List,List)
    (multiReesIdeal,Ideal,RingElement)
  Headline
    Compute the defining ideal of multi-Rees algebra of ideals
  Usage
    multiReesIdeal W
    multiReesIdeal (W,U)
  Inputs
    W:List
      of ideals $I_1,\ldots,I_n$ or @ ofClass Ideal @ if $n=1$
    U:List
      of nonzerodivisors, one from each ideal or a nonzerodivisor @ ofClass RingElement @ if $n=1$ 
  Outputs
    :Ideal
      defining ideal of the multi-Rees algebra of $I_1,\ldots,I_n$
  Description
    Text
      When the base ring is a domain, the function computes the defining ideal of 
      the multi-Rees algebra of ideals by computing the saturation of a binomial 
      ideal with respect to a polynomial. The technique is a generalization of a 
      result of D. Cox, K.-i. Lin and G. Sosa for monomial ideals over a polynomial ring.  
    Example 
      S = QQ[x_0..x_3];
      C = trim monomialCurveIdeal(S,{2,3,5})
      multiReesIdeal C
      I = multiReesIdeal {C,C}
    Text
      This is how we handle degrees
    Example
      transpose gens I 
    Text
      If the base ring is not a domain, then the function computes the defining ideal 
      by computing the presentation of each ideal and returns the kernel between 
      the symmetric algebra of the source and the target symmetric algebras.
      Alternatively, if the user knows nonzerodivisors, one from each input ideal, 
      then the function uses the algorithm of the domain case and saturates the binomial
      ideal with respect to the product of the nonzerodivisors. The alternate method 
      is often much faster.
    Example
      T = QQ[a,b,c];
      m = matrix{{a,b,c},{b,c,a}};
      U = T/minors(2,m);
      J = ideal vars U
      time multiReesIdeal J
      time multiReesIdeal (J, a)
  SeeAlso
    reesIdeal
    mixedMultiplicity
    mMixedVolume
    secMilnorNumbers	
///

doc ///
  Key
    mixedMultiplicity
    (mixedMultiplicity,Sequence,Sequence)
  Headline
    Compute a given mixed multiplicity of ideals
  Usage
    mixedMultiplicity (W1, W2)
  Inputs
    W1:Sequence 
      of ideals $I_0,\ldots,I_r$
    W2:Sequence
      $a=(a_0,\ldots,a_r)$ at which the mixed multiplicity is calculated
  Outputs
    :ZZ
      mixed multiplicity $e_a$ of ideals $I_0,\ldots,I_r$
  Description
    Text
      Given the ideals $I_0,\ldots,I_r$ in a ring $R$ and the tuple 
      $a = (a_0,\ldots,a_r) \in \mathbb{N}^{r+1}$ such that $I_0$ is primary to the
      maximal homogeneous ideal of $R$, $I_1,\ldots,I_r$ have positive grade 
      and $a_0+ \cdots +a_r = dim R -1$, the function computes the mixed 
      multiplicity $e_a$ of the ideals. 
    Example 
      R = QQ[x,y,z,w];
      I = ideal(x*y*w^3,x^2*y*w^2,x*y^3*w,x*y*z^3);
      m = ideal vars R;
      mixedMultiplicity ((m,I,I,I),(0,1,1,1))
    Text
      The function computes the Hilbert polynomial of the graded ring 
      $\bigoplus (I_0^{u_0}I_1^{u_1} \cdots I_r^{u_r}/I_0^{u_0+1}I_1^{u_1} \cdots I_r^{u_r})$ to
      compute the mixed multiplicity. If the ideals $I_1,\ldots,I_r$ are also
      primary to the maximal ideal, then to compute the $(a_0+1, a_1,\ldots, a_r)$-th
      mixed multiplicity, one needs to enter the sequence ${a_0,a_1,\ldots,a_r}$ in 
      the function. The same is illustrated in the following example.
    Example
      R = QQ[x,y,z];
      m = ideal vars R;
      f = z^5 + x*y^7 + x^15;
      I = ideal(apply(0..2, i -> diff(R_i,f)))
      mixedMultiplicity ((m,I),(2,0))
      mixedMultiplicity ((m,I),(1,1))
    Text
      In case the user wants to compute a mixed multiplicity of ideals where one (or many) 
      ideal(s) doesn't have positive grade, then one can pass to the ring $R/(0:I^\infty),$ 
      where $(0:I^\infty)$ denotes the saturation of the ideal $I = I_1 \cdots I_r.$
    Example
      S = QQ[x,y,z,w]/ideal(x*z, y*z);
      I = ideal(x,y);
      m = ideal vars S;
      K = saturate(sub(ideal(),S), I^2);
      T = S/K;
      J = sub(I, T);
      n = sub(m, T);
      mixedMultiplicity ((n,J,J),(2,0,0))
  SeeAlso
    multiReesIdeal
    mMixedVolume
    secMilnorNumbers
///


doc ///
  Key
    homIdealPolytope
    (homIdealPolytope,List)
  Headline
    Compute the homogeneous ideal corresponding to the vertices of a lattice polytope in $\mathbb{R}^n$.
  Usage
    homIdealPolytope W
  Inputs
    W:List 
      list ${p_1,\ldots,p_r}$, where $p_1,\ldots,p_r$ are vertices of the lattice polytope in $\mathbb{R}^n$. 
  Outputs
    :Ideal
      A homogeneous ideal of $k[x_1,\ldots,x_{n+1}].$ 
  Description
    Text
      Given a list of vertices of a lattice polytope, the function outputs a homogeneous 
      ideal of $k[x_1,\ldots,x_{n+1}]$ such that the polytope is the convex hull of the
      lattice points of the dehomogenization of a set of monomials that generates the 
      ideal in $k[x_1,\ldots,x_n]$. 
      
      The following example computes the homogeneous ideal corresponding to a 2-cross polytope.
    Example 
      I = homIdealPolytope {(0,1),(1,0),(0,-1),(-1,0)}
    Text
      The output can be used to compute the mixed volume of a collection of polytopes.
      A list of the output ideals, corresponding to the vertices of various polytopes,  
      can be used as an input in the @TO mMixedVolume@ function to compute the mixed volume of polytopes.
  SeeAlso
    mMixedVolume
    mixedMultiplicity
///


doc ///
  Key
    mMixedVolume
    (mMixedVolume,List)
  Headline
    Compute the mixed volume of a collection of lattice polytopes
  Usage
    mMixedVolume W
  Inputs
    W:List 
      of homogeneous ideals $I_1,\ldots,I_n$ over a polynomial ring,
      or a list of lists of vertices of the polytopes
  Outputs
    :ZZ
      mixed volume
  Description
    Text
      Let $Q_1,\ldots,Q_n$ be a collection of lattice polytopes in $\mathbb{R}^n$ 
      and let $I_1,\ldots,I_n$ be homogeneous ideals in a polynomial ring over 
      the field of rational numbers, corresponding to the given polytopes. 
      These ideals can be obtained using the command @TO homIdealPolytope @. 
      The mixed volume is calculated by computing a mixed multiplicity of these ideals. 
	 
      The following example computes the mixed volume of two 2-cross polytopes.
    Example 
      I = homIdealPolytope {(-1,0),(0,-1),(1,0),(0,1)}
      mMixedVolume {I,I}
    Text
      One can also compute the mixed volume of a collection of lattice polytopes 
      by directly entering the vertices of the polytopes. Mixed Volume in the above 
      example can also be computed as follows.
    Example 
      C = {(-1,0),(0,-1),(1,0),(0,1)}
      mMixedVolume {C,C}
    Text 
      The following example computes the mixed volume of a 2-dimensional 
      hypercube $H$ and a 2-cross polytope $C$.
    Example
      H = {(1,1),(1,-1),(-1,1),(-1,-1)};
      C = {(-1,0),(0,1),(1,0),(0,-1)};
      mMixedVolume {H,C}
  SeeAlso
    homIdealPolytope
    mixedMultiplicity
    "Polyhedra::mixedVolume"
    multiReesIdeal
///


doc ///
  Key
    secMilnorNumbers
    (secMilnorNumbers,RingElement)
  Headline
    Compute the sectional Milnor numbers of a hypersurface with an isolated singularity
  Usage
    secMilnorNumbers f
  Inputs
    f:RingElement
      a polynomial
  Outputs
    :HashTable
      Sectional Milnor numbers of $f$
  Description
    Text
      Let origin be an isolated singular point of a complex analytic hypersurface $H = V(f).$ 
      The $\mathbb{C}$-vector space dimension of $\mathbb{C}\{x_0,\ldots,x_n\}/(f_{x_0},\ldots,f_{x_n})$ 
      is called the Milnor number of the hypersurface $H$ at the origin. Let $(X, 0)$ be a germ 
      of a hypersurface in $\mathbb{C}^{n+1}$ with an isolated singularity at the origin. The Milnor number of 
      $X \cap E$, where $E$ is a general linear subspace of dimension $i$ passing through the origin, is 
      called the $i$-th sectional Milnor number of $X$. B. Teissier identified the $i$-th sectional 
      Milnor number with the $i$-th mixed multiplicity of the maximal homogeneous ideal of the 
      polynomial ring and the Jacobian ideal of $f.$
   
      Let $f$ be an element of a polynomial ring $R$ with characteristic zero, and let $d$ be the 
      dimension of $R$. The function computes the sectional Milnor numbers by computing 
      the mixed multiplicities $e_0(m|J(f)),\ldots,e_{d}(m|J(f))$, where $m$ is the 
      maximal homogeneous ideal of $R$ and $J(f)$ is the Jacobian ideal of $f$ of height $d$. Note that
      in this case, the last sectional Milnor number $e_d(m|J(f))$ is the Milnor number of $f.$
	  
      In this example, the 2-sectional Milnor number, which is the Milnor number of a general 
      hypersurface section, is 28. The Milnor number, which is the last sectional Milnor number, is 364.
    Example 
      R = QQ[x,y,z];
      secMilnorNumbers(z^5 + x*y^7 + x^15)
  SeeAlso
    multiReesIdeal
    mixedMultiplicity
    mMixedVolume
  Caveat
    The user is supposed to check that the given polynomial defines an isolated singularity at
    the homogeneous maximal ideal. 
///

doc ///
  Key 
    [multiReesIdeal, VariableBaseName]
    [homIdealPolytope, VariableBaseName]
  Headline
    choose a base name for variables in the created ring
  Usage
    multiReesIdeal(..., VariableBaseName => X)
    homIdealPolytope(..., VariableBaseName => X)
  Description
    Text
      Each of these functions creates a new ring of the form $R[X_0,\ldots, X_r]$
      or $k[X_0,\ldots, X_r]$, where $R$ is the ring of the input ideal and $k$ is 
      the coefficient ring of the output ideal. This option allows the user to 
      change the base names of the new variables in this ring. The default variable is X.
    Example
      S = QQ[x_0..x_3];
      C = trim monomialCurveIdeal(S,{2,3,5});
      multiReesIdeal (C, VariableBaseName => "w")
    Example
      homIdealPolytope ({(0,1),(1,0),(2,1),(1,2)}, VariableBaseName => "T")
///


doc ///
  Key 
    [homIdealPolytope, CoefficientRing]
  Headline
    choose the coefficient ring of the (output) ideal
  Usage
    homIdealPolytope(..., CoefficientRing => QQ)
  Description
    Text
      The function @TO homIdealPolytope@ creates a new ring of the form 
      $k[X_0,\ldots, X_r]$, where $k$ is the coefficient ring of the output ideal. 
      This option allows the user to chose the coefficient ring $k.$ The default ring is QQ.
    Example
      I = homIdealPolytope ({(0,1),(1,0),(2,1),(1,2)}, CoefficientRing => ZZ/2)	 
///



----Tests----

TEST ///
-- test for multiReesIdeal
  R = QQ[x,y];
  m = ideal vars R;
  Rm = multiReesIdeal m^2
  use ring Rm;
  assert(Rm == ideal(y*X_1-x*X_2, y*X_0-x*X_1, X_1^2-X_0*X_2))
  RI = multiReesIdeal {m^2,m^2}
  use ring RI;
  assert(RI == ideal(X_4*y-X_5*x, X_3*y-X_4*x, X_1*y-X_2*x, X_0*y-X_1*x, X_4^2-X_3*X_5, X_2*X_4-X_1*X_5, X_1*X_4-X_0*X_5, X_2*X_3-X_0*X_5, X_1*X_3-X_0*X_4, X_1^2-X_0*X_2))
  J = ideal(x^3 + y^3, x^2*y + y^2)
  RIJ = multiReesIdeal {m^2,J}
  use ring RIJ;
  assert(RIJ == ideal(X_1*y-X_2*x, X_0*y-X_1*x, X_1^2-X_0*X_2, X_1*X_3*x+X_2*X_3-X_0*X_4*x-X_2*X_4*y, X_3*(y^2+x^2*y)+X_4*(-y^3-x^3)))
  S = multiReesIdeal ({m^2,J},{(m^2)_0,J_0})
  assert(RIJ == sub(S, vars ring RIJ))
///

TEST ///
--test for multiReesIdeal
--non-domain case
  T = QQ[a,b,c];
  m = matrix{{a,b,c},{b,c,a}};
  U = T/minors(2,m);
  J = ideal vars U;
  I = multiReesIdeal J
  K = multiReesIdeal (J, a)
  K = sub(K, vars ring I);
  assert(I == K)
///


TEST ///
-- test for mixedMultiplicity
  R = QQ[x,y,z];
  m = ideal vars R;
  f = z^5 + x*y^7 + x^15;
  I = ideal(apply(0..2, i -> diff(R_i,f)));
  E3 = mixedMultiplicity ((m,I),(2,0))
  E2 = mixedMultiplicity ((m,I),(1,1))
  assert(E3 == 1)
  assert(E2 == 4)
///

TEST ///
-- test for mixedMultiplicity  
  B = QQ[x,y,z];
  I = ideal(x^5*y, x*y^3*z^2, x*y*z^4);
  m = ideal vars B;
  mm = mixedMultiplicity((m,I,I),(0,1,1))
  assert(mm == 8)
///

TEST ///
--test for mixedMultiplicity
  S = QQ[x,y,z,w]/ideal(x*z, y*z);
  I = ideal(x,y);
  m = ideal vars S;
  K = saturate(sub(ideal(),S), I^2);
  T = S/K;
  J = sub(I, T);
  n = sub(m, T);
  en = mixedMultiplicity ((n,J,J),(2,0,0))
  assert(en == 1)
///

TEST ///
-- test for homIdealPolytope 
  I = homIdealPolytope {(-1,0),(0,-1),(1,0),(0,1)}
  use ring I
  assert(I == ideal(X_1^2*X_2, X_1*X_2^2, X_1*X_3^2, X_2*X_3^2))
///


TEST ///
-- test for mMixedVolume
  A = {(-1,0),(0,-1),(1,0),(0,1)};
  I = homIdealPolytope A;
  MVI = mMixedVolume {I,I}
  MVA = mMixedVolume {A,A}
  assert(MVI == MVA)
///

TEST ///
-- test for mMixedVolume
  H = {(1,1),(1,-1),(-1,1),(-1,-1)};
  C = {(-1,0),(0,1),(1,0),(0,-1)};
  MVHC =  mMixedVolume {H,C}
  assert(MVHC == 8)
///

TEST ///
-- test for secMilnorNumbers
  R = QQ[x,y,z];
  f = x^2*y + y^2*z + z^3;
  SMN = secMilnorNumbers(f)
  assert(SMN === hashTable{0 => 1, 1 => 2, 2 => 4, 3 => 8})
///

TEST ///
-- test for secMilnorNumbers
  R = QQ[x,y,z];
  SMN = secMilnorNumbers(z^5 + x*y^7 + x^15)
  assert(SMN === hashTable{0 => 1, 1 => 4, 2 => 28, 3 => 364})
///

end--

restart
uninstallPackage "MixedMultiplicity"
restart
installPackage "MixedMultiplicity"
check "MixedMultiplicity"


