newPackage(
    "MultiGradedRationalMap",
    Headline => "degree and birationality of multi-graded rational maps",
    Authors => {{ Name => "Yairon Cid Ruiz", 
		  Email => "ycid@ub.edu", 
		  HomePage => "http://www.ub.edu/arcades/ycid.html"}},
    Keywords => {"Commutative Algebra"},
    Version => "0.1",
    Date => "2018",
    DebuggingMode => false,
    Configuration => {},
    PackageImports => {"ReesAlgebra"}
)

export { 
    -- Methods --
    "degreeOfMap", 
    "jacobianDualRank", 
    "isBiratMap", 
    "satSpecialFiberIdeal",
    "satSpecialFiber",
    "gensSatSpecialFib",
    "upperBoundDegreeSingleGraded",
    "Hm1Rees0",
    "partialJDRs",
    "degreeOfMapIter", 
    -- Options --
    "Hm1Rees0Strategy",
    "SatSpecialFibStrategy"
}



--------------------
--------------------
---------- M2 code
--------------------
--------------------


------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
    	    	   -- SOME TECHNICAL/AUXILIARY FUNCTIONS 
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------


-- Computes the Rees algebra with emphasis on the R-grading.
-- It simply calls the package ``ReesAlgebras'' on Macaulay2.
RgradRees := (I) -> ( 
    R := ring I;
    n := numgens R;
    lvars := flatten entries vars R;   
    ReesEq := reesIdeal I;
    e := numgens ring ReesEq;
    K := coefficientRing R;
    Z := symbol Z;
    xxx := symbol xxx;
    AA := K[Z_1 .. Z_e][xxx_1 .. xxx_n, Degrees => degrees R]; --bigraded ring
    AA' := ring ReesEq;
    F := map(AA, AA', {Z_1 .. Z_e, xxx_1 .. xxx_n});
    F(ReesEq) 
)


-- This function tries to recover the multi-projective space encoded by R.
-- If R is not a multi-graded polynomial ring with weight 1 on each variable,
-- then it returns false.
getGrading := (R) -> (
    L := degrees R;    	    
    m := length L_0;
    D := new MutableList from toList(m:0);
    for i from 0 to length L - 1 do (
    	j := 0, s := 0;
	for k from 0 to m-1 do (
	    if L_i_k != 0 and L_i_k != 1 then return (, false);
	    s = s + L_i_k;
	    if L_i_k == 1 then j = k;
	);	
    	if s != 1 then return (, false);
	D#j = D#j + 1;
    );
    (toList D, true)
)


-- Checks if an ideal is homogeneous and equally generated
isEquallyGenerated := (I) -> (
    if not isHomogeneous I then return false;
    L := flatten entries gens I;
    f0 := L_0;
    all(L, f -> (degree f) == (degree f0))         
)


-- Makes some sanity checks in the multi-graded case
checkMultiGraded := (I) -> (
    if not isEquallyGenerated I 
       then error "The ideal needs to be homogeneous and equally generated.";
    R := ring I;
    grading := getGrading R;
    if not isPolynomialRing R or not grading_1 
       then error "The ring of the ideal needs to be a polynomial ring with standard multi-grading.";
       
    grading_0 
)


-- Makes some sanity checks in the single-graded case
checkSingleGraded := (I) -> (
    if not isEquallyGenerated I 
       then error "The ideal needs to be homogeneous and equally generated.";
    R := ring I;
    grading := getGrading R;
    if not isPolynomialRing R or not grading_1 or length grading_0 != 1 
       then error "The ring of the ideal needs to be a standard single-graded polynomial ring.";
)


-- Emulates the action of the elements of R over H_m^n(R),
-- where m is the maximal irrelevant ideal of R
prod := (X, Lmono) -> (
    M := mutableMatrix(ring X, 1, numcols Lmono);
    for i from 0 to (numcols Lmono)-1 do M_(0,i) = X // Lmono_(0,i);
    matrix M       
)


-- Computes the multi-homogeneous irrelevant ideal of R
getIrrelevantIdeal := (R) ->(
    grading := getGrading R;
    m := length grading_0;
    NN := ideal(1_R);
    for i from 1 to m do (
    	deg := toList((i-1):0) | {1} | toList((m-i):0);
	NN = NN * ideal image super basis(deg, R); 
    );    
    NN
)

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
 
 
 
 
 
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
    	    	   -- FUNCTIONS RELATED TO THE SATURATED SPECIAL FIBER RING   
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------


-- Given a map g: F --> G of free AA-modules, it gives the degree zero part in 
-- R-grading of the induced map in top local cohomology.
-- INPUT: the ring of the variables Z_1,...,Z_e
-- INPUT: F free AA-module of the source
-- INPUT: G free AA-module of the image
-- INPUT: the map g is represented by the matrix A
-- OUTPUT: the matrix M representing the degree zero part the R-grading of
--            the induced map $H_m^n(g): H_m^n(F) --> H_m^n(G)$
getMapInLocalCohom := (kkZ, F, G, A) -> ( 
      AA := ring F;
      n := numgens AA;
           
      -- get the size of the matrix M
      colsM := 0;
      rowsM := 0;
      lMonoCols := { };
      lMonoRows := { }; 
      degCols := { };
      degRows := { };
            
      -- compute the form of the columns
      for i from 0 to (rank F)-1 do (
      	  di := (degree F_i)_0;
	  ei := (degree F_i)_1;
    	  lMonoCols = append(lMonoCols, 
	               flatten entries super basis(di-n, AA));
    	  li := length lMonoCols_i;
	  degCols = join(degCols, toList(li:-ei));
	  colsM = colsM + li;        
      );
            
      -- compute the form of the rows
      for i from 0 to (rank G)-1 do (
      	  di := (degree G_i)_0;
    	  ei := (degree G_i)_1;
    	  lMonoRows = append(lMonoRows, 
	               flatten entries super basis({di-n, 0}, AA));
      	  li := length lMonoRows_i;
 	  degRows = join(degRows, toList(li:-ei));
	  rowsM = rowsM + li;
      );
    
      -- the matrix representing the map in local cohomology     
      M := mutableMatrix(AA, rowsM, colsM);
      
      -- process of constructing the matrix M
      counterCols := 0;
      for j from 0 to (rank F)-1 do (
	  counterRows := 0;
	  for i from 0 to (rank G)-1 do ( 
	      a := A_(i,j);
	      if a != 0 and lMonoCols_j != {} and lMonoRows_i != {} then (
    	      	  (Ma, Ca) := coefficients a;
		  for l from 0 to (length lMonoCols_j)-1 do (
		      X := lMonoCols_j_l;		      
		      newMa := prod(X, Ma);
		      Y := newMa * Ca;
		      (Mres, Cres) := coefficients(Y, Monomials => lMonoRows_i);  
		      for k from 0 to (length lMonoRows_i)-1 do 
		          M_(counterRows + k, counterCols + l) = Cres_(k, 0); 
		  );	                    	      
	      );
	      counterRows = counterRows + length lMonoRows_i;
	  );
    	  counterCols = counterCols + length lMonoCols_j;     	    	  
       ); 
     
     mapAAtokkZ := map(kkZ, AA, join(toList(n:0), flatten entries vars kkZ));
     
     -- We compute $[H_m^1(Rees(I))]_0$ as a graded S-module
     -- We use this computation to obtain an upper bound of the maximum degree of 
     -- the generators of the saturated special fiber ring
     map(kkZ^degRows, kkZ^degCols, mapAAtokkZ matrix M)
)


-- Computes the module $[H_m^1(Rees(I))]_0$ in Corollary 2.12     
--INPUT: the defining equations of Rees(I)
localHm1Rees0 := (ReesEq) -> (
    AA := ring ReesEq;
    n := numgens AA;
    e := numgens coefficientRing AA;  
    K := coefficientRing coefficientRing AA;
    Z := symbol Z;
    kkZ := K[Z_1 .. Z_e];
        
    -- It is computed by means of the spectral sequences coming from the double complex
    -- obtained by the tensor product of a resolution of ReesEq and the Cech complex.
    -- (check Proposition 2.7(i) for more details)	
    rs := res ReesEq;
    M1 := getMapInLocalCohom(kkZ, rs_(n-1), rs_(n-2), rs.dd_(n-1));
    M2 := getMapInLocalCohom(kkZ, rs_n, rs_(n-1), rs.dd_(n));
   
    (ker M1) / (image M2)        
)



-- It simply calls localHm1Rees0 after a sanity check.
-- INPUT: A single-graded ideal I.
-- OUTPUT: it computes the module  $[H_m^1(Rees(I))]_0$.
-- CAVEAT: For the moment, it only supports single-graded ideals on a polynomial ring.
Hm1Rees0 = method()
Hm1Rees0(Ideal) := (I) -> (
    checkSingleGraded(I);
        	    
    localHm1Rees0 RgradRees I 
)


-- By considering the powers {I^1, I^2, ..., I^nsteps} of I, it computes a set of generators of the saturated special fiber ring.
-- The algorithm is correct only if nsteps is big enough to obtain all the generators.
-- INPUT: A multi-graded ideal.
-- INPUT: The number of steps.
-- OUTPUT: Computes the possible generators of the saturated special fiber ring in the graded parts  
--          given by [(I^1)^sat]_d, [(I^2)^sat]_2d, ..., [(I^nsteps)^sat]_nsteps*d.
gensSatSpecialFib = method()
gensSatSpecialFib(Ideal, ZZ) := (I, nsteps) -> (    
    checkMultiGraded(I);
    d := degree (gens I)_(0,0);
    NN := getIrrelevantIdeal ring I;
    satIpow := saturate(I, NN);
    tot := flatten entries super basis(d, satIpow);
    L := { ideal tot };
    	
    for i from 2 to nsteps do (
        satIpow = saturate(I * satIpow, NN);
   	curr := ideal image super basis(i*d, satIpow);
	   
	-- delete those that can be also obtained by multiplication of lower graded parts
	toDel := ideal();
	for j from 1 to i - 1 do toDel = toDel + (L_(j-1) * L_(i-j-1));
	toAdd := flatten entries mingens (curr / toDel);
	   	     
        tot = join(tot, toAdd);
        L = append(L, curr);
    );

    tot
)

-- This method first computes an upper bound for nsteps and then simply calls gensSatSpecialFib(Ideal, ZZ)
-- INPUT: A single-graded ideal.
-- OUTPUT: The generators of the saturated special fiber ring
-- CAVEAT: It only works for an ideal in a single graded polynomial rings
gensSatSpecialFib(Ideal) := (I) -> (
    checkSingleGraded(I);
    nsteps := max flatten degrees Hm1Rees0 I;-- degree of the generators of Hm1Rees0
    nsteps = max(nsteps, 1);  -- degree of the generators of S
    
    gensSatSpecialFib(I, nsteps)
)


-- Tries to compute the defining ideal of the saturated special fiber ring.
-- INPUT: A multi-graded ideal.
-- INPUT: nsteps is the number of steps used in the process of obtaining a set of generators.
-- OUTPUT: returns the ideal defining the saturated special fiber ring.
-- CAVEAT: It only gives a correct answer if nsteps is bigger than the highest degree of the generators of the 
--       saturated special fiber ring.
satSpecialFiberIdeal = method()
satSpecialFiberIdeal(Ideal, ZZ) := (I, nsteps) -> (
    checkMultiGraded(I);
    R := ring I;
    d := degree (gens I)_(0,0);
    
    lGens := gensSatSpecialFib(I, nsteps);
    lDegs := apply(lGens, G -> (degree G)_0 // d_0);
          	   
    K := coefficientRing R;	    	
    Z := symbol Z;		
    B := K[Z_1 .. Z_(length lGens), Degrees => lDegs]; 
    F := map(R, B, lGens);
      
    ker F
)


-- This method first computes an upper bound for nsteps and then simply calls satSpecialFiberIdeal(Ideal, ZZ)
-- INPUT: A single-graded ideal.
-- OUTPUT: The defining ideal of the saturated special fiber ring
-- CAVEAT: It only works for an ideal in a single graded polynomial rings
satSpecialFiberIdeal(Ideal) := (I) -> (
    checkSingleGraded(I);
    nsteps := max flatten degrees Hm1Rees0 I;-- degree of the generators of Hm1Rees0
    nsteps = max(nsteps, 1);  -- degree of the generators of S
    
    satSpecialFiberIdeal(I, nsteps)
)


-- It simply calls the method satSpecialFiberIdeal
-- INPUT: A multi-graded ideal.
-- INPUT: nsteps is the number of steps used in the process of obtaining a set of generators.
-- OUTPUT: returns the saturated special fiber ring.
-- CAVEAT: It only gives a correct answer if nsteps is bigger than the highest degree of the generators of the 
--       saturated special fiber ring.
satSpecialFiber = method()
satSpecialFiber(Ideal, ZZ) := (I, nsteps) -> (
    checkMultiGraded(I);
    satFibEq := satSpecialFiberIdeal(I, nsteps);
    
    (ring satFibEq) / satFibEq
)


-- This method first computes an upper bound for nsteps and then simply calls satSpecialFiber(Ideal, ZZ)
-- INPUT: A single-graded ideal.
-- OUTPUT: The saturated special fiber ring
-- CAVEAT: It only works for an ideal in a single graded polynomial rings
satSpecialFiber(Ideal) := (I) -> (
    checkSingleGraded(I);
    nsteps := max flatten degrees Hm1Rees0(I);-- degree of the generators of Hm1Rees0
    nsteps = max(nsteps, 1);  -- degree of the generators of S
    
    satSpecialFiber(I, nsteps)
)

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
 
 
 
 
 
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
    	    	   -- FUNCTIONS RELATED TO THE DEGREE AND BIRATIONALITY OF RATIONAL MAPS
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------


degreeOfMap = method(Options => {Strategy => Hm1Rees0Strategy})

-- Computes the degree of the rational represented by the generators of the ideal I.
-- It contains a computational implementation of Corollary 2.12.
-- CAVEAT: For the moment, it only supports projective spaces.
-- INPUT: A single-graded ideal.
-- OUTPUT: Returns the degree of the rational map.
--         If the map is not generically finite then the output is 0.
degreeOfMapHm1Rees0 := (I) -> (
    checkSingleGraded(I);
    R := ring I;
    ReesEq := RgradRees(I); -- equations of Rees(I)
    AA := ring ReesEq;
            
    -- computes degree of the image of phi
    mm := ideal vars AA; 
    S := AA / (mm + ReesEq); 
    degIm := degree S;
    
    -- if the map is not genericaly finite, then return 0   
    if dim S < dim R then return 0;
        
    -- computes multiplicity of $[H_m^1(Rees(I))]_0$ in Corollary 2.12     
    L := localHm1Rees0(ReesEq);    
    if dim L < dim S then return 1;
    mult := degree L;     
            
    -- the degree of phi      
    1 + mult//degIm    
)

-- This method first computes an upper bound for nsteps and then simply calls degreeOfMapIter(Ideal, ZZ)
-- It is extremely slow compared with the other strategy, because it actually needs to compute Hm1Rees0
-- INPUT: A single-graded ideal.
-- OUTPUT: The degree of the map represented by the generators of I
degreeOfMapSatStrategy := (I) -> (
    checkSingleGraded(I);
    nsteps := max flatten degrees Hm1Rees0 I;-- degree of the generators of Hm1Rees0
    nsteps = max(nsteps, 1);  -- degree of the generators of S

    degreeOfMapIter(I, nsteps)
)


-- This method computes the degree of a map depending on the stratey used.
-- By default the it is used -Hm1Rees0Strategy-
-- INPUT: A single-graded ideal.
-- OUTPUT: The degree of the map represented by the generators of I
degreeOfMap(Ideal) := opts -> (I) -> (
    if opts.Strategy == Hm1Rees0Strategy then 
    	degreeOfMapHm1Rees0(I)	
    else if opts.Strategy == SatSpecialFibStrategy then 
    	degreeOfMapSatStrategy(I)
    else 
    	error "The Strategy has to be either -Hm1Rees0Strategy- or -SatSpecialFibStrategy-"	        	
)




degreeOfMapIter = method()
-- This map compute the degree of rational map by computing the multiplicity of the saturated special fiber ring (see Theorem 2.4).
-- It also works in the multi-graded setting. 
-- INPUT: A multi-graded ideal. 
-- INPUT: The number of steps for computing the saturated special fiber ring.
-- OUTPUT: The degree of the rational map represented by the generators of I.
--         If the map is not generically finite then the output is 0.
-- CAVEAT: It only gives a correct answer if nsteps is bigger than the highest degree of the generators of the 
--       saturated special fiber ring.
degreeOfMapIter(Ideal, ZZ) := (I, nsteps) -> (
    grading := checkMultiGraded(I);
    r := (sum grading) - (length grading);
    S := specialFiber I;
    
    -- if the map is not genericaly finite, then return 0   
    if (dim S) - 1 < r then return 0;
    
    satFib := satSpecialFiber(I, nsteps);
    N := numerator reduceHilbert hilbertSeries satFib;
    mult := sub(N, { (vars ring N)_(0,0) => 1 });
    degIm := degree S;
    
    mult // degIm
)



-- It computes the partial Jacobian dual ranks.
-- INPUT: A multi-graded ideal. 
-- OUTPUT: The partial Jacobian dual ranks.
partialJDRs = method()
partialJDRs(Ideal) := (I) -> (
    grading := checkMultiGraded(I);
    R := ring I;
    m := length grading;
    ReesEq := RgradRees(I);
    AA := ring ReesEq;
    gensRees := flatten entries gens ReesEq;
   
    -- coordinate ring of the image   
    mm := ideal vars AA;
    S := AA / (mm + ReesEq);
        
    JDRs := { };	

    -- compute the JDRs	 
    for i from 1 to m do (
    	deg := toList((i-1):0) | {1} | toList((m-i):0);
        L := select(gensRees, f -> apply(m, j -> (degree f)_j) == deg);
     	if L == {} then JDRs = append(JDRs, 0) 
	else (
	    M := jacobian matrix{L};
	    JDRs = append(JDRs, rank(M ** S));     
     	);    
    );

    JDRs
)


-- Computes the full Jacobian dual rank of a rational map (this is defined in Notation 4.2)
-- INPUT: A multi-graded ideal. 
-- OUTPUT: The full Jacobian dual rank. 
-- CAVEAT: For the moment, it only supports multi-projective spaces in the source.
jacobianDualRank = method()
jacobianDualRank(Ideal) := (I) -> (
    checkMultiGraded(I);
    ReesEq := RgradRees(I); -- equations of Rees(I)
    AA := ring ReesEq;
    m := length (getGrading ring I)_0;
        
    -- computes the total Jacobian dual matrix
    L := select(flatten entries gens ReesEq, f -> sum(m, j -> (degree f)_j) == 1);
    if L == {} then return 0;
    M := jacobian matrix{L};    
 
    --coordinate ring of the image
    mm := ideal vars AA;
    S := AA / (mm + ReesEq);
  
    -- computes the total Jacobian dual rank  
    rank (M ** S)	   
)   



-- Given a multigraded rational map, it determines the birationality of the rational map
-- INPUT: A multi-graded ideal
-- OUTPUT: true/false if the rational map is birational/non-birational onto its image
-- CAVEAT: For the moment, it only supports multi-projective spaces in the source
-- REMARK: From Theorem 4.4 we can simply compute the rank of the "full" Jacobian dual matrix.
--         Therefore, we only need to check the rank of one matrix and it allows us to treat 
--         the muli-graded case similarly to the single-graded.
isBiratMap = method()
isBiratMap(Ideal) := (I) -> (    
    grading := checkMultiGraded(I);
    
    r := (sum grading) - (length grading);
    JDR := jacobianDualRank I;
            
    (JDR == r)
)


-- This function computes the upper bound given in Theorem 3.22 for a single graded rational map.
-- INPUT: A single-graded ideal.
-- OUTPUT: An upper bound which can be computed with some Hilbert function computations.
upperBoundDegreeSingleGraded = method()
upperBoundDegreeSingleGraded(Ideal) := (I) -> (
    checkSingleGraded(I);
    if dim I > 1 then 
           error "The base locus should have dimension zero.";
    d := (degree I_0)_0;
    n := numgens ring I;
    J := saturate(I);
    B := 1 + binomial(d-1,n-1) + hilbertFunction(d,I) - hilbertFunction(d,J);
    for i from 2 to n-2 do B = B + hilbertFunction((n-i)*d-n,I);
    
    B
) 


------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------



--------------------
--------------------
---------- Documentation
--------------------
--------------------


beginDocumentation()


doc ///
Key 
   MultiGradedRationalMap
Description 
  Text
   MultiGradedRationalMap provides functions for computing the degree of a multi-graded rational map.
       
   In the paper  @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps") @, a new algebra called the {\bf saturated special fiber ring} was introduced.
   This algebra is related to several features in the study of rational maps. 

   Some functions of this package are capable of working in the multi-graded setting.
   Let $R$ be the multi-homogeneous polynomial ring $R=k[x_{1,0},x_{1,1},...,x_{1,r_1}, x_{2,0},x_{2,1},...,x_{2,r_2}, ......, x_{m,0},x_{m,1},...,x_{m,r_m}]$ where the multidegree of a variable $x_{i,j}$ is $\{0,...,1,...,0\}$.
   Let $\mathbf{m}$ be the multi-homogeneous irrelevant ideal  $\mathbf{m}=(x_{1,0},x_{1,1},...,x_{1,r_1})\cap (x_{2,0},x_{2,1},...,x_{2,r_2}) \cap ... \cap (x_{m,0},x_{m,1},...,x_{m,r_m})$ of $R$.     
   Let $I$ be a multi-homogeneous ideal in $R$, which is generated by multi-homogeneous polynomials of the same multi-degree.  
   The saturated special fiber ring of $I$ is defined by the algebra
   $$
   \oplus_{n=0}^\infty [(I^n)^{sat}]_{n*d}.
   $$
   
   The main idea of this package is to exploit this algebra to compute the degree and test the birationality of rational maps.
   
   We also implement the Jacobian dual criterion in the multi-graded setting.
      
      
    {\bf Overlap with other packages:} {\bf -}  The package @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.12/share/doc/Macaulay2/Cremona/html/", "Cremona") @ performs several computations related to rational and birational maps between irreducible projective varieties.
    Among other things, it can compute the degree of a rational map, test birationality and find the inverse of a birational map. 
    There is a deterministic implementation and a fast probabilistic implementation.
      
    {\bf -} The package @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.11/share/doc/Macaulay2/RationalMaps/html/index.html", "RationalMaps") @ computes several things related to rational maps between projective varieties.
    Among other things, it can detect birationality and compute the inverse of a rational map.
    It contains an implementation of the remarkable Jacobian dual criterion.
    
    {\bf -} The package @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.11/share/doc/Macaulay2/Parametrization/html/index.html", "Parametrization") @ mostly deals with rational parametrizations of rational curves defined over ℚ.
    It includes a function to compute the inverse of a rational map.
    
    {\bf -} The present implementation of this package can only handle rational maps where the source is a multiprojective space. 
    On the other hand, the packages @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.12/share/doc/Macaulay2/Cremona/html/", "Cremona") @, @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.11/share/doc/Macaulay2/RationalMaps/html/index.html", "RationalMaps") @ and @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.11/share/doc/Macaulay2/Parametrization/html/index.html", "Parametrization") @  can handle more general varieties.
        
    {\bf Acknowledgements:} The author is grateful to the organizers of the Macaulay2 workshop in Leipzig.
    The author is grateful to Laurent Busé for his support on the preparation of this package.
   	    	    	    	    	     
///


 
doc ///
  Key
   degreeOfMap
   (degreeOfMap,Ideal)
  Headline
   computes the degree of a rational map
  Usage
   degreeOfMap(I)
  Inputs
    I : Ideal
    	an ideal defining the map
  Outputs
    :ZZ
        the degree of the corresponding rational map 
  Description
    Text
        Let $R$ be the polynomial ring $R=k[x_0,...,x_r]$ and $I$ be the homogeneous ideal $I=(f_0,f_1,...,f_s)$ where $deg(f_i)=d$.
	We compute the degree of the rational map $\mathbb{F}: \mathbb{P}^r \to \mathbb{P}^s$ defined by
	$$
	(x_0: ... :x_r) \to (f_0(x_0,...,x_r), f_1(x_0,...,x_r), ..... , f_s(x_0,...,x_r)).
	$$
	The degree can be computed by two different strategies and the default one is "Hm1Rees0Strategy".
    
        The following example is a rational map without base points: 
    Example
      R = QQ[x,y,z]
      I = ideal(random(4, R), random(4, R), random(4, R));
      betti res I
      degreeOfMap I
    Text
    	In the following examples we play with the relations of the Hilbert-Burch presentation and the degree of $\mathbb{F}$ (see Proposition 5.2 and Theorem 5.12):
    Example 	
      A = matrix{ {x, x^2 + y^2},
                  {-y, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a birational map
      degreeOfMap I
      A = matrix{ {x^2, x^2 + y^2},
                  {-y^2, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      degreeOfMap I 
      A = matrix{ {x^3, x^2 + y^2},
                  {-y^3, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      degreeOfMap I 
      A = matrix{ {x^3, x^4},
                  {-y^3, y^4},
	          {z^3, x^4}
	        };
      I = minors(2, A) -- a non birational map
      degreeOfMap I 
    Text
      The following examples are computed with the strategy "SatSpecialFibStrategy".
    Example
      R = QQ[x,y,z,v,w]
      I = ideal(random(1, R), random(1, R), random(1, R), random(1, R), random(1, R));
      degreeOfMap(I, Strategy=>SatSpecialFibStrategy)    	
      I = ideal(29*x^3 + 55*x*y*z, 7*y^3, 14*z^3, 17*v^3, 12*w^3)
      degreeOfMap(I, Strategy=>SatSpecialFibStrategy)
 	
  Caveat
    To call the method "degreeOfMap(I)", the ideal $I$ should be in a single graded polynomial ring.	    
///



doc ///
  Key
    degreeOfMapIter
    (degreeOfMapIter,Ideal,ZZ)
  Headline
    computes the degree of a rational map
  Usage
    degreeOfMapIter(I, nsteps)
  Inputs
    I : Ideal
    	an ideal defining the map
    nsteps: ZZ
        the number of steps used for computing the saturated special fiber ring
  Outputs
    :ZZ
        the degree of the corresponding rational map 
  Description
    Text
        Let $R$ be the multi-homogeneous polynomial ring $R=k[x_{1,0},x_{1,1},...,x_{1,r_1}, x_{2,0},x_{2,1},...,x_{2,r_2}, ......, x_{m,0},x_{m,1},...,x_{m,r_m}]$ and $I$ be the multi-homogeneous ideal $I=(f_0,f_1,...,f_s)$ where the polynomials $f_i$'s have the same multi-degree.
	We compute the degree of the rational map $\mathbb{F}: \mathbb{P}^{r_1} \times \mathbb{P}^{r_2} \times ... \times \mathbb{P}^{r_m}  \to \mathbb{P}^s$ defined by
	$$
	(x_{1,0} : ... : x_{1,r_1}; ...... ;x_{m,0} : ... : x_{m,r_m}) \to (f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m}), ..... , f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m})).
	$$
	This method calls "satSpecialFiber(I, nsteps)" in order to obtain the saturated special fiber ring and then computes the degree of $\mathbb{F}$ from the multiplicity of the saturated special fiber ring. 
    Example
     	R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
        I = ideal(x*u, y*u, y*v) -- a birational map
        degreeOfMapIter(I, 5)
     	I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
        degreeOfMapIter(I, 5)
	A = matrix{ {x^5*u,  x^2*v^2},
    	            {y^5*v, x^2*u^2},
	            {0,     y^2*v^2}
    	          };
        I = minors(2, A)  -- a non birational
        degreeOfMapIter(I, 5)

  Caveat
       It only gives the correct answer if nteps is big enough to attain all the generators of the saturated special fiber ring.    
///



doc ///
  Key
    isBiratMap 
    (isBiratMap ,Ideal)
  Headline
    tests the birationality of a rational with the Jacobian dual criterion
  Usage
    isBiratMap(I)
  Inputs
    I : Ideal
    	an ideal defining the map
  Outputs
    :Boolean
        true/false if the rational map is birational/non birational 
  Description
    Text
        Let $R$ be the multi-homogeneous polynomial ring $R=k[x_{1,0},x_{1,1},...,x_{1,r_1}, x_{2,0},x_{2,1},...,x_{2,r_2}, ......, x_{m,0},x_{m,1},...,x_{m,r_m}]$ and $I$ be the multi-homogeneous ideal $I=(f_0,f_1,...,f_s)$ where the polynomials $f_i$'s have the same multi-degree.
	We compute the degree of the rational map $\mathbb{F}: \mathbb{P}^{r_1} \times \mathbb{P}^{r_2} \times ... \times \mathbb{P}^{r_m}  \to \mathbb{P}^s$ defined by
	$$
	(x_{1,0} : ... : x_{1,r_1}; ...... ;x_{m,0} : ... : x_{m,r_m}) \to (f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m}), ..... , f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m})).
	$$
	This method calls "jacobianDualRank" in order to obtain the full Jacobian dual rank  and then it tests the birationality of $\mathbb{F}$ (see Theorem 4.4 in @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps")@). 
    
        First, we compute some examples in the bigraded setting.  
    Example
     	R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
        I = ideal(x*u, y*u, y*v) -- a birational map
        isBiratMap I
     	I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
        isBiratMap I
	A = matrix{ {x^5*u,  x^2*v^2},
    	            {y^5*v, x^2*u^2},
	            {0,     y^2*v^2}
    	          };
        I = minors(2, A)  -- a non birational
        isBiratMap I
	I = ideal(x*u^2, y*u^2, x*v^2) -- a non birational map
        isBiratMap I
    Text	
    	Next, we test some rational maps over three projective spaces.
    Example
    	R = QQ[x,y,z,w]
        A = matrix{ {x + y,  x, x},
                    {3*z - 4*w, y, z},
	            {w,  z, z + w}, 
	            {y - z,  w, x + y}
	          };
        I = minors(3, A) -- a birational map
        isBiratMap I
        I = ideal(random(2, R), random(2, R), random(2, R), random(2, R)); -- a non birational 
        isBiratMap I
///




doc ///
    Key 
       Hm1Rees0
       (Hm1Rees0, Ideal)
    Headline
       computes the module [Hm^1(Rees(I))]_0
    Usage
       Hm1Rees0(I)
    Inputs
    	 I : Ideal	 
	   an ideal in a single graded polynomial ring
    Outputs
    	: Module
	    the module $[H_m^1(Rees(I))]_0$ 	   
    Description
       Text
       	 Let $R$ be the polynomial ring $R=k[x_0,...,x_n]$ and $\mathbf{m}$ be the maximal irrelevant ideal  $\mathbf{m}=(x_0,...,x_n)$.    
	 Let $I \subset R$ be the ideal $I=(f_0,...,f_m)$ where $deg(f_i)=d$. 
	 The Rees algebra $\mathcal{R}(I)$ is a bigraded algebra which can be given as a quotient of the polynomial ring $\mathcal{A}=R[y_0,...,y_m]$.
    	 We denote by $S$ the polynomial ring $S=k[y_0,...,y_m]$.
	 
	 The local cohomology module $H_{m}^1(\mathcal{R}(I))$ with respect to the maximal irrelevant ideal $\mathbf{m}$ is actually a bigraded $\mathcal{A}$-module.  
    	 We denote by $[H_m^1(Rees(I))]_0$ the restriction to degree zero part in the $R$-grading, that is $[H_m^1(Rees(I))]_0=[H_m^1(Rees(I))]_{(0,*)}$.     
       	 So we have that $[H_m^1(Rees(I))]_0$ is naturally a graded $S$-module.
       Example
         R = QQ[x,y,z]
	 A = matrix{ {x, x^6 + y^6 + z*x^5},
                     {-y, y^6 + z*x^3*y^2},
	             {0, x^6 + x*y^4*z}
	           };
         I = minors(2, A) -- a birational map
         prune Hm1Rees0 I
         A = matrix{ {x^2, x^2 + y^2},
                     {-y^2, y^2 + z*x},
	             {0, x^2}
	           };
         I = minors(2, A) -- a non birational map
	 Hm1Rees0 I	  
    Caveat
    	To call the method "Hm1Rees0(I)", the ideal $I$ should be in a single graded polynomial ring.
    
///


doc ///
    Key 
        gensSatSpecialFib
	(gensSatSpecialFib, Ideal, ZZ)
    	(gensSatSpecialFib, Ideal)
    Headline 
        computes generators of the saturated special fiber ring
    Usage 
        gensSatSpecialFib(I, nsteps)
	gensSatSpecialFib(I)
    Inputs
    	I : Ideal
	    a homogeneous ideal generated by elements of the same degree   
        nteps : ZZ
	    the number steps in the saturation of the powers of I. Optional.	
    Outputs
    	: List	    	
    	    a list of generators of the saturated special fiber ring. 
	    In the case where we use the function as "gensSatSpecialFib(I, nsteps)", the answer is correct only if nsteps is big enough to attain all the generators.
    Description
       Text
    	 This function computes generators of the saturated special fiber ring. 

    	 When we call "gensSatSpecialFib(I, nsteps)", the method iteratively computes the graded pieces
	 $$
	 [(I^1)^{sat}]_d, [(I^2)^{sat}]_{2d},  ......... , [(I^{nsteps})^{sat}]_{nsteps*d},
	 $$
	 where $(I^k)^{sat}$ denotes the saturation of $I$ with respect to the irrelevant ideal. 
	 
	 When we call "gensSatSpecialFib(I)", the method first computes the module $[H_m^1(Rees(I))]_0$ from which an upper bound nsteps.
	 After that, it simply calls "gensSatSpecialFib(I, nsteps)".
	 
	 First, we compute some examples in the case of plane rational maps.
       Example
       	 R = QQ[x,y,z]
    	 A = matrix{ {x, x^2 + y^2},
                     {-y, y^2 + z*x},
	             {0, x^2}
	           };
         I = minors(2, A) -- a birational map
         gensSatSpecialFib I
	 gensSatSpecialFib(I, 5)
    	 A = matrix{ {x^3, x^2 + y^2},
                     {-y^3, y^2 + z*x},
	             {0, x^2}
	           };
         I = minors(2, A) -- a non birational map
  	 gensSatSpecialFib I
	 gensSatSpecialFib(I, 5)
	 
       Text
         Next, we compute an example in the bigraded case.
       Example	 	 
	 R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
    	 I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
         gensSatSpecialFib(I, 5)
    Caveat
    	To call the method "gensSatSpecialFib(I)", the ideal $I$ should be in a single graded polynomial ring.
    			 
///

doc ///
    Key 
        satSpecialFiberIdeal
	(satSpecialFiberIdeal, Ideal, ZZ)
    	(satSpecialFiber, Ideal)
    Headline 
    	computes the defining equations of the saturated special fiber ring
    Usage 
        satSpecialFiberIdeal(I, nsteps)
        satSpecialFiberIdeal(I)
    Inputs
    	I : Ideal
	    a homogeneous ideal generated by elements of the same degree   
        nteps : ZZ
	    the number steps in the saturation of the powers of I. Optional.	
    Outputs
    	:Ideal
            the defining equations of the saturated special fiber ring
    Description
       Text
         The purpose of this function is to compute the defining equations of the special fiber ring.
		 
    	 Suppose that $\{g_1,...,g_m\}$ is the set of generators of the saturated special fiber ring (which can be obtained from  "gensSatSpecialFib").
      	 This function returns the kernel of the map $k[z_1, ... ,z_m] \to k[g_1, ... ,g_m]$ which is given by
	 $$
	 z_i \to g_i.
	 $$
	 
 	 First, we compute some examples of plane rational maps. 
       Example 
       	 R = QQ[x,y,z]
         A = matrix{ {x, x^5 + y^5},
                     {-y, y^5 + z*x^2*y^2},
	             {0, x^5}
	           };
         I = minors(2, A) -- a birational map
         satSpecialFiberIdeal I
	 A = matrix{ {x^3, x^2 + y^2},
                     {-y^3, y^2 + z*x},
 	             {0, x^2}
	           };
         I = minors(2, A) -- a non birational map
         satSpecialFiberIdeal I 
       Text
         Next, we test some bigraded rational maps.
       Example
         R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
         I = ideal(x*u, y*u, y*v) -- a birational map
         satSpecialFiberIdeal(I, 5)
	 I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
	 satSpecialFiberIdeal(I, 5) 
	    
    Caveat
    	To call the method "satSpecialFiberIdeal(I)", the ideal $I$ should be in a single graded polynomial ring.
	
	The answer of "satSpecialFiberIdeal(I, nsteps)" is correct only if nsteps is big enough to attain all the generators of the saturated special fiber ring.

///


doc ///
  Key
    jacobianDualRank 
    (jacobianDualRank ,Ideal)
  Headline
    computes the full Jacobian dual rank
  Usage
    jacobianDualRank(I)
  Inputs
    I : Ideal
    	an ideal defining the map
  Outputs
    :ZZ
        the total Jacobian dual rank 
  Description
    Text
        Let $R$ be the multi-homogeneous polynomial ring $R=k[x_{1,0},x_{1,1},...,x_{1,r_1}, x_{2,0},x_{2,1},...,x_{2,r_2}, ......, x_{m,0},x_{m,1},...,x_{m,r_m}]$ and $I$ be the multi-homogeneous ideal $I=(f_0,f_1,...,f_s)$ where the polynomials $f_i$'s have the same multi-degree.
	We compute the degree of the rational map $\mathbb{F}: \mathbb{P}^{r_1} \times \mathbb{P}^{r_2} \times ... \times \mathbb{P}^{r_m}  \to \mathbb{P}^s$ defined by
	$$
	(x_{1,0} : ... : x_{1,r_1}; ...... ;x_{m,0} : ... : x_{m,r_m}) \to (f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m}), ..... , f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m})).
	$$
	This function computes the full Jacobian dual rank of $\mathbb{F}$ (see Notation 4.2 in @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps")@). 
    
        First, we compute some examples in the bigraded setting.  
    Example
     	R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
        I = ideal(x*u, y*u, y*v) -- a birational map
        jacobianDualRank I
     	I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
        jacobianDualRank I
	A = matrix{ {x^5*u,  x^2*v^2},
    	            {y^5*v, x^2*u^2},
	            {0,     y^2*v^2}
    	          };
        I = minors(2, A)  -- a non birational
       	jacobianDualRank I
	I = ideal(x*u^2, y*u^2, x*v^2) -- non birational map
        jacobianDualRank I 
    Text	
    	Next, we test some rational maps over three projective spaces.
    Example
    	R = QQ[x,y,z,w]
        A = matrix{ {x + y,  x, x},
                    {3*z - 4*w, y, z},
	            {w,  z, z + w}, 
	            {y - z,  w, x + y}
	          };
        I = minors(3, A) -- a birational map
       	jacobianDualRank I
        I = ideal(random(2, R), random(2, R), random(2, R), random(2, R)); -- a non birational 
        jacobianDualRank I
///


doc ///
  Key
    partialJDRs 
    (partialJDRs ,Ideal)
  Headline
    computes the partial Jacobian dual ranks
  Usage
    partialJDRs(I)
  Inputs
    I : Ideal
    	an ideal defining the map
  Outputs
    :List
        the partial Jacobian dual ranks 
  Description
    Text
        Let $R$ be the multi-homogeneous polynomial ring $R=k[x_{1,0},x_{1,1},...,x_{1,r_1}, x_{2,0},x_{2,1},...,x_{2,r_2}, ......, x_{m,0},x_{m,1},...,x_{m,r_m}]$ and $I$ be the multi-homogeneous ideal $I=(f_0,f_1,...,f_s)$ where the polynomials $f_i$'s have the same multi-degree.
	We compute the degree of the rational map $\mathbb{F}: \mathbb{P}^{r_1} \times \mathbb{P}^{r_2} \times ... \times \mathbb{P}^{r_m}  \to \mathbb{P}^s$ defined by
	$$
	(x_{1,0} : ... : x_{1,r_1}; ...... ;x_{m,0} : ... : x_{m,r_m}) \to (f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m}), ..... , f_0(x_{1,0},...,x_{1,r_1}, ...... ,x_{m,0},...,x_{m,r_m})).
	$$
	This function computes the partial Jacobian dual ranks of $\mathbb{F}$ (see Notation 4.2 in @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps")@). 
    
        First, we compute some examples in the bigraded setting.  
    Example
     	R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
        I = ideal(x*u, y*u, y*v) -- a birational map
        partialJDRs I
     	I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
        partialJDRs I
	A = matrix{ {x^5*u,  x^2*v^2},
    	            {y^5*v, x^2*u^2},
	            {0,     y^2*v^2}
    	          };
        I = minors(2, A)  -- a non birational
        partialJDRs I
	I = ideal(x*u^2, y*u^2, x*v^2) -- non birational map
        partialJDRs I
    Text	
    	Next, we test some rational maps over three projective spaces.
    Example
    	R = QQ[x,y,z,w]
        A = matrix{ {x + y,  x, x},
                    {3*z - 4*w, y, z},
	            {w,  z, z + w}, 
	            {y - z,  w, x + y}
	          };
        I = minors(3, A) -- a birational map
        partialJDRs I
        I = ideal(random(2, R), random(2, R), random(2, R), random(2, R)); -- a non birational 
        partialJDRs I
///



doc ///
  Key
   upperBoundDegreeSingleGraded
   (upperBoundDegreeSingleGraded,Ideal)
  Headline
   computes an upper bound for the degree of a rational map
  Usage
   upperBoundDegreeSingleGraded(I)
  Inputs
    I : Ideal
    	an ideal defining the map
  Outputs
    :ZZ
        an upper bound for the degree of the corresponding rational map 
  Description
    Text
        Let $R$ be the polynomial ring $R=k[x_0,...,x_r]$ and $I$ be the homogeneous ideal $I=(f_0,f_1,...,f_s)$ where $deg(f_i)=d$.
	We compute the degree of the rational map $\mathbb{F}: \mathbb{P}^r \to \mathbb{P}^s$ defined by
	$$
	(x_0: ... :x_r) \to (f_0(x_0,...,x_r), f_1(x_0,...,x_r), ..... , f_s(x_0,...,x_r)).
	$$
        Using certain Hilbert functions the degree of the map is bounded (see Theorem 3.22 in @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps")@).
    
        The following example is a rational map without base points: 
    Example
      R = QQ[x,y,z]
      I = ideal(random(4, R), random(4, R), random(4, R));
      betti res I
      degreeOfMap I
      upperBoundDegreeSingleGraded I
    Text
    	In the following examples we play with the relations of the Hilbert-Burch presentation and the degree of $\mathbb{F}$ (see Proposition 5.2 and Theorem 5.12):
    Example 	
      A = matrix{ {x, x^2 + y^2},
                  {-y, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a birational map
      degreeOfMap I
      upperBoundDegreeSingleGraded I
      A = matrix{ {x^2, x^2 + y^2},
                  {-y^2, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      degreeOfMap I 
      upperBoundDegreeSingleGraded I
      A = matrix{ {x^3, x^2 + y^2},
                  {-y^3, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      degreeOfMap I 
      upperBoundDegreeSingleGraded I
 	
  Caveat
    To call the method "degreeOfMap(I)", the ideal $I$ should be in a single graded polynomial ring and dim(R/I) <= 1.	    
///




doc ///
    Key 
    	Hm1Rees0Strategy
    Headline
    	A strategy for degreeOfMap
    Description
    	Text
	    When this strategy is used then the degree of the map is computed directly from the multiplicity of $[H_m^1(Rees(I))]_0$.
    	    It contains an implementation of Corollary 2.12 in @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps")@.
///



doc ///
    Key 
    	SatSpecialFibStrategy
    Headline
    	A strategy for degreeOfMap
    Description
    	Text
	    When this strategy is used then the degree of the map is computed directly from the multiplicity of the saturated special fiber ring of $I$.
	    The degree of the rational map then can be obtained from Theorem 2.4 in @ HREF("https://arxiv.org/abs/1805.05180", "Degree and birationality of multi-graded rational maps")@.
///


doc ///
   Key
      [degreeOfMap, Strategy]
   Headline
       Choose a strategy for computing the degree of map
   Usage
       degreeOfMap(...,Strategy => ...)
   Description
       Text
       	   Depending on this strategy the function "degreeOfMap" computes the degree of a map by two different approaches.
	   The two strategies are "Hm1Rees0Strategy" and "SatSpecialFibStrategy".

///


TEST ///
    R = QQ[x,y,z]
      I = ideal(random(4, R), random(4, R), random(4, R));
      betti res I
      assert(degreeOfMap I != 1)
      A = matrix{ {x, x^2 + y^2},
                  {-y, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a birational map
      assert(degreeOfMap I == 1)
      A = matrix{ {x^2, x^2 + y^2},
                  {-y^2, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      assert(degreeOfMap I != 1) 
      A = matrix{ {x^3, x^2 + y^2},
                  {-y^3, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      assert(degreeOfMap I != 1) 
      A = matrix{ {x^3, x^4},
                  {-y^3, y^4},
	          {z^3, x^4}
	        };
      I = minors(2, A) -- a non birational map
      assert(degreeOfMap I != 1)
      R = QQ[x,y,z,v,w]
      I = ideal(random(1, R), random(1, R), random(1, R), random(1, R), random(1, R));
      degreeOfMap(I, Strategy=>SatSpecialFibStrategy)    	
      I = ideal(29*x^3 + 55*x*y*z, 7*y^3, 14*z^3, 17*v^3, 12*w^3)
      degreeOfMap(I, Strategy=>SatSpecialFibStrategy)
 
///


TEST ///
    R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
    I = ideal(x*u, y*u, y*v) -- a birational map
    assert(degreeOfMapIter(I, 5) == 1)
    I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
    assert(degreeOfMapIter(I, 5) != 1)
    A = matrix{ {x^5*u,  x^2*v^2},
                {y^5*v, x^2*u^2},
                {0,     y^2*v^2}
              };
    I = minors(2, A)  -- a non birational
    assert(degreeOfMapIter(I, 5) != 1)
    
///

TEST ///
     R = QQ[x,y,z]
     A = matrix{ {x, x^2 + y^2},
                  {-y, y^2 + z*x},
                  {0, x^2}
               };
     I = minors(2, A) -- a birational map
     assert(length gensSatSpecialFib I == 3)
     assert(length gensSatSpecialFib(I,5) == 3)
     A = matrix{ {x^3, x^2 + y^2},
                 {-y^3, y^2 + z*x},
                 {0, x^2}
                };
     I = minors(2, A) -- a non birational map
     assert(length gensSatSpecialFib I != 3)
     assert(length gensSatSpecialFib(I,5) != 3)
     R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
     I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
     assert(length gensSatSpecialFib(I,5) != 3)
    
///


TEST ///
     R = QQ[x,y,z]
     A = matrix{ {x, x^5 + y^5},
                 {-y, y^5 + z*x^2*y^2},
                 {0, x^5}
               };
     I = minors(2, A) -- a birational map
     assert(isPolynomialRing satSpecialFiber I)
     A = matrix{ {x^3, x^2 + y^2},
                 {-y^3, y^2 + z*x},
                 {0, x^2}
               };
     I = minors(2, A) -- a non birational map
     assert(not isPolynomialRing satSpecialFiber I)
     R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
     I = ideal(x*u, y*u, y*v) -- a birational map
     assert(isPolynomialRing satSpecialFiber(I,5))
     satSpecialFiber(I, 5)
     I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
     assert(not isPolynomialRing satSpecialFiber(I,5))
///


TEST ///
     R = QQ[x,y,z]
     A = matrix{ {x, x^5 + y^5},
                 {-y, y^5 + z*x^2*y^2},
                 {0, x^5}
               };
     I = minors(2, A) -- a birational map
     assert(numgens satSpecialFiberIdeal I == 0)
     A = matrix{ {x^3, x^2 + y^2},
                 {-y^3, y^2 + z*x},
                 {0, x^2}
                };
     I = minors(2, A) -- a non birational map
     assert(numgens satSpecialFiberIdeal I != 0) 
     R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
     I = ideal(x*u, y*u, y*v) -- a birational map
     assert(numgens satSpecialFiberIdeal(I,5) == 0)
     satSpecialFiberIdeal(I, 5)
     I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
     satSpecialFiberIdeal(I, 5) 
     assert(numgens satSpecialFiberIdeal(I,5) != 0)
	
///


TEST ///
    	R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
        I = ideal(x*u, y*u, y*v) -- a birational map
        assert(jacobianDualRank I == 2)
     	I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
        assert(jacobianDualRank I != 2)
	A = matrix{ {x^5*u,  x^2*v^2},
    	            {y^5*v, x^2*u^2},
	            {0,     y^2*v^2}
    	          };
        I = minors(2, A)  -- a non birational
        assert(jacobianDualRank I != 2)
	I = ideal(x*u^2, y*u^2, x*v^2) -- non birational map
        jacobianDualRank I 
    	R = QQ[x,y,z,w]
        A = matrix{ {x + y,  x, x},
                    {3*z - 4*w, y, z},
	            {w,  z, z + w}, 
	            {y - z,  w, x + y}
	          };
        I = minors(3, A) -- a birational map
        assert(jacobianDualRank I == 3)
        I = ideal(random(2, R), random(2, R), random(2, R), random(2, R)); -- a non birational 
        assert(jacobianDualRank I != 3)

///


TEST ///
      	R = QQ[x,y,u,v, Degrees => {{1,0}, {1,0}, {0,1}, {0,1}}]
        I = ideal(x*u, y*u, y*v) -- a birational map
        assert((partialJDRs I) == {1,1})
     	I = ideal(x*u, y*v, x*v + y*u) -- a non birational map
        partialJDRs I
	A = matrix{ {x^5*u,  x^2*v^2},
    	            {y^5*v, x^2*u^2},
	            {0,     y^2*v^2}
    	          };
        I = minors(2, A)  -- a non birational
     	assert((partialJDRs I) != {1,1})
	I = ideal(x*u^2, y*u^2, x*v^2) -- non birational map
        partialJDRs I
    	R = QQ[x,y,z,w]
        A = matrix{ {x + y,  x, x},
                    {3*z - 4*w, y, z},
	            {w,  z, z + w}, 
	            {y - z,  w, x + y}
	          };
        I = minors(3, A) -- a birational map
        partialJDRs I
        I = ideal(random(2, R), random(2, R), random(2, R), random(2, R)); -- a non birational 
        partialJDRs I


///


TEST ///
      R = QQ[x,y,z]
      I = ideal(random(4, R), random(4, R), random(4, R));
      betti res I
      assert((degreeOfMap I) <= (upperBoundDegreeSingleGraded I))
      A = matrix{ {x, x^2 + y^2},
                  {-y, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a birational map
      assert((degreeOfMap I) <= (upperBoundDegreeSingleGraded I))
      A = matrix{ {x^2, x^2 + y^2},
                  {-y^2, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      assert((degreeOfMap I) <= (upperBoundDegreeSingleGraded I))
      A = matrix{ {x^3, x^2 + y^2},
                  {-y^3, y^2 + z*x},
	          {0, x^2}
	        };
      I = minors(2, A) -- a non birational map
      assert((degreeOfMap I) <= (upperBoundDegreeSingleGraded I))
    
///


end--

uninstallPackage "MultiGradedRationalMap"
restart
installPackage "MultiGradedRationalMap"
viewHelp "MultiGradedRationalMap"
check "MultiGradedRationalMap"

loadPackage "MultiGradedRationalMap"

