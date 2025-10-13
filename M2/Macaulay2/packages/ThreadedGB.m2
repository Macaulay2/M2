newPackage("ThreadedGB",
    Version => "1.1",
    Date => "November 12, 2020",
    Authors => {
	{
	    Name => "Sonja Petrovic",
	    Email => "sonja.petrovic@iit.edu",
	    HomePage => "http://math.iit.edu/~spetrov1/"
	},
        {
	    Name => "Sara Jamshidi Zelenberg",
	    Email => "szelenberg@mx.lakeforest.edu",
	    HomePage => "https://www.sjzelenberg.com/"
	}
    },
    Keywords => {"Groebner Basis Algorithms"},
    Headline => "Compute a Groebner basis using the classical Buchberger with multiple threads",
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "https://msp.org/jsag/",
	 "article title" => "Threaded Gröbner bases: a Macaulay2 package",
	 "acceptance date" => "8 October 2021",
	 "published article URI" => "https://msp.org/jsag/2021/11-1/p12.xhtml",
	 "published article DOI" => "10.2140/jsag.2021.11.123",
	 "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x12-ThreadedGB.m2",
	 "release at publication" => "51d352fbdb9f5903c5bedbd5dce0c14d3fc66d2d",	    -- git commit number in hex
	 "version at publication" => "1.1",
	 "volume number" => "11",
	 "volume URI" => "https://msp.org/jsag/2021/11-1/"
	 }
    )

-- The factory library code is not thread-safe, and 'gcd' calls it.  Here we insert some simple routines to avoid it.
-- This code assumes we are over a field, so we don't have to take gcd's of the coefficients, and that f and g are monomials, which is the case in the code.
manualGCD = (f,g) -> (
     R := ring f;
     a := first exponents f;
     b := first exponents g;
     c := apply(a,b,min);
     R_c)
manualLCM = (f,g) -> (
     R := ring f;
     a := first exponents f;
     b := first exponents g;
     c := apply(a,b,max);
     R_c)


export {
    "tgb",
    "reduce",
    "Minimal",
    "LineageTable"
    }
endGB = local endGB;
tasks = local tasks;


--***************************************************************************--
--                          Lineage Tables  	                     	     --
--***************************************************************************--
LineageTable = new Type of HashTable
-- The point: endGB will be a LineageTable; 
-- Not just a HashTable -- it's special b/c keys are lineages and values are a GB. 

lineageTable := method(TypicalValue => LineageTable)
lineageTable(List) := (F) -> (
    new LineageTable from  apply(#F,i-> i=>F_i) 
    )

matrix (LineageTable) := opts -> (g) -> (
    g = new MutableHashTable from g;  
    if  g#?"trivial"  then remove(g,"trivial"); 
    matrix {delete(null,values g)}
)



--***************************************************************************--
--                          Main Function      	                     	     --
--***************************************************************************--

-------------------------------
--      tgb
-------------------------------
--inputs:
    -- basisList = the starting basis;
--output:
    -- a Groebner basis, of type HashTable with lineage keys.
-------------------------------
tgb = method(
    TypicalValue => LineageTable,
    Options => {
      Verbose => false,
      Minimal => false
      }
    )
tgb (List) := LineageTable => o -> (basisList) -> (
    
    -- Creating tasks and distributing the computation:
    tasks = new MutableHashTable;
    endGB = new MutableHashTable from lineageTable(basisList);
    
    -- go through all the base pairs and schedule a task for reducing their S-polynomial: 
    apply(#basisList-1,i-> apply(i+1..#basisList-1, j-> (
		-- but only schedule tasks that fail the LCM criterion: 
		if not manualGCD(leadTerm basisList_i, leadTerm basisList_j)==1 then (
		    currentPairKey := (i,j);
      		    tasks#currentPairKey = createTask taskFn (basisList_i, basisList_j, currentPairKey, Verbose=>o.Verbose);
      		    if o.Verbose then << "Scheduling a task for lineage " << toString currentPairKey << endl;
      		    schedule tasks#currentPairKey)))
    );

    -- clean up in case we already know gb={1}:
    if endGB#?"trivial" then (
	if o.Verbose then << "Found a unit in the Groebner basis; reducing now." << endl; 
	scan(delete("trivial",keys endGB),k-> endGB#k = null); -- null everyone first
	endGB#(endGB#"trivial") = 1; -- keep the one that produced  a trivial GB
	remove(endGB,"trivial"); -- kill the trivial thing  - it really has no info now.
	return new LineageTable from endGB
	);


    -- Grabbing results from tasks and closing off:
    allReady := false;
    while not allReady do(
      allReady = true;
      tasksValues := values(tasks);
      for i to #tasksValues-1 do(
        if not isReady(tasksValues_i) then allReady = false;
    	);
      if not  allReady then sleep 1; 
    );

    -- final clean up:
    if endGB#?"trivial" then (
	-- endGB#"trivial" contains the key of the (first) element that produced  a unit remainder
	-- here, we turn every element from endGB, except endGB#(endGB#"trivial"), equal to NULL:  
	if o.Verbose then << "Found a unit in the Groebner basis; reducing now." << endl; 
	scan(delete("trivial",keys endGB),k-> endGB#k = null); -- null everyone first
	endGB#(endGB#"trivial") = 1; -- keep the one that produced  a trivial GB
	remove(endGB,"trivial"); -- kill the list of trivial keys  - this really has no further info now.
	return new LineageTable from endGB
	-- Caveat (non-critical):
	-- if  two different elements return 1 (b/c parallel threads), then I guess 
	-- the one that came in last is the one that got preserved? 
	-- but  it really doesn't matter, does it?:  if they were sent out as tasks and  
	-- were running at the same time, it'a  a matter of luck - not necessarily design - which one produced a  1. 
	);
    if o.Minimal then  minimize new LineageTable from endGB  else  new LineageTable from endGB
);

-- Ideal as input:
tgb (Ideal) := MutableHashTable => o -> (I) -> (
    tgb(I_*,Verbose=>o.Verbose,Minimal=>o.Minimal)
);


---------------------------------------------------------------------------------------------
--      minimize
---------------------------------------------------------------------------------------------
-- Scans values of a hash table H and retains only those whose leadTerms are minimal generators
-- of the ideal (leadTerm f : f in values H). 
-- If values H constitutes a Groebner basis of the ideal it generates,
-- this method returns a minimal Groebner basis.
-- Keys of non-minimal elements are retained within the hashtable (H#those=null).
---------------------------------------------------------------------------------------------
--minimize = method(TypicalValue => LineageTable)
minimize(LineageTable) := LineageTable => (H) -> (
    -- nothing to do if gens gb == {1}:
    if H#?"trivial" then (
	<< "Groebner basis trivial, already reduced." << endl; 
	return H
	);
    -- if null values in H then computations below break. But this can only happen if minimize was already called, so:
--    if any(values H,h-> h===null) then (
    if any(H, (k,h) -> h===null) then (
	<< "null values found in hash table. If the table has been obtained from
	a threaded Groebner bases computation, then this means the basis is already minimal or reduced." << endl; 
	return H;
	);
    -- end of catching errors. 
    
    G := new MutableHashTable from H;
    -- ensure all leading coefficients are 1:
    scan(keys G, kf-> if leadCoefficient G#kf != 1 then (
	    f:=G#kf;
	    G#kf = lift(f/leadCoefficient f,ring f)) -- else don't touch:  else f);
	);
    -- divide the leading terms:
    nonminimalElements := new MutableHashTable;
    scan(keys G, kf-> ( 
	    f:=G#kf;
	    remove(G,kf);
	    lt := leadTerm matrix {values G};
	    -- if LT(f) is divisible by any LT(g) for (g\in G\setminus f), then f is not minimal:
	    if leadTerm f%lt==0 then nonminimalElements#kf=null else G#kf=f;
	    )
	);
    new LineageTable from merge(G, nonminimalElements,identity)
    )


---------------------------------------------------------------------------------------------
--      reduce
---------------------------------------------------------------------------------------------
-- Takes a LineageTable, which is meant to be output of the method tgb, and returns 
-- the reduced Groebner basis. 
-- Values of the input table are polynomials; 
-- the method replaces each by its remainder on division by the rest. 
---------------------------------------------------------------------------------------------
reduce = method(TypicalValue => LineageTable)
reduce(LineageTable) := LineageTable => (H) -> (
    -- nothing to do if gens gb == {1}; the output is already reduced: 
    if H#?"trivial"  then 	return H; 

    -- the basis should be minimal first:
    H = minimize(H);
    
    -- replace each g by its remainder on division by G\setminus g
    G := new MutableHashTable from H;
    -- but make sure to remove the null elements first (else division errors):
    -- (note the null elements only appear as output of minimize or if gb == {1}; the latter case won't even reach this line.)
    scanPairs(H, (kf,f)-> if f === null then remove(G,kf));
    g := 0;
    scan(keys G, kf-> (
	    remove(G,kf);
    	    -- replace each g by its remainder on division by G\setminus g.
	    g = remainderFn(H#kf,values G); 
	    G#kf = 1/(leadCoefficient g)*g;
	    )
	);
    -- the following merge with "min" simply adds back the null values from H
    -- (that we had to clean out (for division to work) after minimalizing):
    new LineageTable from merge(G, H, min) 
    )




--***************************************************************************--
--                          Internal functions 	                     	     	 --
--***************************************************************************--

---------------------------------------------------------------------------------------------
--         SPoly
---------------------------------------------------------------------------------------------
-- takes two polynomials and computes the corresponding S-polynomial
---------------------------------------------------------------------------------------------
spoly = method()
spoly(RingElement, RingElement) := RingElement => (f, g) -> (
    gamma := manualLCM(leadMonomial f, leadMonomial g);
    (gamma // leadTerm f) * f - (gamma // leadTerm g) * g
    )
---------------------------------------------------------------------------------------------
--         taskFn
---------------------------------------------------------------------------------------------
-- taskFn is the function used in scheduled tasks for threads to compute.
-- It takes in a pair of polynomials with the key from endGB and places the remainder of their
-- S-polynomial on division by endGB (the current master copy of the Groebner basis).
-- The method taskFn then schedules a new set of tasks based on a new remainder
-- (one task for each new pair with an existing endGB polynomial and the current remainder).
-- If the remainder is 1, then endGB is replaced with 1 (but lineage keys of all non-zero remainders
-- computed so far are saved).
-- Polynomials whose initial terms are relatively prime are not considered. 
---------------------------------------------------------------------------------------------
taskFn = {Verbose=>false} >> o-> (f1,f2,currentPairKey) -> () -> (
    r := 0; 
    if not endGB#?"trivial" then if manualGCD(leadTerm f1, leadTerm f2)==1 then r=0 else r = remainderFn(spoly(f1,f2), values endGB);
    -- note if LCM criterion holds we know remainder = 0 so nothing gets scheduled. 

    if r!=0 and r!=1 and r!=-1 then (
      scan(delete ("trivial", keys endGB),i-> (
        currentPairKeyChild := (currentPairKey,i); 
        tasks#currentPairKeyChild = createTask taskFn (r,endGB#i,currentPairKeyChild,Verbose=>o.Verbose);
	if o.Verbose then << "Scheduling task for lineage " <<  toString currentPairKeyChild << endl; 
        schedule tasks#currentPairKeyChild
		    )
      );
	endGB#currentPairKey = r;
	if o.Verbose then << "Adding the following remainder to GB: " << toString r << " from lineage " << toString currentPairKey << endl;
	);
    
    if r==1 or r==-1 then (
	endGB#currentPairKey = r/leadCoefficient r;
	endGB#"trivial" = currentPairKey; -- tell "trivial" which lineage produced a unit! 
        if o.Verbose then  << "Adding the following remainder to GB: " << toString r << " from lineage " << toString currentPairKey << endl;
	);

)
---------------------------------------------------------------------------------------------
--      remainderFn
---------------------------------------------------------------------------------------------
-- remainderFn calculates the remainder of a polynomial f
-- with respect to a list of polynomials G.
-- (Function borrowed from Greg Smith's CLO class https://mast.queensu.ca/~ggsmith/)
---------------------------------------------------------------------------------------------
remainderFn = method()
remainderFn(RingElement, List) := RingElement => (f,G) -> (
    S := ring f;
    p := f;
    r := 0_S;
    m := #G;
    while p != 0 do (
	i := 0;
	while i < m and leadTerm(p) % leadTerm(G#i) != 0 do i = i+1;
	if i < m then (
    p = p - (leadTerm(p) // leadTerm(G#i)*G#i))
	else (
    r = r + leadTerm(p);
    p = p - leadTerm(p)));
    r
);

--***************************************************************************--
--                          DOCUMENTATION     	                     	     	 --
--***************************************************************************--
beginDocumentation()
doc ///
  Key
    ThreadedGB
  Headline
    a package for distributed computation of Gr\"obner bases
  Description
    Text
      The complexity of Gr\"obner computations has inspired many improvements to Buchberger's
      algorithm over the years.
      While this package does not propose an improvement to the way the algorithm operates mathematically,
      it offers a way to distribute the algorithm among threads that run in parallel.
      It is our hope that such a distributed version of the algorithm should be written in the core of the program;
      however, there are still important insights one can obtain from the current implementation.

      To us, the most interesting is the insight into {\it lineages} (see below) of non-zero remainders
      that are added to the basis during a run of Buchberger. How are these affected by the structure of the
      input system? What do they say about the complexity of the computation itself (and not only the complexity
      of the basis)? These are questions at the heart of what we are aiming to discover, and the output of the
      threaded Gr\"obner bases method @TO tgb@ returns this information in form of a {\bf lineage table}.
    Example
      QQ[x_1,x_0,x_3,x_5,x_4,x_2,MonomialOrder=>Lex]
      rnc = minors(2, matrix{{x_0..x_4},{x_1..x_5}})
      allowableThreads  =  4
      g = tgb(rnc)
    Text
      The lineage table is a hash table, whose values are Gr\"obner basis elements, and whose keys are the {\it lineages}.

      {\bf Definition.} A lineage of a polynomial is a natural number, or an ordered pair of lineages, tracing 
      its history in the given Gr\"obner basis computation.

      Lineages that are natural numbers are assigned to the original input polynomials.
      In the example above, the 10 minors have lineages $0,\dots,9$. 
    Example
      g#1
      g#2
    Text
      If the S-polynomial of g#"i" and g#"j" produces a non-zero remainder in Buchberger's algorithm,
      that remainder is added to the hashtable g with key (i-j), as in the following example.
    Example
      g#(1,2)
    Text
      As the algorithm continues, keys are concatenated, so that for example the remainder of S(0,S(1,2)) will
      have lineage (0,(1,2)), and so on.   For more complicated lineage examples, see @TO tgb@.

      Naturally, one can obtain
      a minimal basis or the reduced one as follows. In the output below, elements that are reduced
      are replaced by null, but their lineage keys are retained for informative purposes.
    Example
      minimize g
      gRed = reduce g
    Text
      To get the Gr\"obner basis in standard M2 matrix format, simply call the following:
    Example
      matrix gRed
    Text
      {\bf Nuts and Bolts}

      The main function,  @TO tgb@, uses  @TO Task@s to distribute the reduction of S-polynomials using a
      a current version of the Groenber basis.
      It can reduce and minimize upon request or print out  task scheduling information
      as it creates new tasks.
      The interesting part of the output may be the lineages of the basis polynomials,
      in addition to the Gr\"obner basis itself.
      Here is an example where the Gr\"obner basis is trivial.
    Example
      QQ[a..d]
      I=ideal( -c^3+a^2+b*d, a*b*c-1,a*b*c)
      allowableThreads =  2;
      T = tgb(I,Verbose=>true)
      allowableThreads = 1;
    Text
      In particular, the lineages of null values tell us what S-polynomials didn't reduce to zero until $1$ was found as
      a remainder.
  SeeAlso
    tgb
  Contributors
    @HREF{"https://www.linkedin.com/in/tannerzielinski/", "Tanner Zielinski"}@ <@HREF{"mailto:tzielin1\@hawk.iit.edu", "tzielin1\@hawk.iit.edu"}@>
///
doc ///
  Key
    tgb
    (tgb, List)
    (tgb, Ideal)
  Headline
    threaded Gr\"obner bases
  Usage
    tgb(List)
    tgb(Ideal)
  Inputs
    L : List
      of polynomials
    I : Ideal
  Outputs
    : LineageTable
      a hashtable whose values are a Gr\"obner basis for the ideal {\tt I} or the ideal generated by {\tt L},
      and keys are the lineages of the corresponding elements.
  Description
    Text
      Threaded Gr\"obner basis uses  @TO Task@s to compute a Gr\"obner basis of {\tt I} or {\tt ideal L}
      using $n$ threads.
    Example
      R = ZZ/101[x,y,z, MonomialOrder=>Lex];
      I = ideal {2*x + 10*y^2*z, 8*x^2*y + 10*x*y*z^3, 5*x*y^3*z^2 + 9*x*z^3, 9*x*y^3*z + 10*x*y^3};
      allowableThreads  = 4;
      H = tgb I
    Text
      The keys of the hashtable are meaningful; for example, the polynomial with key
      {\tt ((0,2),1)} in the hashtable {\tt tgb(L,n)} is the remainder upon division of the S-polynomial $(g, I_1)$,
      where $g$ is calculated from the S-pair $(I_0, I_2)$.
      For this reason, we say that the key communicates the "lineage" of the resulting polynomial.
      (See @TO ThreadedGB@.)

      Note that the keys in the hash table are strings, and the keys of input polynomials are 0..#L, as in the following example.
    Example
      H#(0,1)
    Text
      Some may be curious  how tgb works.

      The starting basis $L$ (the input list {\tt L} or {\tt L=gens I})
      populates the entries numbered $0$ through $n-1$ of a mutable hash table $G$, where  $n$ is the length of $L$. 
      The method creates all possible
      S-polynomials of $L$ and schedules their reduction with respect to $G$ as tasks.
      Throughout the computation, every nonzero remainder added to the basis is added to $G$
      with its lineage as the key. Each such remainder also triggers the creation of S-polynomials
      using it and every element in $G$ and scheduling the reduction thereof as additional tasks.
      The process is done when there are no remaining tasks.

      There is a way to track the tasks being created by turning on the option {\tt Verbose}.
    Example
      QQ[a..d];
      f0 = a*b-c^2;
      f1 = b*c-d^2;
      allowableThreads=2;
      tgb({f0,f1},Verbose=>true)
    Text
      In the example above, the S-polynomial S(f0,f1) didn't reduce to zero, hence the remainder was added to the
      output with key (0,1). The additional two S-polynomials reduced and the process ended.
  SeeAlso
    (minimize, LineageTable)
    reduce
  Caveat
    Due to threads running in parallel, it can happen that there are redundant elements in the final
    Gr\"obner basis. However these can be easily removed using @TO (minimize, LineageTable)@, for example.
    
    Also, {\tt  allowableThreads} needs to be set to an integer larger than 1, prior to calling @TO tgb@.
    Otherwise, errors may occur. It may be a good idea to reset allowableThreads to 1 after the threaded computations are done. 
///
doc ///
  Key
    LineageTable
  Headline
    a hash table of Gr\"obner basis polynomials and their lineages
  Description
    Text
      A lineage table is a hashtable whose values are a Gr\"obner basis for the ideal {\tt I} or the ideal generated by {\tt L},
      and keys are the lineages of the corresponding elements.

      A lineage of a polynomial is a natural number, or an ordered pair of lineages, tracing 
      its history in the given Gr\"obner basis computation.
      Lineages that are natural numbers are assigned to the original input polynomials.
  SeeAlso
    tgb
///
doc ///
  Key
    (minimize, LineageTable)
  Headline
    turn a Gr\"obner basis computed using threaded Gr\"obner bases into a minimal one
  Usage
    minimize(LineageTable)
  Inputs
    H : LineageTable
      whose values are polynomials
  Outputs
    : LineageTable
      where values in H whose initial terms are divisible by others are replaced by null
  Description
    Text
      Scans values of a hash table H and retains only those whose initial terms are minimal generators
      of the ideal generated by the leading terms of the values of H. If the values of H constitute a Gr\"obner basis of the ideal they generate,
      this method returns a minimal Gr\"obner basis.
    Example
      R = ZZ/101[a,b,c];
      allowableThreads= 2;
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2")
      minimize T
    Text
      Polynomials are normalized so that the leading coefficient is 1.
      Note that keys of non-minimal entries are retained, and the corresponding table value is null.
///
doc ///
  Key
    reduce
    (reduce, LineageTable)
  Headline
    produce a reduced Gr\"obner basis from one computed by threaded Gr\"obner bases
  Usage
    reduce LineageTable
  Inputs
    H : LineageTable
      whose values are polynomials
  Outputs
    : LineageTable
      where values in H whose initial terms are divisible by others are replaced by null and the remaining values are replaced by their remainder upon division by the rest
  Description
    Text
      Minimalizes first, then replaces each of the values of a hash table H by its remainder on the division by the remaining values H.

      If values H constitute a Gr\"obner basis of the ideal they generate,
      this method returns a reduced Gr\"obner basis.
    Example
      R = ZZ/101[a,b,c];
      allowableThreads= 2;
      T = tgb ideal "abc+c2,ab2-b3c+ac,b2"
      reduce T
    Text
      Polynomials are normalized so that the leading coefficient is 1.
      Note that keys of non-minimal entries are retained, and the corresponding table value is null.
///
doc ///
  Key
    (matrix, LineageTable)
  Headline
    extract a matrix of polynomials from values of a LineageTable after deleting null values
  Usage
    matrix LineageTable
  Inputs
    H : LineageTable
      whose values are polynomials
  Outputs
    : Matrix
      of non-null values of H
  Description
    Text
      This simple function just returns the Gr\"obner basis computed with threaded Gr\"obner basis function @TO tgb@
      in the expected Macaulay2 format, so that further computation are one step easier to set up.
    Example
      R = ZZ/101[a,b,c];
      allowableThreads= 2;
      T = reduce tgb( ideal "abc+c2,ab2-b3c+ac,b2")
      matrix T
///
doc ///
  Key
    [tgb, Verbose]
  Headline
    Option to specify whether additional output is wanted.
  Usage
    tgb(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>True} as an argument in the function @TO tgb@
      for additional output, including information
      about each new generated thread (i.e., each new S-polynomial reduction
      computation) as well as each new Gr\"obner basis element added to the current basis.
      Lineages are reported as well.
    Example
      S = QQ[x,y,z,w];
      allowableThreads= 2;
      tgb({x*y-z^2,y*z-w^2},Verbose=>true);
///
doc ///
  Key
    Minimal
    [tgb, Minimal]
  Headline
    Option to specify whether the end Gr\"obner basis should be a minimal Gr\"obner basis
  Usage
    tgb(...,Minimal=>Boolean)
  Description
    Text
      Use {\tt Minimal=>True} as an argument in the function @TO tgb@
      for ensure the resulting Gr\"obner basis is minimized.
      Lineages of non-minimal Gr\"obner basis elements that were added to the basis during the
      distributed computation are saved, with the corresponding entry in the table being null.
    Example
      S = ZZ/101[a,b,c];
      allowableThreads= 2;
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2", Minimal=>true)
    Text
      By default, the option is false. The basis can also be minimized after the distributed computation
      is finished:
    Example
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2")
      minimize T
///

--***************************************************************************--
--                          Tests             	                             --
--***************************************************************************--

TEST /// -- GB of {1}
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = {x, x - 1};
    allowableThreads= 4;
    H = tgb(I);
    G = new MutableHashTable from H;
    scanPairs(H, (kf,f)-> if f === null then remove(G,kf));
    assert (ideal apply(values G,g->g_(frac R)) ==  ideal 1_(frac R));
    allowableThreads= 1;    
///;

TEST /// -- already minimal/reduced
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = {y - z, x - y};
    allowableThreads= 4;
    H = tgb(I);
    assert (H===minimize H);
    allowableThreads= 1;    
///;

TEST /// -- indivisible S-poly added to hash table correctly
    R = ZZ/101[x,y,z, MonomialOrder=>Lex];
    I = {x^3*y, 4*x^2*y*z^4 - 6*y^5*z^2 - 9*y^2, x*y^3*z^5};
    allowableThreads= 4;
    H = tgb(I);
    f = I_0;
    g = I_1;
    gamma = lcm(leadMonomial f, leadMonomial g);
    s = (gamma // leadTerm f) * f - (gamma // leadTerm g) * g;
    assert (s == H#(0,1)) 
    allowableThreads= 1;    
///;

TEST /// -- first element check
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = {x*y*z + z^2, x*y^2 - y^3*z + x*z, y^2};
    allowableThreads= 4;
    H = tgb(I);
    assert (x^2*z == H#(0,1))
    allowableThreads= 1;    
///;

TEST /// -- consistent with gens gb 
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = ideal(x - y, y - z, -x + z);
    allowableThreads= 4;    
    H = tgb I;
    assert (gens gb  I == matrix reduce H)
    allowableThreads= 1;    
///;


--***************************************************************************--
end   


--***************************************************************************--
-- Useful commands
--***************************************************************************--
restart
installPackage("ThreadedGB", RemakeAllDocumentation=>true, RerunExamples=>true)
loadPackage("ThreadedGB",Reload=>true)
viewHelp tgb
viewHelp "ThreadedGB"
viewHelp (minimize, LineageTable)
installPackage("ThreadedGB")
check ThreadedGB
