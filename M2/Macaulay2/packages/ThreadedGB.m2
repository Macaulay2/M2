newPackage("ThreadedGB",
    Version => "1.0",
    Date => "June 12, 2020",
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
	},
        {
      	    Name => "Contributing Author: Tanner Zielinski",
	    Email => "tzielin1@hawk.iit.edu",
	    HomePage => "https://www.linkedin.com/in/tannerzielinski/"
	}
    },
    Keywords => {"Groebner Basis Algorithms"},
    Headline => "Compute a Groebner basis using the classical Buchberger with multiple threads"
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
    "minimalize",
    "reduce",
    "Minimal",
    "cleanUp"
    }
endGB = local endGB;
tasks = local tasks;
trivial = local trivial;

--***************************************************************************--
--                          Main Function      	                     	     	 --
--***************************************************************************--

-------------------------------
--      tgb
-------------------------------
--inputs:
    -- nThreads = maximum number of allowable threads,
    -- basisList = the starting basis;
--output:
    -- a Groebner basis, of type HashTable with lineage keys.
-------------------------------
tgb = method(
    TypicalValue => HashTable,
    Options => {
      Verbose => false,
      Minimal => false
      }
    )
tgb (List,ZZ) := HashTable => o -> (basisList, nThreads) -> (
    -- Error checking:
    if nThreads <= 1 then error ("expected a natural number greater than 1 for the number of threads");
    -- Handling optional arguments:
    if o.Verbose then print "You turned on Verbose! You will be notified of each new S-polynomial task created and each new GB element added to the HashTable as we go.";
    -- Creating tasks and distributing the computation:
    allowableThreads = nThreads;
    tasks = new MutableHashTable;
    endGB = new MutableHashTable;
    trivial = new MutableHashTable;
    apply(#basisList, i->endGB#(toString i) = basisList_i);
    apply(#basisList-1,i-> apply(i+1..#basisList-1, j-> (
      currentPairKey := concatenate("(",toString(i),"-",toString(j),")");
      tasks#currentPairKey = createTask taskFn (basisList_i, basisList_j, currentPairKey, Verbose=>o.Verbose);
      if o.Verbose then print concatenate("Scheduling a task for lineage ", currentPairKey);
      schedule tasks#currentPairKey
      ))
    );
    -- we do this here AND after collecting all the results from all tasks, because maybe we'll already know gb={1}:
    if # keys trivial > 0 then (
	scan(keys endGB,k-> if not member(k,keys trivial) then endGB#k=null);
	if o.Verbose then print "Found 1 or -1 in the Groebner basis; reducing now.";
	return new HashTable from endGB
	);
    -- Grabbing results from tasks and closing off:
    allReady := false;
    while not allReady do(
      allReady = true;
      tasksValues := values(tasks);
      for i to #tasksValues-1 do(
        if not isReady(tasksValues_i) then allReady = false;
    	);
      if allReady then allowableThreads=1;
      sleep 1;
    );
    -- final clean up:
    if # keys trivial > 0 then (
	scan(keys endGB,k-> if not member(k,keys trivial) then endGB#k=null);
	if o.Verbose then print "Found 1 or -1 in the Groebner basis; reducing now.";
	return new HashTable from endGB
	);
    if o.Minimal then  minimalize new HashTable from endGB  else  new HashTable from endGB
);
-- Ideal as input:
tgb (Ideal,ZZ) := MutableHashTable => o -> (I, nThreads) -> (
    tgb(flatten entries gens I,nThreads,Verbose=>o.Verbose,Minimal=>o.Minimal)
);


---------------------------------------------------------------------------------------------
--      minimalize
---------------------------------------------------------------------------------------------
-- Scans values of a HashTable H and retains only those whose leadTerms are minimal generators
-- of the ideal (leadTerm f : f in values H). If values H constitutes a Groebner basis of the ideal it generates,
-- this method returns a minimal Groebner basis.
-- Keys of non-mimial elements are retained within the hashtable (H#those=null).
---------------------------------------------------------------------------------------------
minimalize = method(TypicalValue => HashTable)
minimalize(HashTable) := HashTable => (H) -> (
    -- if null values in H then computations below break. But this can only happen if minimalize was already called, so:
    if any(values H,h-> h===null) then (
	print"null values found in hash table. If the table has been obtained from
	a threaded Groebner bases computation, then this means basis already minimal or reduced."; return H;
	);
    -- nothing to do if gens gb == {1}:
    if # keys trivial > 0 then (
	print "Groebner basis trivial, already reduced.";
	return H
	);
    -- ensure all leading coefficients are 1:
    H = applyValues(H, f-> if leadCoefficient f != 1 then lift(f/leadCoefficient f,ring f)  else f);
    -- divide the leading terms:
    G := new MutableHashTable from H;
    nonminimalElements := new MutableHashTable;
    applyPairs(H, (kf,f)-> (
	    remove(G,kf);
	    lt := leadTerm matrix {values G};
	    -- if LT(f) is divisble by any LT(g) for (g\in G\setminus f), then f is not minimal:
	    if leadTerm f%lt==0 then nonminimalElements#kf=null else G#kf=f;
	    )
	);
       merge(new HashTable from G, nonminimalElements,identity)
    )


---------------------------------------------------------------------------------------------
--      reduce
---------------------------------------------------------------------------------------------
-- Takes a HashTable of polynomials and replaces each by its remainder on division by the rest.
-- If values H constitutes a Groebner basis of the ideal it generates,
-- this method returns the reduced Groebner basis.
-- Keys of non-mimial elements are retained (H#those=null).
---------------------------------------------------------------------------------------------
reduce = method(TypicalValue => HashTable)
reduce(HashTable) := HashTable => (H) -> (
    -- the basis should be minimal first:
    H = minimalize(H);
    -- replace each g by its remainder on division by G\setminus g
    G := new MutableHashTable from H;
    -- but make sure to remove the null elements first (else division errors):
    -- (note the null elements only appear as output of minimalize or if gb == {1}.)
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
    merge(new HashTable from G, H, min)
    )


---------------------------------------------------------------------------------------------
--      cleanUp
---------------------------------------------------------------------------------------------
-- Takes a HashTable -- output of tgb --,
-- removes null values,
-- and returns a matrix of GB polynomials,
-- just like "gens gb" does.
---------------------------------------------------------------------------------------------
cleanUp = method(TypicalValue => Matrix)
cleanUp(HashTable) := Matrix => (g) -> (
    matrix {delete(null,values g)}
    )


--***************************************************************************--
--                          Internal functions 	                     	     	 --
--***************************************************************************--
---------------------------------------------------------------------------------------------
--      initializeBasis
---------------------------------------------------------------------------------------------
-- takes a list of n polynomials and stores
-- it into a MutableHashTable with keys "i" from i=0..n-1:
---------------------------------------------------------------------------------------------
initializeBasis = method(TypicalValue => HashTable)
initializeBasis(List) := MutableHashTable => (basisList)->(
    endGB = new MutableHashTable;
    apply(#basisList, i-> endGB#(toString i) = basisList_i);
    endGB
    )
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
    if manualGCD(leadTerm f1, leadTerm f2)==1 then r := 0 else r = remainderFn(spoly(f1,f2),values endGB);
    if r!=0 and r!=1 and r!=-1 then (
      scan(keys endGB,i-> (
        currentPairKeyChild := concatenate("(",currentPairKey,"-",toString(i),")");
        tasks#currentPairKeyChild = createTask taskFn (r,endGB#i,currentPairKeyChild,Verbose=>o.Verbose);
	if o.Verbose then print concatenate("Scheduling task for lineage ", currentPairKeyChild);
        schedule tasks#currentPairKeyChild
		    )
      );
	endGB#currentPairKey = r;
        --if o.Verbose in tgb was set to true, then we want to see the r added to the GB!
	if o.Verbose then print concatenate("Adding the following remainder to GB: ",toString r," from lineage ", currentPairKey);
	);
    if r==1 or r==-1 then (
	endGB#currentPairKey = r/leadCoefficient r;
	trivial#currentPairKey = r/leadCoefficient r;
	if o.Verbose then print concatenate("Adding the following remainder to GB: ",toString r," from lineage ", currentPairKey);
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
    a package for distributed computation of Groebner bases
  Description
    Text
      The complexity of Groebner computations has inspired many improvements to Buchberger's
      algorithm over the years.
      While this package does not propose an improvement to the way the algorithm operates mathematically,
      it offers a way to distribute the algorithm among threads that run in parallel.
      It is our hope that such a distributed version of the algorithm should be written in the core of the program;
      however, there are still important insights one can obtain from the current implementation.

      To us, the most interesting is the insight into {\it lineages} (see below) of non-zero remainders
      that are added to the basis during a run of Buchberger. How are these affected by the structure of the
      input system? What do they say about the complexity of the computation itself (and not only the complexity
      of the basis)? These are questions at the heart of what we are aiming to discover, and the output of the
      threaded Groebner bases method @TO tgb@ returns this information in form of a {\bf lineage table}.
    Example
      QQ[x_1,x_0,x_3,x_5,x_4,x_2,MonomialOrder=>Lex]
      rnc = minors(2, matrix{{x_0..x_4},{x_1..x_5}})
      g = tgb(rnc,4)
    Text
      The lineage table is a hash table, whose values are Groebner basis elements, and whose kesy are the {\it lineages}.

      {\bf Definition.} A Lineage of a polynomial is a string tracing
      its history in the given Groebner basis computation.

      Lineages of length 1-- in the example above $0,\dots,9$-- are the original input polynomials.
      In the example above, these are the 10 minors.
    Example
      g#"1"
      g#"2"
    Text
      If the S-polynomial of g#"i" and g#"j" produces a non-zero remainder in Buchberger's algorithm,
      that remainder is added to the hashtable g with key (i-j), as in the following example.
    Example
      g#"(1-2)"
    Text
      As the algorithm continues, keys are concatenated, so that for example the remainder of S(0,S(1,2)) will
      have lineage (0-(1-2)), and so on.   For more complicated lineage examples, see @TO tgb@.

      Naturally, one can obtain
      a minimal basis or the reduced one as follows. In the output below, elements that are reduced
      are replaced by null, but their lineage keys are retained for informative purposes.
    Example
      minimalize g
      gRed = reduce g
    Text
      To get the Groebner basis in stanard M2 matrix format, use @TO cleanUp@:
    Example
      cleanUp gRed
    Text
      {\bf Nuts and Bolts}

      The main function,  @TO tgb@, uses  @TO Task@s to distribute the reduction of S-polynomials using a
      a current version of the Groenber basis.
      It can reduce and minimalize upon request, or print out  task scheduling information
      as it creates new tasks.
      The interesting part of the output may be the actual  lineages of the basis polynomials,
      in addition to the Groebner basis itself.
      Here is a verbose example when the Groebner basis is trivial.
    Example
      QQ[a..d]
      I=ideal( -c^3+a^2+b*d, a*b*c-1,a*b*c)
      T = tgb(I,2,Verbose=>true)
    Text
      In particular, the lineages of null values tell us what S-polynomials didn't reduce to zero until $1$ was found as
      a remainder.
  SeeAlso
    tgb
///
doc ///
  Key
    tgb
    (tgb, List, ZZ)
    (tgb, Ideal, ZZ)
  Headline
    threaded Groebner bases
  Usage
    tgb(List, ZZ)
    tgb(Ideal, ZZ)
  Inputs
    L : List
      of polynomials
    I : Ideal
    n : ZZ
      number of threads to use to compute the Groebner basis
  Outputs
    : HashTable
      whose values are a Groebner basis for the ideal {\tt I} or the ideal generated by {\tt L},
      and keys are the lineages of the corresponding elements.
  Description
    Text
      Threaded Groebner basis uses  @TO Task@s to compute a Groebner basis of {\tt I} or {\tt ideal L}
      using $n$ threads.
    Example
      R = ZZ/101[x,y,z, MonomialOrder=>Lex];
      I = ideal {2*x + 10*y^2*z, 8*x^2*y + 10*x*y*z^3, 5*x*y^3*z^2 + 9*x*z^3, 9*x*y^3*z + 10*x*y^3};
      H = tgb(I,4)
    Text
      The keys of the hashtable are meaningful; for example, the polynomial with key
      {\tt ((0-2)-1)} in the hashtable {\tt tgb(L,n)} is the remainder upon division of the S-polynomial $(g, I_1)$,
      where $g$ is calculated from the S-pair $(I_0, I_2)$.
      For this reason, we say that the key communicates the "lineage" of the resulting polynomial.
      (See @TO ThreadedGB@.)

      Note that the keys in the hash table are strings, and the keys of input polynomials are 0..#L, as in the following example.
    Example
      H#"(0-1)"
    Text
      Some may be curious with respect to how tgb works.

      The starting basis $L$ (the input list {\tt L} or {\tt L=gens I})
      populate the 0..#L entries of a MutableHashTable $G$. The method creates all possible
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
      tgb({f0,f1},2,Verbose=>true)
    Text
      In the above example, the S-polynomial S(f0,f1) didn't reduce to zero hence the remainder was added to the
      output with key "(0-1)". The additional two S-polynomials reduced and the process ended.
  SeeAlso
    minimalize
    reduce
  Caveat
    Due to threads running in parallel, it can happen that there are redundant elements in the final
    Groebner basis. However these can be easily removed using @TO minimalize@, for example.
///
doc ///
  Key
    minimalize
    (minimalize, HashTable)
  Headline
    turn a Groebner basis computed using threaded Groebner bases into a minimal one
  Usage
    minimalize(HashTable)
  Inputs
    H : HashTable
      whose values are polynomials
  Outputs
    : HashTable
      where values in H whose initial terms are divisible by others are replaced by null
  Description
    Text
      Scans values of a HashTable H and retains only those whose initial terms are minimal generators
      of the ideal (leadTerm f : f in values H). If values H constitute a Groebner basis of the ideal they generate,
      this method returns a minimal Groebner basis.
    Example
      R = ZZ/101[a,b,c];
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2",2)
      minimalize T
    Text
      Polynomials are normalized so that the leading coefficient is 1.
      Note that keys of non-minimal entries are retained, and the corresponding table value is null.
///
doc ///
  Key
    reduce
    (reduce, HashTable)
  Headline
    turn a Groebner basis computed using threaded Groebner bases into a reduced one
  Usage
    reduce HashTable
  Inputs
    H : HashTable
      whose values are polynomials
  Outputs
    : HashTable
      where values in H whose initial terms are divisible by others are replaced by null and the remaining values are replaced by their remainder upon division by the rest
  Description
    Text
      Minimalizes first, then replaces each of the values of a hash table H by its remainder on the division by the remaining values H.

      If values H constitute a Groebner basis of the ideal they generate,
      this method returns a reduced Groebner basis.
    Example
      R = ZZ/101[a,b,c];
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2",2)
      reduce T
    Text
      Polynomials are normalized so that the leading coefficient is 1.
      Note that keys of non-minimal entries are retained, and the corresponding table value is null.
///
doc ///
  Key
    cleanUp
    (cleanUp, HashTable)
  Headline
    extract a matrix from values of a hash table after deleting null values
  Usage
    cleanUp HashTable
  Inputs
    H : HashTable
      whose values are polynomials
  Outputs
    : Matrix
      of non-null values of H
  Description
    Text
      This simple function just returns the Groebner basis computed with threaded GB
      in the expected Macaulay2 format, so that further computation are one step easier to set up.

      The function works on any object that is of type HashTable, because it simply removes
      null values and returns the remaining values a entries of a matrix. However, the
      usage intended is when the hash table is the output of a run of @TO tgb@.
    Example
      R = ZZ/101[a,b,c];
      T = reduce tgb( ideal "abc+c2,ab2-b3c+ac,b2",2)
      cleanUp T
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
      computation) as well as each new Groebner basis element added to the current basis.
      Lineages are reported as well.
    Example
      S = QQ[x,y,z,w];
      tgb({x*y-z^2,y*z-w^2},2,Verbose=>true);
///
doc ///
  Key
    Minimal
    [tgb, Minimal]
  Headline
    Option to specify whether the end Groebner basis should be a minimal Groebner basis
  Usage
    tgb(...,Minimal=>Boolean)
  Description
    Text
      Use {\tt Minimal=>True} as an argument in the function @TO tgb@
      for ensure the resulting Groebner basis is minimalized.
      Lineages of non-minimal Groebner basis elements that were added to the basis during the
      distributed computation are saved, with the corresponding entry in the table being null.
    Example
      S = ZZ/101[a,b,c];
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2",2, Minimal=>true)
    Text
      By default, the option is false. The basis can also be minimalized after the distributed computation
      is finished:
    Example
      T = tgb( ideal "abc+c2,ab2-b3c+ac,b2",2)
      minimalize T
///

--***************************************************************************--
--                          Tests             	                             --
--***************************************************************************--

TEST /// -- GB of {1}
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = {x, x - 1};
    H = tgb(I,4);
    G = new MutableHashTable from H;
    scanPairs(H, (kf,f)-> if f === null then remove(G,kf));
    assert (ideal values G ==  ideal 1_(frac R));
///;

TEST /// -- already minimal/reduced
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = {y - z, x - y};
    H = tgb(I,4);
    assert (H===minimalize H);
///;

TEST /// -- indivisible S-poly added to hash table correctly
    R = ZZ/101[x,y,z, MonomialOrder=>Lex];
    I = {x^3*y, 4*x^2*y*z^4 - 6*y^5*z^2 - 9*y^2, x*y^3*z^5};
    H = tgb(I,4);
    f = I_0;
    g = I_1;
    gamma = lcm(leadMonomial f, leadMonomial g);
    s = (gamma // leadTerm f) * f - (gamma // leadTerm g) * g;
    assert (s == H#"(0-1)") 
///;

TEST /// -- first element check
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = {x*y*z + z^2, x*y^2 - y^3*z + x*z, y^2};
    H = tgb(I,4);
    assert (x^2*z == H#"(0-1)")
///;

TEST /// -- consistent with gens gb 
    R = ZZ/101[x,y,z, MonomialOrder=>GLex];
    I = ideal(x - y, y - z, -x + z);
    H = tgb(I,4);
    assert (gens gb  I == cleanUp reduce H)
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
viewHelp minimalize
viewHelp Minimal
installPackage("ThreadedGB")
check ThreadedGB
