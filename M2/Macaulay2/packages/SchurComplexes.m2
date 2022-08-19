newPackage(
	"SchurComplexes",
    	Version => "1.1", 
    	Date => "June 1, 2019",
    	Authors => {
	    {Name => "Michael K. Brown", 
		  Email => "mkbrown5@wisc.edu", 
		  HomePage => "https://www.math.wisc.edu/~mkbrown5/"},
	    {Name => "Amy Huang", 
		  Email => "hhuang235@math.wisc.edu", 
		  HomePage => "https://www.math.wisc.edu/~hhuang235/"}, 
	    {Name => "Robert Laudone", 
		  Email => "robert.laudone@gmail.com", 
		  HomePage => "https://www.math.wisc.edu/~laudone/"}, 
	    {Name => "Michael Perlman", 
		  Email => "mperlman@nd.edu", 
		  HomePage => "https://www3.nd.edu/~mperlman/"}, 
	    {Name => "Claudiu Raicu", 
		  Email => "craicu@nd.edu", 
		  HomePage => "https://www3.nd.edu/~craicu/"}, 
	    {Name => "Steven V Sam",
		  Email => "ssam@sd.edu", 
		  HomePage => "http://math.ucsd.edu/~ssam/"}, 
	    {Name => "Joao Pedro Santos", 
		  Email => "jsantos3@nd.edu", 
		  HomePage => "http://math.nd.edu/people/graduate-students/graduate-directory-with-photos/"} 
	    },
    	Headline => "Schur functors of complexes",
	Keywords => {"Representation Theory", "Homological Algebra"},
	Certification => {
	     "journal name" => "Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Computing Schur complexes",
	     "acceptance date" => "21 August 2019",
	     "published article URI" => "https://msp.org/jsag/2019/9-2/p02.xhtml",
	     "published article DOI" => "10.2140/jsag.2019.9.111",
	     "published code URI" => "https://msp.org/jsag/2019/9-2/jsag-v9-n2-x02-SchurComplexes.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/SchurComplexes.m2",
	     "release at publication" => "61384b8d76f8bfef42010911dae401d24bcc6ebe",	    -- git commit number in hex
	     "version at publication" => "1.1",
	     "volume number" => "9",
	     "volume URI" => "https://msp.org/jsag/2019/9-2/"
	     }
    	)

export {"straightenTableau", "schurComplex"}


-- Input:
-- A Partition lambda and integers m and n, the admissible entries in tableaux of shape lambda.
-- Output: 
-- A List of all the standard Z/2-graded tableaux with shape lambda and possible "odd"
-- (resp. "even") entries {1, ..., m} (resp. {1, ..., n}). The tableaux
-- are encoded as HashTables. See Section 1.1 of Weyman's "Cohomology of Vector Bundles
-- and Syzygies" for background on Z/2-graded tableaux.
standardZ2Tableaux = method();
standardZ2Tableaux(Partition, ZZ, ZZ) := (lambda,m,n) ->
(
    L :=new MutableList from {};
    addBox:= method();
    --addBox recursively populates L with all standard Z/2 tableaux.
    addBox(ZZ, ZZ, HashTable) := (x,y,T) ->
    (
    	if y == #lambda and x == last lambda then L#(#L)=T
    	else 
    	(
	    i := local i;
	    j := local j;
	    if lambda#(y-1) == x then (i = 1; j = y + 1) else(i = x + 1; j = y);
    	    a := if i > 1 then T#(i - 1,j) else -infinity;
	    b := if j > 1 then T#(i,j-1) else -infinity;
	    for c from -m to -1 do
	    if c > a and c >= b then 
	    (T':= new MutableHashTable from T;
	     	T'#(i,j) = c; 
	     	addBox(i,j,new HashTable from T');
       		);
       	    for c from 1 to n do
	    if c >= a and c > b then 
	    (T':= new MutableHashTable from T;
	     	T'#(i,j) = c; 
	     	addBox(i,j,new HashTable from T');
		);
	    );
    	);
    addBox(0, 1, new HashTable from {});
    L
)


-- Input: 
-- integers m and n encoding the admissible entries in the tableau.
--
-- a List lT = {lambda, T}, where T is a HashTable representing a Z/2-graded tableau
-- (with possible entries governed by the integers m and n, as described in the description
-- of the method standardZ2Tableaux), and lambda is a Partition encoding the shape of T; 
--
-- a Hashtable D encoding the differential of a bounded complex F of finite rank free
-- modules. The keys of D are positive (resp. negative) integers representing labels of even 
-- (resp. odd) degree basis elements of the complex F, and the values of D 
-- are lists of pairs of the form {integer, ring element}, where the integer indicates a 
-- basis element of F, and the ring element is the coefficient on that basis element.
--
-- Output: A HashTable whose keys are standard Z/2-graded tableaux, represented by HashTables, and whose values are elements
-- of a ring. This is the result of applying the differential in the Schur complex S_{lambda}(F) to the tableau T. 
-- The result is written in terms of the basis of the Schur complex given by standard tableaux. The keys of the HashTable
-- are the basis elements which appear in the linear combination, and the values are the coefficients.
--
-- Note: to compute the differential in a Schur complex, we embed it in a tensor complex and use the usual 
-- formula for the differential on a tensor product of complexes. In particular, this involves embedding a divided
-- power in a tensor power, and this makes computing the value of a tableau under the differential slightly subtle. 
-- For instance, this is the reason we must keep track of the number of "odd elements" 
-- in each column of the tableau T and multiply by a certain extra "divided power coefficient" when applying 
-- the differential in the method below. Example 2.3 in the companion paper to this package illustrates this subtlety. 

tableauxDiff = method()
tableauxDiff(ZZ, ZZ, List, HashTable) := (m,n,lT,D) -> (
    lambda := lT#0;
    T := lT#1;
    repetitions := for j from 1 to lengthrow(T,1) list (
	tally for i from 1 to lengthcolumn(T,j) list T#(j,i)
	);
    T':=new MutableHashTable from T;
    resH := new MutableHashTable from {};
    sgn:=1;
    for j from 1 to lengthrow(T,1) do (
	negNumbersSeen := {};
       	for i from 1 to lengthcolumn(T,j) do (
	    x:=T#(j,i);
	    if not (x < 0 and member(x,negNumbersSeen)) then (
	      	negNumbersSeen = append(negNumbersSeen, x);
	      	if D#?x then for p in D#x do (
		-- The "if D#?x then" is required because we want to allow our complexes to end with a nonzero component 
		-- (in which case the differential will not be defined on basis elements of the lowest-degree component)
	      	    T'#(j,i)=p#0;
		    dividedCoefficient := if p#0 > 0 then 1 else (repetitions_(j-1))_(p#0)+1;
	      	    T'' := new HashTable from T';
		    --This is one tableau in the linear combination which forms the image of T under the differential.
		    --But, it isn't necessarily standard, so we need to apply the straightening algorithm to it:
	      	    str := straightenTableau(T'',lambda);
	      	    for strT in keys str do
		    if resH#?strT then (
		    -- The keys of resH will be the standard tableaux which appear in the linear combination which forms
		    -- the image of T under the differential. The value of each key is its coefficient in the linear combination.
			resH#strT = resH#strT + sgn*p#1*str#(strT)*dividedCoefficient;
			) 
		    else ( 
			resH#strT = sgn*p#1*str#(strT)*dividedCoefficient;
			);
		    );
	  	T'#(j,i)=x;	 
	  	);
	    sgn=sgn*(if x>0 then 1 else -1);
	    );
     	);
    new HashTable from resH
    )

--Input: 
--a List Lambda which encodes a partition;
--
--a bounded complex F of finite rank free modules over some commutative ring. 
--
--Output: 
--a ChainComplex, the Schur Complex of F associated to the given partition. When F = 0, the output is a 
--new ChainComplex.
schurComplex= (Lambda,F) ->
(
    lambda:=new Partition from Lambda;
    Size:=sum Lambda;
    Min:=min{min(F),0};
    F=F[2*Min];----moves F into non-negative homological degree
    R := ring(F);
    l := max(F);
    evengen := flatten for i from 0 to l//2 list (
    d := numgens F_(2*i);
    for j from 1 to d list (2*i,j)
        );
        n := #evengen; --number of even variables
    oddgen := flatten for i from 0 to (l-1)//2 list (
    d := numgens F_(2*i+1);
    for j from 1 to d list(2*i+1,j)
        );
    m := #oddgen;  --number of odd variables
    inversehash := new HashTable from --computes the map that associates a label to a generator of F
         (for i from 1 to n list evengen#(i-1) => i) | 
	 (for i from 1 to m list oddgen#(i-1) => -i);
    D := new HashTable from flatten for i from 1 to l list --encodes the differentials of F
             for j from 1 to numgens F_i list
	         inversehash#(i,j) => for r from 1 to numgens F_(i-1) list 
	              (inversehash#(i-1,r),(F.dd_i)_(r-1,j-1));
    tabs := standardZ2Tableaux(lambda,m,n); --computes tableaux indexing the generators in S_lambda(F)	      
    if #tabs > 0 then--checks if F is the zero complex.
    (
	degreeList := for T in tabs list homologicalDegree(T, evengen, oddgen);
	shift := min(degreeList);--keeps track of first nonzero homological degree of the Schur complex
    	differential := new HashTable from for T in tabs list T => tableauxDiff(m,n,{lambda,T},D);--the differential in the Schur complex
    	mutG := new MutableHashTable from {};
    	for T in tabs do
    	(
     	    deg := homologicalDegree(T,evengen,oddgen);
     	    if mutG#?deg then
     	    (
	    	l := #(mutG#deg);
	    	(mutG#deg)#l = T;
	    	)
     	    else mutG#deg = new MutableList from {T};
     	    ); 
    	tabsByDegree := for i in sort(keys mutG) list toList(mutG#i);
	--tabsByDegree is a List of Lists: the i-th entry is a List of standard tableaux of 
	--homological degree i. 
        tabsInEachDegree := for l in tabsByDegree list #l;
	--the i-th entry in the list tabsInEachDegree is the number of tableaux in the Schur complex of 
	--homological degree i.
    	componentList := for i in tabsInEachDegree list R^i;
    	r := #componentList-1;
    	matrixList := for i from 0 to r-1 list(
            transpose matrix for u in tabsByDegree#(i+1) list 
	    for v in tabsByDegree#i list try((differential#u)#v) else 0
	    );
	--The i-th entry of matrixList is the matrix giving the map between the (i+1)-st and i-th components 
	--of the Schur complex. 
	    C:= chainComplex for i from 0 to (#matrixList - 1) list map(componentList_i, componentList_(i + 1), matrixList_i);
	    C.ring = R;
	    C[-shift-2*Min*Size]---moves the Schur complex based on the shift of F at the beginning
	) 
    else new ChainComplex    
)




    


--Input:
--a HashTable T encoding a Z/2-graded tableau. We are thinking of T as an element of a Schur complex S_lambda(F)
--of some complex F with respect to some partition lambda.
--
--Lists evengen and oddgen of pairs of integers (i, j). Here, we've chosen a basis of each component F_i of our
--complex F, and the pair (i, j) corresponds to the j-th basis element of F_i.
--
--Output:
--an integer, the homological degree of T in the Schur complex S_lambda(F).
homologicalDegree = method();
homologicalDegree(HashTable,List,List) := (T,evengen,oddgen) -> sum for b in keys T list (
         x := T#b;
     	 if x>0 then (evengen#(x-1))_0 else (oddgen#(-x-1))_0
	 )

------Straightening algorithm

--Inputs: U= List, V= permutation represented as a list. Outputs: sign of the induced permutation on the unmarked elements of U.
sign = (U,V)-> (
    n:=#U;
    answer:=1;
    for i from 0 to n-2 do (
	for j from i+1 to n-1 do (
	    if U_(V_i)<0 and U_(V_j)<0 then answer=answer
	    else if U_(V_i) > U_(V_j) then answer = -answer;
	    );
	);
    answer
    )



--Inputs: T=Tableau, X=pair of subsets that we use to permute entries in col1 and col2, row1 is cutoff for computing places to permute in first column i.e. we permute down from row 1, we permute up from row2 in the second column. 
--Output: Permuted tableau.
permutedTableau= (T,X,row1, row2, col1,lengthcol1,L)-> (
    T1:=new MutableHashTable from T;
    for i from row1 to lengthcol1 do T1#(col1,i)=L_((X_0)_(i-row1));
    for i from 1 to row2 do T1#(col1+1,i)=L_((X_1)_(i-1));
    new HashTable from T1
    )
    
   
    


-- Inputs: T=Tableau, vio = pair (column, row) where violation occurs, downCol2 is the number of times the entry in the second column repeats. 
---Outputs: Linear combination represented as a hashtable with tableau keys.
shuffle = (T, vio, downCol2, lambdaprime) -> (
    lengthcol1 := lambdaprime#(vio_0-1);
    truncatedcol1:=apply(toList (vio_1..lengthcol1),i -> T#(vio_0,i));
    truncatedcol2:=apply(toList(1..vio_1+downCol2), i-> T#(vio_0 + 1,i));
    L:=join(truncatedcol1,truncatedcol2);
    indicesofL:=toList (0..(#L-1));
    subsetsofLofsizetruncatedcol1:=subsets(indicesofL,#truncatedcol1);
    pairs1:=apply(subsetsofLofsizetruncatedcol1, x-> (x, indicesofL-(set x)));
    signs:=apply(pairs1, x-> sign(L,join(x)));
    outputlist:={}; 
    dividedContributions := for i from 0 to (#pairs1-1) list (
	dividedContribution := 1; --contribution from divided power multiplication
	T':= permutedTableau(T,pairs1_i,vio_1, vio_1+downCol2, vio_0,lengthcol1,L);
	newtruncatedcol1 := for j from vio_1 to lengthcol1 list T'#(vio_0,j);
	newuntruncatedcol1 := for j from 1 to vio_1-1 list T'#(vio_0,j);
	newtruncatedcol2 := for j from 1 to vio_1+downCol2 list T'#(vio_0+1,j);
	newuntruncatedcol2 := for j from vio_1+downCol2+1 to lambdaprime#(vio_0) list T'#(vio_0+1,j);
	talnewtruncatedcol1 := tally newtruncatedcol1;
	talnewuntruncatedcol1 := tally newuntruncatedcol1;
	talnewtruncatedcol2 := tally newtruncatedcol2;
	talnewuntruncatedcol2 := tally newuntruncatedcol2;
	neglist1 := unique select(newtruncatedcol1, i -> i < 0);
	neglist2 := unique select(newtruncatedcol2, i -> i < 0);
	for n in neglist1 do (
	    dividedContribution = dividedContribution*binomial(talnewtruncatedcol1_n+talnewuntruncatedcol1_n,talnewuntruncatedcol1_n);
	    );
	for n in neglist2 do (
	    dividedContribution = dividedContribution*binomial(talnewtruncatedcol2_n+talnewuntruncatedcol2_n,talnewuntruncatedcol2_n);
	    );
	dividedContribution
	);
    for i from 1 to (#pairs1-1) do (
	T':= permutedTableau(T,pairs1_i,vio_1,vio_1+downCol2,vio_0,lengthcol1,L);
	C1:= columnStandardize(T',vio_0,lambdaprime#(vio_0-1));
	C2:= columnStandardize(C1_0,vio_0+1,lambdaprime#(vio_0));
	outputlist = append(outputlist, C2_0 => -signs_i * signs_0 * C2_1 * C1_1 * dividedContributions_i);--I removed the division because dividedContributions_0 will always be 1 now.
	);
    --the shuffling relation has multiplicities when dealing with repeated odd elements; the T=>0 takes care of the multiplicity of T when rewriting it.
    new HashTable from append(outputlist,T=>0)
    )


--Inputs: lis=List of integers
--Outputs: a new list obtained from lis by putting the elements in weakly increasing order, along with sgn, the sign of the permutation required to sort lis. The sign rule is the same as the sign rule for columns of a tableau.
colsign := lis -> 
(
    len := #lis;
    sgn := 1;
    for i from 0 to len-2 do	
    	for j from i+1 to len-1 do(
	     if lis#j < 0 and lis#i < 0 then sgn = sgn else(
	      if lis#i == lis#j and lis#j > 0 then sgn = 0 else if lis#i>lis#j then sgn = sgn * (-1));
	  );
    (sgn,sort lis)
    )


--Inputs T=tableau, col=column number, len=length of col. 
--Output: T1 with the columns of indices col and col+1 put in order together with correct sign.
columnStandardize = (T,col,len) -> (
    colelts := for j from 1 to len list T#(col,j);
    p := colsign(colelts);
    T' := new MutableHashTable from T;
    for j from 1 to len do T'#(col,j) = (p_1)#(j-1);
    (new HashTable from T',p_0)
    )




--Inputs T=tableau, i=integer. 
--Output: The length of row i of T.
lengthrow = (T,i) -> (
    answer :=0;
    while T#?(answer+1,i) do answer = answer+1;
    answer
    )

--Inputs: T=tableau, j=integer. 
--Output: The length of column j of T.
lengthcolumn = (T,j) -> (
    answer :=0;
    while T#?(j,answer+1) do answer = answer+1;
    answer
    )


    
--Input: H=linear combination of column standard Tableau represented by a hashtable. 
--Output: linear combination of standard tableaux.
recursiveStraighten = (H,lambda) -> (
    lambdaprime:=conjugate lambda;
    repeat:= false;
    hashlist:= new HashTable from {};
    for T in keys(H) do (
	if H#T!=0 then (
	    stopSearching := false;
	    isZero := false;
	    vio := null;
	    downCol2 := 0; --This keeps track of potential repeated negative entries in the second column
	    for i from 2 to lambda#0 do
	    	for j from 1 to lambdaprime#(i-1) do
		    if T#(i-1,j) == T#(i,j) and T#(i,j)<0 and not (stopSearching) then ( 
			vio = (i-1,j);
		    	stopSearching = true;
			for k from j+1 to lambdaprime#(i-1) do(
			    if T#(i,j) == T#(i,k) then(
				downCol2 = downCol2+1;)
			    )
			)
		    else if T#(i-1,j) > T#(i,j) and not (stopSearching) then ( 
			vio = (i-1,j);
			stopSearching = true;
			for k from j+1 to lambdaprime#(i-1) do(
			    if T#(i,j) == T#(i,k) then(
				downCol2 = downCol2+1;)
			    )
			);
	    	    if not isZero then (
			if vio === null then hashlist = merge(hashlist,new HashTable from {T => H#T},plus)
		    	else (
			    repeat = true;
		     	    hashlist = merge(hashlist,scalarMultiply(H#T,shuffle(T,vio,downCol2,lambdaprime)),plus);
			    );	
	    		);
		    );
    		);    
    	    if repeat then recursiveStraighten(hashlist,lambda)
            else hashlist
   	    )


--Inputs: T=tableau in the form of a HashTable, Lambda is the partition of shape T in List form.
--Output: linear combination of standard tableaux.
straightenTableau= (T,Lambda) -> 
(
    lambda:= new Partition from Lambda;
    lambdaprime := conjugate lambda;
    coe := 1;
    auxT := T;
    for i from 1 to #lambdaprime do 
    (
	CS := columnStandardize(auxT,i,lambdaprime#(i-1));
	coe = coe * CS_1;
	auxT = CS_0;
	);
    if coe == 0 then new HashTable from {} else
       recursiveStraighten(new HashTable from {auxT=>coe},lambda)
    )
     

--Inputs: s=integer, H=HashTable.
--Output: a new HashTable obtained from H by multiplying the entries by s         
scalarMultiply = (s,H) -> (
    L := keys H;
    new HashTable from apply(L, i -> (i=>H#i*s))
    )




beginDocumentation()
doc ///
  Key
   SchurComplexes
  Headline
     Schur functors of chain complexes
  Description
   Text
      This package computes the Schur complex $S_{\lambda}F_{\bullet}$ associated to a bounded chain complex $F_{\bullet}$ of finitely-generated free modules,  and a partition $\lambda$. Our conventions are the transpose of the convention in "Cohomology of Vector Bundles and Syzygies", by Jerzy Weyman, Chapter 2.4.

///

doc///
  Key
   straightenTableau
  Headline
   Straightening law for Z/2Z-graded tableau
  Usage
   S=straightenTableau(T,lambda)
  Inputs
   T: HashTable
     with keys $(i,j)$ representing the box of the Young diagram in column $i$ and row $j$. The values of {\tt T} are the entries of the boxes.
   lambda: List
  Outputs
   S: HashTable
     with keys representing standard tableaux and values representing the coefficients.
  Description
   Text
     This function takes a $\mathbb{Z}/2\mathbb{Z}$-graded Young tableau and expresses it as a linear combination of standard tableau. Positive entries in the tableaux correspond to even elements, and negative entries correspond to odd elements.
     
     The user inputs the Young tableau {\tt T} in the form of a hash table, and a partition of the same shape as {\tt T}, in the form of a list. The key $(i,j)$ in the hash table of {\tt T} corresponds to the box of {\tt T} in column $i$ and row $j$. The values are the entries of the boxes of {\tt T}. 
     
     The output is a hash table with keys representing standard tableaux and values representing the coefficients in the linear combination.
     
   Example
    T = new HashTable from {(1,1) => -3, (1,2) => -2, (1,3) => -2, (2,1) => 1, (2,2) => 2, (2,3) => 3, (3,1) => -1, (3,2) => -1};
    lambda = {3,3,2};
    straightenTableau(T,lambda)
    
   Text
     We compute a second example.
     
   Example
    T = new HashTable from {(1,1) => -1, (1,2) => -2, (1,3) => 3, (2,1) => 2, (2,2) => 1, (2,3) => -3};
    lambda = {2,2,2};
    straightenTableau(T,lambda)
    
  SeeAlso
    schurComplex
    
///

doc ///  
  Key
   schurComplex
  Headline
   Schur functors of chain complexes
  Usage
   G=schurComplex(lambda,F)
  Inputs
   lambda: List
   F: ChainComplex
  Outputs
   G: ChainComplex
  Description
   Text
     This function computes the Schur complex associated to a partition $\lambda$ and a bounded complex $F_{\bullet}$ of finitely-generated free modules over a commutative ring.
   
     The user inputs the partition $\lambda$ as a list and the chain complex $F_{\bullet}$. 
   
     In the following example, the complex {\tt F} is the free resolution of the ideal $(x,y,z)\subset \mathbb{Z}[x,y,z]$, and {\tt lambda} is the partition $(1,1)$ in the form of a {\tt List}. In this case, the Schur complex {\tt G} is the second exterior power of {\tt F}.
     
   Example
    R=ZZ[x,y,z];
    I=ideal(x,y,z);
    F=res I;
    lambda={1,1};
    G=schurComplex(lambda,F)
    G.dd
    
   Text
     As a second example, we consider the ring of polynomial functions $R=\mathbb{Q}[x_{i,j}]$ on the space of 2 x 4 generic matrices. We set the complex {\tt F} to be the map $R^4\to R^2$ given by the generic matrix $(x_{i,j})$. We compute the third symmetric power {\tt G} of {\tt F}, in which case {\tt lambda} is the partition $(3)$. By Weyman "Cohomology of Vector Bundles and Syzygies", Exercise 6.34(d), the Schur complex {\tt G} is exact except in degree zero. We verify this by computng the Hilbert series of each homology module of {\tt G}.
     
   Example
    R=QQ[x11,x21,x12,x22,x13,x23,x14,x24];
    M=genericMatrix(R,x11,2,4);
    F = new ChainComplex; F.ring = R; F#0=target M; F#1=source M; F.dd#1=M;
    lambda={3};
    G=schurComplex(lambda,F)
    G.dd
    apply((length G)+1,i->reduceHilbert hilbertSeries HH_(i)(G))
    
   Text
     We compute a third example.
     
   Example
    R=ZZ/7[x,y,z,w];
    I=ideal(x*z-y^2,x*w-y*z, y*w-z^2);
    F=res I;
    lambda={2,1};
    G=schurComplex(lambda,F)
    G.dd
    
    
        
  SeeAlso
    straightenTableau
///

TEST ///
    R=QQ[x11,x21,x12,x22,x13,x23,x14,x24]
    M=genericMatrix(R,x11,2,4)
    F = new ChainComplex; F.ring = R; F#0=target M; F#1=source M; F.dd#1=M;
    lambda={3}
    G=schurComplex(lambda,F)
    H=reduceHilbert hilbertSeries HH_(1)(G)
    H1=lift(numerator(H),ZZ)
    assert (H1 === 0)
///


TEST ///-------this Schur complex is exact by Proposition 2.4.7(a) Weyman "Cohomology of Vector Bundles and Syzygies"
    R=ZZ[x,y,z]
    M=id_(R^3)
    F= new ChainComplex; F.ring = R; F#-7=target M; F#-6=source M; F.dd#-6=M;
    lambda={3,1}
    G=schurComplex(lambda,F)
    H={}
    for i from -28 to -24 do H=H|{reduceHilbert hilbertSeries HH_i(G)}
    H1=unique H
    H2=lift(numerator(H1_0),ZZ)
    assert (#H1 === 1 and H2 === 0)
///



TEST ///
    R=ZZ[x,y]
    F=res ideal (x,y)
    lambda={1,1} 
    S=schurComplex(lambda,F)
    N2=S.dd_2
    M2=matrix{{y,x,0,x},{0,y,x,-y}}
    assert((N2-M2==0))
///

TEST ///
    T = new HashTable from {(1,1) => 2, (1,2) => 1}
    lambda = {1,1}
    S=straightenTableau(T,lambda)
    T2=new HashTable from{(1,1)=> 1, (1,2)=>2}
    output=new HashTable from{T2=> -1}
    assert (S===output)
///

TEST ///
    T = new HashTable from {(1,1) => -3, (1,2) => -2, (1,3) => -2, (2,1) => 1, (2,2) => 2, (2,3) => 3, (3,1) => -1, (3,2) => -1}
    lambda = new Partition from {3,3,2}
    S=straightenTableau(T,lambda)
    T1= new HashTable from {(1,1)=> -3, (1,2)=>-2, (1,3)=> -2, (2,1)=> -1, (2,2)=> -1, (2,3)=> 1, (3,1)=> 2, (3,2)=> 3}
    T2= new HashTable from {(1,1)=> -3, (1,2)=>-2, (1,3)=> -2, (2,1)=> -1, (2,2)=> -1, (2,3)=> 2, (3,1)=> 1, (3,2)=> 3}
    T3= new HashTable from {(1,1)=> -3, (1,2)=>-2, (1,3)=> -2, (2,1)=> -1, (2,2)=> -1, (2,3)=> 3, (3,1)=> 1, (3,2)=> 2}
    Output= new HashTable from {T1=> 1, T2=> -1, T3=> 1}
    assert(S===Output)
///





end

