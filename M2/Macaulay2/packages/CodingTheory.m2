-- -*- coding: utf-8 -*-
-*
Copyright (C) 2021  Taylor Ball, Eduardo Camps, Henry Chimal-Dzul,
Delio Jaramillo-Velez, Hiram H. Lopez, Nathan Nichols, Matthew Perkins,
Ivan Soprunov, German Vera, Gwyn Whieldon

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*-

newPackage(
	"CodingTheory",
    	Version => "1.0", 
    	Date => "May 25, 2020",
    	Authors => {
	     {Name => "Taylor Ball", Email => "trball13@gmail.com"},
	     {Name => "Eduardo Camps", Email => "camps@esfm.ipn.mx"},
	     {Name => "Henry Chimal-Dzul", Email => "hc118813@ohio.edu"},
	     {Name => "Delio Jaramillo-Velez", Email => "djaramillo@math.cinvestav.mx"},
	     {Name => "Hiram H. Lopez", Email => "h.lopezvaldez@csuohio.edu"},
	     {Name => "Nathan Nichols", Email => "nathannichols454@gmail.com"},
	     {Name => "Matthew Perkins", Email => "m.r.perkins73@vikes.csuohio.edu"},
	     {Name => "Ivan Soprunov", Email => "i.soprunov@csuohio.edu"},
	     {Name => "German Vera", Email => "gveram1100@alumno.ipn.mx"},
	     {Name => "Gwyn Whieldon", Email => "gwyn.whieldon@gmail.com"}
	     },
    	HomePage => "https://academic.csuohio.edu/h_lopez/",
    	Headline => "tools for coding theory",
	AuxiliaryFiles => false, -- set to true if package comes with auxiliary files,
	Configuration => {},
        DebuggingMode => false,
	PackageImports => {
	    "SRdeformations",
	    "Polyhedra",
	    "NAGtypes",
	    "RationalPoints", 
	    "Matroids",
	    "PrimaryDecomposition"
	    },
        PackageExports => {
	    "Graphs"
	    },
       Keywords => { "Coding Theory" },
       Certification => {
	    "journal name" => "The Journal of Software for Algebra and Geometry",
	    "journal URI" => "https://msp.org/jsag/",
	    "article title" => "Coding theory package for Macaulay2",
	    "acceptance date" => "10 August 2021",
	    "published article URI" => "https://msp.org/jsag/2021/11-1/p11.xhtml",
	    "published article DOI" => "10.2140/jsag.2021.11.113",
	    "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x11-CodingTheory.m2",
	    "release at publication" => "9224486f3fc4b8e00e883570756ab969be351009",	    -- git commit number in hex
	    "version at publication" => "1.0",
	    "volume number" => "11",
	    "volume URI" => "https://msp.org/jsag/2021/11-1/"
	    }
       )

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export {
    -- helper/conversion methods
    "generatorToParityCheck",
    "parityCheckToGenerator",
    "reducedMatrix",
    
    -- Linear Code
    -- Types and Constructors
    "LinearCode",
    "linearCode",
    "AmbientModule",
    "BaseField",
    "Generators",
    "GeneratorMatrix",
    "ParityCheck",
    "ParityCheckRows",
    "ParityCheckMatrix",
    "Code",
    "chooseStrat",    
    -- Evaluation Code
    -- Types and Constructors
    "EvaluationCode",
    "VanishingIdeal",
    "PolynomialSet",
    "ExponentsMatrix",
--    "IncidenceMatrix",
    "Sets",
    "evaluationCode",
    "toricCode",
    "evCodeGraph",
    "cartesianCode",
    "reedMullerCode",
    "orderCode",
    "reedSolomonCode",
    
    -- Families of Codes
    "zeroCode",
    "universeCode",
    "repetitionCode",
    "zeroSumCode",
    "cyclicMatrix",
    "quasiCyclicCode",
    "hammingCode",
    "cyclicCode",
    "randomCode",    
    
    -- LRC codes
    "locallyRecoverableCode",
    "getLRCencodingPolynomial",
    
    -- Methods
    "field",
    "vectorSpace",
    "ambientSpace",
    "informationRate",
    "dualCode",
    "alphabet",
    "messages",
    "codewords",
    "genericCode",
    "bitflipDecode",
    "shorten",
    "vNumber",
    "footPrint",
    "hyp",
    "genMinDisIdeal",
    "vasconcelosDegree",
    "tannerGraph",
    "randNoRepeats",
    "randLDPC",
    "syndromeDecode",
    "shortestPath",
    "minimumWeight",
    "Strat",
--    "matroidPartition",
    "weight",
    "enumerateVectors"
    }

exportMutable {}

------------------------------------------
------------------------------------------
-- Linear Code Data Types and Constructors
------------------------------------------
------------------------------------------

------------------------------------------
-- Helper functions for constructors:
------------------------------------------

findPivots = method(TypicalValue => List)
findPivots(Matrix) := List => M -> (
    -- if the reduced basis for the code does NOT
    -- have an identity matrix on the right, 
    -- find positions of each column.
    colsOfM := entries transpose M;
    -- extract (ordered) positions of standard basis vectors.
    apply(entries id_(M.target), col -> position(colsOfM, colM -> colM == col))
    )

permuteMatrixColumns = method(TypicalValue => Matrix)
permuteMatrixColumns(Matrix,List) := (M,P) -> (
    -- given a list P representing a permutation,
    -- permute the columns via P.
    transpose matrix((entries transpose M)_P)
    )

permuteMatrixRows = method(TypicalValue => Matrix)
permuteMatrixRows(Matrix,List)  := (M,P) -> (
    -- given a list P representing a permutation,
    -- permute the columns via P.
    matrix((entries M)_P)
    )

permuteToStandardForm = method()
permuteToStandardForm(Matrix) := M -> (
    -- input: matrix M.
    -- output: matrix P*M (permuted to move pivots to right identity block) and permutation P used.
    pivotPositions := findPivots(M);
    P := select(toList(0..rank M.source -1), i-> not member(i,pivotPositions)) | pivotPositions;
    {permuteMatrixColumns(M, P), P}
    )

generatorToParityCheck = method(TypicalValue => Matrix)
generatorToParityCheck(Matrix) := Matrix => M -> (    
    -- produce canonical form of the generating matrix.
    G := transpose groebnerBasis transpose M;
    
    -- save permutation of G to standard form and permutation used.
    GandP := permuteToStandardForm(G);    
    
    -- update G to use this correct version, save P to variable.
    Gred  := GandP_0;
    P := GandP_1;
    
    -- take (n-k) columns of standard generating matrix above.
    redG := Gred_{0..(rank Gred.source - rank Gred -1)};
    
    -- take the Galois Field over which G is defined.
    F := ring G.source;
    
    -- take the rank of redG.
    nk := rank redG.source;
      
    -- vertically concatenate an identity matrix of rank (n-k),
    -- then transpose.
    permuteMatrixColumns(transpose (id_(F^nk) || -redG),inversePermutation(P))
    )

parityCheckToGenerator = method(TypicalValue => Matrix)
parityCheckToGenerator(Matrix) := Matrix => M -> (
    transpose generators kernel M
    )

-- If the generator matrix or the parity check matrix is not of full rank, 
-- choose a subset of rows that are generators.
reducedMatrix = method(TypicalValue => Matrix)
reducedMatrix(Matrix) := Matrix => M -> (
    transpose groebnerBasis transpose M
    )

reduceRankDeficientMatrix = method(TypicalValue => Matrix)
reduceRankDeficientMatrix(Matrix) := Matrix => M -> (
    -- check if matrix is of full rank, otherwise return reduced.
    if (rank M == min(rank M.source,rank M.target)) then (
	M
	) else (
	reducedMatrix M
	)
    )

-- Internal function to validate user's input.
wellDefinedInput  = method(TypicalValue => List)
wellDefinedInput(List) :=  UserInput -> (
    -- UserInput = {GaloisField or Ring, lengthCode, ListGenerators}
    -- or UserInput = {GaloisField or Ring, lengthCode,ListParityCheckRows}
    
    -- Check if "baseField" is a Galois field, throw an error otherwise.
    if not isField UserInput_0 then(
	error "Codes over non-fields are not supported.";
    	);
    if UserInput_2 == {} then(
	return UserInput_2;
	);
    
    -- check that the length of all generating codewords equals the rank of AmbientModule.
    if not all(UserInput_2,codeword -> (length codeword) == UserInput_1) then (
	error "Expected codewords all to be the same length and equal to the rank of the module";
	);
    
    -- If possible, coerce generators into base field. Otherwise, throw an error.
    try(
	apply(UserInput_2, codeword -> apply(codeword, entry -> sub(entry, UserInput_0)))
	) else (
	error "Entries of codewords do not live in base field/ring.";
	)
    )

------------------------------------------
-- Linear Code Type and constructors:
------------------------------------------

-- Use this section to add basic types and
-- constructors for error correcting codes
 
LinearCode = new Type of HashTable

-- internal function to validate inputs:
rawLinearCode = method()
rawLinearCode(List) := LinearCode => (inputVec) -> (
    -- use externally facing functions to create list:	
    -- { AmbientModule, BaseField, Generators, ParityCheckRows}
   
    if inputVec_2 != {} then {
	-- save generators into new variable.
	newGens := inputVec_2;
	newGenMat := matrix(newGens);
    } else {
	-- if generators and generator matrix were undefined.
	newGens = {};
	newGenMat = matrix({newGens});
    };
    
    if inputVec_3 != {} then {
	-- save generators into new variable.
	newParRow := inputVec_3;
	newParMat := matrix(newParRow);
	
     } else {
	newParMat = generatorToParityCheck(newGenMat);
	newParRow = entries newParMat;
    };

    -- compute generating matrix from parity check matrix, if not already set.
    if newGens == {} then {
        newGenMat = parityCheckToGenerator(newParMat);
	newGens = entries newGenMat;
    };
    
    codeSpace := image transpose newGenMat;
          
    new LinearCode from {
        symbol AmbientModule => inputVec_0,
	symbol BaseField => inputVec_1,
        symbol Generators => newGens,
	symbol GeneratorMatrix => newGenMat,
	symbol ParityCheckRows  => newParRow,
	symbol ParityCheckMatrix =>  newParMat,
	symbol Code => codeSpace,
	symbol cache => new CacheTable
	}
    )

-- by default, assume that inputs are generators or generating matrices
-- set ParityCheck => true to have inputs be rows of parity check matrix.
linearCode = method(Options => {symbol ParityCheck => false})
linearCode(Module,List) := LinearCode => opts -> (M,L) -> (
    -- constructor for a linear code.
    -- input: ambient vector space/module S, list of generating codewords.
    -- outputs: code defined by submodule given by span of elements in L.
    
    -- first, check whether user's input is valid or not.
    newL := wellDefinedInput {M.ring,rank M,L};
 
    -- { AmbientModule, BaseField, Generators, GeneratorMatrix, ParityCheckRows, ParityCheckMatrix}
    if opts.ParityCheck then {
	outputVec := {M, M.ring, {}, newL};
	} else {
	outputVec =  {M, M.ring, newL , {}};
	};
    
    rawLinearCode outputVec    
    )

linearCode(GaloisField,ZZ,List) := LinearCode => opts -> (F,n,L) -> (
    -- input: field, ambient dimension, list of generating codewords.
    -- outputs: code defined by module given by span of elements in L.
    
    if n>0 then {
    	-- first, check whether user's input is valid or not.
    	newL := wellDefinedInput {F,n,L};    
        -- ambient module F^n.
    	M := F^n;
	if opts.ParityCheck then {
	    outputVec := {F^n, F, {}, newL};
	    } else {
	    outputVec =  {F^n, F, newL , {}};
	    };
	} else {
        error "The length of the code should be positive."
	};
    rawLinearCode outputVec
    )

linearCode(GaloisField,List) := LinearCode => opts -> (F,L) -> (
    -- input: field, list of generating codewords.
    -- outputs: code defined by module given by span of elements in L.
    
    -- calculate length of code via elements of L.
    n := # L_0;
    
    --check whether user's input is valid or not.
    newL := wellDefinedInput {F,n,L};
    
    if opts.ParityCheck then {
     	outputVec := {F^n, F, {}, newL};
	} else {
	outputVec =  {F^n, F, newL , {}};
	};
    
    rawLinearCode outputVec
    )

linearCode(ZZ,ZZ,ZZ,List) := LinearCode => opts -> (p,q,n,L) -> (
    -- Constructor for codes over Galois fields.
    -- input: prime p, exponent q, dimension n, list of generating codewords L.
    -- output: code defined by module given by span of elements in L.
    
    -- Galois Field.
    F := GF(p,q);
    
    if n>0 then {       
    	--check whether user's input is valid or not.
    	newL := wellDefinedInput {F,n,L};
    	if opts.ParityCheck then {
     	    outputVec := {F^n, F, {}, newL};
	    } else {
	    outputVec =  {F^n, F, newL , {}};
	    };
    	return rawLinearCode(outputVec)
    	} else {
    	error "The length of the code should be positive."
    	};
   )

linearCode(Module) := LinearCode => opts -> V -> (
    -- constructor for a linear code.
    -- input: some submodule V of S.
    -- outputs: if ParityCheck => false then code defined by submodule V.
    --	      	if ParityCheck => true then code defined as the dual 
    --                            of the code defined by V.
    
    -- produce a set of generators for the specified submodule V.
    GorP := transpose generators V;
    
    --obtaining the base ring.
    R := GorP.ring;
    
    --check whether the base ring is a GaloisField.
    if not isField R then  error "Codes over non-fields are not defined in this version yet.";
    
    if opts.ParityCheck then {
	outputVec := {GorP.source,R,{}, entries GorP};
	} else {
	outputVec = {GorP.source,R,entries GorP,{}};	
	};
    rawLinearCode outputVec
    )

linearCode(Matrix) := LinearCode => opts -> M -> (
    -- constructor for a linear code.
    -- input: a generating matrix for a code.
    -- output: if ParityCheck => true then code defined by kernel of M.
    --         if ParityCheck => false then code defined by rows of M.
    
    --check whether the base ring is a GaloisField.
    if not isField M.ring then  error "Codes over non-fields are not defined in this version yet.";

    if opts.ParityCheck then {
	outputVec := {M.source, M.ring, {}, entries M};
	} else {
	outputVec =  {M.source, M.ring, entries M, {}};
	};
    rawLinearCode outputVec
    )

--net LinearCode := c -> (
--     "Code with Generator Matrix: " | net c.GeneratorMatrix)
toString LinearCode := c -> toString c.Generators

-----------------------------------------------
-----------------------------------------------
--Minimum Weight Algorithm---------------------
-----------------------------------------------
-----------------------------------------------

--Perform BFS to find shortest path between a vertex and a set of
--vertices in a digraph.
shortestPath = method(TypicalValue => List)
shortestPath (Digraph, Thing, List) := List => (D,start,finishSet) -> (
    V    := vertexSet(D);
    assert(member(start, V));
    r    := length vertexSet(D);
    --just pick some dummy variable to initialize predecessor array
    local dummy;
    dummy = symbol dummy;
    pred := new MutableHashTable from apply(V,i-> i=>dummy);
    dist := new MutableHashTable from apply(V,i-> i=>infinity);
    visited := new MutableHashTable from apply(V,i-> i=>false);
    dist#start = 0;
    visited#start = true;
    queue := {start};
    
    while not queue == {} do (
    	v := first queue;
	queue = drop(queue,1);
	for u in elements children(D,v) do (
	    if (visited#u) == false 
	    then (
		visited#u = true;
	    	dist#u = (dist#v) + 1;
		pred#u = v;
	    	queue=append(queue,u);
	    	if member(u, finishSet) 
	    	then (
		    P := {u};
		    back := u;
		    while(not (pred#back) === dummy) do (
		    	P = prepend(pred#back,P);
		    	back = pred#back;
		    );
		return P;
		);
	    );
	);
    );
    {}
)

--input: A list of matroids with the same ground set.
--output: A partition if possible. Otherwise, the emptylist.
matroidPartition = method(TypicalValue => List)
matroidPartition List := List => mls -> (
    --check to make sure list of matroids with same ground set.
    r   := length mls;
    assert(all(0..r-1, i-> instance(mls_i,Matroid)));
    E   := (mls_0).groundSet;
    assert(all(0..r-1, i->((mls_i).groundSet)===E));
    --set up initial values: special symbols z and list of lists that'll hopefully become our partition
    local z;
    Z   := apply(new List from 1..r, i -> symbol z_i);
    Els := new MutableList from prepend(elements(E),apply(new List from 1..r, i->{}));
    
    
    --function to make relation for the digraph.
    arrow := (x,y) -> (
	if (member(y,Els#0) or member(x,Z) or x===y) then return 0;
	if member(y,Z) 
	then if (not isDependent(mls_(((baseName y)#1)-1),append(Els#((baseName y)#1),x)))
	    then return 1
	    else return 0
	else (
	    j := first select(1..r, i->member(y,Els#i));
	    if not isDependent(mls_(j-1),append(delete(y,Els#j),x)) 
	    then return 1
	    else return 0
	    )
    );
    
    --Once shortest path is found between x and z_j, update the partition.
    repaint := (P,Els) -> (
	l := (length P)-2;
	for i from 1 to l do (
	    --We are traversing the path a 2-tuple at a time starting with (P_0,P_1)
	    --We want to replace P_i from its current set of partition with P_(i-1) until we get to some element of Z
	    j1 := first select(0..r,k->member(P_(i-1),Els#k));
	    j2 := first select(0..r,k->member(P_i,Els#k));
	    Els#j1 = delete(P_(i-1),Els#j1);
	    Els#j2 = append(Els#j2,P_(i-1));
	    );
	--P_(i-1) is a z_j, so just rip off index.
	j1 := first select(0..r,k->member(P_l,Els#k));
	Els#j1 = delete(P_l,Els#j1);
	Els#((baseName P_(l+1))#1) = append(Els#((baseName P_(l+1))#1),P_l);
	);
    --unless we've exhausted elements, try to make a partition.
    while not (Els#0) == {} do (
	newVertex   := first first Els;
	constructed := mingle drop(Els,1);
	V   := join({newVertex},constructed, Z);
    	M   := matrix for x in V list for y in V list arrow(x,y);
	D   := digraph(V,M);
	if any(1..r, i->isReachable(D,Z_(i-1),newVertex)) then (
	    repaint(shortestPath(D,newVertex,Z),Els)
	    ) else (
	    --WOMP. No partition.
	    return {};
	    )
    );
    --We found a partition! Now sort it by length, largest to smallest.
    apply(rsort apply(new List from drop(Els,1),i->(#i,i)),i->i_1)
)

weight = method(TypicalValue => Number)
weight BasicList := Number => c -> (
    sum(new List from (apply(0..length c-1, i-> if c_i == 0 then 0 else 1)))
    )

subsetToList := (n, subset) -> (
    for i from 0 to (n-1) list(
	if member(i, subset) then 1 else 0
       	)
    );

-- A brute force implementation of minimum distance.
minDistBrute = method(TypicalValue => Number)
minDistBrute LinearCode := Number => C -> (
    X := messages(C);
    G := C.GeneratorMatrix;
    words := apply(select(X, i -> (weight i) > 0), x -> (matrix({x}))*G);
    words = apply(words, i -> weight first entries i);
    minWeightC := min words;
    C.cache#"minWeight" = minWeightC;
    minWeightC
    )

minDistOneInfoSet = method(TypicalValue => Number)
minDistOneInfoSet LinearCode := ZZ => C -> (
    genMat := stdForm C.GeneratorMatrix;
    k := rank genMat;
    n := numcols genMat;
    
    dlb := 1;
    dub := n - k + 1;
    w := 1;
    
    while w <= k and dlb < dub do(
	--print("-----------------------------------------------");
	--print("Current weight: "|toString(w)|" / "|toString(k));
	--print("Current lower bound:"|toString(dlb));
	--print("Current upper bound:"|toString(dub));
	
	-- Set msgsK to a list of all weight k messages.
	msgsK := apply(subsets(k,w), x -> subsetToList(k,x));
	msgsK = flatten apply(msgsK, x -> enumerateVectors(ring(C), x));
	-- Encode the messages in msgsK.
	codewordsK := apply(msgsK, u -> flatten entries ((matrix({toList u}))*genMat));
	-- Update the lower/upper bounds.
	dub = min(append(apply(codewordsK, i->weight i),dub));
	dlb = w + 1;
	w = w + 1;
	);
    C.cache#"minWeight" = dub;
    dub
    )

-- Calculate minimum distance using the matroid partition algorithm.
minDistMatroidPart = method(TypicalValue => Number)
minDistMatroidPart LinearCode := ZZ => C -> (
    M := matrix C.Generators;
    k := rank reducedMatrix(C.GeneratorMatrix);
    n := length C;
    l := ceiling(n/k);
    D := l; --D could probably be modified to be better
    w := 1;
    j := 1;
    
    --Partition columns of LinearCode into information sets.
    cMatroid := matroid(M);
    cMatroids := apply(toList(1..l),i->cMatroid);
    T := matroidPartition(cMatroids);
    
    if T == {} then(
	-- No matroid partition exists. 
	-- Not sure if there is a better option in this case.
       	return minDistOneInfoSet C;
	);
    r := {}; --list of relative ranks
    currentUnion := set();
    for i from 0 to length T-1 do (
	r = append(r,#(T_i-currentUnion));
	currentUnion = currentUnion + set(T_i);
	);
    
    dupper := n-k+1; --Start with Singleton Bound
    dlower := 0;
    while(true) do (
	--print("-----------------------------------------------");
	--print("Current weight: "|toString(w)|" / "|toString(k));
	--print("Current information set: "|toString(j)|" / "|toString(D));
        --print("Current lower bound:"|toString(dlower));
        --print("Current upper bound:"|toString(dupper));
        permutation := join(T_(j-1),toList(0..n-1)-set(T_(j-1)));
	G := reducedMatrix(M_permutation);
    	
	sameWeightWords := apply(subsets(k,w), x -> subsetToList(k,x));
	sameWeightWords = flatten apply(sameWeightWords, x -> enumerateVectors(ring(C), x));
	specialCodewords := apply(sameWeightWords, u -> flatten entries ((matrix({toList u}))*G));
    	
        dupper = min(append(apply(specialCodewords, i->weight i),dupper));
        dlower =          sum(toList apply(1..j,i->max(0,w-k+r_(i-1))));
	dlower = dlower + sum(toList apply(j+1..D,i->max(0,w-k+r_(i-1))));
	
	if dlower >= dupper then (
	    C.cache#"minWeight" = dupper;
	    --print("-----------------------------------------------");
    	    --error "stop";
	    return dupper;
    	    );
	
	if j < D then (
	    j = j + 1;
	    ) else(
	    w = w + 1;
	    );
	-- This error is a failsafe to prevent an infinite loop.   
    	if w > k then error "No minimum weight found. (This is a bug, try using a different strategy.)";
    	)
    )

-- Unlike the function reducedMatrix, this function may change the actual vector 
-- space that defines the linear code. However, the minimum distance is preserved.
stdForm = M -> (
    M = reducedMatrix M;
    -- remove the columns equal to zero
    M = submatrix'(M, toList select(0..(numcols M)-1, x -> M_x == M_x - M_x));
    first permuteToStandardForm M
    );

-- Estimate the best strategy for a given linear code.
-- The reason this is a separate function is because it is sometimes desirable to know what 
-- strategy minimumDistance chooses. For example, during debugging, development and testing.
chooseStrat = method(TypicalValue => String)
chooseStrat LinearCode := C -> (
    M := matrix C.Generators;
    k := rank reducedMatrix(C.GeneratorMatrix);

    -- The number of matrix multiplications needed to perform the brute force algorithm.
    R := ring C;
    numCodewords := (R.order)^k;
    -- The number of  (k x k) matrices it will need to compute the rank of.
    -- This computation takes place in the matroid constructor, matroid(Matrix). 
    numMatrices := binomial(numcols M, k);
    
    -- This estimation is such that the only way that it can choose to use the
    -- brute force algorithm when it should have used the matroid partition 
    -- algorithm is if the code in the Matroids package changes. (This assumes that
    -- a call to "rank" on a (k x k) matrix and a message encoding of C take about the 
    -- same amount of time. Also, it assumes that this function actually does call "matroid" 
    -- on the generator matrix of C).
    if numMatrices > numCodewords then(
	-- The "OneInfoSet" strategy is a direct improvement over "BruteForce."
	"OneInfoSet"
	)else(
	"MatroidPartition"
	)   
    );

minimumWeight = method(TypicalValue => ZZ, Options => {Strat=>""})
minimumWeight LinearCode := ZZ => opts -> C -> (
    
    if C.cache#?("minWeight") then(
	return C.cache#"minWeight";
	); 
    
    if opts.Strat == "MatroidPartition" then (
    	return minDistMatroidPart C;
	);
    if opts.Strat == "BruteForce" then(
	return minDistBrute C;
	);
    if opts.Strat == "OneInfoSet" then(
	return minDistOneInfoSet C;
	);
    if opts.Strat != "" then(
	error "Strategy '"|toString(opts.Strat)|"' not recognized.";
	);
    
    -- If no strategy specified, try to guess which one to use.
    minimumWeight(C, Strat=>(chooseStrat C) )
    )


-----------------------------------------------
-----------------------------------------------
-- Evaluation Code Data Types and Constructors
-----------------------------------------------
-----------------------------------------------

-*
    new EvaluationCode from{
	symbol Points => P, --- a set of points of F^m
	symbol VanishingIdeal => I, --the vanishing ideal of polynomials in m variables
	symbol ExponentsMatrix => LL, -- the matrix of exponents, exponent vectors are rows
	symbol IncidenceMatrix => M, -- the incidence matrix of a graph
	symbol PolynomialSet => S,  --- a set of polynomials 
	symbol LinearCode => linearCode(G), -- the linear code associated with the evaluation code
	symbol Sets => S, -- the collection of subsets used for constructing a Cartesian code
	symbol AmbientModule => F^(#P),  --- the ambient space for an evaluation code
	symbol cache => new CacheTable
	}
*-

EvaluationCode = new Type of HashTable

evaluationCode = method(Options => {})
evaluationCode(Ring,List,List) := EvaluationCode => opts -> (F,P,S) -> (
    -- constructor for the evaluation code.
    -- input: a field F, a list of points in F^m, a set of polynomials over F in m variables.
    -- outputs: The list of points, the list of polynomials, the vanishing ideal and the linear code, the linear code.
    
    m := # P#0;
    if class(ring ideal S) === PolynomialRing then R:=(ring ideal S) else (t := getSymbol "t", R=F[t_1..t_m], S=apply(S,i->promote(i,R)));
    I := intersect apply(P,i->ideal apply(numgens R,j->R_j-i#j)); -- Vanishing ideal of the set of points.
    G := transpose matrix apply(P,i->flatten entries sub(matrix(R,{S}),matrix(F,{i}))); -- Evaluate the elements in S over the elements on P.
    new EvaluationCode from{
	symbol VanishingIdeal => I, 
	symbol Points => P,
	symbol PolynomialSet => S,
	symbol LinearCode => linearCode G, -- the linear code produced by the evaluation code construction
	symbol cache => new CacheTable
	}
    )

evaluationCode(Ring,List,Matrix) := EvaluationCode => opts -> (F,P,M) -> (
    -- Constructor for a evaluation (monomial) code.
    -- inputs: a field, a list of points (as a tuples) of the same length and a matrix of exponents.
    -- outputs: a F-module.    
    -- We should check if all the points of P are in the same F-vector space.
    m := numgens image M; -- number of monomials.
    t := getSymbol "t";
    R := F[t_0..t_(m-1)];
    S := apply(entries M, i -> vectorToMonomial(vector i,R));    
    evaluationCode(F,P,S)
    )

--net EvaluationCode := c -> (
--    c.LinearCode)

dualCode = method()
dualCode(LinearCode) := LinearCode => C -> (
    -- creates dual code to code C.
    -- defn: the dual C^ is the code given by all c'.
    -- such that c'.c == 0 for all c in C.
    linearCode(dual cokernel gens C.Code)
    )

------------------------------------------
-- Evaluation Code constructors:
------------------------------------------

toricCode = method(Options => {})
toricCode(Ring,Matrix) := EvaluationCode => opts -> (F,M) -> (
    -- Constructor for a toric code.
    -- inputs: a Galois field, an integer matrix. 
    -- outputs: the evaluation code defined by evaluating all monomials corresponding to integer 
    ---         points in the convex hull (lattice polytope) of the rows of M at the points of the algebraic torus (F*)^n.
    
    z:=F_0;  --- define the primitive element of the field.
    q:=F.order; --- define the size of the field.
    s:=set apply(q-1,i->z^i); -- set of non-zero elements in the field.
    m:=numgens target transpose M; --- the length of the exponent vectors, i.e., number of variables for monomials, i.e., the dim of the ambient space containing the polytope.
    ss:=s; 
    for i from 1 to m-1 do (
    	ss=set toList ss/splice**s;  
    );
    P:=toList ss/splice;   -- the loop above creates the list of all m-tuples of non-zero elements of F, i.e.,  the list of points in the algebraic torus (F*)^m.
    Polytop:=convexHull transpose M; -- the convex hull of the rows of M.
    L:=latticePoints Polytop; -- the list of lattice points in Polytop.
    LL:=matrix apply(L, i-> first entries transpose i); --converts the list of lattice points to a matrix of exponents.
    G:=matrix apply(entries LL,i->apply(P,j->product apply(m,k->(j#k)^(i#k)))); -- the matrix of generators; rows form a generating set of codewords.
    
    t := getSymbol "t";
    
    R:=F[t_1..t_m]; --- defines the ring containing monomials corresponding to exponents.
    I := ideal apply(m,j->R_j^(q-1)-1); --  the vanishing ideal of (F*)^m.
    
    new EvaluationCode from{
	symbol Points => P, --- the points of (F*)^m.
	symbol VanishingIdeal => I, --the vanishing ideal of (F*)^m.
	symbol ExponentsMatrix => LL, -- the matrix of exponents, exponent vectors are rows.
	symbol LinearCode => linearCode(G), -- the linear code.
	symbol cache => new CacheTable
	}
) 

----------Reed–Muller-type code of degree d over a graph using our the algorithm of evaluationCode.
evCodeGraph  = method(Options => {});

evCodeGraph (Ring,Matrix,List) := evCodeGraph  => opts -> (F,M,S) -> (
    -- input: a field, Incidence matrix of the graph , a set of polynomials.
    -- outputs: a monomial code over the list of points.    
    -- We should check if all the points live in the same F-vector space.
    -- Should we check if all the monomials live in the same ring?
    
    P := entries transpose M;
    R := ring S#0;  --- MAY NOT WORK if the first element of S is a constant polynomial!
    I := intersect apply(P,i->ideal apply(numgens R-1,j->R_j-i#j)); -- Vanishing ideal of the set of points.
    S = toList apply(apply(S,i->promote(i,R/I)),j->lift(j,R))-set{0*S#0}; -- Drop the elements in S that was already in I.
    G := matrix apply(P,i->flatten entries sub(matrix(R,{S}),matrix(F,{i}))); -- Evaluate the elements in S over the elements on P.    
    
    new EvaluationCode from{
	symbol AmbientModule => F^(#P),
	symbol Points => P,
	symbol VanishingIdeal => I,
	symbol PolynomialSet => S,
	symbol LinearCode => linearCode(G),
	symbol cache => new CacheTable
	}
    )


-------Reed–Muller-type code of degree d over a graph using the function evaluate from package "NAGtypes"---------------

cartesianCode = method(Options => {})

cartesianCode(Ring,List,List) := EvaluationCode => opts -> (F,S,M) -> (
    --constructor for a cartesian code.
    --input: a field, a list of subsets of F and a list of polynomials.
    --outputs: The evaluation code using the cartesian product of the elements in S and the polynomials in M.
    
    m := #S;
    if class(ring ideal M) === PolynomialRing then R:=(ring ideal M) else (t := getSymbol "t", R=F[t_1..t_m], M=apply(M,i->promote(i,R)));
    I := ideal apply(m,i->product apply(S#i,j->R_i-j));
    P := set S#0;
    for i from 1 to m-1 do P=P**set S#i;
    if m==1 then {P = apply(toList(P/deepSplice),i->{i})} else
    {P = apply(toList(P/deepSplice),i->toList i)};
    G := transpose matrix apply(P,i->flatten entries sub(matrix(R,{M}),matrix(F,{i})));
    
    new EvaluationCode from{
	symbol Sets => S,
	symbol Points => P,
	symbol VanishingIdeal => I,
	symbol PolynomialSet => M,
	symbol LinearCode => linearCode(G),
	symbol cache => new CacheTable
	}
    )

cartesianCode(Ring,List,ZZ) := EvaluationCode => opts -> (F,S,d) -> (
    -- Constructor for cartesian codes.
    -- inputs: A field F, a set of tuples representing the subsets of F and the degree d.
    -- outputs: the cartesian code of degree d.
    m := #S;
    t := getSymbol "t";
    R := F[t_0..t_(m-1)];
    M := apply(flatten entries basis(R/monomialIdeal basis(d+1,R)),i->lift(i,R));
    cartesianCode(F,S,M)
    )
   
cartesianCode(Ring,List,Matrix) := EvaluationCode => opts -> (F,S,M) -> (
    -- constructor for a monomial cartesian code.
    -- inputs: a field, a list of sets, a matrix representing as rows the exponents of the variables.
    -- outputs: a cartesian code evaluated with monomials.
    
    -- Should we add a second version of this function with a third argument an ideal? For the case of decreasing monomial codes.
    
    m := #S;    
    t := getSymbol "t";
    R := F[t_0..t_(m-1)];
    T := apply(entries M,i->vectorToMonomial(vector i,R));
    cartesianCode(F,S,T)
    )

reedMullerCode = method(TypicalValue => EvaluationCode)
reedMullerCode(ZZ,ZZ,ZZ) := EvaluationCode => (q,m,d) -> (
    -- Constructor for a Reed-Muller code.
    -- Inputs: A prime power q (the order of the finite field), m the number of variables in the defining ring  and an integer d (the degree of the code).
    -- outputs: The cartesian code of the GRM code.
    F := GF(q);
    S := apply(q-1, i->F_0^i)|{0*F_0};
    S = apply(m, i->S);
    cartesianCode(F,S,d)
    )

reedSolomonCode = method(TypicalValue => EvaluationCode)
reedSolomonCode(Ring,List,ZZ) := EvaluationCode => (F,S,d) -> (
    -- Constructor for a Reed-Solomon code.
    -- Inputs: Field, subset of the field and an integer d (polynomials of degree less than d will be evaluated).
    cartesianCode(F,{S},d-1)
    )

orderCode = method(Options => {})
orderCode(Ring,List,List,ZZ) := EvaluationCode => opts -> (F,P,G,l) -> (
    -- Order codes are defined through a set of points and a numerical semigroup.
    -- Inputs: A field, a list of points P, the minimal generating set of the semigroup (where G_1<G_2<...) of the order function, a bound l.
    -- Outputs: the evaluation code evaluated in P by the polynomials with weight less or equal than l.    
    -- We should add a check to way if all the points are of the same length.
    m := length P#0;
    t := getSymbol "t";
    R := F[t_0..t_(m-1), Degrees=>G];
    M := matrix apply(toList sum apply(l+1, i -> set flatten entries basis(i,R)),j->first exponents j);

    evaluationCode(F,P,M)
    )

orderCode(Ideal,List,List,ZZ) := EvaluationCode => opts -> (I,P,G,l) -> (
    -- If we know the defining ideal of the finite algebra associated to the order function, we can obtain the generating matrix.
    -- Inputs: The ideal I that defines the finite algebra of the order function, the points to evaluate over, the minimal generating set of the semigroups associated to the order function and the bound.
    -- Outputs: an evaluation code.

    m := #flatten entries basis(1,I.ring);    
    t := getSymbol "t";
    R := (coefficientRing I.ring)[t_1..t_m, Degrees=>G, MonomialOrder => (reverse apply(flatten entries basis(1,I.ring),i -> Weights => first exponents i))];
    J := sub(I,matrix{gens R});
    S := R/J;
    M := matrix apply(toList sum apply(l+1,i->set flatten entries basis(i,S)),i->first exponents i);
    
    evaluationCode(coefficientRing I.ring, P, M)
    )

orderCode(Ideal,List,ZZ) := EvaluationCode => opts -> (I,G,l) -> (
    -- The same as before, but taking P as the rational points of I.
    P := rationalPoints I;
    orderCode(I,P,G,l)
    )

------------------------------------------
------------------------------------------
-- Basic Code Types
------------------------------------------
------------------------------------------

zeroCode = method()
zeroCode(GaloisField,ZZ) := LinearCode =>(F,n)->(
    -- Generates the zero code in F^n.
    -- check n is positive.
    
    if n >0 then {    
    	GenMat := matrix {apply(toList(0..n-1),i->0)};
    	GenRow := {{}};
    	ParMat := generators F^n;
    	ParRows := entries ParMat;
    	return new LinearCode from {
            symbol AmbientModule => F^n,
	    symbol BaseField => F,
            symbol Generators => GenRow,
	    symbol GeneratorMatrix => GenMat,
	    symbol ParityCheckMatrix =>  ParMat,
	    symbol ParityCheckRows  => ParRows,
	    symbol cache => new CacheTable
	    }
    } else {
    error "The length of the code should be positive."
    };
  )

universeCode = method()
universeCode(GaloisField,ZZ) := LinearCode => (F,n) -> (
    -- construct the universe code F^n.
    -- check n is positive.
    if n>0 then {
	GenMat := generators F^n;
    	GenRow := entries GenMat;
    	ParMat := matrix {apply(toList(0..n-1),i->0)};
    	ParRows := {{}};
    	return new LinearCode from {
            symbol AmbientModule => F^n,
	    symbol BaseField => F,
            symbol Generators => GenRow,
	    symbol GeneratorMatrix => GenMat,
	    symbol ParityCheckMatrix =>  ParMat,
	    symbol ParityCheckRows  => ParRows,
	    symbol cache => new CacheTable
	    }	
	} else {
	error "The length of the code should be positive."
	};    
    )

repetitionCode = method()
repetitionCode(GaloisField,ZZ) := LinearCode => (F,n) -> (
    --construct the repetition code of length n over F.
    --check n is positive.
    if n > 0 then {
	l := {apply(toList(0..n-1),i-> sub(1,F))};
	return linearCode(F,n,l)
	} else {
	error "The length of the code should be positive."
	};
)

zeroSumCode = method ()
zeroSumCode(GaloisField,ZZ):= LinearCode => (F,n) -> (
    -- construct the dual of the repetition code of length n over F.
    --check n is positive.
    if n>0 then {
	l := {apply(toList(0..n-1),i-> sub(1,F))};
	return linearCode(F,n,l,ParityCheck => true)
	} else {
	error "The length of the code should be positive."
	}
  )

------------------------------------------
------------------------------------------
-- Binary Operations
------------------------------------------
------------------------------------------

-- mathematical equality of linear codes
LinearCode == LinearCode := (C,D) -> ( 
    MC := matrix apply(C.Generators, a -> vector a );
    MD := matrix apply(D.Generators, a -> vector a );
    image MC == image MD
    )


------------------------------------------
------------------------------------------
-- Families of Codes
------------------------------------------
------------------------------------------

-- Use this section to add methods that 
-- construct families of codes

------------------------------------------------------
-- Added helper functions to produce cyclic matrices:
------------------------------------------------------
cyclicMatrix = method(TypicalValue => Matrix)
cyclicMatrix(List) := Matrix => v -> (
    -- constructs the cyclic matrix with first
    -- row given by v.
    
    -- calculate number of rows/columns.
    ndim := # v;
    
    -- produce cyclic matrix of right-shifts with
    -- first row given by v.
    matrix(apply(toList(0..ndim-1), i -> apply(toList(0..ndim-1),j -> v_((j-i)%ndim))))
    
    )

cyclicMatrix(GaloisField,List) := Matrix => (F,v) -> (
    -- constructs the cyclic matrix with first
    -- row given by v, coercing elements into F.
    
    try {
	-- attempt to coerce all entries into
	-- same field, if necessary.
	newV := apply(v, entry -> sub(entry,F));
	} else {
	-- otherwise, throw error.
	error "Elements of input cannot be coerced into same field.";
	}; 
    
    cyclicMatrix newV
    )

quasiCyclicCode = method(TypicalValue => LinearCode)
quasiCyclicCode(GaloisField,List) := LinearCode => (F,V) -> (
    -- produce cyclic matrices with each v in V as first row.
    cyclicMatrixList := apply(V, v-> cyclicMatrix(F,v)); 
    
    -- vertically concatenate all of the codewords in blocks
    -- of our quasi-cyclic code.
    linearCode(fold((m1,m2) -> m1 || m2, cyclicMatrixList))
    )

quasiCyclicCode(List) := LinearCode => V -> (
    -- constructs a cyclic code from a 
    -- list of lists of  elements in some field F.
    
    -- check field that elements live over.
    baseField := class V_0_0;
    
    try quasiCyclicCode(baseField,V) else error "Entries not over a field."
    )

-*
F = GF(5)
L = apply(toList(1..2),j-> apply(toList(1..4),i-> random(F)))
C=quasiCyclicCode(L)
*-
hammingCode = method(TypicalValue => LinearCode)
hammingCode(ZZ,ZZ) := LinearCode => (q,r) -> (
        
    -- produce Hamming code
    -- q is the size of the field.
    -- r is the dimension of the dual.
    K := GF(q);
    -- setK is the set that contains all the elements of the field.
    setK := set(  {0}| apply(toList(1..q-1),i -> K_1^i));
    -- C is the transpose of the parity check matrix of the code. Its rows are the points of the
    -- projective space P(r-1,q).
    j := 1;
    C := matrix(apply(toList(1..q^(r-j)), i -> apply(toList(1..1),j -> 1))) | matrix apply(toList(toList setK^**(r-j)/deepSplice),i->toList i);
    for j from 2 to r do (
	C = C || (matrix(apply(toList(1..q^(r-j)), i -> apply(toList(1..(j-1)),j -> 0)))) 
	| (matrix(apply(toList(1..q^(r-j)), i -> apply(toList(1..1),j -> 1))))
	| (matrix apply(toList(toList setK^**(r-j)/deepSplice),i->toList i));
	);
	
    -- The Hamming code is defined by its parity check matrix.
    linearCode(transpose C, ParityCheck => true)
    );

-*
Example:
hammingCode(2,3)
ParityCheckMatrix => | 1 1 1 1 0 0 0 |
                     | 0 1 0 1 1 1 0 |
                     | 0 1 1 0 0 1 1 |
*-


cyclicCode = method (TypicalValue => LinearCode) 
cyclicCode(GaloisField ,RingElement, ZZ) := LinearCode => (F,G,n) -> (
    --Constructor for Cyclic Codes generated by a polynomial.
    -- input: The generating polynomial and the length of the code.
    --outputs: a cyclic code defined by the initial polynomial.
    
    -- We should make a list of the coefficients of the polynomial. 
    ring G;
    x:=(gens ring G)#0;
    f:=x^n-1;
    t:=quotientRemainder(G,f);
    g:=t#1;  
    if (quotientRemainder(f,g))#1==0 then (
	r:=toList apply(0.. (n-1),i->first flatten entries sub(matrix{{g//x^i}}, x=>0 ));
	-- Generate the generating matrix using the function cyclicMatrix.
	R:=toList apply(toList(0..n-1-(degree g)#0), i -> apply(toList(0..n-1),j -> r_((j-i)%n)));
	linearCode(coefficientRing (ring G),R)
	) else (
	l := toList apply(0.. (n-1),i->first flatten entries sub(matrix{{g//x^i}}, x=>0 ));
	-- Generate the generating matrix using the function cyclicMatrix.
	L := toList apply(toList(0..n-1), i -> apply(toList(0..n-1),j -> l_((j-i)%n)));
	linearCode(coefficientRing (ring G),L)
	)
    )

cyclicCode(GaloisField, ZZ, ZZ) := LinearCode => (F,G,n) -> (
    a := promote(G,F);
    if a==0 then (
	zeroCode(F,n)
	)else(
	universeCode(F,n)
	)
    )

-*
EXAMPLE:
GF(7)[x]
cyclicCode(GF(7),1,5)
cyclicCode(GF(7),(x+3)*(x-1)*(x^3-2),9)
cyclicCode(GF(7),5,4)
*- 

------------------------ -------------
--     Helper functions for constructing 
--             LRC CODES
-------------------------------
locallyRecoverableCode = method(TypicalValue => LinearCode)
locallyRecoverableCode(List,List,RingElement) := LinearCode => (L,A,g) -> (
    -- generate a linear Locally Recoverable Code.
    -- input:   L={q,n,k,r}  alphabet size q, target code length n, dimension k, and locality r.
    --          A is a partition of n symbols from the alphabet,
    --          g is a polynomial that is constant on each subset of A (a "good" polynomial).
    
    -- output:  a linear code for which given a symbol c_i in a codeword, there exists
    --           "r" other symbols in the codeword c_j such that f(c_i)=f(c_j).
    -- R:  is the polynomial ring generated by g.
    -- informationSpaceGenerators:  is a list of generators for the information space (ZZ/q)^k where k is the target dimension.
    -- encodingPolynomials:  is a list of the encoding polynomials, where each polynomial corresponds to a generator of (ZZ/q)^k.
    -- codeGenerators:  contains the set of generators for the code, which are obtained by evaluation each element of the subsets of A at the encoding polynomials.
    q := L#0;
    n := L#1;
    k := L#2;
    r := L#3;
    -- note: check that n less than or equal to q and if the symbols of A lie in F.
    if not n<=q then (
	error "Warning: construction requires that target length <= field size.";
	);
        
    --verify that target dimension is divisible by locality.
    if not k%r==0 then(
	error "target dimension is not divisible by target locality";
    	);

    R := ring g;
    informationSpaceGenerators := entries gens (ZZ/q)^k; 
    encodingPolynomials := apply(informationSpaceGenerators,i-> (getLRCencodingPolynomial(k, r, i, g)));
    codeGenerators := apply(encodingPolynomials, polyn -> (apply( (flatten A), sym -> ( polyn[sym]%(q) ) ) ) );
    linearCode(GF(q),codeGenerators) 
    )

---------------------------------------------
--   ENCODING POLYNOMIAL FOR LRC CODES    --
---------------------------------------------
getLRCencodingPolynomial = method(TypicalValue => RingElement)
getLRCencodingPolynomial(ZZ,ZZ,List,RingElement) := RingElement => (k,r,informationList,g) -> (
    --      generates the encoding polynomial for an LRC code.
    -- input:    p  is a HashTable of the target parameters,
    --    	   informationList  is a list of generators for the information space (ZZ/q)^k,
    --           g  is a good polynomial for some partition of symbols in (ZZ/q).
    -- output:   the encoding polynomial for an information vector in F^k.
    
    -- R:  is the polynomial ring generated by g.
    -- x:  is the variable(s) in the ring R.
    -- i:  is a set of limits for the summation in the formula for an encoding polynomial.
    R := ring g;
    x := (gens R)#0;
    g1 := sub(g,R);
    i := toList(0..(r-1));
    -- f:  generates the coefficient polynomial for an LRC code.
    -- input:    p  is a HashTable of the target parameters,
    --    	   informationList  is a list of generators for the information space (ZZ/q)^k,
    --           g  is a good polynomial for some partition of symbols in (ZZ/q)
    --           i is the row index of the matrix a_ij  in the formula for a coefficient polynomial.
    -- output:   the coefficient polynomial for an information vector in F^k.
    -- j:  is the column index of the matrix a_ij  in the formula for a coefficient polynomial.
    f:=(k, r, informationList, g, i) -> (
	j := toList(0..(k//r-1));
	sum apply(j,inc -> ( (informationList_{i*2+inc}_0) * (g^inc) ))
	);
    sum apply(i,inc -> ( (f(k, r, informationList, g1, inc))*((x^inc) ) )) 
    )

-*  example
 needsPackage("CodingTheory")
 p=targetParameters(13,9,4,2)
 A={{1,3,9},{2,6,5},{4,12,10}}
 R=p.BaseField[x]
 g=x^3
 locallyRecoverableCode(p,A,g)
 *-


-------------------------   END   MATT --------------------------------------------

------------------------------------------
------------------------------------------
-- Linear Code Methods
------------------------------------------
------------------------------------------

-- Use this section to add methods that
-- act on codes. Should use this section for
-- writing methods to convert between 
-- different types of codes

-- Overloading the ring function to return the base field of a LinearCode.
-- This will work even when AmbientModule and BaseField are not properly defined.
ring LinearCode := Ring => C -> (
    ring(C.GeneratorMatrix)
    )

--input: A linear code C.
--output: The field C is a code over.
--description: Given a linear code, the function returns the field C is a code over:
field = method(TypicalValue => Ring)
field LinearCode := Ring => C -> (
    C.BaseField
    )

--input: A linear code C.
--output: The vector space spanned by the generators of C.
vectorSpace = method(TypicalValue => Module)
vectorSpace LinearCode := Module => C -> (
    C.Code
    )

--input: A linear code C.
--output: The ambient vector space the code is a subspace of:
ambientSpace = method(TypicalValue => Module)
ambientSpace LinearCode := Module => C -> (
    C.AmbientModule
    )

--input: A linear code C.
--output: The vector space dimension of the ambient vector space 
--C is a subspace of:
length LinearCode := ZZ  => C -> (
    rank(C.AmbientModule)
    )

--input: A linear code C.
--output: The vector space dimension of the subspace given by the
--span of the generators of C:
dim LinearCode := Number => C -> (
    rank (C.Code)
    )

--input: A linear code C.
--output: The ratio (dim C)/(length C).
informationRate = method(TypicalValue => QQ)
informationRate LinearCode := QQ => C -> (
    (dim C)/(length C)
    )
--input: A linear code C.
--output: the number of codewords in C.
size LinearCode := ZZ => C -> (
    (C.BaseField.order)^(dim C)
    )

alphabet = method(TypicalValue => List)
alphabet(LinearCode) := List => C -> (
    -- "a" is the multiplicative generator of the
    -- field that code C is over:
    
    -- check if "base ring" is ZZ/q.
    if C.BaseField.baseRings === {ZZ} then {
	a := sub(1,C.BaseField);
	-- generate elements additively.
	alphaB := apply(toList(1..(C.BaseField.order)), i-> i*a)
	} else {
	a = C.BaseField.generators_0;
 	-- take 0, and compute non-zero elements of C.BaseField.
	alphaB = {sub(0,C.BaseField)} | apply(toList(1..(C.BaseField.order-1)), i-> a^i);
	};
    
    alphaB
    )

genericCode = method(TypicalValue => LinearCode)
genericCode(LinearCode) := LinearCode => C -> (
    linearCode(C.AmbientModule)
    )

-- method to generate all message words in code.
messages = method(TypicalValue => List)
messages(LinearCode) := List => C -> (
    k := dim C ;
    A := alphabet C;
    messageSpace := apply(toList((set A)^**k) / deepSplice, c -> toList(c));
    messageSpace
    )

-- method to compute the set of q^k codewords in an [n,k]-code.
codewords = method(TypicalValue => List)
codewords(LinearCode) := List => C -> (
    -- save generator matrix as G.
    G := reducedMatrix(C.GeneratorMatrix);
    
    -- convert message vectors as lists into matrices.
    M := apply(messages C, m-> matrix({m}));
    
    -- map m -> mG to compute codewords.
    flatten apply(M, m -> entries (m*G))
    )

-- input: An [n,k] linear code C and a set S of distinct integers { i1, ..., ir} such that 1 <= ik <= n.
-- output: A new code from C by selecting only those codewords of C having a zeros in each of the coordinate 
--     positions i1, ..., ir, and deleting these components. Thus, the resulting 
--     code will have length n - r. 
shorten = method(TypicalValue => LinearCode)
shorten ( LinearCode, List ) := LinearCode => ( C, L ) -> (
    local newL; local codeGens; local F;
    C = linearCode(matrix (codewords C));
        
    F = C.BaseField;
    codeGens = C.Generators;
    
    newL = delete(0, apply( codeGens, c -> (
	if sum apply( L, l -> if c#l == 0_F then 0_ZZ else 1_ZZ ) == 0_ZZ
	then c
	else 0
	)));

    if newL == {} then(
	C 
	) else (
	newL = entries submatrix'(matrix newL, L);
	linearCode(C.BaseField, newL)
	)
    )

-*
shorten ( LinearCode, List ) := LinearCode => ( C, L ) -> (
    local newL; local codeGens;
    
    codeGens = C.Generators;
    newL = delete(0, apply( codeGens, c -> (
	if sum apply( L, l -> c#l ) == 0
	then c
	else 0
	)));
    
    if newL == {} then return C else (
	newL = entries submatrix' ( matrix newL, L );
	return linearCode ( C.BaseField , newL );
	)
    )
*-

-- input: An [n,k] linear code C and an integer i such that 1 <= i <= n.
-- output: A new code from C by selecting only those codewords of C having a zero as their 
--     i-th component and deleting the i-th component from these codewords. Thus, the resulting 
--     code will have length n - 1. 
shorten ( LinearCode, ZZ ) := LinearCode => ( C, i ) -> (
    shorten(C, {i})
    )

-- input: A module as the base field/ring, an integer n as the code length, and an integer
--    k as the code dimension.
-- output: a random codeword with AmbientModule M^n of dimension k.

--random (Module, ZZ, ZZ) := LinearCode => (M, n, k) -> (
--    linearCode( M, apply(toList(1..n),j-> apply(toList(1..k),i-> random(M))) )
--    )


randomCode = method(TypicalValue => LinearCode);
randomCode (GaloisField,ZZ,ZZ) := (LinearCode) => (F, n, k) -> (
    linearCode(F, n, apply(toList(1..k), j-> apply(toList(1..n),i-> random(F))))
    )
randomCode (QuotientRing,ZZ,ZZ) := (LinearCode) => (R, n, k) -> (
    linearCode(matrix apply(toList(1..k), j-> apply(toList(1..n),i-> random(R))))
    )

    
-----------------------Generalized functions in coding theory---------------------
--------------------------------------------------------------

--================= v-number function ========================
vNumber = method(TypicalValue => ZZ);
vNumber (Ideal) := (I) -> (
    L := ass I;
    G := apply(0..#L-1,i->flatten flatten degrees mingens(quotient(I,L#i)/I)); 
    N := apply(G,i->toList(set i-set{0}));
    min flatten N 
    )

-----------------------------------------------------------
--****************** Footprint Function ********************
footPrint = method(TypicalValue => ZZ);
footPrint (ZZ,ZZ,Ideal) := (d,r,I) ->(
    var1 := subsets(flatten entries basis(d,coker gens gb I),r); 
    var2 := apply(var1,toSequence);
    var3 := apply(var2,ideal);
    var4 := apply(var3,x->if not quotient(ideal(leadTerm gens gb I),x)==ideal(leadTerm gens gb I) then 
    	degree coker gens gb ideal(ideal(leadTerm gens gb I),x)
    	else 0 );
    degree coker gens gb I - max var4
    )

-----------------------------------------------------------
--****************** GMD Functions ********************
 
--------------------------------------------------------
--=====================hyp function======================
hyp = method(TypicalValue => ZZ);
hyp (ZZ,ZZ,Ideal) := (d,r,I) ->(
    var1 := apply(toList (set(0..char ring I-1))^**(hilbertFunction(d,coker gens gb I))
     	-(set{0})^**(hilbertFunction(d,coker gens gb I)),toList);
    var2 := apply(var1,x -> basis(d,coker gens gb I)*vector deepSplice x);
    var3 := apply(var2,z->ideal(flatten entries z));
    var4 := subsets(var3,r);
    var5 := apply(var4,ideal);
    var6 := apply(var5,x -> if #set flatten entries mingens ideal(leadTerm gens x)==r and not quotient(I,x)==I
    	then degree(I+x)
    	else 0);
    max var6
    ) 


------------------------GMD Function--------------------------------

genMinDisIdeal = method(TypicalValue => ZZ);
genMinDisIdeal (ZZ,ZZ,Ideal) := (d,r,I) ->(
    degree(coker gens gb I)-hyp(d,r,I)
    )

--------------------------------------------------------------
--===================== Vasconcelos Function ================

vasconcelosDegree = method(TypicalValue => ZZ);
vasconcelosDegree (ZZ,ZZ,Ideal) := (d,r,I) ->(
    var1:=apply(toList (set(0..char ring I-1))^**(hilbertFunction(d,coker gens gb I))
	-(set{0})^**(hilbertFunction(d,coker gens gb I)),toList);
    var2:=apply(var1,x -> basis(d,coker gens gb I)*vector deepSplice x); 
    var3:=apply(var2,z->ideal(flatten entries z));
    var4:=subsets(var3,r);
    var5:=apply(var4,ideal);
    var6:=apply(var5, x -> if #set flatten entries mingens ideal(leadTerm gens x)==r and not quotient(I,x)==I
	then degree(coker gens gb quotient(I,x))
	else degree(coker gens gb I)
       	);
    min var6
    )



----------------------------------------------------------------------------------

-*

Bitflip decode the codeword v relative to the parity check matrix H.

Example:
R=GF(2);
H := matrix(R, {
	{1,1,0,0,0,0,0},
	{0,1,1,0,0,0,0},
	{0,1,1,1,1,0,0},
	{0,0,0,1,1,0,0},
	{0,0,0,0,1,1,0},
	{0,0,0,0,1,0,1}});
v := vector transpose matrix(R, {{0,1,0,0,1,0,0}});
print(bitflipDecode(H,v,100));

*-
bitflipDecode = method(TypicalValue => List)
bitflipDecode(Matrix, Vector, ZZ) := (H, v, maxI) -> (
    w := v;
    if(H*w == 0_(target H)) then(
	return entries w;
	);
    
    for iteration from 0 to maxI-1 do(
    	n := rank target H;
    	fails := positions(entries (H*w), i -> i==1);
    	failsRows := select(pairs entries H, i -> member(first i, set(fails)));
    	-- matrix representing only the homogeneous eqns that fail.
    	failSubgraph := lift(matrix toList(apply(failsRows, i -> last i)),ZZ);
    	oneVec := vector apply(entries (0_(target failSubgraph)), i -> 1);
    	-- number of times each variable appears in a failing equation.
    	numFails := entries (transpose(failSubgraph)*oneVec);
    	toFlip := positions(numFails, n -> n == (max numFails));
    	flipVec := sum apply(toFlip, i -> vector ((entries basis source H)#i));
    	w = flipVec+w;
		
	if(H*w == 0_(target H)) then(
	    return entries w;
	    );
    	);
    {}
    );
    
tannerGraph = method(TypicalValue => Graphs$Graph)
tannerGraph(Matrix) := H -> (
    R := ring(H);
    cSym := getSymbol "c";
    rSym := getSymbol "r";
    symsA := toList (cSym_0..cSym_((numgens source H)-1)); 
    symsB := toList (rSym_0..rSym_((numgens target H)-1));
    
    -- The vertex sets of the bipartite graph.
    tannerEdges := for i from 0 to (numgens source H)-1 list(
    	for j from 0 to (numgens target H)-1 list(
    	if H_(j,i) != 0 then(
	    {symsA#i, symsB#j}
	    )else(
	    continue;
	    )
	)
    );
    Graphs$graph(symsA|symsB, flatten tannerEdges)    
    );

randNoRepeats = method(TypicalValue => List)
randNoRepeats (ZZ, ZZ) := (a, k) -> (
    
    if a < 0 or k < 1 then (
    	error "Invalid arguments for randNoRepeats.";
    	);
    
    -- we want it to work in cases like a=0, k=1.
    if k > a+1 then(
    	error "Argument k to randNoRepeats is too large.";
	);
    
    n := a;
    population := toList(0..n);
    result := new MutableList from (toList (0..(k-1)));
    pool := new MutableList from population;
    
    for i from 0 to k-1 do(
	j := random(0, n-i);
	result#i = pool#j;
	-- Move the non-selected item to a place where it can be selected. 
	pool#j = pool#(n-i);
	); 
    toList result
    );

randLDPC = method(TypicalValue => Matrix)
randLDPC(ZZ, ZZ, RR, ZZ) := (n, k, m, b) -> (
    if(n <= k) then(
	error "n must be less than k.";
	);
    
    popcount := floor(n*m + b);
    
    if popcount > n*(n-k) then(
	popcount = n*(n-k);
	);
    
    R := GF(2);
    
    H := new MutableList from for i from 1 to n*(n-k) list(0_R);
    ones := randNoRepeats( ((n-k)*n)-1, popcount);
    for i from 0 to (length ones)-1 do(
	H#(ones#i) = 1_R;
	);
    matrix(R, pack(toList H, n))
    );

-- Given a 0,1 valued list errorBinary, return a list of all the possible ways to replace the
-- one values in errorBinary with a nonzero element of the finite field R. 
enumerateVectors = method(TypicalValue => List)
enumerateVectors(Ring, List) := (R, errorBinary) -> (
    elts := for i from 1 to (R.order)-1 list( (first gens R)^i);
    ones := positions(errorBinary, x -> x == 1);
    prim := first gens R;
    
    if length ones == 0 then return {errorBinary};
    
    -- I would use fold here, but I can't figure out how to pass fold a function I don't
    -- know how to write in prefix notation (instead of infix notation).
    -- (I.e., how do you use fold when you know the operator but not the identifier?)
    ugly := set(elts);
    for i from 1 to (length ones)-1 do(ugly = ugly ** set(elts));    
    for i from 1 to (length ones)-1 do(ugly = ugly/splice);
    ugly = apply(toList ugly, x -> toList x);
    
    -- ugly now contains lists of symbols we need to substitute in errorBinary.
    current := new MutableList from errorBinary;
    for i from 0 to (length ugly)-1 list(
    	possibility := ugly#i;
	
	for j from 0 to (length ones)-1 do(
	    current#(ones#j) = possibility#j;
	    );	
	apply(toList current, x -> promote(x, R))
    	)
    );

syndromeDecode = method(TypicalValue => List)
syndromeDecode(LinearCode, Matrix, ZZ) := (C, v, minDist) -> (
    R := ring(v);
    if(minDist <= 0) then error "cannot have minimum distance less than 0.";
        
    H := C.ParityCheckMatrix;
    syndrome := H*v;
    
    if (C.cache#?("syndromeLUT")) then(
	syndromeLUT := C.cache#"syndromeLUT";
	return v + (syndromeLUT#(syndrome));
	);
    
    -- The idea is to associate all possible error vectors with their corresponding coset.
    numErrors := floor((minDist-1)/2);
    ground := toList(0..((length C)-1));
        
    lookupTable := flatten for i from 0 to numErrors list(subsets(ground, i));    
    
    lookupTable = apply(lookupTable, x -> 
      	for i from 0 to (length C)-1 list(
   	    if member(i, x) then 1 else 0
	    )
	);
    lookupTable = flatten apply(lookupTable, x -> enumerateVectors(R, x));
    lookupTable = apply(lookupTable, x -> transpose matrix(R, {x}));
    lookupTable = apply(lookupTable, x -> {H*x,x});
    lookupHash := new HashTable from lookupTable;
    
    C.cache#"syndromeLUT" = lookupHash;
    coset := lookupHash#(syndrome);
    v + coset
    );


------------------------------------------
------------------------------------------
-- Tests
------------------------------------------
------------------------------------------

-----------------------------------------------
-----------------------------------------------
-- Use this section for LinearCode tests:
-----------------------------------------------
-----------------------------------------------

TEST ///
-- minimumWeight test

-- This example is not over GF(2) and takes the matroid partition algorithm path. 
M := {{1,1,1,1,1,1},{1,0,1,0,1,0},{0,0,0,1,0,0}};
C := linearCode(GF(5),M);
assert(minimumWeight(C) == 1);

-- The binary golay code (has a minimum weight of 8).
-- This example takes the brute force path.
G:={{1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1},
    {0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,1,0,0,1,0},
    {0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,0,1,1},
    {0,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,1,0},
    {0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,1,0,0,1},
    {0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,1,0,1},
    {0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,1,1},
    {0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0,0,0},
    {0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0,0},
    {0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0},
    {0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1},
    {0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,1}};
C = linearCode(matrix(GF(2),G));
assert(minimumWeight(C) == 8);
///

TEST ///
-- shortestPath.
D = digraph ({{1,2},{2,3},{3,4},{1,4},{3,5}}, EntryMode => "edges");
assert(length shortestPath (D,1,{3,5}) ==3)
///

TEST ///
-- syndromeDecode test.
R := GF(2);
-- The binary Golay code. It can correct 3 errors.
G:={{1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1},
    {0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,1,0,0,1,0},
    {0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,0,1,1},
    {0,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,1,0},
    {0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,1,0,0,1},
    {0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,1,0,1},
    {0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,1,1},
    {0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0,0,0},
    {0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0,0},
    {0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0},
    {0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1},
    {0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,1}};
G = matrix(R,G);
C := linearCode G;
for i from 1 to 1 do(
    message := transpose matrix {(for n from 1 to numgens target G list(random(R)))};
    codeword := (transpose G)*message;
    errors := sum take(random entries basis target codeword, 3);
    errors = transpose matrix({errors});
    received := codeword+errors;
    decoded := syndromeDecode(C, received, 8);
    assert(decoded == codeword);
    );
///

TEST ///
-- linearCode(Module,List)
R := GF 4;
M := R^4;
L := {{1,0,1,0},{1,0,1,0}};
C := linearCode(M,L);
assert(C.AmbientModule == M)
m := matrix apply(L,generator->apply(generator,entry->sub(entry,R)));
assert(C.GeneratorMatrix == m)
H := C.ParityCheckMatrix;
z := matrix apply(toList(1..rank H),i -> apply(toList(1..#L), j->sub(0,R)));
assert(H*(transpose C.GeneratorMatrix)==z)
///

TEST ///
-- linearCode(GaloisField,ZZ,List)
R := GF 2;
n := 4;
L := {{1,0,1,0},{0,1,0,1}};
C := linearCode(R,n,L);
assert(C.AmbientModule == R^n)
newL := apply(L,generator->apply(generator,entry->sub(entry,R)));
assert(C.Generators == newL)
G := matrix newL;
assert(C.GeneratorMatrix == G)
H := C.ParityCheckMatrix;
z := matrix apply(toList(1..rank H),i -> apply(toList(1..#L), j->sub(0,R)));
assert(H*(transpose C.GeneratorMatrix) == z)
///

TEST ///
-- linearcode(GaloisField,List)
R := GF(8,Variable =>a);
n := 4;
L := {{1,0,a,0},{0,a,0,a+1}};
C := linearCode(R,n,L);
assert(C.AmbientModule == R^n)
newL := apply(L,generator->apply(generator,entry->sub(entry,R)));
assert(C.Generators == newL)
G := matrix newL;
assert(C.GeneratorMatrix == G)
H := C.ParityCheckMatrix;
z := matrix apply(toList(1..rank H),i -> apply(toList(1..#L), j->sub(0,R)));
assert(H*(transpose C.GeneratorMatrix) == z)
///

TEST ///
-- linearCode(ZZ,ZZ,ZZ,List)
p := 2;
n := 3;
l := 4;
R := GF(p,n);
L := {{1,1,0,0},{0,0,1,1}};
C := linearCode(p,n,l,L);
assert(C.Generators == L)
assert(C.GeneratorMatrix == C.ParityCheckMatrix)
///

TEST ///
-- linearCode(Module)
R = GF 2;
M = transpose matrix {apply({1,1,1,1},entry -> sub(entry,R))};
V = image M;
C = linearCode(V);
assert(C.AmbientModule == R^4)
assert(C.GeneratorMatrix ==  transpose M)
H = C.ParityCheckMatrix;
z = transpose matrix {apply({0,0,0},entry ->sub(entry,R))};
assert(H*(transpose C.GeneratorMatrix) == z)
///

TEST ///
-- linearCode(Matrix)
R = GF 4;
L = apply({{1,0,1,0},{0,1,1,1}},codeword ->apply(codeword,entry->sub(entry,R)));
M = matrix L;
C = linearCode(M);
assert(C.AmbientModule == R^4)
assert(C.Generators == L)
G = C.GeneratorMatrix;
assert(G == M)
H = C.ParityCheckMatrix;
z = matrix apply(toList(1..rank H),i -> apply(toList(1..rank G), j->sub(0,R)))
assert(H*(transpose G) == z)
///

TEST ///
-- generatorToParityCheck constructor
F = GF(8,Variable => a);
G = matrix {{1,0,0,a,0,1,1,a},{0,0,0,1,1,1,1,0},{1,1,0,0,0,1,0,0},{1,0,1,0,0,1,1,0}};
H = generatorToParityCheck G;
z = matrix apply(toList(1..rank H),i -> apply(toList(1..rank G), j->sub(0,F)));
assert (rank(G.source) - rank G == rank H)
assert (H* (transpose G) == z)
///

TEST ///
--parityCheckToGenerator
F = GF 2
H =  matrix apply({{1,1,1,0}},l->apply(l,entry -> sub(entry,F)))
G = parityCheckToGenerator H
z = matrix apply(toList(1..rank H),i -> apply(toList(1..rank G), j->sub(0,F)))
assert (rank(H.source) == rank H + rank G)
assert (H* (transpose G) == z)
K = GF(8,Variable => a)
H = matrix {{1,0,0,0,1,1,0,0},{0,1,0,0,0,1,1,0},{0,0,1,0,1,0,1,a^2+1},{0,0,0,1,1,0,0,1}}
G = parityCheckToGenerator H
z = matrix apply(toList(1..rank H),i -> apply(toList(1..rank G), j->sub(0,K)))
assert (rank(H.source) == rank H + rank G)
assert (H* (transpose G) == z)
///

TEST ///
-- zeroCode constructor
F = GF 2
n = 7
C = zeroCode(F,n)
assert (length C == 7)
///

TEST ///
--universeCode constructor
F = GF(2,3)
n = 7
C = universeCode(F,n)
assert (length C == 7)
///

TEST ///
--repetitionCode constructor
F = GF 9
n = 5
C=repetitionCode(F,n)
assert (length C == 5)
///

TEST ///
--zeroSumCode constructor
C = zeroSumCode(GF 3,5)
assert (length C == 5)
///


TEST ///
-- randLDPC test
for i from 0 to 1 do(
    n := random(10, 20);
    k := random(1, n-1);
    
    H := randLDPC(n, k, 3.0, 0);
    assert(numgens target H == (n-k));
    assert(numgens source H == n);    
    );
///
TEST ///
-- randNoRepeats test
assert(randNoRepeats(0,1) == {0});
for i from 0 to 1 do(
    a := random(0,100);
    k := random(1,a+1);  
    assert(set(randNoRepeats(a, a+1)) == set(toList(0..a)));
    -- check it actually has no repeats.
    test := randNoRepeats(a, k);
    assert(length test == #(set(test)))
    );
///

TEST ///
-- tannerGraph test
R := GF(2);
for i from 1 to 1 do(
    H := random(R^10, R^10);
    G := tannerGraph H;
    -- Edges correspond 1:1 with ones in H.
    assert(length (Graphs$edges G) == sum flatten entries (lift(H,ZZ)));  
);
///


TEST ///
-- Mathematical Equality Test.
F = GF(2)
codeLen = 10
codeDim = 4
L = apply(toList(1..codeDim),j-> apply(toList(1..codeLen),i-> random(F)))
H = L|L
C = linearCode(F,codeLen,H)
D = linearCode(F,codeLen,L)
assert( C == D)
///


-- TEST ///
-- bitflipDecode
-- Make sure that it only outputs codewords.
-- R := GF(2);
-- H := random(R^10, R^15)
-- for i from 1 to 1 do(
--     v := vector (for i from 1 to 15 list(random(R)));
--     w := bitflipDecode(H, v);
--     if(w != {}) then (
--    	assert(H*(vector w) == 0_(target H));
--     );
--  );
-- ///

TEST///
-- shorten test, integer.
F = GF(2)
codeLen = 10
L = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 0, 1, 0, 0}, {1, 1, 0, 0, 0, 1, 0, 0, 1, 0}, {1, 0, 0, 1, 0, 0, 0, 1, 1, 1}}
H = L|L

C2 = linearCode(F,codeLen,H)
C3 = linearCode(F,codeLen,L)

shortL = {{0, 1, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 1, 1, 0, 1, 0, 0}, {1, 1, 0, 0, 1, 0, 0, 1, 0}}

assert( numColumns ( C2.GeneratorMatrix ) == numColumns (shorten( C2, 3)).GeneratorMatrix + 1 )
assert( numColumns ( C3.GeneratorMatrix ) == numColumns (shorten( C3, 3)).GeneratorMatrix + 1 )
assert( shorten( C2, 3 ) == linearCode(F, shortL) )
assert( shorten( C3, 3 ) == linearCode(F, shortL) )
///

TEST///
-- shorten test, list.
F = GF(2)
codeLen = 10
L = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 0, 1, 0, 0}, {1, 1, 0, 0, 0, 1, 0, 0, 1, 0}, {1, 0, 0, 1, 0, 0, 0, 1, 1, 1}}
H = L|L

C2 = linearCode(F,codeLen,H)
C3 = linearCode(F,codeLen,L)
K = {3,6,8,9}

shortL = {{0, 1, 0, 0, 0, 0}, {0, 0, 1, 1, 1, 1}}

assert( numColumns ( C2.GeneratorMatrix ) == numColumns (shorten( C2, K)).GeneratorMatrix + 4 )
assert( numColumns ( C3.GeneratorMatrix ) == numColumns (shorten( C3, K)).GeneratorMatrix + 4 )
assert( shorten( C2, K ) == linearCode(F, shortL) )
assert( shorten( C3, K ) == linearCode(F, shortL) )
///

TEST ///
-- vNumner of the ideal I=ideal(t1*t2^2-t1^2*t2,t1*t3^3-t1^3t3,t2*t3^3-t2^3*t3).
K=ZZ/3
R=K[t3,t2,t1,MonomialOrder=>Lex]
I=ideal(t1*t2^2-t1^2*t2,t1*t3^3-t1^3*t3,t2*t3^3-t2^3*t3)
vNumber(I)
assert(vNumber(I) == (regularity coker gens gb I)-1)
///

TEST ///
-- footPrint function of the ideal I=ideal(t1^3,t2*t3) with parameters d=2, r=3.
K=QQ
R=K[t1,t2,t3]
I=ideal(t1^3,t2*t3)
footPrint(3,4,I)
assert(footPrint(3,4,I)==4)
///

TEST ///
-- hyp of the ideal I=ideal(t1*t6-t3*t4,t2*t6-t3*t5) with parameters d=1, r=1.
K=ZZ/3
R=K[t1,t2,t3,t4,t5,t6]
I=ideal(t1*t6-t3*t4,t2*t6-t3*t5)
hyp(1,1,I)
assert(hyp(1,1,I)==1)
///


TEST ///
-- genMinDisIdeal of the ideal I=ideal(t1*t6-t3*t4,t2*t6-t3*t5) with parameters d=1, r=1.
K=ZZ/3
R=K[t1,t2,t3,t4,t5,t6]
I=ideal(t1*t6-t3*t4,t2*t6-t3*t5)
genMinDisIdeal(1,1,I)
assert(genMinDisIdeal(1,1,I)==3)
///


TEST ///
 -- vasconcelosDegree of the ideal I=ideal(t1^2,t1*t2,t2^2) with parameters d=1, r=1.
K=ZZ/3
R=K[t1,t2]
I=ideal(t1^2,t1*t2,t2^2)
vasconcelosDegree(1,1,I)
assert(vasconcelosDegree(1,1,I)==1)
///


TEST /// 
-- random test.
F = GF(2, 4)
n = 5
k = 3
C = randomCode (F,n,k)

assert( length C == 5 )

QR = ZZ/3
n = 5
k = 3
C = randomCode (QR,n,k)

assert( length C == n)
///


TEST ///
-- Hamming code over GF(2) and dimension of the dual 3.
C1= hammingCode(2,3)
assert( length C1 == 7)
///

TEST ///
-- Hamming code over GF(2) and dimension of the dual 4.
C2= hammingCode(2,4)
assert( length C2 == 15)
///

TEST ///
-- Cyclic codes.
C=cyclicCode(GF(7),1,5)
assert( length C == 5)
///

TEST ///
-- Cyclic codes.
GF(7)[x]
C=cyclicCode(GF(7),(x+3)*(x-1)*(x^3-2),9)
assert( length C == 9)
///

TEST ///
-- alphabet.
F=GF 4
C=linearCode(random(F^3,F^5))
A={sub(0,F)}|apply(3,i->F_0^i)
assert(set alphabet C == set A)
///

TEST ///
-- ambient space.
F=GF(4)
C=linearCode(random(F^3,F^5))
assert(ambientSpace C == F^5)
///

TEST ///
-- codewords.
F=GF(4,Variable=>a)
C=linearCode(matrix{{1,a,0},{0,1,a}})
cwt={{0,0,0},{0,1,a},{0,a,a+1},{0,a+1,1},{1,a,0},{1,a+1,a},{1,0,a+1},{1,1,1},{a,a+1,0},{a,1,a+1},{a,0,1},{a,a,a},{a+1,1,0},{a+1,a,1},{a+1,0,a},{a+1,a+1,a+1}}
cwt=apply(cwt,i->apply(i,j->sub(j,F)))
assert(set cwt == set codewords C)
///

TEST ///
-- cyclic matrix.
F=GF(3)
v={0,1,0,2}
M=matrix{{0,1,0,2},{2,0,1,0},{0,2,0,1},{1,0,2,0}}
M=sub(M,F)
assert( M == cyclicMatrix(F,v))
///

TEST ///
-- dual Code.
F=GF(4)
C=linearCode(matrix{{1,0,1,a,a},{0,1,a,a+1,1}})
D=linearCode(matrix{{1,a,1,0,0},{a,a+1,0,1,0},{a,1,0,0,1}})
assert( dualCode(C)==D)
///

TEST ///
-- field.
F=GF(4)
C=linearCode(random(F^3,F^5))
assert(field C===F)
///

TEST ///
-- toString.
L = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 0, 1, 0, 0}, {1, 1, 0, 0, 0, 1, 0, 0, 1, 0}, {1, 0, 0, 1, 0, 0, 0, 1, 1, 1}}
C = linearCode(GF(2),L)
assert(length toString C == 128)
///


TEST ///
-- genericCode.
F=GF(4)
C=linearCode(random(F^3,F^5))
assert(genericCode(C)==linearCode(F^5))
///

TEST ///
-- dimension of a linear code.
F = GF(4);
C= linearCode(F,{{1,1,0,0},{0,0,1,1}});
assert(dim C == 2)
///

TEST ///
-- informationRate.
R = GF(5); 
L = {{1,1,1,1},{2,2,2,2}};
C = linearCode(R,L);
assert(informationRate(C) == 1/4)
C = hammingCode(2,3);
assert(informationRate(C) == 4/7)
///


TEST ///
-- size of a code.
F = GF(2); L = {{1,1,1,1}};
C = linearCode(F,L);
assert (size(C) == 2)
F = GF(8); L = {{1,1,1,1,1}}
C = linearCode(F,L);
assert(size(C) == 8);
F = GF(4); L = {{1,1,1,1,1}};
C = linearCode(F,L,ParityCheck => true);
assert(size(C) == 4^4)
///

TEST ///
-- length of a code.
F = GF(2); L = {{1,1,1,1}};
C = linearCode(F,L);
assert (length(C) == 4)
F = GF(8); L = {{1,1,1,1,1}}
C = linearCode(F,L);
assert(length(C) == 5);
C = hammingCode(2,3);
assert(length(C) == 7)
///


TEST ///
-- vectorSpace.
F = GF(8);
L = apply({{1,1,1,1,1}},codeword->apply(codeword,entry->sub(entry,F)));
C = linearCode(F,L);
M = matrix(L);
D = image transpose M;
assert(vectorSpace(C) == D)
///


TEST ///
-- messages.
F = GF(4,Variable => a); L = {{1,1,1,1,1}};
C = linearCode(F,L);
m = set apply({{0},{1},{a},{a+1}}, me -> apply(me,entry -> sub(entry,F)));
mm = set messages(C);
assert(mm == m)
H = hammingCode(2,3);
m = {{0,0,0,0},{0,0,0,1},{0,0,1,0},{0,0,1,1},{0,1,0,0},{0,1,0,1},{0,1,1,0},{0,1,1,1},{1,0,0,0},{1,0,0,1},{1,0,1,0},{1,0,1,1},{1,1,0,0},{1,1,0,1},{1,1,1,0},{1,1,1,1}};
Lmessage = set apply(m,plain -> apply(plain,entry->sub(entry,H.BaseField)));
hmessage = set messages(H);
assert(hmessage == Lmessage)
///

TEST ///
--quasi-cyclic Codes.
F = GF(5)
L = apply(toList(1..2),j-> apply(toList(1..4),i-> random(F)))
C=quasiCyclicCode(L)
assert ( length C==4)
///

TEST ///
--quasi-cyclic codes.
F = GF(8)
L = apply(toList(1..2),j-> apply(toList(1..5),i-> random(F)))
C=quasiCyclicCode(F,L)
assert ( length C==5)
///

TEST ///
-- reducedMatrix.
F = GF(4)
n = 7
k = 3
L = apply(toList(1..k),j-> apply(toList(1..n),i-> random(F)))
m=matrix(L)
M=reducedMatrix(m)
assert (rank m== rank M)
///


-----------------------------------------------
-----------------------------------------------
-- Use this section for Evaluation Code Tests
-----------------------------------------------
-----------------------------------------------

TEST ///
-- Evaluation code.
F=GF(4);
R=F[x,y,z];
P={{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,1,1},{a,a,a}};
S={x+y+z,a+y*z^2,z^2,x+y+z+z^2};
C=evaluationCode(F,P,S);
assert(length C.LinearCode == 6)
assert(dim C.LinearCode == 3)
///

TEST ///
-- Toric code.
M=matrix{{1,4},{2,5},{10,6}} -- matrix of exponent vectors defining the polytope P, exponents vectors are rows
T=toricCode(GF 4,M) --- a toric code over F_4 with polytope P
assert(length T.LinearCode == 9)
assert(dim T.LinearCode == 5)
///

TEST ///
-- Cartesian code.
F=GF(4);
R=F[x,y];
C=cartesianCode(F,{{0,1,a},{0,1,a}},{1+x+y,x*y})
assert(length C.LinearCode == 9)
assert(dim C.LinearCode == 2)
///

TEST ///
-- Cartesian codes.
C=cartesianCode(ZZ/11,{{1,2,3},{2,6,8}},3)
assert( length C.LinearCode == 9)
///

TEST ///
-- Reed-Muller codes.
C=reedMullerCode(3,3,4);
assert( length C.LinearCode == 27)
///

TEST ///
-- Reed-Solomon codes.
C=reedSolomonCode(ZZ/11,{1,2,3},3);
assert( length C.LinearCode == 3)
///

TEST ///
-- Reed-Solomon codes.
C=reedSolomonCode(ZZ/17,{0,1,2,3,7,11},4)
dim C.LinearCode
assert( dim C.LinearCode == 4)
///

TEST ///
-- Order codes.
F=GF(4);
R=F[x,y];
I=ideal(x^3+y^2+y);
l=7;
C=orderCode(I,{2,3},l);
assert(length C.LinearCode==8)
assert( dim C.LinearCode==7)
///




 TEST ///
 -- Given the target parameters (n,k,r)  of an LRC code to be constructed over finite field F
 -- with a partition of symbols A that has good polynomial g, take an information
 -- vector in F^k and generate its corresponding encoding polynomial.
 n=9
 k=4
 r=2
 q=13
 S=ZZ/(q)[a,b,c,d][x]   --arbitrary vector in F^k.
 g=x^3
 encodingPolynomial=getLRCencodingPolynomial(k,r,{a,b,c,d},g)
 polynomial1=sub(encodingPolynomial,{a=>1,b=>1,c=>0,d=>0})
 polynomial2=sub(encodingPolynomial,{a=>0,b=>1,c=>0,d=>1})
 test1=getLRCencodingPolynomial(k,r,{1,1,0,0},g)
 test2=getLRCencodingPolynomial(k,r,{0,1,0,1},g)
 assert( polynomial1==test1 )
 assert( polynomial2==test2 )
 ///

 TEST ///
 -- LRC code over GF(13).
 A1={{1,5,12,8},{2,10,11,3},{4,7,9,6}}
 n=12
 k=6
 r=3
 q=13
 R=ZZ/(q)[x]
 g=x^4
 C=locallyRecoverableCode({q,n,k,r},A1,g)
 assert( rank(C.GeneratorMatrix)==k )
 sampleWords=(entries C.GeneratorMatrix)_{2,3}
 evaluations=apply(sampleWords,i->toList set apply(i,j->g[j]%q))
 assert( #evaluations_0==r )
 assert( #evaluations_1==r )
 ///


TEST ///
 -- Evaluation code over a graph.
   G = graph({1,2,3,4}, {{1,2},{2,3},{3,4},{4,3}})
   B=incidenceMatrix G
   S=ZZ/2[t_(0)..t_(#vertexSet G-1)]
   C=evCodeGraph(coefficientRing S,B,flatten entries basis(1,S))
   assert(length C.LinearCode==4)
   assert( dim C.LinearCode==3)
///
------------------------------------------
------------------------------------------
-- Documentation
------------------------------------------
------------------------------------------


beginDocumentation()
-*document { 
	Key => CodingTheory,
	Headline => "a package for coding theory",
	PARA {
	    EM "CodingTheory", " is a package to provide both
	basic coding theory objects and routines, and methods
	for computing invariants of codes using commutative 
	algebra techniques."
	},
    
	PARA { "This package currently provides constructors for
	linear codes, evaluation codes, and a few methods for each."
	},    
    
	SUBSECTION "Contributors", "The following people have generously
	contributed code or worked on our code at various Macaulay2 workshops.",
	
	UL {
	    "Branden Stone"
	},
    
	SUBSECTION "Modified Methods",
	
	UL {
	    TO "random(GaloisField,ZZ,ZZ)",
	    TO "ring(LinearCode)"

	}
    	
	}
*-
doc ///
	Key
		CodingTheory
	Headline
		tools for Coding Theory
	Description
		Text
			{\tt CodingTheory} is a package designed to provide
			basic coding theory objects, and
			methods for computing the basic parameters of
			linear codes. Some of the implemented functions use commutative
			algebra techniques.
		Text
			{\tt CodingTheory} currently provides constructors for
			linear codes and evaluation codes, and a few methods for each.
			
	Contributors
			Branden Stone generously contributed code or worked on our
			code at various Macaulay2 workshops.
	Subnodes
	 :Main objects
	 EvaluationCode
	 LinearCode
	 :Modified methods
	 @TO (dim,LinearCode)@
	 @TO (length,LinearCode)@
	 @TO (ring,LinearCode)@
	 @TO (size,LinearCode)@
	 @TO (toString,LinearCode)@
	 :Implemented functions that are independent of coding theory
	 enumerateVectors
	 randNoRepeats
	 reducedMatrix
	 shortestPath
///

doc ///
    Key 
    	LinearCode
    Headline
    	class of linear codes
    Description
    	Text
	    A linear code is the image of some mapping between vector spaces,
	    where each vector space is taken to be over the same finite field. A codeword is an element
	    of the image. A linear code in {\it Macaulay2} is implemented as a hash table.
	    The values of the hash table correspond to common representations of the code, as well as
	    information about its structure. The values include the base field of the modules, a set
	    of generators for the code, and more. To construct a linear code, see @TO linearCode@.
	Example
	    F1=GF(2)
	    G1={{1,1,0,0,0,0},{0,0,1,1,0,0},{0,0,0,0,1,1}}
	    C1=linearCode(F1,G1)
	    C1.Code	
	Text
	    For the mapping defined above, we call the codomain of the mapping the ambient module. The length of a code is defined
	    to be the rank of this module. 
      	Example 
	    F2=GF(3)
	    G2={{1,0,0,0,0,1,1,1},{0,1,0,0,1,0,1,1},{0,0,1,0,1,1,0,1},{0,0,0,1,1,1,1,0}}  
	    C2=linearCode(F2,G2)
	    AM=C2.AmbientModule
	    rank(AM)==length(C2)  
	Text
	    Since a linear code $C$ is a vector subspace over some finite field, we may represent it using a Generator Matrix, i.e., a
	    matrix whose rows form a basis for $C$. The dimension of a code is the rank of the generator matrix.
	Example
	    dim(C2)==rank(C2.GeneratorMatrix)
	Text
	    A linear code in Macaulay2 also includes a parity check matrix $H$, which generates the vector space orthogonal to $C$. Let $c$
	    be a code word in $C$ and $h$ a vector in the space generated by the rows of $H$. Then the dot product between $c$ and $h$
	    is zero.
	Example
	    c=matrix{G2_0}
	    h=transpose matrix({(entries(C2.ParityCheckMatrix))_0})
 	    c*h
    Caveat
	While some functions may work even when a ring is given, instead of a finite field,
	it is possible that the results are not the expected ones.
    Subnodes
	:Related functions and symbols:
	ambientSpace
	bitflipDecode
	Code
	codewords
	GeneratorMatrix
	informationRate
	linearCode
	messages
	weight
	syndromeDecode
	@TO (symbol ==,LinearCode,LinearCode)@
///
-----------------------------------------------
-----------------------------------------------
-- Use this section for Linear Code documentation:
-----------------------------------------------
-----------------------------------------------


doc ///
	Key
		linearCode
		[linearCode,ParityCheck]
		(linearCode,Module,List)
		(linearCode,GaloisField,ZZ,List)
		(linearCode,GaloisField,List)
		(linearCode,ZZ,ZZ,ZZ,List)
		(linearCode,Module)
		(linearCode,Matrix)
	Headline
		functions to construct linear codes over Galois fields
	Usage
		linearCode(G)
		linearCode(M)
		linearCode(M,L)
		linearCode(F,L)
		linearCode(F,n,L)
		linearCode(p,r,n,L)
	Inputs
		G:Matrix
		M:Module
		F:GaloisField
		p:ZZ
		r:ZZ
		n:ZZ
		L:List
	Outputs
		:LinearCode
			$C$
	Description
		Text
			We present below the ways in how a linear code $C$ can be defined.
	Subnodes
		:Constructions of linear codes
		cyclicCode
		dualCode
		genericCode
		hammingCode
		locallyRecoverableCode
		ParityCheck
		randomCode
		repetitionCode
		shorten
		universeCode
		zeroCode
		zeroSumCode
    	Caveat
		While some functions may work even when a ring is given, instead of a finite field,
		it is possible that the results are not the expected ones.
    Synopsis
    	Heading
		a matrix is given
	BaseFunction
		linearCode
	Usage
		linearCode(G)
	Inputs
		G:Matrix
	Outputs
		:LinearCode
			$C$
	Description
		Text
			Given a matrix {\tt G}, whose entries are in a Galois field {\tt F},
			this function returns a linear code $C$ over {\tt F}.
		Text
			If no optional input is specified, then the generator matrix of
			the code $C$ is {\tt G}.
		Text
			If the optional input {\tt ParityCheck => true} is specified, then the
			code $C$ is the dual of the linear code generated by the matrix {\tt G}.
		Example
			F = GF 4;
			L = apply({{1,0,1,0},{0,1,1,1}},codeword ->apply(codeword,entry->sub(entry,F)));
			M = matrix L
			C = linearCode(M)
			C.GeneratorMatrix
			C.ParityCheckMatrix
		Text
			This is an example using the optional argument {\tt ParityCheck=>true}.
		Example
			F = GF(4,Variable => a);
			L = {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a,0}};
			M = matrix L;
			C = linearCode(F,L,ParityCheck => true)
			C.GeneratorMatrix
			C.ParityCheckMatrix
    Synopsis
    	Heading
		a module is given
	BaseFunction
		linearCode
	Usage
		linearCode(M)
	Inputs
		M:Module
	Outputs
		:LinearCode
			$C$
	Description
		Text
			Given a submodule {\tt M} of a free module {\tt F}$^n$, where $F$ is a Galois field,
			this function returns a linear code $C$ whose ambient space is {\tt F}$^n$.
		Text
			If no optional input is specified, then the code $C$ is generated by the
			elements of {\tt M}.
		Text
			If the optional input {\tt ParityCheck => true} is specified, then the code $C$
			is the dual of the linear code generated by the elements of {\tt M}.
		Example
			F = GF 2; G = transpose matrix {apply({1,1,1,1},entry -> sub(entry,F))};
			M = image G;
			C = linearCode(M)
			C.AmbientModule
			C.BaseField
			C.GeneratorMatrix
			C.ParityCheckMatrix
    Synopsis
    	Heading
		a module and a list are given
	BaseFunction
		linearCode
	Usage
		linearCode(M,L)
	Inputs
		M:Module
		L:List
	Outputs
		:LinearCode
			$C$
	Description
		Text
			Given a free module {\tt M}$=F^n$, where $F$ is a Galois field,
			and a non-empty list {\tt L} of vectors of {\tt M}, this function returns a
			linear code $C$ whose ambient space is $F^n$.
		Text
			If no optional input is specified, then the code $C$ is generated by the
			vectors of {\tt L}.
		Text
			If the optional input {\tt ParityCheck => true} is specified, then the code $C$
			is the dual of the linear code generated by the vectors of {\tt L}.
		Example
			F = GF(4,Variable => a); M = F^5; L = {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a,0}};
			C = linearCode(M,L)
			C.AmbientModule
			C.BaseField
			C.Generators
			C.GeneratorMatrix
			C.ParityCheckMatrix
			C.Code
		Text
			This is an example using the optional argument {\tt ParityCheck=>true}.
		Example
			F = GF(8,Variable =>a); M = F^4; L = {{a+1,a+1,a+1,a+1}};
			C = linearCode(M,L,ParityCheck => true)
			G = C.GeneratorMatrix
			H = C.ParityCheckMatrix
    Synopsis
    	Heading
		a Galois field and a list are given
	BaseFunction
		linearCode
	Usage
		linearCode(F,L)
	Inputs
		F:GaloisField
		L:List
	Outputs
		:LinearCode
			$C$
	Description
		Text
			Given a Galois field {\tt F}, and a non-empty list {\tt L}
			of vectors of the same size and whose entries are coercible into the field {\tt F},
			this function returns a linear code $C$ over the field {\tt F}.
		Text
			If no optional input is specified, then the code $C$ is generated by the
			vectors of {\tt L}.
		Text
			If the optional input {\tt ParityCheck => true} is specified, then the code $C$
			is the dual of the linear code generated by the vectors of {\tt L}.
		Example
			F = GF 4; L = {{1,0,1,0},{1,0,1,0}};
			C = linearCode(F,L)
			C.GeneratorMatrix
			C.ParityCheckMatrix
		Text
			This is an example using the optional argument {\tt ParityCheck=>true}.
		Example
			F = GF(9,Variable => a); L = {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a,0}};
			C = linearCode(F,L,ParityCheck => true)
			C.GeneratorMatrix
			C.ParityCheckMatrix
    Synopsis
    	Heading
		a Galois field, a positive integer and a list are given
	BaseFunction
		linearCode
	Usage
		linearCode(F,n,L)
	Inputs
		F:GaloisField
		n:ZZ
		L:List
	Outputs
		:LinearCode
			$C$
	Description
		Text
			Given a Galois field {\tt F}, a positive integer {\tt n}, and a non-empty list {\tt L}
			of vectors of size {\tt n} and entries that are coercible into the field {\tt F},
			this function returns a linear code $C$ of length {\tt n} over the field {\tt F}.
		Text
			If no optional input is specified, then the code $C$ is generated by the
			vectors of {\tt L}.
		Text
			If the optional input {\tt ParityCheck => true} is specified, then the code $C$
			is the dual of the linear code generated by the vectors of {\tt L}.
		Example
			F = GF 4; n = 4; L = {{1,0,1,0},{1,0,1,0}};
			C = linearCode(F,n,L)
			C.GeneratorMatrix
			C.ParityCheckMatrix
		Text
			This is an example using the optional argument {\tt ParityCheck=>true}.
		Example
			F = GF(9,Variable => a); n = 5; L = {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a,0}};
			C = linearCode(F,n,L,ParityCheck => true)
			C.GeneratorMatrix
			C.ParityCheckMatrix
    Synopsis
    	Heading
		a prime number, two positive integers and a list are given
	BaseFunction
		linearCode
	Usage
		linearCode(p,r,n,L)
	Inputs
		p:ZZ
		r:ZZ
		n:ZZ
		L:List
	Outputs
		:LinearCode
			$C$
	Description
		Text
			Given a prime number {\tt p}, positive integers {\tt r} and {\tt n},			
			and a non-empty list {\tt L} of vectors of size {\tt n} and entries that are
			coercible into the Galois field {\tt GF}$(\mathtt{p}^\mathtt{r})$,
			this function returns a linear code $C$ of length {\tt n} over the
			Galois field {\tt GF}$(\mathtt{p}^\mathtt{r})$.
		Text
			If no optional input is specified, then the code $C$ is generated by the
			vectors of {\tt L}.
		Text
			If the optional input {\tt ParityCheck => true} is specified, then the code $C$
			is the dual of the linear code generated by the vectors of {\tt L}.
		Example
			p = 2; r = 2; n=4; L = {{1,0,1,0},{0,1,1,1}};
			C=linearCode(p,r,n,L)
			p = 3; r = 2; n = 5;
			ambient GF(p,r)
			L = {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a+1,0}};
			C=linearCode(p,r,n,L)
///

doc ///
        Key
               AmbientModule
        Headline
                the ambient module of a code
        Usage
                C.AmbientModule
        Inputs
                C:LinearCode
        Outputs
                :Module
                           
        Description
                Text
                        Given a linear code {\tt C} of length $n$ over a Galois Field $F$,  
                        this symbol is used as a key for storing the free module $F^n$,  
                        which is referred to as the ambient module of {\tt C}.
         	Text 
		    	This symbol is provided by the package @TO CodingTheory@.
                Example
                               C = linearCode(GF(4,Variable => a), {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a,0}})
                               C.AmbientModule
                        
			       
///

doc ///
        Key
               BaseField
        Headline
                the field of a code
        Usage
                C.BaseField
        Inputs
                C:LinearCode
        Outputs
                :GaloisField   
        Description
                Text
                        Given a linear code {\tt C} over a Galois field $F$, 
                        this symbol is used as a key for storing the Galois field $F$.
         	Text 
		    	This symbol is provided by the package @TO CodingTheory@.
                Example
                               C = linearCode(GF(8,Variable => b), {{1,0,b,0,0},{0,b,b+1,1,0},{1,1,1,b,0}})
	                       C.BaseField
///

doc ///
    Key 
      Code
    Headline
      a code as image
    Usage
      C.Code
    Inputs
      C:LinearCode
    Outputs
      :Module
    Description
         Text
            Given a linear code {\tt C}, this symbol is used as a key for storing {\tt C} as the
            image of some mapping between finitely generated modules,
            where each module is over the same Galois field.
         Text 
            This symbol is provided by the package @TO CodingTheory@.
         Example
            C = linearCode(GF(8,Variable => b), {{1,1,b,0,0},{0,b,b,1,0},{1,1,1,b,0}});
	    C.Code
///

doc ///
    Key 
      ExponentsMatrix
    Headline
     specifies the matrix of exponents
    Usage
      C.Code
    Inputs
      C:EvaluationCode
    Outputs
      :Matrix
    Description
         Text
            This symbol is used as a key for storing the matrix of exponents,
            which is used for the function @TO toricCode@.
         Text
            This symbol is provided by the package @TO CodingTheory@.
	 Example
		   M=matrix{{1,4},{2,5},{10,6}};
	           T=toricCode(GF 4,M);
	           T.ExponentsMatrix
///

doc ///
    Key 
      GeneratorMatrix
    Headline
      gives the generator matrix of a linear code
    Usage
            C.GeneratorMatrix
    Inputs
            C:LinearCode
    Outputs
            :Matrix
    Description
         Text
            Given a linear code {\tt C}, this symbol is used as a key for storing
            a generator matrix of {\tt C}.
         Text 
            This symbol is provided by the package @TO CodingTheory@.
         Example
            C = linearCode(GF(8,Variable => b), {{1,1,b,0,0},{0,b,b,1,0},{1,1,1,b,0}});
	    C.GeneratorMatrix
    Subnodes
         :Related function:
         Generators
///

doc ///
    Key 
      Generators
    Headline
      list of generators of a code
    Usage
      C.Generators
    Inputs
      C:LinearCode
    Outputs
      :List
    Description
         Text
            Given a linear code {\tt C}, this symbol is used as a key for 
            storing the list of rows of a generator matrix of {\tt C}.
         Text 
            This symbol is provided by the package @TO CodingTheory@.
         Example
            C = linearCode(GF(8,Variable => a), {{1,1,a,0,0},{0,a,a,1,0},{1,1,1,a,0}});
	    C.Generators
///

-*
doc ///
    Key 
      IncidenceMatrix
    Headline
      gives the incident matrix of a graph
    Description
         Text
            This symbol is provided by the package @TO CodingTheory@.
///
*-

doc ///
    Key 
      ParityCheck
    Headline
      an optional input for the linearCode constructor
    Usage
      linearCode(..., ParityCheck => ...)
    Description
         Text
            This is a Boolean symbol, returns value false or true.
            The default value is false.
         Text 
            This symbol is provided by the package @TO CodingTheory@.
         Example
            F = GF(4,Variable => a);
	    L = {{1,0,a,0,0},{0,a,a+1,1,0},{1,1,1,a,0}};
	    M = matrix L;
	    C = linearCode(F,L,ParityCheck => true);
	    C.GeneratorMatrix;
	    C.ParityCheckMatrix
    Subnodes
         :Related functions and symbols:
	 generatorToParityCheck
         ParityCheckMatrix
	 ParityCheckRows
	 parityCheckToGenerator
	 randLDPC
	 tannerGraph
///

doc ///
    Key
    	ParityCheckMatrix
    Headline
    	a parity check matrix of a code
    Usage
    	C.ParityCheckMatrix
    Inputs
    	C:LinearCode
    Outputs
    	:Matrix
    Description
    	Text
	    This symbol is used as a key for storing a parity check matrix of {\tt C}.
    	Text 
            This symbol is provided by the package @TO CodingTheory@.
	Example
	    C=linearCode(GF(8,Variable => b), {{1,1,b,0,0},{0,b,b,1,0},{1,1,1,b,0}});
	    C.ParityCheckMatrix
    SeeAlso
    	ParityCheckRows
	Generators
	reducedMatrix
	generatorToParityCheck
	parityCheckToGenerator
///

doc ///
    Key
    	ParityCheckRows
    Headline
    	rows of a parity check matrix of a code
    Usage
    	C.ParityCheckRows
    Inputs
    	C:LinearCode
    Outputs
    	:List
    Description
    	Text
	    This symbol is a key to store a list with the rows of a parity check matrix of {\tt C}.
    	Text 
            This symbol is provided by the package @TO CodingTheory@.
	Example
	    C = linearCode(GF(8,Variable => b), {{1,1,b,0,0},{0,b,b,1,0},{1,1,1,b,0}});
	    C.ParityCheckMatrix
	    C.ParityCheckRows
    SeeAlso
    	ParityCheckMatrix
	Generators
	reducedMatrix
	generatorToParityCheck
	parityCheckToGenerator
///

doc ///
    Key
    	weight
	(weight, BasicList)
    Headline
    	Hamming weight of a list
    Usage
    	weight(L)
    Inputs
    	L:List
    Outputs
    	:ZZ
    Description
    	Text
	    Computes the Hamming weight of a list {\tt L}, which is the number of its non-zero entries.
	Example
	    weight({1,0,1,0,1})
	Example
	    weight({0,123,48,0,256})
    Subnodes
        :Related functions
	minimumWeight
///
    
doc ///
        Key
               syndromeDecode
               (syndromeDecode, LinearCode, Matrix, ZZ)
        Headline
                syndrome decoding on a code
        Usage
                syndromeDecode(C,v,minDist)
        Inputs
                C:LinearCode
	        v:Matrix
	        minDist:ZZ
        Outputs
                :List
        Description
                Text  
			When this function runs, it checks the cache of the
			{\tt LinearCode} {\tt C} for an existing syndrome look-up
			table. If a look-up table is not found, it automatically
			generates one. Because of
			this, the first time this function is called will take longer 
			than subsequent calls. If you want to access the look-up table,  
			it can be obtained from {\tt C.cache#"syndromeLUT"}. The 
			{\tt minDist} argument only affects the behavior of this function 
			on the first call, because it is only used when generating the 
			syndrome look-up table. 
		Example
			C = hammingCode(2,3);
			msg = matrix {{1,0,1,0}};
			v = msg*(C.GeneratorMatrix);
			err = matrix take(random entries basis source v, 1);
			received = (transpose (v+err));
			syndromeDecode(C, received, 3);
		 	C.cache#"syndromeLUT"
///

doc ///
    Key
    	parityCheckToGenerator
	(parityCheckToGenerator, Matrix)
    Headline
    	generator matrix given a parity check matrix
    Usage
    	parityCheckToGenerator H
    Inputs
    	H:Matrix
    Outputs
    	:Matrix
	    $G$
    Description
	Text
	    Given parity check matrix {\tt H} of a code $C$, this function
	    recovers a generator matrix {\tt G} of $C$.
	Example
	    F = GF 2;
	    H = matrix apply({{1,1,1,0}},l->apply(l,entry->sub(entry,F)))
	    G = parityCheckToGenerator H
	    H*(transpose G)
	Example
	    F = GF(8,Variable => a);
	    H = matrix{{1,0,0,0,1,1,0,0},{0,1,0,0,0,1,1,0},{0,0,1,0,1,0,1,a^2+1}}
	    G = parityCheckToGenerator H
	    H*(transpose G)
///

doc ///
    Key
    	zeroCode
	(zeroCode,GaloisField,ZZ)
    Headline
    	zero code
    Usage
    	zeroCode(F,n)
    Inputs
    	F:GaloisField
	n:ZZ
    Outputs
    	:LinearCode
    Description
    	Text
	    Constructs the linear code of length {\tt n} over {\tt F}
	    whose only codeword is the zero codeword.
	Example
	    C=zeroCode(GF(4),7)
	    C.GeneratorMatrix
///

doc ///
        Key
               universeCode
               (universeCode,GaloisField,ZZ)
        Headline
                linear code $\mathtt{F}^\mathtt{n}$
        Usage
                universeCode(F,n)
        Inputs
                F:GaloisField
                n:ZZ
        Outputs
                :LinearCode
                           
        Description
                Text  
			Returns the code with largest dimension such that
			its length is {\tt n} and the entries of its codewords
			are in the field {\tt F}.
		Example
			    F = GF(2,3); 
                            n=7;
	                    C=universeCode(F,n)
	                    C.ParityCheckMatrix
///

doc ///
	Key
		repetitionCode
		(repetitionCode,GaloisField,ZZ)
	Headline
		repetition code  
	Usage
		repetitionCode(F,n)
	Inputs
		F:GaloisField
		n:ZZ
	Outputs
		:LinearCode
	Description
		Text
                        Returns the repetition code over {\tt F} of length {\tt n}.
		Example
			F = GF(2,3); 
			n=7;
			C=repetitionCode(F,n);
			C.ParityCheckMatrix
///

doc ///
    Key
    	zeroSumCode
	(zeroSumCode,GaloisField,ZZ)
    Headline
    	linear code in which the entries of each codeword add up zero
    Usage
    	zeroSumCode(F,n)
    Inputs
    	F: GaloisField
	n: ZZ
    Outputs
    	:LinearCode
	    $C$
    Description
    	Text
	    Returns the code $C$ of length {\tt n} over {\tt F} such that for any
            $c\in C$, $\sum_{i=1}^n c_i=0$. The dual of the code $C$ is the repetition
	    code. In the binary case, this code $C$ equals the code of all even-weight codewords.
	Example
	    D = zeroSumCode(GF 3,5)
	Example
	    E = zeroSumCode(GF 8,5)
///

doc ///
	Key
		reducedMatrix
		(reducedMatrix, Matrix)
	Headline
		reduced matrix
	Usage
		reducedMatrix(Matrix)
	Inputs
		M:Matrix
	Outputs
		:Matrix
			
	Description
		Text
			Returns a full rank matrix whose row space equals the row space of {\tt M}.
		Example
			F = GF(4);
			n = 7;
			k = 3;
			L = apply(toList(1..k),j-> apply(toList(1..n),i-> random(F)));
			m=matrix(L)
			reducedMatrix(m)
///

doc ///
	Key
		bitflipDecode
		(bitflipDecode, Matrix, Vector, ZZ)
	Headline
		an experimental implementation of a message passing decoder
	Usage
		bitflipDecoder(H,v,maxI)
	Inputs
		H:Matrix
		v:Vector
                maxI:ZZ
	Outputs
		:List
	Description
		Text
			Attempts to decode the vector {\tt v} relative to the parity check
			matrix {\tt H} using a message passing decoding algorithm. The
			matrix {\tt H} and the vector {\tt v} must have entries in
			{\tt GF(2)}. Returns the empty list if {\tt maxI} is exceeded.

		Text
			At each iteration, this function flips all the bits of {\tt v}
			that fail the maximum number of parity check equations from {\tt H}.
			This is experimental because it has not been fully tested. The
			output is only guaranteed to be a codeword of the code defined by
			{\tt H}.
    
		Example
			R=GF(2);
	                H := matrix(R, {{1,1,0,0,0,0,0},{0,1,1,0,0,0,0},{0,1,1,1,1,0,0},{0,0,0,1,1,0,0},{0,0,0,0,1,1,0},{0,0,0,0,1,0,1}});
	                v := vector transpose matrix(R, {{1,0,0,1,0,1,1}});
	                bitflipDecode(H,v,100)
///

doc ///
        Key
               tannerGraph
               (tannerGraph,Matrix)
        Headline
                outputs the tanner graph associated with the given parity check matrix
        Usage
                tannerGraph(H)
        Inputs
                H:Matrix
        Outputs
                :Graphs$Graph
        Description
                Text  
			Given a linear code $C$ with parity-check matrix {\tt H},
			the function returns the Tanner graph associated to {\tt H}.
			This is a bipartite graph with one set of vertices indexed by
			the rows of  {\tt H} and the other by the columns of {\tt H}.
			The vertex corresponding to the $i$-th row is connected to the 
			vertex corresponding to the $j$-th column if and only if the $(i,j)$-th 
			entry of {\tt H} is nonzero.
		Example
			  H := matrix(GF(2), {{1,1,0,0,0,0,0},{0,1,1,0,0,0,0}, {0,1,1,1,1,0,0},{0,0,0,1,1,0,0},{0,0,0,0,1,1,0},{0,0,0,0,1,0,1}});
                          tannerGraph(H)
///

doc ///
    Key
    	hammingCode
	(hammingCode,ZZ,ZZ)
    Headline
    	generates a Hamming code
    Usage
    	hammingCode(q,s)
    Inputs
    	q: ZZ
	s: ZZ
    Outputs
       	: LinearCode
	    $C$
    Description
    	Text
	    Returns the Hamming code $C$ over {\tt GF(q)} whose dual
	    has dimension {\tt s}.
    	Example
	    C1 = hammingCode(2,3);
	    C1.ParityCheckMatrix
///

doc ///
        Key
               shorten
               (shorten, LinearCode, List)
               (shorten, LinearCode, ZZ)
        Headline
                shortens a code 
        Usage
                shorten(LinearCode, List)
                shorten(LindearCode, ZZ)
        Inputs
		C:LinearCode
		L:List
		i:ZZ
        Outputs
		:LinearCode
        Description
		Text  
			Given a code {\tt C} of length $n$ and a list {\tt L}
			(or an integer {\tt i}),
			returns a new code obtained from {\tt C} by selecting only
			those codewords of {\tt C} that have zeros in each of the
			coordinate positions in the list {\tt L} (or the
			position {\tt i}), and then deleting these positions.
		Text  
			The resulting code will have length $n - r$, where $r$ is the  
                       number of elements in {\tt L} (or 1 when the integer {\tt i} is 
                       used).
     Synopsis
    	Heading
		a code and a list are given
	BaseFunction
		shorten
	Usage
		shorten(LinearCode, List)
	Inputs
		C:LinearCode
		L:List
	Outputs
		:LinearCode
	Description
		Text  
			Given a code {\tt C} of length $n$ and a list {\tt L},
			returns a new code obtained from {\tt C} by selecting only
			those codewords of {\tt C} that have zeros in each of the
			coordinate positions in the list {\tt L},
			and then deleting these positions.
		Text
			The resulting code will have length $n - r$, where $r$ is the  
			number of elements in {\tt L}.
		Example
			F = GF(2);
	                codeLen = 10;
	                L = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 0, 1, 0,0}, {1, 1, 0, 0, 0, 1, 0, 0, 1, 0}, {1, 0, 0, 1, 0, 0, 0, 1, 1,1}};
                        C = linearCode(F,codeLen,L);
	                shorten(C, {3,6,8,9});
     Synopsis
    	Heading
		a linear code and an integer are given
	BaseFunction
		shorten
	Usage
		shorten(LinearCode, ZZ)
	Inputs
		C:LinearCode
	        i:ZZ
	Outputs
		:LinearCode
                        $C$
	Description
		Text  
			Given a code {\tt C} of length $n$ and a list {\tt L},
			returns a new code obtained from {\tt C} by selecting only
			those codewords of {\tt C} that have zero in the position
			{\tt i}), and then deleting this position.
		Text
			The resulting code will have length $n - 1$.
		Example
			F = GF(2);
	                codeLen = 10;
	                L = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 0, 1, 0,0}, {1, 1, 0, 0, 0, 1, 0, 0, 1, 0}, {1, 0, 0, 1, 0, 0, 0, 1, 1,1}};
                        C = linearCode(F,codeLen,L);
                        shorten(C, 3)   

///


doc ///
	Key
		randomCode
		(randomCode,GaloisField,ZZ,ZZ)
		(randomCode,QuotientRing,ZZ,ZZ)
	Headline
		constructs a random linear code over a finite field
	Usage
		randomCode(F,n,k)
		randomCode(QR,n,k)
	Inputs
		F:GaloisField
		QR:QuotientRing
		n:ZZ
		k:ZZ
	Outputs
		:LinearCode
	Description
		Text
		    Given a finite field {\tt F} (or a quotient ring {\tt QR}) and two positive
		    integers {\tt n} and {\tt k}, returns a random linear code over {\tt F} (or
		    {\tt QR}) of length $n$ and dimension at most $k$.
		Example
			F = GF(2, 4)
			C = randomCode(F,5,3)
			QR = ZZ/3
			C = randomCode(QR,5,3)
///

-*
doc ///
	Key
		(random,QuotientRing,ZZ,ZZ)
	Headline
		constructs a random linear code over a quotient ring
	Usage
		random(QR,n,k)
	Inputs
		QR:QuotientRing
		n:ZZ
		k:ZZ
	Outputs
		:LinearCode
			$C$
	Description
		Text
		    Given a quotient ring {\tt QR} and positive integers {\tt n} and {\tt k},
		    returns a random linear code $C$ over {\tt QR} of length $n$ and dimension at most $k$.
		Example
			QR = ZZ/3
			C = random(QR,5,3)
///
*-


doc ///
   Key
       (ring, LinearCode)
   Headline
       the ring of a code
   Usage
       ring LinearCode
   Inputs
        C:LinearCode
   Outputs
       :Ring
   Description
       Text
       	   Given a code {\tt C}, returns the ring that contains the entries of the
	   generator matrix of {\tt C}.
       Example
       	   C = hammingCode(2, 3)
	   ring(C)
///

doc ///
        Key
               (toString, LinearCode)
        Headline
                string with the vectors of a generator matrix of a code
        Usage
                toString C
        Inputs
                C:LinearCode
        Outputs
                :String    
        Description
                Text
                        Given a linear code {\tt C}, this function returns a string that 
                        contains the rows of a generator matrix of {\tt C}.
                Example
                               L = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 1, 1, 0,1, 0, 0}, {1, 1, 0, 0, 0, 1, 0, 0, 1, 0}, {1, 0, 0, 1, 0, 0, 0, 1, 1, 1}}
	                       C = linearCode(GF(2),L)
	                       S=toString C
///

doc ///
   Key
       (symbol ==,LinearCode,LinearCode)
   Headline
       determines if two linear codes are equal
   Usage
       LinearCode == LinearCode
   Inputs
        C1:LinearCode
	C2:LinearCode
   Outputs
       :Boolean
   Description
       Text  
       	   Given linear codes {\tt C1} and {\tt C2}, this function determines if they
	   define the same subspace over the same field or ring.
       Example
           F = GF(3,4)
           codeLen = 7; codeDim = 3;
           L = apply(toList(1..codeDim),j-> apply(toList(1..codeLen),i-> random(F)))
           C1 = linearCode(F,L)
	   C2 = linearCode(matrix L)
	   C1 == C2
///


doc ///
    Key
    	Strat
	[minimumWeight, Strat]
    Headline
    	Specify the algorithm used to perform a minimum weight computation.
    Usage
    	minimumWeight(C, Strat=>StratName)
    Inputs
    	C:LinearCode
	StratName:String
    Description
        Text
	    {\tt StratName} is the name of the desired algorithm to use.
	    By default, the function @TO "minimumWeight"@ uses the function @TO "chooseStrat"@ to estimate the optimal strategy for a 
	    given linear code. Specifying a strategy manually is not recommended in the majority of cases because @TO "chooseStrat"@ 
	    reliably chooses the best strategy based on approximations of performance.
	
            The valid options of the argument @TT "StratName"@ are:
	    
	    @UL {
	    	{BOLD {"MatroidPartition"}, ": The most advanced algorithm, but requires a longer up-front computation."},
	    	{BOLD {"OneInfoSet"},  ": An algorithm that is always faster than ", TT {"BruteForce"},"."},
		{BOLD {"BruteForce"}, ": (Not recommended) Determine the minimum weight by enumerating all codewords."}
	   	}@
	    	    
	    @TT "MatroidPartition"@ is the most advanced strategy, but requires a longer up-front computation. Specifically, it has to 
	    compute the matroid associated with the given linear code's generator matrix and then compute a partition of it into independent
	    sets. If such a partition exists, this algorithm will be strictly faster than @TT "OneInfoSet"@ after the matroid partition
	    has been computed.
	    
	    @TT "OneInfoSet"@ can be viewed as a direct improvement over the @TT "BruteForce"@ strategy. The properties of this algorithm
	    imply that it is always as fast or faster than @TT "BruteForce"@. 
	       
	    @TT "BruteForce"@ is the simplest and most reliable strategy, but also almost always the slowest. It is intended mainly for
	    internal purposes such as debugging and testing the other strategies.
        Example
	    C=hammingCode(2,3);
	    minimumWeight(C, Strat=>"BruteForce")
    SeeAlso
        chooseStrat
	       

///

doc ///
    Key
    	chooseStrat
	(chooseStrat,LinearCode)
    Headline
    	Estimate the optimal strategy to compute the minimum weight of a linear code. 
    Usage
    	chooseStrat C
    Inputs
    	C: LinearCode
    Outputs
    	:String
    Description
    	Text
       	    This function returns the name of the strategy that would be automatically chosen by function @TO "minimumWeight"@ if no
	    value of the optional argument @TT "Strat"@ is specified. 
	Example
	    chooseStrat(hammingCode(2,3))
	    F = GF(16);
	    chooseStrat(linearCode random(F^5, F^10))
    SeeAlso
    	[minimumWeight, Strat]
///

doc ///
    Key
    	minimumWeight
	(minimumWeight,LinearCode)
    Headline
    	computes the minimum weight of a linear code
    Usage
    	minimumWeight C
    Inputs
    	C: LinearCode
    Outputs
    	:ZZ
    Description
    	Text
    	    The minimum weight of a linear code $C$ is the minimum Hamming weight of its non-zero codewords. 
	    It is known that computing the minimum weight of a linear code is an NP-hard problem. 
	    	    
    	    If no value is specified for the optional argument @TT "Strat"@ is specified, 
	    the function @TO "chooseStrat"@ is used internally to choose a strategy.
	    Usually it is best not to specify a value of the optional strategy argument, because a strategy is
	    automatically chosen based on built-in approximations of performance. 
	Example
    	    C=hammingCode(2,3);
	    minimumWeight C
	    minimumWeight(C, Strat=>"BruteForce")
    SeeAlso
        weight
    Subnodes
        :Related functions
	chooseStrat
	Strat

///

doc ///
        Key
               shortestPath
               (shortestPath, Digraph, Thing, List)
        Headline
                shorthest path in a digraph
        Usage
                shortestPath(D,start,finishSet)
        Inputs
                D:Digraph
	        start:Thing
	        finishSet:List
        Outputs
                :List
        Description
                Text  
			Given a digraph {\tt D}, a vertex {\tt start} in {\tt D}, and
			a list of vertices {\tt finishSet} of {\tt D}, this function
			returns the shortest path in {\tt D} from {\tt start} to
			{\tt finishSet}.
		Text
                        It is safe to use this in applications that have nothing to do     
                        with coding theory.
		Example
			D = digraph({x,y,z,u,v}, matrix {{0,1,0,1,0},{0,0,1,0,0},{0,0,0,1,1},{0,0,0,0,0},{0,0,0,0,0}});
	                shortestPath (D,x,{z,v})
///

doc ///
	Key
		enumerateVectors
		(enumerateVectors, Ring, List)
	Headline
		a particular way to enumerate vectors over a finite field
	Usage
		enumerateVectos(F, L)
	Inputs
		L:List
		F:GaloisField
	Outputs
		:List
	Description
		Text
			Given a $0$, $1$ valued list {\tt L}, this function returns
			the list with all the possible ways to replace the
			one values in {\tt L}, with a nonzero element of the
			finite field {\tt F}.
		Example
			F = GF(3);
	                enumerateVectors(F, {1,0,1,0,1})
///

doc ///
	Key
		randLDPC
		(randLDPC, ZZ, ZZ, RR, ZZ)
	Headline
		low density parity check matrix
	Usage
		randLDPC(n, k, m, b)
	Inputs
		n:ZZ
		k:ZZ
                m:RR
                b:ZZ
	Outputs
		:Matrix
			$H$
	Description
		Text
			The parameter {\tt n} indicates the number of columns of $H$. The 
			number of rows of $H$ is {\tt n}-{\tt k}. The real number
			{\tt m} indicates the 
			slope of the line which relates {\tt n} and the number of ones in 
			$H$. Finally, {\tt b} indicates the constant term of the line 
			which relates $n$ and the number of ones in $H$. The number of 
			ones in $H$ is determined by the formula
			floor({\tt  n}*{\tt m}) + {\tt b}.
			Since this formula is linear in the number    
			of columns of $H$, {\tt randLDPC} produces a sparse 
			matrix, for a fixed set of parameters {\tt n}, {\tt k}, {\tt m} 
			and {\tt b}.

		Example
			randLDPC(15,5,3.0,0)
    
///

doc ///
	Key
		randNoRepeats
		(randNoRepeats,ZZ,ZZ)
	Headline
	         list of random integers from a specified range with no repetitions  
	Usage
		randNoRepeats(n,k)
	Inputs
		n:ZZ 
	        k:ZZ
	Outputs
		:List
			
	Description
		Text
			Given the integers {\tt k} and {\tt k}, this function returns a
			list of {\tt k} random integers between $0$ and {\tt n}
			(inclusive) with no repetitions.
		Text
                        It is safe to use this in applications that have nothing to do     
                        with coding theory.
		Example
			randNoRepeats(10,4)
			randNoRepeats(0,1)
			randNoRepeats(25,5)
///

doc ///
        Key
               vNumber
               (vNumber,Ideal)
        Headline
                the v-number of a graded ideal
        Usage
                vNumber(I)
        Inputs
                 I:Ideal
        Outputs
                :ZZ
                           
        Description
                Text  
			Returns the v-number of a graded ideal {\tt I}.
			This invariant is used to express the regularity index of the 
			generalized minimum distance function of the ideal {\tt I}
			on the parameter $r=1$ @TO genMinDisIdeal@. Moreover,
			the v-number has other combinatorial implications.
			More information about the v-number
			can be found in Definition 4.1 at        
			\url{https://arxiv.org/pdf/1812.06529v1.pdf}.
		Example
			       K=ZZ/3;
                               R=K[t3,t2,t1,MonomialOrder=>Lex];
                               I=ideal(t1*t2^2-t1^2*t2,t1*t3^3-t1^3*t3,t2*t3^3-t2^3*t3);
                               vNumber(I)
 

///
 

doc ///
	Key
		footPrint
                (footPrint,ZZ,ZZ,Ideal)
	Headline
		generalized footprint function of an ideal
	Usage
		footPrint(d,r,I)
	Inputs
		I:Ideal
                d:ZZ
                r:ZZ
	Outputs
		:ZZ
	Description
		Text
			Returns the value of the generalized footprint function of the ideal {\tt I}
			on the parameters {\tt d} and {\tt r}.
                        Given an ideal {\tt I}, a monomial is called standard of {\tt I} if it is not a
			leading monomial of any polynomial of {\tt I}.
                        The parameters {\tt d} and {\tt r} are used as follows. The function computes the
			degree of the ideal generated by sets of {\tt r}
                        standard monomials of degree at most {\tt d}.
                        The footprint function is a lower bound of the
			generalized minimum function @TO genMinDisIdeal@. More information about the
			footprint function
			can be found in Definition 1.3 at \url{https://arxiv.org/pdf/1812.06529v1.pdf}
		Example
			K=QQ;
                        R=K[t1,t2,t3];
                        I=ideal(t1^3,t2*t3);
                        footPrint(2,3,I)
///
 

    
doc ///
        Key
               hyp
               (hyp,ZZ,ZZ,Ideal)
        Headline
                hyp function of an ideal
        Usage
                hyp(d,r,I)
        Inputs
                d:ZZ
                r:ZZ
                I:Ideal
        Outputs
                :ZZ
                           
        Description
                Text
			Returns the value of the
			hyp function (HypF) of the ideal {\tt I}
			on the parameters {\tt d} and {\tt r}. The HypF
			computes the maximum degree of the ideals
			generated by {\tt r}-tuples of 
			polynomials of degree at most {\tt d} that are linearly
			independent modulo the ideal {\tt I}.
			Finding upper bounds for this functions is 
			equivalent to finding lower bounds for the generalized minimum 
			distance function @TO genMinDisIdeal@. 
			More information about the Hyp
			can be found in Definition 1.2 at        
			\url{https://arxiv.org/pdf/1812.06529v1.pdf}.
		Example
			      K=ZZ/3;
                              R=K[t1,t2,t3,t4,t5,t6];
                              I=ideal(t1*t6-t3*t4,t2*t6-t3*t5);
                              hyp(1,1,I)
 

///
 
doc ///
        Key
               genMinDisIdeal
               (genMinDisIdeal,ZZ,ZZ,Ideal)
        Headline
                generalized minimum distance function of an ideal
        Usage
                genMinDisIdeal(d,r,I)
        Inputs
                d:ZZ
                r:ZZ
                I:Ideal
        Outputs
                :ZZ
                           
        Description
                Text
			Returns the value of the
			generalized minimum distance function (GMDF) of the ideal {\tt I}
			on the parameters {\tt d} and {\tt r}. The GMDF
			generalizes the Hamming weights
			of Reed-Muller-type codes.
			The integers {\tt d} and {\tt r} indique  
			that the function is computing the degree of
			the ideal generated by {\tt r}-tuples of 
			polynomials of degree at most {\tt d} that are linearly
			independent modulo
			the ideal {\tt I}. More information about the GMDF
			can be found in Definition 1.1 at        
			\url{https://arxiv.org/pdf/1812.06529v1.pdf}.
		Example
			       K=ZZ/3; 
                               R=K[t1,t2,t3,t4,t5,t6];
                               I=ideal(t1*t6-t3*t4,t2*t6-t3*t5);
                               genMinDisIdeal(1,1,I)
///

doc ///
        Key
               vasconcelosDegree
               (vasconcelosDegree,ZZ,ZZ,Ideal)
        Headline
                Vasconcelos function of an ideal
        Usage
                vasconcelosDegree(d,r,I)
        Inputs
                d:ZZ
                r:ZZ
                I:Ideal
        Outputs
                :ZZ
                           
        Description
                Text
			Returns the value of the
			Vasconcelos function of the ideal {\tt I}
			on the parameters {\tt d} and {\tt r}.
			In the case of a graded unmixed radical ideal {\tt I}, the Vasconcelos
			function is equal to the generalized minimum distance function
			@TO genMinDisIdeal@. The Vasconcelos function computes the minimum value
			of the degree of {\tt I} minus the degree of the ideal
			generated by {\tt r}-tuples of polynomials of degree at
			most {\tt d} that are linearly independent
			modulo the ideal {\tt I}. More information about the Vasconcelos function
			can be found in Definition 3.4 at        
			\url{https://arxiv.org/pdf/1812.06529v1.pdf}.
		Example
			     K=QQ; 
                             R=K[t1,t2,t3];
                             I=ideal(t1^3,t2*t3);
                             vasconcelosDegree(1,1,I)
///

doc ///
	Key
		alphabet
		(alphabet,LinearCode)
	Headline
		elements of the base ring of a code
	Usage
		alphabet(C)
	Inputs
		C:LinearCode
	Outputs
		:List
	Description
		Text
			Given a linear code {\tt C}, the function returns
			the list of all elements of the ring $R$, where $R$ is the 
			ring containing the entries of the
			generator matrix of {\tt C}.
		Example
			F=GF(4, Variable=>a);
			C=linearCode(matrix{{1,a,0},{0,1,a}});
    	    	    	alphabet(C)
///


doc ///
	Key
		ambientSpace
		(ambientSpace,LinearCode)
	Headline
		recovers the ambient module of a code
	Usage
		ambientSpace C
	Inputs
		C: LinearCode
	Outputs
		:Module
	Description
		Text
			Given a code {\tt C} of length $n$ over the field $F$, this function
			extracts the ambient module $F^n$, which is the value stored under the
			key {\tt AmbientModule} of the hash table {\tt C.LinearCode}.
		Example
			F=GF(4,Variable=>a);
                        C=linearCode(matrix{{1,a,0},{0,1,a}});
                        ambientSpace C
	Subnodes
	 :Related functions and symbols:
	 alphabet
	 AmbientModule
	 BaseField
	 field
	 vectorSpace
///

doc ///
	Key
		codewords
		(codewords, LinearCode)
	Headline
		codewords of the code
	Usage
		codewords(C)
	Inputs
		C:LinearCode
	Outputs
		:List
	Description
		Text
			Obtains all the codewords of a code {\tt  C} by multiplying all
			the elements of the ambient space (obtained with the function
			messages) by the generator matrix of {\tt C}.

		Example
			F=GF(4,Variable=>a);
			C=linearCode(matrix{{1,a,0},{0,1,a}});
			codewords(C)
///

doc ///
	Key
		cyclicMatrix
		(cyclicMatrix, List)
		(cyclicMatrix, GaloisField, List)
	Headline
		cyclic matrix
	Usage
		cyclicMatrix(v)
		cyclicMatrix(F,v)
	Inputs
		v:List
		F:GaloisField
	Outputs
		:Matrix 
                         $M$
		
	Description
		Text
			A cyclic matrix (also known as circulant matrix) is a square matrix
			whose every row (starting from the second) is the right cyclic shift by one entry of the previous row.
			Below we present ways to define a cyclic matrix $M$.
    Synopsis
    	Heading
		a list is given
	BaseFunction
		cyclicMatrix
	Usage
		cyclicMatrix(v)
	Inputs
		v:List
	Outputs
		:Matrix
			$M$
	Description
		Text
			Assume that $v_1,\ldots,v_n$ are the entries of {\tt v}. The
			output is an $n\times n$ matrix. The entries of row 1 are
			$v_1,\ldots,v_n$, the entries of row 2 are $v_2,\ldots,v_n,v_1$,
			in general,
			the entries of row $i$ are $v_i,\ldots,v_n,v_1,\ldots, v_{i-1}$.
		Example
			v=toList(1,0,2,0);
			M =cyclicMatrix(v)            
    Synopsis
    	Heading
		a finite field and a list are given
	BaseFunction
		cyclicMatrix
	Usage
		cyclicMatrix(v,F)
	Inputs
		v:List
		F:GaloisField
	Outputs
		:Matrix
			$M$
	Description	    
		Text
			Assume that $v_1,\ldots,v_n$ are the entries of {\tt v}. The
			output is an $n\times n$ matrix over {\tt F}. The entries of
			row 1 are $v_1,\ldots,v_n$, the entries of row 2 are $v_2,\ldots,v_n,v_1$,
			in general,
			the entries of row $i$ are $v_i,\ldots,v_n,v_1,\ldots, v_{i-1}$.
		Example
			F=GF(4,Variable=>a);
                        v={0,1,a};
                        M=cyclicMatrix(F,v)                     
///

doc ///
	Key
		dualCode
		(dualCode,LinearCode)
	Headline
		dual of a code
	Usage
		dualCode(C)
	Inputs
		C:LinearCode
	Outputs
		:LinearCode
	Description
		Text    
			The dual of a code {\tt C} of length $n$ over the field $F$ is the code
			whose codewords are the elements $v$ in $F^n$ such that for any $c$ in {\tt C},
			the inner product $<c,v>$ is equal to zero. This function returns
			the dual of {\tt C}.																																																																																																																																																																																																																																																																																																																																																																																																																																								
		Example
			F=GF(4,Variable=>a);
                        C=linearCode(matrix{{1,a,0},{0,1,a}});
                        D=dualCode(C)
///


doc ///
	Key
		field
		(field, LinearCode)
	Headline
		the field of a code
	Usage
		field (C)
	Inputs
		C:LinearCode
	Outputs
		:Ring
	Description
		Text
			Given a code {\tt C}, returns the field (or ring) that contains
			the entries of the generator matrix of {\tt C}.
		Example
			F=GF(4,Variable=>a);
			C=linearCode(matrix{{1,a,0},{0,1,a}});
			field C
///

doc ///
    Key
    	genericCode
	(genericCode, LinearCode)
    Headline
    	ambient space of a code
    Usage
    	genericCode(C)
    Inputs
    	C:LinearCode
    Outputs
    	:LinearCode
    Description
    	Text
	    Given a code {\tt C} over a finite field $F$ of length $n$,
	    returns the code $F^n$.
	Example
	    F=GF(4,Variable=>a);
	    C=linearCode(matrix{{1,a,0},{0,1,a}})
	    genericCode(C)
///



doc ///
        Key
               (dim,LinearCode)
        Headline
                dimension of a linear code
        Usage
                dim C
        Inputs
                C:LinearCode
        Outputs
                :Number
        Description
                Text
                        Given a linear code {\tt C}, returns the dimension of    
                        {\tt C}. The dimension of {\tt C} is defined as the 
                        dimension of {\tt C} as a vector space.
		Example
	                       C = linearCode(GF(2),{{1,1,0,0},{0,0,1,1}});
	                       dim C
	                       H = hammingCode(2,3)
	                       dim H
///

doc ///
    Key
    	informationRate
    	(informationRate,LinearCode)
    Headline
    	information rate of a code
    Usage
    	informationRate C
    Inputs
    	C: LinearCode
    Outputs
    	:QQ
    Description
    	Text
	    Given a linear code {\tt C} of length $n$ and dimension $k$, this function returns
	    the number $\frac{k}{n}$, which is known as the information rate of {\tt C}.
	Example
	    C=linearCode(GF(4),{{1,1,1,1}});
	    informationRate C
	Example
	    H=hammingCode(2,3);
	    informationRate H
    	Text
	    The next is an example for the class of @TO EvaluationCode@.
	Example
	    RM=reedMullerCode(2,3,1);
	    informationRate(RM.LinearCode)
///

doc ///
        Key
               (size,LinearCode)
        Headline
                gives the number of codewords in a linear code
        Usage
                size C
        Inputs
                C:LinearCode
        Outputs
                :ZZ       
        Description
                Text
                        Given a linear code {\tt C} of dimension $k$ over a 
                        Galois field with $q$ elements, returns 
                        the number of codewords in {\tt C}, which is $q^k$.
    		Text
		    This method is provided by the package @TO CodingTheory@.
		Example
                               C = linearCode(GF(4),{{1,1,1,1}});
	                       size C
	                       H = hammingCode(2,3);
	                       size H
	                       F = GF(4,Variable=>a);
	                       L = {{1,a,a+1},{a+1,1,a},{a,a+1,1},{1,0,1}};
	                       C = linearCode(F,L);
	                       size C			       
                Text
                        The next example illustrates how to use this function for the class of
			Evaluation Codes.
		Example
	                       RM = reedMullerCode(2,3,1);
	                       size RM.LinearCode
///

doc ///
        Key
               (length,LinearCode)
        Headline
                returns the length of a linear code
        Usage
                length C
        Inputs
                C:LinearCode
        Outputs
                :ZZ
                           
        Description
                Text
                        Given a linear code {\tt C} over a Galois field,
			returns the number of entries in any codeword in {\tt C}.
                        This parameter is called the length of the code.			
    		Text
		    This method is provided by the package @TO CodingTheory@.
		Example
	                       C = linearCode(GF(4),{{1,1,1,1}});
	                       length C
	                       H = hammingCode(2,3);
	                       length H			       
                Text
                        The next example illustrates how to use this function for the class of
			Evaluation Codes.
		Example
	                       RM = reedMullerCode(2,3,1);
	                       length RM.LinearCode
///

doc ///
    Key
    	vectorSpace
	(vectorSpace,LinearCode)
    Headline
    	vector space of a code
    Usage
    	vectorSpace C
    Inputs
    	C:LinearCode
    Outputs
    	:Module
	    $V$
    Description
    	Text
	    Given a linear code {\tt C}, this function returns $V$, the vector
	    space spanned by the rows of a generator matrix of {\tt C}.
    	Example
	    H = hammingCode(2,3);
	    vectorSpace H
    	Text
	    The next is an example for the class of @TO EvaluationCode@.
       	Example
	    RM = reedMullerCode(2,4,1);
	    vectorSpace(RM.LinearCode)
///

doc ///
    Key
    	messages
	(messages,LinearCode)
    Headline
    	set of messages to be encoded by a code
    Usage
    	messages C
    Inputs
    	C:LinearCode
    Outputs
    	:List
    Description
    	Text
	    Given a code {\tt C} of dimension $k$ over a finite field $F$,
	    this function returns the list that contains all the elements of $F^k$.
	    Every element of the list can be used to encode a message using the linear code {\tt C}.
    	Example
	    F=GF(4,Variable=>a);
	    R=linearCode(F,{{1,1,1}});
	    messages R
    	Example
	    messages hammingCode(2,3)
	Example
	    RM=reedMullerCode(2,2,1);
	    messages(RM.LinearCode)
    	Text
            This method is provided by the package @TO CodingTheory@.
    SeeAlso
    	codewords
///

doc ///
	Key
		cyclicCode
		(cyclicCode, GaloisField, RingElement, ZZ)
                (cyclicCode, GaloisField, ZZ, ZZ)
                
	Headline
	       cyclic codes
	Usage
		cyclicCode(F, g, n)
                cyclicCode(F, m, n)
	Inputs
		F:GaloisField
		g:RingElement
		m:ZZ
                n:ZZ
                
	Outputs
		:LinearCode
			$C$
        Description 
                   Text
                         A linear code is called cyclic if $(a_{n},a_1,\ldots,a_{n-1})\in C$ for
			 all $(a_1,a_2,\ldots,a_n)\in C$.
                         A cyclic code can be defined by a polynomial.
	Subnodes
                   :Related functions:
		   cyclicMatrix
		   quasiCyclicCode
        Synopsis
             Heading
                  a polynomial is given
             BaseFunction
                  cyclicCode
             Usage    
                  cyclicCode(F,g,n)
             Inputs
                 F:GaloisField
                 g:RingElement
                 n:ZZ
	     Description
		  Text
			Given a finite field {\tt F}, an integer {\tt n},
			and a polynomial {\tt g} in {\tt F}$[x]\setminus${\tt F} that is a
			divisor of $x^n-1$, this function returns the cyclic code $C$
			with generating polynomial {\tt g} and length {\tt n}.	

		  Text
			If the polynomial {\tt g} is not a divisor of $x^n-1$,
			the function returns a code with a circulant matrix as
			generator matrix.
		  Example
			F=GF(5);
			R=F[x];
			g=x-1;
			C=cyclicCode(F,g,8)
        Synopsis
             Heading 
                  a constant polynomial is given
             BaseFunction
                  cyclicCode
             Usage
                  cyclicCode(F, m, n)
             Inputs 
                 F:GaloisField
                 m:ZZ
                 n:ZZ
             Description
                  Text
                       If {\tt m} is a nonzero constant, then this function returns
                       the universal code of length {\tt n} over the field {\tt F}.
                  Text 
                       If {\tt m} is zero, then this function returns the zero code
                       of length {\tt n}.
                  Example
                       F=GF(5);
                       R=F[x];
                       C=cyclicCode(F,0,5)
                       C=cyclicCode(F,2,5)
///

doc ///
	Key
		quasiCyclicCode
		(quasiCyclicCode,List)
		(quasiCyclicCode,GaloisField,List)
	Headline
		constructs a quasi-cyclic code
	Usage
		quasiCyclicCode(L)
		quasiCyclicCode(F,L)
	Inputs
		L:List
		F:GaloisField
	Outputs
		:LinearCode
			$C$
	Description
		Text
			We present below the ways in how a quasi-cyclic
			code $C$ can be defined.
    Synopsis
    	Heading
		a list is given
	BaseFunction
		quasiCyclicCode
	Usage
		quasiCyclicCode(L)
	Inputs
		L:List
	Outputs
		:LinearCode
			$C$
	Description
		Text
			{\tt L} is a list of vectors.
			Every vector \(v_i\) in {\tt L} generates a cyclic matrix $A_i$.
			Returns the quasi-cyclic code $C$ whose generator matrix is the
			concatenation of the matrices $A_i$.
		Example
			F = GF(5);
			L = apply(toList(1..2),j-> apply(toList(1..4),i-> random(F)));
    	    	    	L
			C2=quasiCyclicCode(L)
    Synopsis
    	Heading
		a finite field and a list are given
	BaseFunction
		quasiCyclicCode
	Usage
		quasiCyclicCode(F,L)
	Inputs
		L:List
		F:GaloisField
	Outputs
		:LinearCode
			$C$
	Description	    
		Text
			{\tt L} is a list of vectors, whose entries belong to the
			field {\tt F}.
			Every vector \(v_i\) in {\tt L} generates a cyclic matrix $A_i$.
			Returns the quasi-cyclic code $C$ whose generator matrix is the
			concatenation of the matrices $A_i$.
		Example
			F = GF(5);
			L = apply(toList(1..2),j-> apply(toList(1..4),i-> random(F)));
    	    	    	L
			C2=quasiCyclicCode(F,L)
///
-----------------------------------------------
-----------------------------------------------
-- Use this section for Evaluation Code documentation:
-----------------------------------------------
-----------------------------------------------
doc ///
	Key
    	 EvaluationCode
	Headline
	 class of evaluation codes
	Description
	 Text
	  EvaluationCode is the class of linear codes obtained by evaluating polynomials in
	  $F[X_1,\ldots,X_m]$, where $F$ is a finite field, at a set of points in $F^m$. There are various
	  constructions of evaluation codes depending on how the polynomials and points are chosen.
	  Important examples include Reed-Solomon codes, Reed-Muller codes, monomial codes, Cartesian codes,
	  and toric codes. To construct a linear code, see @TO evaluationCode@.
	 Text
	  The basic structure is a hash table. One of the values is the resulting linear code of type
	  @TO LinearCode@. Other values include the set of points, its vanishing ideal,
	  the set of polynomials, and more.
	 Example
	  F=GF(4);
	  R=F[x,y];
	  P={{0,0},{1,0},{0,1},{a,a}};
	  S={x+y,x^2+y^2, a+x*y^2};
	  C=evaluationCode(F,P,S);
	  C.VanishingIdeal
	  C.PolynomialSet
	  C.LinearCode
	  length C.LinearCode
	  dim C.LinearCode
	Subnodes
	 :Functions related to ideals and evaluation codes
	  evaluationCode
	  footPrint
	  genMinDisIdeal
	  hyp
	  vasconcelosDegree
	  vNumber
	 :Symbols that are used as a key for storing information of an evaluation code
	  PolynomialSet
	  VanishingIdeal
///

doc ///
	Key
	    locallyRecoverableCode
	    (locallyRecoverableCode,List,List,RingElement)
	Headline
	    constructs a locally recoverable code (LRC)
	Usage
	    locallyRecoverableCode(L,A,g)
	Inputs
	    L:List
	    A:List
	    g:RingElement
	Outputs
	    :LinearCode
	    	$C$
	Description
	    Text
	    	{\tt L} is the list $\{q,n,k,r\},$ where $q$ is a prime power, and $n$, $k$ and $r$ are
		positive integers. {\tt A} is a list that contains lists of elements of the field
		{\tt GF(q)}. Every sublist contains different elements of {\tt GF(q)}.
		The intersection between every two sublists is empty. The polynomial {\tt g} is "good",
		which means
		that is constant on every sublist of {\tt A}. This function
	    	generates an LRC code $C$ of length $n$, dimension $k$, and locality $r$, over
		{\tt GF(q)}. This code $C$ has the property that for every $1\leq i \leq n$,
		there exist $i_1,\ldots,i_r$ such that for every codeword $c$ in $C$, the entry $c_i$ 
		can be recovered from the entries $c_{i_1},...,c_{i_r}$. This construction was introduced
		by Tamo and Barg in the paper {\it A family of optimal locally recoverable codes}:
		\url{https://arxiv.org/pdf/1311.3284v2.pdf}.
	    Example
	    	A={{1,3,9},{2,6,5},{4,12,10}}
		R=(ZZ/13)[x]
		g=x^3
		locallyRecoverableCode({13,9,4,2},A,g)
	Subnodes
	 :Related function:
	 getLRCencodingPolynomial
///

doc ///
	Key
		evaluationCode
		(evaluationCode, Ring, List, List)
		(evaluationCode, Ring, List, Matrix)
	Headline
		functions to construct evaluation codes over Galois fields
	Usage
		evaluationCode(F,P,S)
		evaluationCode(F,P,M)
	Inputs
		F:Ring
		P:List
		S:List
		M:Matrix
	Outputs
		:EvaluationCode
			$C$
	Description
		Text
			We present below the ways in how an evaluation
			code $C$ can be defined.
	Subnodes
	 :Constructions of evaluation codes
	  evCodeGraph
	  cartesianCode
	  orderCode
	  reedSolomonCode
	  reedMullerCode
	  toricCode
	Caveat
		While this function may work even when a ring is given,
		instead of a finite field, it is possible that the results
		are not the expected ones.
    Synopsis
    	Heading
		a ring and a two lists are given
	BaseFunction
		evaluationCode
	Usage
		evaluationCode(F,P,S)
	Inputs
		F:Ring
		P:List
		S:List
	Outputs
		:EvaluationCode
			$C$
	Description
		Text
			Given a finite field {\tt F}, an ordered list {\tt P}
			of points in {\tt F}$^m$, and an ordered list {\tt S}
			of polynomials over {\tt F} in $m$ variables,
			this method produces an {\tt EvaluationCode} $C$ generated
			by codewords obtained by evaluating the given polynomials in
			{\tt S} at the given points in {\tt P}.
		Example
			F=GF(4);
                        R=F[x,y,z];
	                P={{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,1,1},{a,a,a}};
                        S={x+y+z,a+y*z^2,z^2,x+y+z+z^2};
	                C=evaluationCode(F,P,S)
    Synopsis
    	Heading
		a ring, a list and a matrix are given
	BaseFunction
		evaluationCode
	Usage
		evaluationCode(F,P,M)
	Inputs
		F:Ring
		P:List
		M:Matrix
	Outputs
		:EvaluationCode
			$C$
	Description
		Text
			Given a finite field {\tt F}, an ordered list {\tt P}
			of points in {\tt F}$^m$, and a matrix {\tt M} with
			$m$ columns, this method produces a linear code $C$
			generated by the codewords obtained by evaluating the
			monomials defined by the rows of {\tt M} at the given
			points in {\tt P}.
		Example
			F=GF(4);
                        R=F[x,y,z];
	                P={{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,1,1},{a,a,a}};
                        M=matrix{{0,0,1},{1,1,1}};
	                C=evaluationCode(F,P,M)
///

doc ///
        Key
		toricCode
		(toricCode,Ring,Matrix)
        Headline
		a toric code construction
        Usage
                toricCode(F,M)
        Inputs
                F:Ring
                M:Matrix
        Outputs
                :EvaluationCode
                            $C$
        Description
                Text  
       	                  Given a finite field {\tt F} and an integer matrix {\tt M}, this 
                          method produces a toric code whose lattice polytope $P$ is the 
                          convex hull of the row vectors of {\tt M}. By definition, the 
                          toric code is generated by codewords obtained by evaluating the 
                          monomials corresponding to the lattice points of $P$ at the 
                          points of the algebraic torus ({\tt F}*)$^m$, where $m$ is the 
                          number of columns of {\tt M}. 
     
		Example
			   M=matrix{{1,4},{2,5},{10,6}};
	                   T=toricCode(GF 4,M);
	                   T.VanishingIdeal
	                   T.ExponentsMatrix
	                   T.LinearCode
	                   length T.LinearCode
	                   dim T.LinearCode
	Subnodes
	 :Symbols that are used as a key for storing information of a toric code
	  ExponentsMatrix
///

doc ///
	Key
	        cartesianCode
		(cartesianCode,Ring,List,List)
		(cartesianCode,Ring,List,ZZ)
                (cartesianCode,Ring,List,Matrix)
	Headline
		Cartesian code
	Usage
		cartesianCode(F,L,d)
		cartesianCode(F,L,S)
		cartesianCode(F,L,M)
	Inputs
		F:Ring
		L:List
		S:List
		d:ZZ
		M:Matrix
	Outputs
		:EvaluationCode
			$C$
	Description
		Text
			We present below the ways in how a Cartesian
			code $C$ can be defined.
	Caveat
		While this function may work even when a ring is given,
		instead of a finite field, it is possible that the results
		are not the expected ones.
	Subnodes
	 :Symbols that are used as a key for storing information of a Cartesian code
	  Sets
    Synopsis
	Heading
		a ring, a list and an integer are given
	BaseFunction
		cartesianCode
	Usage
		cartesianCode(F, L, d)
	Inputs
		F:Ring
		L:List
		d:ZZ
	Outputs
		:EvaluationCode
			$C$
	Description	    
		Text
			{\tt F} is a field, {\tt L}  is a list of subsets of
			{\tt F} and {\tt d} is an integer. Returns the Cartesian code $C$
		 	obtained when polynomials of degree at most {\tt d} are evaluated
			over the points of the Cartesian product made by the subsets of
			{\tt L}.
		Example
			C=cartesianCode(ZZ/11,{{1,2,3},{2,6,8}},3)
    Synopsis
    	Heading
		a ring and two lists are given
	BaseFunction
		cartesianCode
	Usage
		cartesianCode(F, L, S)
	Inputs
		F:Ring
		L:List
		S:List
	Outputs
		:EvaluationCode
			$C$
	Description	    
		Text
			{\tt F} is a field, {\tt L}  is a list of subsets of {\tt F} and
			{\tt S} is a set of polynomials. Returns the Cartesian code $C$
			obtained when polynomials in the list {\tt S} are evaluated over
			the points of the Cartesian product made by the subsets of {\tt L}.
		Example
			F=GF(4);
			R=F[x,y];
			C=cartesianCode(F,{{0,1,a},{0,1,a}},{1+x+y,x*y})
			C.LinearCode
    Synopsis
    	Heading
		a ring, a list and a Matrix are given
	BaseFunction
		cartesianCode
	Usage
		cartesianCode(F, L, M)
	Inputs
		F:Ring
		L:List
                M:Matrix
	Outputs
		:EvaluationCode
			$C$
	Description	    
		Text
			{\tt F} is a field, {\tt L} is a list of subsets of {\tt F} and
			{\tt M} is the matrix whose rows are the exponents of the monomials
			to evaluate. Returns the Cartesian code $C$ obtained when the
			monomials defined by the matrix {\tt M} are evaluated over
			the points of the Cartesian product made by the subsets of {\tt L}.
		Example
			F=GF(4);
			R=F[x,y];
			C=cartesianCode(F,{{0,1,a},{0,1,a}},matrix{{1,2},{2,3}})
///

doc ///
	Key
		reedMullerCode
		(reedMullerCode,ZZ,ZZ,ZZ)
	Headline
	        constructs the Reed-Muller code  
	Usage
		reedMullerCode(q,m,d)
	Inputs
		q:ZZ
                m:ZZ
                d:ZZ
	Outputs
		:EvaluationCode
	Description
		Text
			Given the integers {\tt q}, {\tt m} and {\tt d}, returns the
			Reed-Muller code obtained when polynomials in {\tt m} variables
			of total degree at most {\tt d} are evaluated on the points
			of {\tt GF(q)}$^\mathtt{m}$.
		Example
			 C=reedMullerCode(2,3,4);
	                 C.Sets;
	                 C.VanishingIdeal;
	                 C.PolynomialSet;
	                 C.LinearCode;
	                 length C.LinearCode
///

doc ///
	Key
		reedSolomonCode
		(reedSolomonCode,Ring,List,ZZ)
	Headline
	         constructs the Reed-Solomon code  
	Usage
		reedSolomonCode(F,L,k)
	Inputs
		F:Ring
                L:List
                k:ZZ
	Outputs
		:EvaluationCode
	Description
		Text
			Given a field {\tt F}, a list {\tt L} of different elements
			of {\tt F}, and an integer {\tt k}, returns the Reed-Solomon
			code obtained when polynomials of degree less than {\tt k}
			are evaluated on the elements of {\tt L}.
		Example
			 C=reedSolomonCode(ZZ/31,{1,2,3},3);
	                 peek C
///

doc ///
	Key
		generatorToParityCheck
		(generatorToParityCheck, Matrix)
	Headline
		parity check matrix of a linear code
	Usage
		generatorToParityCheck(G)
	Inputs
		G:Matrix
	Outputs
		:Matrix
			$H$
	Description
		Text
			Constructs a parity check matrix $H$ of the linear code
			generated by {\tt G}.
		Example
			F = GF 2;
			L = {{0,1,1,0},{1,0,1,0},{0,0,0,1}};
			G = matrix apply(L, codeword->apply(codeword, en -> sub(en,F)))
			H = generatorToParityCheck G
			K = GF(8,Variable => a);
			G = matrix {{1,0,0,a,0,1,1,a},{0,0,0,1,1,1,1,0},{1,1,0,0,0,1,0,0},{1,0,1,0,0,1,1,0}}
			H = generatorToParityCheck G
///

doc ///
    Key
    	orderCode
	(orderCode,Ring,List,List,ZZ)
	(orderCode,Ideal,List,List,ZZ)
	(orderCode,Ideal,List,ZZ)
    Headline
    	computes an order code for a given weight
    Usage
    	orderCode(F,P,v,d)
	orderCode(I,P,v,d)
	orderCode(I,v,d)
    Inputs
    	F:Ring
	P:List
	v:List
	I:Ideal
	d:ZZ
    Outputs
    	:EvaluationCode
	    $C$
    Description
    	Text
	    Let {\tt F} be a finite field and {\tt F}$[t_1,\ldots,t_m]$
	    a polynomial ring with a weight order defined by the list {\tt v} of size $m$.
	    For {\tt P}$=\{P_1,\ldots,P_n\}\subset${\tt F}$^m$, the order
	    code of degree $d$ is the {\tt F}-vector space generated
	    by the vectors $(f(P_1),\ldots,f(P_n))$,
	    where $f$ is a monomial of weight at most $d$.
	    We describe ways to obtain an order code below.
    Caveat
	While this function may work even when a ring is given,
	instead of a finite field, it is possible that the results
	are not the expected ones.
    Synopsis
    	Heading
	    a list of points and the  weight vector are given
	BaseFunction
	    orderCode
	Usage
	    orderCode(F,P,v,d)
	Inputs
	    F:Ring
	    P:List
	    v:List
	    d:ZZ
	Outputs
	    :LinearCode
	    	$C$
	Description
	    Text
	    	The order code $C$ of degree {\tt d} over the points of {\tt P} using the weight vector {\tt v}.  
	    Example
	    	F = GF(4);
		P = {{0, 0}, {a, a}, {a+1, a}, {1, a}, {a, a+1}, {a+1, a+1}, {1, a+1}, {0, 1}};
		C = orderCode(F,P,{2,3},7);
		peek C
    Synopsis
    	Heading
	    given the ideal of the finite algebra associated to the order function and a list of points
	BaseFunction
	    orderCode
	Usage
	    orderCode(I,P,v,d)
	Inputs
	   I: Ideal
	   P: List
	   v: List
	   d: ZZ
	Outputs
	    :LinearCode
	    	$C$
	Description
	    Text
	    	If {\tt I} is the ideal associated to the semigroup generated by {\tt v},
		this function allows us to improve by knowing a basis defined through {\tt I}.
	    Example
	    	F = GF(4);
		R = F[x,y];
		I = ideal(x^3+y^2+y)
		P = {{0, 0}, {a, a}, {a+1, a}, {1, a}};
		C = orderCode(I,P,{2,3},7);
		peek C
    Synopsis
    	Heading
	    given just an ideal and the weight vector
    	BaseFunction
	    orderCode
	Usage
	    orderCode(I,v,d)
	Inputs
	    I:Ideal
	    v:List
	    d:ZZ
	Outputs
	    :LinearCode
	    	$C$
	Description
	    Text
	    	The order code of degree {\tt d}, using the order function defined by {\tt v} and the set of points the zeroes of {\tt I}.
	    Example
	    	F = GF(4);
		R = F[x,y];
		I = ideal(x^3+y^2+y);
		C = orderCode(I,{2,3},7);
		peek C
///


doc ///
	Key
		getLRCencodingPolynomial
		(getLRCencodingPolynomial, ZZ,ZZ, List, RingElement)
    Headline
    	encoding polynomial for an LRC code
    Usage
    	getLRCencodingPolynomial(k,r,List,informationList,g)
    Inputs
    	k:ZZ
	r:ZZ
	informationList: List
	g:RingElement
    Outputs
    	:RingElement
	    $f(x)$
    Description
    	Text
	    Generates an encoding polynomial $f(x)$ corresponding to an information vector
	    in $F^\texttt{k}$, where $F$ is a field, which can be used to generate an
	    encoding in $F^\texttt{r}$.
	    
	Example
	    R=ZZ/(13)[x];
	    getLRCencodingPolynomial(4,2,{1,0,1,1},x^3)
///

doc ///
	Key
		evCodeGraph
		(evCodeGraph,Ring,Matrix,List)
	Headline
		Reed–Muller-type code over a graph
	Usage
		evCodeGraph(F,M,S)
	Inputs
		F:Ring
		M:Matrix
		S:List
	Outputs
		:EvaluationCode
			$C$
	Description
		Text
			Given a finite field {\tt F}, an incidence matrix {\tt M}
			of a connected graph $G$, and an ordered list {\tt S} of
			polynomials over {\tt F}, this method produces an
			{\tt EvaluationCode} $C$ generated by evaluating the given
			polynomials in {\tt S} at the columns of the matrix {\tt M}.
		Example
			G = graph({1,2,3,4}, {{1,2},{2,3},{3,4},{4,3}});
			B=incidenceMatrix G;
			S=ZZ/2[t_(0)..t_(#vertexSet G-1)];
			Y=evCodeGraph(coefficientRing S,B,flatten entries basis(1,S))
	Caveat
		While this function may work even when a ring is given,
		instead of a finite field, it is possible that the results
		are not the expected ones.
///

--------------- Documentation PolynomialSet-----------------
doc ///
    Key
    	PolynomialSet
    Headline
    	a set of polynomials for an evaluation code
    Usage
    	C.PolynomialSet
    Inputs
    	C:EvaluationCode
    Outputs
    	:Set
    Description
    	Text
	    This key stores a polynomial set used to construct an
	    @TO EvaluationCode@.
    	Text 
	    This symbol is provided by the package @TO CodingTheory@.
	Example
	    F=GF(4,Variable=>a);
	    R=F[x,y];
	    P={{0,0},{1,0},{0,1},{a,a}};
	    S={x+y,x^2+y^2,a+x*y^2};
	    C=evaluationCode(F,P,S);
	    C.PolynomialSet
///
--------------- Documentation Sets-----------------
doc ///
    Key
    	Sets
    Headline
    	sets of a Cartesian code
    Usage
    	C.Sets
    Inputs
    	C:EvaluationCode
    Outputs
    	:List
    Description
    	Text
	    This key stores a list of subsets of a field that are
	    used for constructing a @TO cartesianCode@.
	Text
	    This symbol is provided by the package @TO CodingTheory@.
	Example
	    F=GF(4);
	    R=F[x,y];
	    C=cartesianCode(F,{{0,1,a},{0,1,a}},{1+x+y,x*y})
	    C.Sets
///

--------------- Documentation VanishingIdeal-----------------
doc ///
    Key
    	VanishingIdeal
    Headline
    	vanishing ideal of an evaluation code
    Usage
    	C.VanishingIdeal
    Inputs
    	C:EvaluationCode
    Outputs
    	:Ideal
    Description
    	Text
	    This key stores the vanishing ideal of the set of points that are
	    used for constructing an @TO EvaluationCode@.
    	Text
	    This symbol is provided by the package @TO CodingTheory@.
    	Example
	  F=GF(4);
	  R=F[x,y];
	  P={{0,0},{1,0},{0,1},{a,a}};
	  S={x+y,x^2+y^2, a+x*y^2};
	  C=evaluationCode(F,P,S);
	  C.VanishingIdeal
///

 

end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
uninstallPackage "CodingTheory"
installPackage "CodingTheory"
installPackage("CodingTheory", RemakeAllDocumentation=>true)
installPackage("CodingTheory", RemakeAllDocumentation=>true, RunExamples=>false)
installPackage("CodingTheory", MakeDocumentation=>true,FileName=>"~/myCodingTheoryStuff/CodingTheoryEdit5202020.m2")
check CodingTheory
viewHelp CodingTheory
viewHelp doc

-----------------------------------------------------
-- Codes from Generator Matrices (as lists):
-----------------------------------------------------
F = GF(3,4)
codeLen = 7
codeDim = 3
L = apply(toList(1..codeDim),j-> apply(toList(1..codeLen),i-> random(F)))
C = linearCode(F,L)
peek C
-- check that dimension and length are correct:
dim C
length C
-- check that G*H^t = 0:
C.GeneratorMatrix * (transpose C.ParityCheckMatrix)

-----------------------------------------------------
-- Codes from Parity Check Matrices (as a matrix):
-----------------------------------------------------
F = GF(2)
L = {{1,0,1,0,0,0,1,1,0,0},{0,1,0,0,0,0,0,1,1,0},{0,0,1,0,1,0,0,0,1,1},{1,0,0,1,0,1,0,0,0,1},{0,1,0,0,1,1,1,0,0,0}}
C = linearCode(F,L,ParityCheck => true)
peek C


-----------------------------------------------------
-- Codes with Rank Deficient Matrices:
-----------------------------------------------------
R=GF 4
M=R^4
C = linearCode(R,{{1,0,1,0},{1,0,1,0}})
peek C


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=CodingTheory pre-install"
-- End:

