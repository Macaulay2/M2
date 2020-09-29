newPackage(
    "DecomposableSparseSystems",
    Version=>"1.0.0",
    Date=>"May 13, 2020",
    Authors=> {
        {Name=>"Taylor Brysiewicz",
	 Email=>"taylorbrysiewicz@gmail.com",
	 HomePage=>"https://sites.google.com/view/taylorbrysiewicz"},
        {Name=>"Jose Israel Rodriguez",
	 Email=>"Jose@math.wisc.edu",
	 HomePage=>"https://www.math.wisc.edu/~jose/"},
        {Name=>"Frank Sottile",
	 Email=>"sottile@math.tamu.edu",
	 HomePage=>"https://www.math.tamu.edu/~sottile"},
        {Name=>"Thomas Yahl",
	 Email=>"thomasjyahl@tamu.edu",
	 HomePage=>"https://math.tamu.edu/~thomasjyahl"}
        },
    Headline=>"Solving decomposable sparse systems",
    PackageImports=>{"PHCpack","Polyhedra","DeterminantalRepresentations"},
    PackageExports=>{"NumericalAlgebraicGeometry"},
    AuxiliaryFiles=>true,
    OptionalComponentsPresent => (readPackage "PHCpack").OptionalComponentsPresent,
    CacheExampleOutput => true,
    DebuggingMode=>false
    )

export{
	"isLacunary",
	"isTriangular",
	"TriangularSys",
	"LacunarySys",
	"solveDecomposableSystem",
	"isDecomposable",
	"FromGeneric"
    }


relativeComplement = method()
--Computes the complement of A in B
--used only in minimalSubsystem
relativeComplement (List,List) := List => (A,B)->(
    L := select(A,x->not member(x,B));
    L
    )

subsetsLargerThan = method()
--Computes subsets of L of size greater than or equal to n
--used only in minimalSubsystem
subsetsLargerThan (Number,List) := List => (n,L)->(
    totalSubsets := flatten apply(toList(n .. #L),k->subsets(L,k));
    totalSubsets
    )

latticeGens = method()
-- Translates the support (columns of the matrix M) so that the first exponent is 0 (so that affine span is linear span)
-- Applies this to each support in a list L of supports
latticeGens (Matrix) := Matrix => M->(
    L := entries transpose M;
    Gens := transpose matrix apply(L,v->v-first L);
    Gens
    )
latticeGens (List) := Matrix => L->(
    L' := apply(L,M->latticeGens(M));
    Gens := fold(L',(M,N)->M|N);
    Gens
    )

systemRank = method()
--Determines the rank of a matrix or of the concatenation of a list of matrices; this is used to detect subsystems
systemRank (Matrix) := Number => M->(
    return(rank latticeGens(M))
    )
systemRank (List) := Number => L->(
    if (#L === 0) then 0 else rank latticeGens(L)
    )

torusPts = method(Options => {Tolerance=>.00001})
--Removes points too close to the origin by a specified tolerance
torusPts (List) := List => o->L->(
    L' := select(L,v->min (v/abs) > o.Tolerance);
    L'
    )

monomialMapEvaluation = method()
--Evaluates the monomial map corresponding to the matrix A at the point v. 
monomialMapEvaluation (List,Matrix) := List => (v,A)->(
    L := apply(entries transpose A,r->product(v,r,(x,n)->x^n));
    L
    )

generateSystem = method(Options => {Verbose=>0})
--Given a pair of supports A and coefficients C, this forms the monomials and the corresponding system of polynomials.
--This is an internal method to get the system back from our internal data structure (A,C) for polynomial systems
--This is the inverse function to pullSystemData
generateSystem (List,List) := List => o->(A,C)->(
    --error handling
    n := numgens target A#0;
    if not (#A === #C and apply(A,M->numgens source M) === apply(C,c->#c) and all(A,M->numgens target M === n)) then (
	error "generateSystem: Inconsistent number of equations, monomials, or coefficients"
	);
    if not (all(A,M->all(flatten entries M,a->a>=0))) then (
	if (o.Verbose > 0) then (print("generateSystem: Shifting supports to positive orthant"));
	A = apply(A,M->matrix apply(entries M,v->apply(v,z->z-min v)));
	);    
    x := symbol x;
    R := CC[x_1..x_n];
    F := apply(#A,i->sum(C#i,monomialMapEvaluation(gens R,A#i),(c,m)->sub(c,CC)*m));
    F
    )

pullSystemData = method()
--This is the inverse function to generateSystem.  It peels off the exponents and coefficients from a given system F
pullSystemData (List) := Sequence => F->(
    --error handling
    try ideal(F) then(
	F = (ideal(F))_* ;--ensures all polynomials in F are in the same ring.
        A := F/(f->transpose matrix exponents f);
    	C := F/(f->flatten entries last coefficients f);
    	return(A,C)
	) else (
	error "pullSystemData: Expected a list of polynomials in the same ring"
	);
    )

solveBinomials = method()
--Only used to solve binomials of the form x^A = v. Not exported for this reason.
--It converts to A being diagonal via the monomial change of coordinates given by
--  the Smith normal form D, extracts roots, and converts back to original coordinates.
solveBinomials (Matrix,List) := List => (A,v)->(
    --error handling
    if (det(A) === 0) then (
	error "solveBinomials: Matrix not invertible"
	);
    
    (D,P,Q) := smithNormalForm A;
    v' := monomialMapEvaluation(v,Q);
    rootsList := apply(toList(0 .. numgens source A - 1),i-> (v'#i)^(1/D_i_i)*apply(D_i_i,j->{exp(2*pi*ii*j/D_i_i)}));
    L := fold(rootsList,(S,T)-> flatten table(S,T,(x,y)->x|y));
    sols := L/(v->monomialMapEvaluation(v,P));
    sols
    )


homotopyToFibres = method(Options => {Verbose=>0})
--This implements the Amendola-Rodriguez method for computing solutions to a decomposable system.
--Most of this is error handling. The code is really the last 2 blocks.
homotopyToFibres (List,List,List) := List => o->(F,bottomFib,firstTopFib)->(
    --error handling
    try(ideal F) then (
	R := ring ideal F; 
	n := numgens R
	) else (
	error "homotopyToFibres: Expected polynomials of the same ring"
	);
    
    if not (isPolynomialRing R and class coefficientRing R === ComplexField) then (
	error "homotopyToFibres: Expected polynomials in complex polynomial ring"
	);
    
    if not (#F === n) then (
	error "homotopyToFibres: Expected a square system"
	);
    
    r := #(bottomFib#0);
    if (all(bottomFib,s->#s === r)) then (
	S := first selectVariables(toList(r .. n-1),R)
	) else (
	error "homotopyToFibres: Elements of fibre not of same size"
	);
    
    m := #(firstTopFib#0);
    if not (all(firstTopFib,s->#s === m)) then (
	error "homotopyToFibres: Elements of fibre not of same size"
	);
    
    if not (r + m === n) then (
	error "homotopyToFibres: Incorrect sizes of first top fibre"
	);
    -- This is the main part of this function.     
    SystemsOverFibres := apply(#bottomFib,i->(
	subs := apply(r,j->R_j=>(bottomFib_i)_j);
	apply(toList(r .. n-1),i->sub(sub(F#i,subs),S))
	));
    
    Solns := flatten apply(#bottomFib,i->
	apply(track(first SystemsOverFibres,SystemsOverFibres#i,firstTopFib,gamma=>random(CC))/coordinates,s->bottomFib#i|s)
	);
    Solns
    )

solveUnivariate = method()
--When the polynomial system is a univariate polynomial, we solve using companion matrices, as this is better-behaved
--  than calling a black-box solver
solveUnivariate (RingElement) := List => (f)->(
	E:=eigenvalues companionMatrix(f);
	E=toList(E);
	E=apply(E,a->{a});
	E
	)

scaling = method()
--This applies a least-squares method to best scale variables and equations.
scaling (List,List) := Sequence => (A,C)->(
    A' := apply(#A,k->transpose (matrix table(#A,numgens source A#k,(i,j)->if (i==k) then 1 else (0))||A#k));
    M := sub(fold(A',(S,T)->S||T),RR);
    b := -matrix apply(flatten C,z->{log(10,norm z)});
    v := apply(flatten entries transpose solve(M,b,ClosestFit=>true,MaximalRank=>true),z->10^z);
    C' := apply(#C,i->apply(C#i,monomialMapEvaluation(v,transpose (A'#i)),(a,b)->a*b));
    (C',drop(v,#A))
    )

basicSolver = method(Options => {Verbose=>0,Software=>PHCPACK,Verify=>0,Tolerance=>.00001})
--This is the blackbox solver we call for systems that are not decomposable. The software can be changed by Software.
--This version accepts only a list of polynomial systems. The other version is below.
basicSolver (List) := List => o->F->(
    --error handling
    try(ideal F) then (
	R := ring ideal F
	) else (
	error "basicSolver: Expected polynomials of the same ring"
	);
    if not (isPolynomialRing R and class coefficientRing R === ComplexField) then (
	error "basicSolver: Expected polynomials in complex polynomial ring"
	);
	Solns:={};
	--if it is a univariate polynomial system, solve using companion matrices
	if numgens(R)==1 then(
		Solns = solveUnivariate(F#0);
		Solns = torusPts(Solns,Tolerance=>o.Tolerance);
		return(Solns);
	);
	SOLVER:=o.Software;
    if (o.Verify>0) then (
		V := monteCarloMixedVolume(F);
		if o.Verbose >1 then print("The mixed volume of ");
		if o.Verbose >1 then print(toString(F));
		if o.Verbose >1 then print(" is "|toString(V));
		AllSolns := NumericalAlgebraicGeometry$solveSystem(F,Software=>SOLVER)/coordinates;
		Solns' :=torusPts(AllSolns,Tolerance=>o.Tolerance);
		if (#Solns' == V) then(
			Solns = Solns';
			if o.Verbose >1 then print("All solutions were found");
			) else (
			if o.Verbose >1 then print (" yet we found "|toString(#Solns')|" points");
			if o.Verbose >1 then print("Attempting to find all "|toString(V)|" points via monodromy.");
			MonodromySolns := torusPts(populateViaMonodromy(F,Solns',Tolerance=>o.Tolerance,Verbose=>o.Verbose));
			if (#MonodromySolns == V) then(
				if o.Verbose >1 then print("Monodromy recovery was successful");
				Solns=MonodromySolns;
				) else (
					print("BasicSolver: Couldn't compute all solutions");
					print(toString F);
					return({});
				);
			);
		) else (
	    Solns = torusPts(NumericalAlgebraicGeometry$solveSystem(F,Software=>SOLVER)/coordinates,Tolerance=>o.Tolerance);
		);    
    if (o.Verbose > 0) then print("basicSolver: Computed " | toString(#Solns) | " solutions");
    Solns
    )

--This version accepts systems written as lists of supports and coefficients.
basicSolver (List,List) := List => o->(A,C)->(
    if (max apply(flatten C,z->norm z) > 1e10 or max apply(flatten C,z->norm z) < 1e-10) then (
	(C',S) := scaling(A,C);
	FScaled := generateSystem(A,C');
	solnsScaled := basicSolver(FScaled,o);
	apply(solnsScaled,z->apply(#z,i->z#i*S#i))
	) else (
        F := generateSystem(A,C);
	basicSolver(F,o)
    	)
    )


minimalSubsystem = method()
--Computes the indices of a subsystem of a system supported on A of minimal size.
minimalSubsystem (List) := List => A->(
    --error handling
    n := #A;
    if not (all(A,M->numgens target M === n)) then (
	error "minimalSubsystem: Expected matrices with same codomain"
	);
    if not (systemRank(A) === n) then (
	error "minimalSubsystem: Expected a nondegenerate system"
	);
    
    latGens := A/(M->sub(latticeGens M,QQ));
    sortedIndices := (sort apply(#A,i->{systemRank A#i,i}))/last;
    subsystemList := {};
    for i from 0 to n-1 do (
	I := take(sortedIndices,i+1);
	k := last I;
	containmentIndices := select(I,j->latGens#j % latGens#k == 0);
	remainingIndices := relativeComplement(I,containmentIndices);
	possiblePartialSubsystems := subsetsLargerThan(rank latGens#k - #containmentIndices, remainingIndices);
	partialSubsys := select(possiblePartialSubsystems,S->systemRank((S|{k})/(i->A#i)) === #S + #containmentIndices);
	if (#partialSubsys > 0) then break (subsystemList = (first partialSubsys)|containmentIndices)
	);
    sort subsystemList
    )

triangularReduction = method()
--Computes the reduction of a triangular system F=(A,C) corresponding to the subsystem indexed by minimalSubsystem
--Returns three values, P, F̂, and F̂_I
----P is the matrix corresponding to a monomial change of coordinates making the subsystem apparent
----F̂ is the original system after the change of coordinates
----F̂_I is subsystem after the change of coordinates
triangularReduction (List,List) := Sequence => (A,C)->(
    --error handling
    n := numgens target A#0;
    if not (#A === #C and apply(A,M->numgens source M) === apply(C,c->#c) and all(A,M->numgens target M === n)) then (
        error "subsystemReduction: Inconsistent number of equations, monomials, or coefficients"
        );
    
    I := minimalSubsystem A;
    (D,P,Q) := smithNormalForm latticeGens A_I;
    
    L := unique(I|toList(0 .. n-1));
    B := A/(M->P*(latticeGens M));
    subsysSupp := B_I/(M->M^(toList(0 .. #I-1)));
    (P,(B_L,C_L),(subsysSupp,C_I))
    )

isTriangular = method()
--checks if the system if triangular.  It can either be given system data (A,C), or simply the 
--list of lists of supports, A, as it only depends upon the supports
isTriangular (List):= Boolean => L->(
	if not(class(L#0)===Matrix) then(
		(A,C):=pullSystemData(L);
		return(isTriangular(A));
	);
	m:=#(minimalSubsystem(L));
	m != #L
	)

latticeIndex = method()
--Returns the index of the lattice affinely spanned the supports in a list A, using smithNormalForm
latticeIndex (List) := ZZ => A->(
    --error handling
    n := numgens target A#0;
    if not (all(A,M->numgens target M === n)) then (
	error "latticeIndex: Expected matrices with same codomain"
	);
    M := latticeGens(A);
    if not (rank M === numgens target A#0) then (
	error "latticeIndex: Lattice not of full rank"
	);
    D := first smithNormalForm M;
    product(numgens target M,i->D_i_i)
    )

latticeReduction = method()
--Takes a Lacunary system/supports and returns the monomial map φ and the support of the reduced system.  
latticeReduction (List) := Sequence => A->(
    --error handling
    n := numgens target A#0;
    if not (all(A,M->numgens target M === n)) then (
        error "subsystemReduction: Expected matrices with same codomain"
        );

    M := latticeGens(A);
    if not (rank M === numgens target A#0) then (
        error "subsystemReduction: Lattice not of full rank"
        );

    (D,P,Q) := smithNormalForm M;
    phi := (inverse P)*D_(toList(0 .. n-1));
    reducedSupports := apply(A,B->solve(phi,latticeGens B));
    (phi,reducedSupports)
    )

isLacunary = method()
--Checks if the system/support is lacunary
isLacunary (List) := Boolean => L->(
    if not(class(L#0)===Matrix) then(
        (A,C):=pullSystemData(L);
	return(isLacunary(A));
	);
    latticeIndex(L)>1
    )

isDecomposable = method()
--Wraps the two, isLacunary and isTriangular, to check if a system/support is decomposable.
isDecomposable (List):= Boolean => L->(
    if not(class(L#0)===Matrix) then(
      	(A,C):=pullSystemData(L);
       	return(isDecomposable(A));
        );
    isLacunary L or isTriangular L
    )





solveDecomposableSystem = method(Options=>{Verbose=>0,LacunarySys=>true,TriangularSys=>true,Verify=>0,Software=>PHCPACK,Tolerance=>.00001,Strategy=>{}})
--This is Algorithm 9 in our paper
solveDecomposableSystem (List,List) := List => o->(A,C)->(
    n := numgens target A#0;
    if not (all(A,M->numgens target M === n) and #A === n and #C === n) then (
	error "solveDecomposableSystem: Expected a square system"
	);
	if instance(o.Strategy,Symbol) and o.Strategy==FromGeneric then(
		(G2,sols) := solveDecomposableSystem(A,,o ++ {Strategy=>{}});
		G1 := generateSystem(A,C);
		S1 := ring(ideal(G1));
		S2 := ring(ideal(G2));
		G2 = apply(G2,f->sub(f,apply(numgens S2,i->S2_i=>S1_i)));
		homeSols := track(G2,G1,sols,gamma=>random(CC)*(ii)^(random(1,4)))/coordinates;
		return(homeSols);		
	);
    
    if (o.LacunarySys === true) then (
	if (latticeIndex A > 1) then (
	    (phi,A') := latticeReduction A;
	    lacBottomFib := solveDecomposableSystem(A',C,o ++ {LacunarySys=>false});
	    lacSolns := flatten (lacBottomFib/(z->solveBinomials(phi,z)));
	    return(lacSolns)
	    ) else (
	    return(solveDecomposableSystem(A,C,o ++ {LacunarySys=>false}))
	    )
	);
    
    if (o.TriangularSys === true) then (
	r := #(minimalSubsystem A);
	if (r < n) then (
	    (P,F,S) := triangularReduction(A,C);
	    B := first F;
	    C = last F;
	    triBottomFib := solveDecomposableSystem(S,o ++ {LacunarySys=>true,TriangularSys=>false});
	    s := first triBottomFib;
	    B' := apply(toList(r .. n-1),i->(B#i)^(toList(r .. n-1)));
	    C' := apply(toList(r .. n-1),i->apply(C#i, monomialMapEvaluation(s,(B#i)^(toList(0 .. r-1))),(c,m)->c*m));
	    firstTopFib := solveDecomposableSystem(B',C',o ++ {LacunarySys=>true});

	    triPartSolns := homotopyToFibres(generateSystem(B,C),triBottomFib,firstTopFib);
	    triSolns := triPartSolns/(s->monomialMapEvaluation(s,P));
	    return(triSolns)
	    ) else (
	    return(solveDecomposableSystem(A,C,o ++ {TriangularSys=>false}))
	    )
	);
    
    if (o.LacunarySys === false and o.TriangularSys === false) then (
		solns := basicSolver(A,C, Verbose=>o.Verbose,Software=>o.Software,Tolerance=>o.Tolerance,Verify=>o.Verify);
		return(solns)
		)
    )

--Solves a system given as a list of polynomials using Algorithm 9 from our paper
solveDecomposableSystem (List) := List => o->F->(
    S := pullSystemData F;
    solveDecomposableSystem(S,o)
    )


--Given a set of supports A, this computes a start system supported on A and its solutions
solveDecomposableSystem (List,Nothing) := Sequence => o->(A,emp)->(
    n := numgens target A#0;
    if not (all(A,M->numgens target M === n) and #A === n) then (
	error "solveDecomposableSystem: Expected a square system"
        );
    C := apply(A,M->apply(numgens source M,i->random(CC)-random(CC)));
    F := generateSystem(A,C);
    Solns := solveDecomposableSystem(A,C,o);
    (F,Solns)
    )


populateViaMonodromy = method(Options =>{Verbose=>0,Tolerance=>0.00001})
--Given a polynomial system and a partial solution set, apply monodromy to find more solutions
populateViaMonodromy (List,List) := List =>o ->(F,sols)->(
	(A,C):=pullSystemData(F);
	C' :=apply(A,M->apply(numgens source M,i->random(CC)-random(CC)));
	F1 := generateSystem(A,C);
	F2 := generateSystem(A,C');
	S1 :=ring(ideal(F1));
	S2 :=ring(ideal(F2));
	F2 = apply(F2,f->sub(f,apply(numgens S2,i->S2_i=>S1_i)));
	NumLoops := 10;
	LoopSize := 5;
	homeSols := sols;
	for i from 1 to 10 do(
		farSols := track(F1,F2,homeSols,gamma=>random(CC)*LoopSize*(ii)^(random(1,4)))/coordinates;
		newHomeSols := track(F2,F1,farSols,gamma=>random(CC)*LoopSize*(ii)^(random(1,4)))/coordinates;
		homeSols = numericallyAppend(o.Tolerance,homeSols,newHomeSols);
		if o.Verbose>5 then print("Found:"|toString(#homeSols)|" solutions via monodromy");
		);
	homeSols
	)


numericallyAppend = method()
numericallyAppend(RR,List,List) := List => (tolerance,sols,newSols)->(
    	newSols = new MutableList from newSols;
	for k from 0 to #newSols-1 do(
    	testSol :=newSols#k; -- A new solution in newSols.
		different:=1;
    	    	--Compare a new solution to other new ones.
		j0:=k+1;
		while (different==1 and j0<(#newSols)) do(
		    if areEqual(testSol,newSols#j0,Tolerance=>tolerance) then different=0 else j0=j0+1;
		    ),
    	    	--Compare a new solution to the old ones.
		j:=0;
		while (different==1 and j<(#sols)) do(
		    if areEqual(testSol,sols#j,Tolerance=>tolerance) then different=0 else j=j+1;
		    ),
		if different==0 then(
		    newSols#k = null
		    ),
		),
	    return(sols|delete(null, toList newSols))
	    )

monteCarloMixedVolume = method()
monteCarloMixedVolume(List) := ZZ => F ->(
    P:=apply(F,newtonPolytope);
	numTries:=5;
    mixedVolumeTests:=apply(numTries,i->Polyhedra$mixedVolume(P));
    min(mixedVolumeTests)
    )


-------------------------
--                     --
--    Documentation    --
--                     --
-------------------------


beginDocumentation()
doc///
	Key 
		DecomposableSparseSystems
	Headline
		Solving decomposable sparse polynomial systems
	Description
		Text
			There are two natural ways a sparse polynomial system can
			be decomposed in the sense of (T.Brysiewicz, J.I.Rodriguez, 
			F.Sottile, and T.Yahl, {\em Solving Decomposable Sparse Systems},
			 {\em arXiv:2001.04228}, 2019). These methods detect and
			compute these decompositions and use them to compute solutions.
///

doc///
	Key
		isDecomposable
			(isDecomposable,List)
	Headline
		Decides whether a polynomial system is decomposable
	SeeAlso
		isLacunary
		isTriangular
	Usage
		isDecomposable F
		isDecomposable A
	Inputs		
		F: List
			of (Laurent) polynomial equations.
		A : List
			of matrices whose column vectors are the support of a system of (Laurent) polynomial equations
	Outputs
		: Boolean
			a boolean asserting whether or not the polynomial system (or set of supports) is decomposable
	Description
		Text
			A polynomial system is decomposable if it is either lacunary or triangular.
			This function checks whether or not a polynomial system is decomposable.
		Text
			isDecomposable accepts a list of polynomials forming a system
		Example
			R=QQ[x,y];
			F={3+x^2*y^2-(17/3)*x^4*y^4,2-x^2+5*y^2-13*x^2*y^2};
			isDecomposable F	
		Text
			isDecomposable also accepts a list of supports encoded as matrices
		Example
			A = {matrix{{0,2,4},{0,2,4}},matrix{{0,0,2,2},{0,2,0,2}}};
			isDecomposable A
			B = {matrix{{0,2,4},{0,2,3}},matrix{{0,1,0},{0,0,1}}};
			isDecomposable B
///

doc///
	Key
		isTriangular
			(isTriangular,List)
	Headline
		Decides whether a polynomial system is triangular
	SeeAlso
		isLacunary
		isDecomposable
	Usage
		isTriangular F
		isTriangular A
	Inputs
		F: List
			of (Laurent) polynomial equations.
		A : List
			of matrices whose column vectors are the support of a system of (Laurent) polynomial equations
	Outputs
		: Boolean
			a boolean asserting whether or not the polynomial system (or set of supports) is triangular
	Description
		Text
			A polynomial system is triangular if, after a monomial change of coordinates,
			there is a proper subset of $k$ equations which only involve the first $k$ variables.
			This function checks whether or not a polynomial system (or set of supports) is triangular.
		Text
			isTriangular accepts a list of polynomials forming a system
		Example
			R=QQ[x,y];
			F={3+x^2*y^2-(17/3)*x^4*y^4,2-x^2+5*y^2-13*x^2*y^2};
			isTriangular F	
		Text
			isTriangular also accepts  a list of supports encoded as matrices
		Example
			A = {matrix{{0,2,4},{0,2,4}},matrix{{0,0,2,2},{0,2,0,2}}};
			isTriangular A
			B = {matrix{{0,2,4},{0,2,3}},matrix{{0,1,0},{0,0,1}}};
			isTriangular B
///

doc///
	Key
		isLacunary
			(isLacunary,List)
	Headline
		Decides whether a polynomial system is lacunary
	SeeAlso
		isTriangular
		isDecomposable
	Usage
		isLacunary F
		isLacunary A
	Inputs
		F: List
			of (Laurent) polynomial equations.
		A : List
			of matrices whose column vectors are the support of a system of (Laurent) polynomial equations
	Outputs
		: Boolean
			a boolean asserting whether or not the polynomial system (or set of supports) is lacunary
	Description
		Text
			A polynomial system is lacunary when its support spans a proper sublattice of full rank of the integer lattice.
			This function checks whether or not a polynomial system is lacunary.
		Text
			isLacunary accepts a  list of polynomials forming a system 
		Example
			R=QQ[x,y];
			F={3+x^2*y^2-(17/3)*x^4*y^4,2-x^2+5*y^2-13*x^2*y^2};
			isLacunary F	
		Text
			isLacunary also accepts a list of supports encoded as matrices
		Example
			A = {matrix{{0,2,4},{0,2,4}},matrix{{0,0,2,2},{0,2,0,2}}};
			isLacunary A
			B = {matrix{{0,2,4},{0,2,4}},matrix{{0,1,0},{0,0,1}}};
			isLacunary B
///

doc///
	Key
		solveDecomposableSystem
			TriangularSys
			LacunarySys
			FromGeneric
			[solveDecomposableSystem,Verbose]
			[solveDecomposableSystem,Verify]
			[solveDecomposableSystem,Software]
			[solveDecomposableSystem,Tolerance]
			[solveDecomposableSystem,TriangularSys]
			[solveDecomposableSystem,LacunarySys]
			[solveDecomposableSystem,Strategy]
			(solveDecomposableSystem,List)
			(solveDecomposableSystem,List,List)
			(solveDecomposableSystem,List,Nothing)
	Headline
		recursively solves a sparse (Laurent) polynomial system through a decomposition
	Usage
		solveDecomposableSystem F
		solveDecomposableSystem (A,C)
		solveDecomposableSystem (A,)
	Inputs
		F: List
			of (Laurent) polynomial equations.
		A : List
			of matrices whose column vectors are the support of a system of (Laurent) polynomial equations
		C:List
			whose i-th entry is the list of coefficients for the i-th polynomial equation.
		Verbose=> ZZ
			describing how much output is printed during the computation
		Verify=> ZZ
			which when set to $1$ confirms at each step of the computation that the number of solutions computed is equal to the mixed volume of the polynomial system
		Software=> Symbol
			describing which numerical solver to use to compute solutions to sparse polynomial systems which are not decomposable
		Tolerance=> RR
			a tolerance governing whether a numerical solution belongs to the algebraic torus
		TriangularSys=> Boolean
			describing if it is, a priori, possible that the input is triangular
		LacunarySys=> Boolean
			describing if it is, a priori, possible that the input is lacunary
		Strategy=> 
			when set to FromGeneric, the software will solve a generic sparse system $G$, supported on $A$, and compute the solutions to $F$ via a parameter homotopy
	Outputs
		: List
			of solutions to the polynomial equations in the algebraic torus
	Description
		Text
			This implements Algorithm 9 in (T.Brysiewicz, J.I.Rodriguez, F.Sottile, and T.Yahl,
			 {\em Solving Decomposable Sparse Systems}, {\em arXiv:2001.04228}, 2019). 
			It recursively checks whether or not the input
			sparse polynomial system is decomposable, computes the decomposition, and then 
			calls itself on each portion of the decomposition. When the input is not decomposable
			it solves multivariate polynomial systems with the numerical solver given by the option 
			Software and it solves univariate polynomial systems using companion matrices.
		Text
			This function accepts a sparse polynomial system in the form of exponents and coefficients.
		Example
			A = {matrix{{0,2,4},{0,2,4}},matrix{{0,0,2,2},{0,2,0,2}}};
			C = {{1,3,7},{1,17,-3,23*ii}};
			solveDecomposableSystem(A,C)
		Text
			It also accepts the sparse polynomial itself
		Example
			R=CC[x,y];
			F = {x^4+3*y^6-1,17*x^2-2*y^2+2};
			solveDecomposableSystem F
		Text
			When $C$ is not entered, the method will choose random coefficients for $C$ and solve 
			that sparse polynomial system. The output is then the pair $(F,S)$ where $F$ is the 
			random sparse polynomial system chosen and $S$ are the solutions to that system in the 
			algebraic torus. 
		Example
			A = {matrix{{0,2,4},{0,2,4}},matrix{{0,0,2,2},{0,2,0,2}}};
			(F,S)=solveDecomposableSystem(A,)
		Text
			Setting Verify greater than zero will run @TO "Polyhedra$mixedVolume"@ five times and return the minimum to determine the mixed volume of any non-decomposable system being solved by @TO "Software"@. If the number of solutions found does not equal this computation, the software will run ten monodromy loops to attempt to populate the missing solutions. As the mixed volume computation is accurate up to some probability, we do not use this as a stopping criterion for the monodromy computation. 
		Example
			R=CC[x,y];
			F = {x^4+3*y^6-1,17*x^2-2*y^2+2};
			solveDecomposableSystem (F,Verify=>1,Tolerance=>0.1,Verbose=>3)
///		

TEST ///
    assert(isLacunary({matrix{{0,2,4},{0,2,4}},matrix{{0,2,0,2},{0,0,2,2}}}));
///
TEST ///
    assert(isTriangular({matrix{{0,2,4},{0,2,4}},matrix{{0,2,0,2},{0,0,2,2}}}));
///

TEST ///
    A={matrix{{0,3,7,9},{1,1,3,-5}},matrix{{2,6,4,4,8,6,4},{2,2,4,6,8,8,10}}};
    assert(isDecomposable(A));
    B={matrix{{1,4,2,3,4},{1,1,2,4,3}},matrix{{-3,-2,2,3},{1,2,3,4}}};
    assert(not isDecomposable(B));
///


TEST ///
	R=CC[x_1,x_2]
	F={5*x_1^4*x_2^6+3*x_1^2*x_2^2+1, 9*x_1^2*x_2^8+13*x_1^2+7*x_2^2-1}
    S1=solveDecomposableSystem(F,Tolerance=>0.000001,Strategy=>FromGeneric);
    assert(#S1==32);
    S2=solveDecomposableSystem(F,Tolerance=>0.445);
    assert(#S2==8);
    S3=solveDecomposableSystem(F,Tolerance=>0.445,Verify=>1)
    assert(#S3==32)
///


TEST///
A = {matrix{{0,2,4},{0,2,4}},matrix{{0,0,2,2},{0,2,0,2}}};
(F,S)=solveDecomposableSystem(A, )
///


TEST ///
    A={matrix{{0,3,7,9},{1,1,3,-5}},matrix{{2,6,4,4,8,6,4},{2,2,4,6,8,8,10}}};
    (F,S)=solveDecomposableSystem(A,);
    assert(#S==112);
///




end--


