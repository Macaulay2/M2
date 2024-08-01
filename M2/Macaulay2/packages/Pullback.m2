newPackage(
"Pullback",
Version => "1.03",
Date => "March 8, 2018",
Authors => {{Name => "Drew Ellingson"},{Name => "Karl Schwede"}},
Headline => "pullback of rings",
Keywords => {"Commutative Algebra"},
DebuggingMode => false,
PackageImports => {"PushForward"},
Reload => false
)

--************************************************************************************************************
--************************************************************************************************************
--***given f,g, it computes the pullback {A -> B <- C} and returns the maps from the pullback to A and C.  ***
--***(We require that f surjects and that the variable names of A and C coincide.  Finally we also require ***
--***the ambient rings of B and A to be the same, so that variables names line up).                        ***
--************************************************************************************************************
--************************************************************************************************************
--***Acknowledgements:                                                                                     ***
--***Special thanks to Claudiu Raicu for useful discussion on the strategy for ring pullbacks.  We also    ***
--***Neil Epstein for useful conversations on Pullbacks.                                                   ***
--************************************************************************************************************
--************************************************************************************************************

pullback(RingMap, RingMap) := {Verbose => false} >> o -> (f,g) -> (
    -- throughout A ---f-->> A/I <-----g----C
    --		   ^			    ^
    --		    \		  	   /
    --		      ----------R---------


    --********************* Variable definitions ****************************

    --***First we define our basic rings***
    A := source f;
    B := target f;
    I := ker f;
    C := source g;
    baseRing := coefficientRing(B);

    --***We grab names of the generators***
    IGens := flatten(entries(gens(I))); --gens(ideal) returns matrix so this one looks different
    CGens := gens(ambient C);
    --***In order to handle bug #350, we are handling the case when CGens is empty specially.
    if (#CGens == 0) then (
        C = baseRing[Heft=>{1}];
        g = map(B, C, {});
    );
    AGens := gens(ambient A);

    --***We take the images of various generators in B, and then also take a set of pre-images in A***
    CGensInB := apply(CGens, i -> g(i)); -- getting gens(C) as elements of A
    CGensInA := apply(CGensInB, i -> sub(i,A));

    --***Next we form the direct sum of A and C.  Note our pullback will always be a subring of this***
    directSumList := internalUseDirectSum(A,C);
    AsumCasRing := directSumList#0;
    idempotent := directSumList#1; --This is the idempotent corresponding to (1,0) in A \oplus C

    --***We next form a list of things we know will be included among the generators of R.  We take their images in A first.  We also take their images in A \oplus C.
    varsCandidatesA := IGens | CGensInA; -- values of starting variables in R
    varsCandidates := apply(IGens, zz->sub(zz,AsumCasRing)) | apply(#CGens, ii->(idempotent*sub(CGensInA#ii,AsumCasRing) + (1-idempotent)*sub(CGens#ii,AsumCasRing)));

    --***These are the list of variable names***
    listIGenVars := varNameFixer(IGens, "IGen");
    listCGenVars := varNameFixer(CGensInA, "CGensInA");

    listOfVarsCandidates := listIGenVars | listCGenVars;   -- names of starting variables in R (this will usually grow).

    --***Now we form our first approximation of R.  Note that our real R will be a sub-quotient of this typically.***	
    R := baseRing[listOfVarsCandidates];  --cannot adjoin polynomials as variables in a ring,
								 --so listofvarscands is just placeholder names while
					 --varsCands is the actual info.

    addedTerms := {}; -- list of values of higher powers of AGens * IGens at each step
    listAddedTerms := {}; -- list of names of higher powers of AGens*IGens at each step

    allHigherAITerms := {}; -- "   -values-   " for all steps
    allHigherAITermsVars:= {}; --"   -names-   " for all steps

    currentATermsInR := {}; -- holds elements of (gens A)^i
    AExponent := 0;

    AasRmodule := null;
    CasRmodule := null;
    RImageInA := {};
    RImageInC := {};
    RtoAMap := null;
    RtoCMap := null;

    AasRmoduleMap := null;
    CasRmoduleMap := null;    	
    RtoAsumCMap := null;
	

    APushFwd := null;
    AtoBasAmodMap := null;
    AtoBasRmodMap := null;
    RtoBMap := null;
	
    RmodJtoRMap := null; -- ill defined, but it does not matter
    RtoRmodJMap := null;
    RmodJtoBMap :=null;
    RmodJPushFwd := null;	
    RmodJtoBasRmodJmodMap := null;
    RmodJtoBasRmodMap := null;
    	
    AsumCtoBasRmodMap := null;
    J := null;
    newC := null;

    AsumCtoBasRmodMapMatrix := null;
    newGasRmodMap := null;
    AsumC := null;

    AFiniteOverR := false;
    exact := false;

    --actual code starts here
    if not ( target f === target g ) then error "pullback: Targets of the maps are not the same"; 	 -- check that the two maps to C are really to the same ring.
    names1 := set(apply(first entries vars ambient A, i -> toString(i)));
    names2 := set(apply(first entries vars ambient C, j -> toString(j)));
    names3 := set(apply(first entries vars ambient B, j -> toString(j)));
    if not (#(names1*names2) == 0 ) then error "pullback: variables of the source of the maps overlap"; -- we verify that no names from A or C intersect.  We want this because it might otherwise cause problems.
    if not (isSubset(names1, names3)) then error "pullback: not all variable names in A make sense in C"; -- we also verify that every variable name for A is also a variable name for C.  We need this for some substitutions.
    --We *should* verify that the variables get sent to appropriate variables.  But we don't right now.
    coeff1 := coefficientRing(A);
    coeff2 := coefficientRing(C);
    coeff3 := coefficientRing(B);
    if not ( ((coeff1 === coeff2) and (coeff1 === coeff3))) then error "pullback: not all coefficient rings are the same"; --we verify all coefficient rings are the same.


    --in order to do our computation, we need A\oplusC to be a finite R-module.  Since C is obviously a finite R-module (it is a quotient of R),
    --it is sufficient to verify that A is a finite R-module.  We begin by building a map R->A.
    while(not AFiniteOverR) do (--checking finiteness
	RImageInA = {};
	m := 0;
	n := 0;
	k := 0;
	
	--this loop builds a list of where the generators of R get sent in A.
	-- send Igens to themselves, send C gens to random elements making the diagram commute, send A^iIGens to themselves.
	while( #(RImageInA) < #(gens R) ) do (
	    if(#(RImageInA) >= ( #(IGens)+#(CGensInA) ) ) then ( --this is only necessary on further loops (if we added more I terms basically).
		RImageInA = append(RImageInA,allHigherAITerms#n);
		n = n+1;
	    );
	    if( (#(RImageInA) >= #(IGens)) and (#(RImageInA) < (#(IGens)+#(CGensInA)))) then(--this appends elements that get mapped to generators of C.
		if( (#(RImageInA) >= #(IGens)) and (#(RImageInA) < (#(IGens)+#(CGensInA)))) then(
			RImageInA = append(RImageInA,(CGensInA)#k);
			k = k+1;
		);				
	    );
	    if( #(RImageInA) < #(IGens) ) then(--this appends elements that get mapped to generators of I.
		RImageInA = append(RImageInA,(IGens)#m);
		m = m+1;
	    );
	);
	   k = 0;

    	--now that we have the list of where things should get sent, we create the map R->A.
	RtoAMap = map(A, R, apply(RImageInA, zz -> sub(zz,A)));

	--now we check to see if A is a finite R-module.
	try( AasRmodule = pushFwd(RtoAMap, NoPrune=>true) )  -- if this is reached, then A is finite as an R-mod,
	    then(   	--we are done, we can leave this giant loop.
		AFiniteOverR = true;
		if (o.Verbose == true) then (
			print(concatenate("Passed finiteness check with vars ", toString(varsCandidates)));
		);
	    )
	    else( --we are not done.  Thus we multiply the generators of the ring (to a power), times the elements of I.
	    	if (o.Verbose == true) then (
			print("Failed finiteness check, add more stuff");
		);
		addedTerms = {};
		listAddedTerms = {};
		
		-- changes A^i to A^{i+1}
		currentATermsInR = listProduct(currentATermsInR,AGens);	
		
		if (o.Verbose == true) then (
			print(concatenate("Working on exponent ", toString(AExponent)));
		);
		AExponent = AExponent+1;
		
		-- adds the (gens A^i) times (gens I) terms to R
    	    	addedTerms = listProduct(currentATermsInR,(IGens));
		
		listAddedTerms = varNameFixer(addedTerms, concatenate("A",toString(AExponent),"I"));
		if (o.Verbose == true) then (-- print the new stuff we are adding
			print "varsCandidatesA, varsCandidates";
			print varsCandidatesA;
			print varsCandidates;
		);
		varsCandidatesA = varsCandidatesA | addedTerms;
		varsCandidates = varsCandidates | apply(addedTerms, zz->sub(zz,AsumCasRing));
		
		allHigherAITerms = allHigherAITerms | addedTerms;
		listOfVarsCandidates = listOfVarsCandidates | listAddedTerms;
		
		allHigherAITermsVars = allHigherAITermsVars | listAddedTerms;
		
		R = baseRing[listOfVarsCandidates];
		
	    );
	);
	


	--next we build a map from R to C as a ring map.  The C generators get sent to themselves.
	RImageInC = {};
	   -- send Igens to 0, sends CGens to themselves, sends A^iI gens to 0
	   j := 0;
	   --RImageInC is just a list of where the variables get sent.
	while( #(RImageInC) < #(gens R) ) do (
	    if(#(RImageInC) >= ( #(IGens))+#(CGensInA)) then ( --if we are at the end, send them to zero.
		RImageInC = append(RImageInC,0);
	    );
	    if( (#(RImageInC) >= #(IGens)) and (#(RImageInC) < (#(IGens)+#(CGensInA)))) then(--C generators get sent to themselves.
		RImageInC = append(RImageInC,(CGens)#j);
		j = j+1;
	    );
	    if( #(RImageInC) < #(IGens) ) then(
		RImageInC = append(RImageInC,0);
	    );	
	);
    	RtoCMap = map(C,R,matrix{RImageInC});
	
	--getting RtoAsumC as both ring maps and R mod maps
	varsCandidates = apply(varsCandidates, zz -> sub(zz, AsumCasRing));
	RtoAsumCasRingMap := map(AsumCasRing, R, varsCandidates );
	
	--now we push forward.
	AsumCasRMod := pushFwd(RtoAsumCasRingMap, NoPrune=>true);
	
	RtoAsumCasRModMap := map(AsumCasRMod#0,R^1,(AsumCasRMod#2)( sub(1,AsumCasRing) ));
	-- done with that
	
	

	--We next describe the strategy, first view B as a C-module.  For every element of A, generating A as an R-module over R, consider the corresponding element of B (as a C-module).
--These are implicitly C-module maps C->g_* B.  Pushforward each these maps as an R-module map.  We can then find the image of 1_C in B for each such map (now all is represented as an R-module).  With this information, we can construct (manually) a map from A (viewed as an R-module), to B=A/I (viewed as an R-module via the map C->B=A/I).  Ie, we know where the generators of A (as an R-module) go to in B.  Now, we can simply subtract those two maps A->B and C->B to get a map A \oplus C -> B (all defined over R).  Then take a kernel...

	BasCmodule := pushFwd(g, NoPrune=>true);
	--the following takes the generators of A as an R-module in B, and then represents
	--them as elements when viewing B as a C-module
	AmodGensInBasCModList := apply(first entries (AasRmodule#1), zz->(BasCmodule#2)(f(zz)));
	--**Warning, in what is below I am assuming that C as an R-module is *always* represented as a quotient module.  In particular, 1 in R gets sent to 1 in C.  This should be fine as long as pushFwd behaves as expected.
	--Now get a list of generators of A, in B, as an R-module, via the map through C.
	m1 := null;
	AmodGensInBasRModListTemp := apply(AmodGensInBasCModList, vv->( pushFwd(RtoCMap, vv, NoPrune=>true)) );
	AmodGensInBasRModList := apply(AmodGensInBasRModListTemp, vv->(map(target vv, R^1, matrix vv) ));
	BasRmodule := target (AmodGensInBasRModList#0);
	AtoBasRmoduleMap := map(BasRmodule, AasRmodule#0, matrix fold( (a,b)->(a|b), AmodGensInBasRModList));
	CtoBasRmoduleMap := (-1)*pushFwd( RtoCMap, (BasCmodule#2)(sub(1,B)),  NoPrune=>true);
	AsumCtoBasRmoduleMap := AtoBasRmoduleMap|CtoBasRmoduleMap;
	--that should construct AsumC -> B.  We can take the kernel etc.
	--Now we need R -> AsumC.  Let's do this directly.
	RtoAasRmodule := (AasRmodule#2)(sub(1,A));
	CasRmodule = pushFwd(RtoCMap, NoPrune=>true);
	RtoCasRmodule := (CasRmodule#2)(sub(1,C));
	RtoAsumCasRmodule := RtoAasRmodule || RtoCasRmodule;
	AsumCasRmodule := source AsumCtoBasRmoduleMap;
	
	
	--ok, next I think we should go through the columns of the kernel, check if that column is in the image of RtoAsumCasRmodule (plus whatever columns we already have thrown in).  If it isn't, then add a variable.  Otherwise, skip it.
	K := kernel AsumCtoBasRmoduleMap;
	if (o.Verbose == true) then (
		print "Let's check if the image is inside the kernel";
		print isSubset(image RtoAsumCasRmodule, K);
	);

	KGens := apply(toList(0..numcols(gens K)-1), i -> map(AsumCasRmodule, R^1, (matrix gens AsumCasRmodule)*(matrix ((gens K)_i)) ) );

	i := 0;
	currentMap := RtoAsumCasRmodule;
	curMapImage := image(currentMap);
	myFlag := false;
--	addedKGens := null;
	z := 1;
	--warning, we are assuming here that the source of pushFwd of a map is the same as pushFwd of the same module
	gensVector := (idempotent*sub(AasRmodule#1, AsumCasRing)) |((1-idempotent)*sub((pushFwd(RtoCMap, NoPrune=>true))#1, AsumCasRing) );
	if (isSubset(K, image currentMap)) then myFlag = true;
	
	while ((myFlag == false) and (i < #KGens) ) do (
		
		if (not isSubset(image (KGens#i), curMapImage) ) then (
			currentMap = currentMap | (KGens#i);
			curMapImage = image currentMap;
			varsCandidates = append(varsCandidates,first first entries (gensVector*RtoAsumCasRingMap(matrix (KGens#i))) );
			listOfVarsCandidates = append(listOfVarsCandidates,concatenate("KGens",toString(z)));
--		    	addedKGens = append(addedKGens, KGens#i);
			z = z+1;
		
			if (isSubset(K, curMapImage)) then myFlag = true;
		);
		i = i+1;
	);
	varsCandidates = apply(varsCandidates, zz -> sub(zz, AsumCasRing));
	R = baseRing[listOfVarsCandidates];
	newRtoAsumCRingMap := map(AsumCasRing, R, varsCandidates);
	finalQuotient := R/(ker newRtoAsumCRingMap);
	newRtoAsumCRingMap = map(AsumCasRing, finalQuotient, varsCandidates);
	projA := map(A,AsumCasRing, matrix{(gens A)|{1}|toList( #(gens C):0)});
	projC := map(C,AsumCasRing, matrix{toList(#(gens A):0)|{0}| gens C});
	newRtoAMap := projA*newRtoAsumCRingMap;
	newRtoCMap := projC*newRtoAsumCRingMap;
	{finalQuotient, newRtoAMap, newRtoCMap}
);

varNameFixer = method();
varNameFixer(List, String) := (inputList, nameOfOutputs) -> (
    outputList := {};
    i := 1;
    while(#(outputList) < #(inputList)) do (
	outputList = append(outputList, (concatenate(nameOfOutputs, toString(i))));
	i = i+1;
    );
    outputList
);

-- This really should be implemented already somewhere, but I can't find it.
-- it gives a product of all the elements in the two lists
-- an error is thrown if the lists are not of ring elements, or are ring elements over different rings,

listProduct = method();
listProduct(List,List) := (list1,list2) -> (
    output := {};
    matrix1 := matrix{list1};
    matrix2 := transpose(matrix{list2});
    allProducts := matrix2 * matrix1;

    if(matrix1 != 0 and matrix2 != 0) then(
    	output = flatten entries allProducts;
    )
    else(
	output = list1|list2;
    );
	
    output
);

-- R is S/I for some S. Pushes forward R module M by the map (S--> R)
-- an alternate pushforward method which is often much faster in the cases outlined above.

myPF = method()
myPF(Ring,Matrix) := (R,A) ->(
    startTime := cpuTime();

    phi := map(ambient R, R, apply(gens R, i -> sub(i, ambient R)));
    newA := phi(matrix A);
    M := coker(newA);

    newM := M ** ((ambient R)^1/ideal(R));
    endTime := cpuTime();
	
    presentation newM
);

--computes the direct sum of two rings.  It outputs the ring and the idempotent.  Note that the variable names of the two rings must be distinct.
export{"internalUseDirectSum"}
internalUseDirectSum = method();
internalUseDirectSum(Ring,Ring) := (myRing1,myRing2) -> (
    	myCoeffRing := coefficientRing(myRing1);
	
	vars1 := first entries vars ambient myRing1;
	vars2 := first entries vars ambient myRing2;
	e1 := local e1;
	
	unQuotiented := myCoeffRing[vars1|{e1}|vars2];
	
	totalVars := first entries vars unQuotiented;
	firstVars := take(totalVars, #vars1);
	secondVars := take(totalVars, -(#vars2));
	
	JObviousRelation := ideal (flatten{e1^2-e1, apply(firstVars, zz -> e1*zz-zz), apply(secondVars, zz->e1*zz) });

	f1 := map(unQuotiented, ambient myRing1, firstVars);
	J1 := f1(ideal(myRing1));
	
	f2 := map(unQuotiented, ambient myRing2, secondVars);
	J2 := f2(ideal(myRing2));
	
	outputRing :=unQuotiented/(JObviousRelation + J1 + J2);
	
	{outputRing, e1}
 )


beginDocumentation()

document{
  Key => Pullback,
  Headline => "pullback in the category of rings",
  EM "Pullback", " is a package that implements pullback for diagrams of rings",
  Caveat => "Works only for maps of rings finitely generated over a base field and one of the two maps is surjective"
  }

document{
  Key => {(pullback,RingMap,RingMap),[(pullback,RingMap,RingMap),Verbose]},
  Headline => "Compute the pullback of a diagram of rings",
  TEX "The pullback functor in the category of rings.  Given ring maps $f : A \\to B$ and $g : C \\to B$, this tries to compute the pullback of $\\{A \\to B \\leftarrow C\\}$ in the category of rings.  It requires that $A \\to B$ is a surjective map of rings (otherwise it will give an error) and it requires that $C \\to B$ is finite (otherwise it will never terminate).  Currently, it requires that the variable names of the rings $A$ and $C$ are distinct and that the variable names of $A$ are variable names of $B$ and those variables get sent to one another.  If the Verbose option is turned on, then certain steps in the process will be specified.",
  Usage => "pullback(f,g)",
  Inputs => { "f","g" },
  Outputs => {{"The pullback R of (f: A->B<-C :g) as a ring"},{"the induced map R->A"},{"the induced map R->C"}},
  TEX "We begin by doing a pullback which glues two lines together.",
  EXAMPLE lines ///
  A = QQ[x];
  I = ideal(x);
  B = A/I;
  C = QQ[y];
  f = map(B, A);
  g = map(B, C, {0});
  (pullback(f,g))#0
  ///,
  TEX "We next construct the pinch point, otherwise known as Whitneys umbrella, by gluing.",
  EXAMPLE lines ///
  A = QQ[x,y];
  I = ideal(x);
  B = A/I;
  C = QQ[u];
  f = map(B, A);
  g = map(B, C, {y^2});
  (pullback(f,g))#0
  ///,
  TEX "We include a final example showing how to create a cusp.",
  EXAMPLE lines ///
  A = QQ[x];
  I = ideal(x^2);
  B = A/I;
  C = QQ[]; 
  f = map(B, A); 
  g = map(B, C, {});
  (pullback(f,g))#0
  ///
  }

document{
  Key => {internalUseDirectSum,(internalUseDirectSum,Ring,Ring)},
  Headline => "Compute direct sum of two rings.",
  TEX "A method which computes the product of two rings in the category of rings.",
  Usage => "internalUseDirectSum(A,C)",
  Inputs => { "f","g" },
  Outputs => {{"The ring A times C"},{"The idempotent (1_A, 0)"}},
  TEX "This function requires that $A$ and $C$ have the same coefficient field and that the variable names of $A$ and $C$ are distinct.  The variable names of the two rings in the direct sum ring remain the same.  This function was originally going to be internal (hence the name), but may be useful in other cases too.  ",
  TEX "We compute an example",
  EXAMPLE lines ///
  A = QQ[x];
  C = QQ[y];
  (internalUseDirectSum(A,C))#0
  ///,
}


-- **TEST0**  This glues two lines together.  We check that it has two minimal primes, two variables, and the product of the first two variables is zero.
TEST ///
  A = QQ[x];
  I = ideal(x);
  B = A/I;
  C = QQ[y];
  f = map(B, A);
  g = map(B, C, {0});
  l1 = pullback(f,g);
  vlist = first entries vars (l1#0);
assert( (#(minimalPrimes (ideal l1#0)) == 2) and (#vlist == 2) and (0 == (vlist#0)*(vlist#1)) )
///

-- **TEST1**  This makes a pinch point.  We check that it has one minimal prime, that it has 3 variables, and that the singular locus is dimension 1 while the ambient object is dimension 2.  We also check that the ring we construct is a subring of A.
TEST ///
  A = QQ[x,y];
  I = ideal(x);
  B = A/I;
  C = QQ[u];
  f = map(B, A);
  g = map(B, C, {y^2}); 
  l1 = pullback(f,g);
  vlist = first entries vars (l1#0);
  assert ( (#(vlist) == 3) and (dim l1#0 == 2) and ((#minimalPrimes (ideal l1#0)) == 1) and (1 == dim singularLocus (l1#0)) and (ker (l1#1) == ideal(sub(0,l1#0))) )
///
  
-- **TEST2**  We now glue three lines together and verify that we get a non-Gorenstein ring of dimension 1 with three minimal primes.
TEST ///
  A = QQ[x,y]/ideal(x*y);
  I = ideal(x,y);
  B = A/I;
  C = QQ[z];
  f = map(B, A);
  g = map(B, C, {0});
  l1 = pullback(f,g);
  vlist = first entries vars (l1#0);
  ambPull = ambient l1#0;
  ambId = ideal l1#0;
assert( (#(minimalPrimes (ideal l1#0)) == 3) and (#vlist == 3) and (0 == (vlist#0)*(vlist#1)) and (0 == (vlist#0)*(vlist#2)) and (0 == (vlist#2)*(vlist#1)) and ( dim(l1#0) == 1) and (pdim ((Ext^2(ambPull^1/ambId, ambPull))**(l1#0)) > 0) )
///

-- **TEST3**  We next construct an interesting example that should be Cohen-Macaulay (and seminormal and weakly normal, but not WN1, note we don't have a good way to check those things yet).  We also check that the singular locus with reduced structure, is regular.  (it should be a copy of A^1).  
TEST ///
  A = ZZ/2[x,y];
  I = ideal(x*y);
  B = A/I;
  C = ZZ/2[z];
  f = map(B, A);
  g = map(B, C, {x^2+y});
  l1 = pullback(f,g);
  vlist = first entries vars (ambient l1#0);
  ambPull = ambient l1#0;
  ambId = ideal l1#0;
  assert ( (dim l1#0 == 2) and ((#minimalPrimes (ideal l1#0)) == 1) and (1 == dim singularLocus (l1#0)) and (dim singularLocus (ambPull/(ass ideal singularLocus l1#0)#0) < 0) and (dim Ext^3(ambPull^1/ambId, ambPull^1) < 0) and (ker (l1#1) == ideal(sub(0,l1#0))) )
///

-- **TEST4** We construct a non-Cohen-Macaulay ring via gluing.  
TEST ///
  A = QQ[x];
  I = ideal(x);
  B = A/I;
  C = QQ[z]/ideal(z^2);
  f = map(B, A);
  g = map(B, C, {x});
  l1 = pullback(f,g);
  vlist = first entries vars (l1#0);
  ambPull = ambient l1#0;
  ambId = ideal l1#0;
  assert ( (dim (l1#0) == 1) and (#vlist == 2) and (dim Ext^2(ambPull^1/ambId, ambPull^1) >= 0) )
///
  
-- **TEST5** We construct another non-Cohen-Macaulay ring by gluing.  We also check that the ring is a subring of A and that the generators of the pullback map to a certain ideal in A
TEST ///
  A = QQ[x,y];
  I = ideal(x^2,x*y,y^2);
  B = A/I;
  C = QQ[u]/ideal(u);
  f = map(B, A);
  g = map(B, C, {0});
  l1 = pullback(f,g);
  vlist = first entries vars (ambient l1#0);
  ambPull = ambient l1#0;
  ambId = ideal l1#0;
  assert ( (dim l1#0 == 2) and ((#minimalPrimes (ideal l1#0)) == 1) and (dim Ext^(#vlist - 1)(ambPull^1/ambId, ambPull^1) == 0 ) and ( ideal(apply(vlist, t->(l1#1)(t))) == (ideal(sub(x,A),sub(y,A)))^2 ) and (ker (l1#1) == ideal(sub(0,l1#0))) )
///

end

--***Changelog***---

--1.01, added support for C with no variables.  Improved documentation.  Turned off some printed text when Verbose is turned off.
--1.02, improved documentation including adding documentation for the product of rings.
--1.03, improved error reporting so that it provides useful information about the problem is

