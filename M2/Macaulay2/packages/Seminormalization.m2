newPackage( "Seminormalization",
	Version => "0.21",
	Date => "September 30th, 2019",
	Authors => {
		{Name => "Karl Schwede",
		Email => "schwede@math.utah.edu",
		HomePage => "http://math.utah.edu/~schwede/"
		},
		{Name => "Bernard Serbinowski",
		Email => "bserbinowski@gmail.com"
		}
	},
	Headline => "seminormalization of rings",
	PackageImports => {"IntegralClosure"},
	PackageExports => {"Pullback", "PushForward"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Seminormalization package for Macaulay2",
	     "acceptance date" => "5 December 2019",
	     "published article URI" => "https://msp.org/jsag/2020/10-1/p01.xhtml",
	     "published article DOI" => "https://doi.org/10.2140/jsag.2020.10.1",
	     "published code URI" => "https://msp.org/jsag/2020/10-1/jsag-v10-n1-x01-Seminormalization.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/Seminormalization.m2",
	     "release at publication" => "7a7e6f96b0122482f5d60306f34d06fd8ae1e885",	    -- git commit number in hex
	     "version at publication" => "0.21",
	     "volume number" => "10",
	     "volume URI" => "https://msp.org/jsag/2020/10-1/"
	     }
)

export{
	"seminormalize",
	"isSeminormal",
	--"renameVariablesOfTarget",
	"ringProduct",
	"betterNormalizationMap",
	--potentially the following functions should not be exported
	"flattenVarDegrees",
	"ringToAlgebraMap",
	--"listOfVariableCreator",
	--"seminormalizationRecursive",
	--"intersectSeminormalizationAndExtension",
	"findElementMappingToTarget" --essentially an internal function, but possibly useful to others
};

exportMutable{
	"Yy"
}

--*************************************************


isSeminormal = method()

isSeminormal(Ring) := R1 -> (
	R1toR1SN := (seminormalize R1)#1;
	pushList := pushFwd(R1toR1SN);
	myMap := (pushList#2)(sub(1, target R1toR1SN));
	isSurjective myMap
);

seminormalize = method( Options => { Variable => symbol Yy, Strategy => {} } )

seminormalize(Ring) := o->R1 -> (
	seminormalizedList := internalSeminormalize(R1, Strategy=>o.Strategy);
	mapR1toR2 := seminormalizedList#1;
	renamedVarsList := renameVariablesOfTarget(o.Variable, mapR1toR2);
	--this returned the {original map fixed up, map from old ring to new ring, map from new ring to old ring}

	{target renamedVarsList#0, renamedVarsList#0, (seminormalizedList#2)*(renamedVarsList#2)}
);

internalSeminormalize = method( Options => { Variable => symbol Yy, Strategy => AllCodimensions  })

internalSeminormalize(Ring) := o -> R1 -> (
	seminormalizationRecursive(R1, 1, Strategy=>o.Strategy)
);

--internal functions

--*****************************************
--this takes the input ring (which is assumed to be reduced)
--and an integer used to be
seminormalizationRecursive = method(Options => {Variable => symbol Yy, Verbose => false, Strategy => {}})
seminormalizationRecursive(Ring, ZZ):=o->(inputRing, inputInteger)->(
	-*
	  STRATEGY
	  The objective is to create the following map diagram
	  (R^N/rad(IR^N))^N <----(psi)-----Semi(R/rad(I))
	  ^                                ^
	  |                                |
	  |                                |
	  (phi)                            |
	  |                                |
	  |                                |
	  R^N <--------------------------- Semi(R)
	  Here ^N means normalization of, Semi means seminormalization
	  I stands for the conductor ideal and rad means radical of
	  phi and psi are used to label the maps.
	  From this diagram, we will pullback to recover Semi(R).
	  While the construction of Phi is clearly doable, the construction of psi
	  requires us to have the seminormalization. Therefore the process is recursive.


	  FUNCTION INFORMATION
	  Return value 1: The seminormalized ringnml;/

	  Return value 2: The map from the input ring to the seminormalized ring
	  Return value 3: The map from the seminormalization to the normalization
	  Input value 1: An input ring to be seminormalized
	  Input value 2: An input integer. For internal use. Is provided by the driver method.
	  Options: Verbose. Allows for disabling print statements.  Strategy.  Allows for controlling integralClosure strategy.

	  Should be called through internalSeminormalization
	*-



	--first we grab the ring, flattened
	inputRing=(flattenRing(inputRing))#0;

	if dim(inputRing)<=0 then(
		if o.Verbose  then print "seminormalizationRecursive: Returning because input ring has dimension 0.";
		return {inputRing, map(inputRing, inputRing), map(inputRing, inputRing)};
	);
	normalizationMapOfInputRing := betterNormalizationMap(inputRing, Strategy=>o.Strategy);
	normalizedInputRing:= target normalizationMapOfInputRing; --this will become the source of map Phi

	conductorIdeal:=conductorOfRingMap(normalizationMapOfInputRing);--this is I in the earlier diagram.
	if(conductorIdeal===ideal(1_inputRing)) then (
		if o.Verbose then print "seminormalizationRecursive: Returning because input ring is normal.";
		return {normalizedInputRing, normalizationMapOfInputRing, map(normalizedInputRing, normalizedInputRing)};
	);

	if o.Verbose  then print "The input ring (or recursive input ring) was not already seminormalized";
	radicalOfConductor:=radical(conductorIdeal); --we need to be careful, radical does not always work, see docs.
	--this is rad(I) in the earlier diagram.
	if o.Verbose then print "checkpoint 1";
	radicalOfConductorR:=radical((normalizationMapOfInputRing(conductorIdeal))*normalizedInputRing); --again, radical not the best

	inputRingModRadical:=(flattenRing(inputRing/radicalOfConductor))#0; --mod out by the radical and flatten
	--this would be R/rad(I). we will recursively pass this to be seminormalized.

	if o.Verbose  then print "checkpoint 2";
	normalizedInputRingModRadicalR:=(flattenRing(normalizedInputRing/radicalOfConductorR))#0;
	--this would correspond to (R^N)/rad(IR^N). Flattended to remove stuff and allow us to interact with it better

	mapPhi:=map(normalizedInputRingModRadicalR, normalizedInputRing); -- The map from the normalized Ring to the normalized ring mod out by the radical.
	--and that's map phi.

	if o.Verbose then << "checkpoint 3" << endl << "input to the recursion is " << inputRingModRadical << endl;
	recursionOutput := seminormalizationRecursive(inputRingModRadical, inputInteger+2, o);--recurse. inputInteger goes up by 2. inputInteger+1 is used below. Should allow the user to track where stuff is renamed (at least a little)
	--recursionOutput now has Semi(R/rad(I)) and a map from inputRingModRadical to it. So R/rad(I) -> Semi(R/rad(I))

	mapPsiV1 := map( normalizedInputRingModRadicalR, inputRingModRadical, sub(matrix normalizationMapOfInputRing, normalizedInputRingModRadicalR) );--have to tell Macaulay2 where to send stuff. Fortunately, as we made the map and know both sets of variables, this can be done.
	--this is a map R/rad(I) -> R^N/rad(IR^N). It's temporary, and included because it's hard to keep things straight when composing maps.

	if o.Verbose then print "checkpoint 4";

	mapPsi1 := (intersectSeminormalizationAndExtension(recursionOutput#1, mapPsiV1, inputInteger+1))#1;
	--both recursionOutput#1 and mapPsiV1 are maps from R/rad(I). Therefore what we are doing here is finding a ring in which both R^N/rad(IR^N) and Semi(R/rad(I)) live in.

	mapPsi:=mapPsi1*((ringRenamerAndMap(source(mapPsi1), inputInteger))#2);
	--finally compose the above with a renamed version of itself. This is just to rename variables so that we avoid collisions.
	--to clarify, this is a map from  the overring to R^N/rad(IR^N)

	sorPsi:=source mapPsi;
	sorPsi1:=prune(sorPsi);
	mapPsi=mapPsi*(sorPsi.minimalPresentationMapInv);
	--sources and such because it's easier when they're extracted.

	normalizedInputRingModRadicalRRenamed:=((ringRenamerAndMap(target(mapPhi), inputInteger))#1);
	--renaming R^N/rad(IR^N) and providing a map to go with it, but actually only taking the map
	normalizedInputRingRenamed := ringRenamerAndMap(source(mapPhi), inputInteger);
	--renaming R^N and providing a map to go with it.
	mapPhi=normalizedInputRingModRadicalRRenamed*mapPhi*(normalizedInputRingRenamed#2);
	--chaining maps together. this is only so that we avoid naming collisions.
	normalizationMapOfInputRing = (normalizedInputRingRenamed#1)*normalizationMapOfInputRing;
	--chaining maps together. this is only so that we avoid naming collisions.
	mapPsi=normalizedInputRingModRadicalRRenamed*mapPsi*((ringRenamerAndMap(source(mapPsi), inputInteger))#2);
	--chaining maps together. this is only so that we avoid naming collisions.

	mapPsi=map(target mapPhi, flattenVarDegrees(source mapPsi), matrix mapPsi);
	--flatten the source of mapPsi and then remap it to the target. target is given by mapPhi, because we already sent that one to the right place and they go to the same place.
	if o.Verbose then  << "checkpoint 5" << endl << "We are about to do a pullback.  Normalization to conductor is" << mapPhi << endl << "gluing map is" << mapPsi;
	temp := pullback(mapPhi, mapPsi, Verbose=>false);
	if o.Verbose then << "Here is the pullback output:" << temp;

	--now we need to reconstruct the map from inputRing to RSN = temp#0
	RSNtoRN := temp#1;--the map from Semi(R) to R^N
	RVarsInRN := apply(gens inputRing, xy -> normalizationMapOfInputRing(xy));
	RVarsInRSN := apply(RVarsInRN, yz -> findElementMappingToTarget(RSNtoRN, yz));

	SNmap := map(temp#0, inputRing, RVarsInRSN);
	{
		temp#0, -- the seminormalization
		SNmap, --  the seminormalization map,
		(normalizedInputRingRenamed#2)*(RSNtoRN) --the map from the seminormalization to the normalization
	}
)

--*****************************************
--RENAMING FUNCTIONS
--*****************************************

--given a list of variables, this creates a new list which can be used for variables which should help avoid var name collisions
listOfVariableCreator=(inputList, inputInteger)->(
	for count from 0 to (length inputList) -1 list concatenate(replace("[^[:alnum:]]+", toString count, toString inputList#count), "RE", toString inputInteger)
)

--this renames the variables of a ring (to avoid conflicts)
ringRenamerAndMap = method()
ringRenamerAndMap(Ring, ZZ):=(inputRing, inputInteger)->(
	currentList:=gens ambient inputRing;
	nextList:=listOfVariableCreator(currentList, inputInteger);
	outputRing:=newRing(inputRing, Variables=>nextList);
	additionalList:=gens ambient outputRing;
	{outputRing, map(outputRing, inputRing, additionalList), map(inputRing, outputRing, currentList)}
);

ringRenamer=(inputRing, inputInteger)->(
	currentList:=gens ambient inputRing;
	nextList:=listOfVariableCreator(currentList, inputInteger);
	outputRing:=newRing(inputRing, Variables=>nextList);
	outputRing
)

--this is an internal function which renames the variables of the seminormalized ring based on the
--symbol the user choose (default Y)
renameVariablesOfTarget = method()

--returns the {original map, map from old ring to new ring, map from new ring to old ring}

renameVariablesOfTarget(Symbol, RingMap) := (varName, mapR1toR2) -> (
	R2 := target mapR1toR2;
	newVarList := apply(#(gens R2), tt -> varName_tt);
	newSeminormalization := newRing(R2, Variables=>newVarList);
	gMap := map(newSeminormalization, R2, gens newSeminormalization);
	hMap := map(R2, newSeminormalization, gens R2);
	(gMap*mapR1toR2, gMap, hMap)
);

--one more way to do this, this renames the variables, then returns the maps both ways
varRenamer = method()
varRenamer(String, Ring) := (varName, R1) -> (
	newVarList := apply(#(gens R1), tt -> (varName | toString(tt)) );
	R1A := newRing(R1, Variables=>newVarList);
	mapTo := map(R1A, R1, gens R1A);
	mapFrom := map(R1, R1A, gens R1);
	{R1A, mapTo, mapFrom}
);

--*********************************
--Helper functions
--*********************************

--the following function computes the conductor of a map of rings
conductorOfRingMap = method()
conductorOfRingMap(RingMap) :=RingMapInput ->(
	mapFromSorToTar:=mapModules(RingMapInput);
	ann(coker(mapFromSorToTar))
)

mapModules = method()
mapModules(RingMap) := RingMapInput ->(
--Creates a map from the Source viewed as a Source-module to the target viewed as a Source-module.
--note needs loadPackage "PushForward";
	actualMod:= pushFwd(RingMapInput);
	tempVal:=target RingMapInput;
	(actualMod#2)(sub(1, tempVal))
)


findElementMappingToTarget = method()
--given phi : R -> S, and an element g in S, this finds f in R such that phi(f) = g (if possible).
--if it fails it throws an error?  (or maybe we should avoid this check...)
--If I recall correctly, I believe the technique used was suggested to us by Adam Boocher
findElementMappingToTarget(RingMap, RingElement) := (phi, gg) -> (
--STRATEGY
--We use an elimination order to accomplish this
	R1 := source(phi);
	S1 := target(phi);
	newVar := local newVar;
	R2 := coefficientRing(R1)[{newVar} | (gens ambient R1), MonomialOrder=>Eliminate 1];
	phi2 := map(S1, R2, {gg} | apply(gens R1, xx -> phi(xx)));
	K2 := ker phi2;
	answer := sub(sub(newVar, R2)%K2, R1);
	if (phi(answer) == gg) then return answer else error "findElementMappingToTarget: No element mapping to target element was found.";
);


--this was formerly findOverringV2
findOverring = method()
--    given f : A -> B, g : A -> C, both integral (typically f : A->A^SN), this finds an integral
--  extension D in which B and C live
--      so that A is in the intersection of B and C.
--  Furthermore, D should have the same number of minimal primes
--      as C (and they should line up in terms of pullback by containment).
--Finally, the D that is constructed has the nice property that its variable names are lined up with the variable names of the new B and C

findOverring(RingMap, RingMap) := (f,g) -> (
--STRATEGY
--We tensor the two rings together.  Then line up minimal primes (if necessary)
	assert(source f === source g);  --first do checking to see if these rings map to the same place
	A := source f; --define the name of the source

	rB := ringToAlgebraMap(f, 0);  --turn B into an A algebra
	newB := rB#0; --grab the actual ring
	flatB := flattenVarDegrees((flattenRing(newB))#0); --remove extraneous variable degrees
	newfAlg := (rB#1)*f; --grab the map to the new f,
	newf := map(flatB, A, sub(matrix newfAlg, flatB)); --but we need to actually map to the one without funny degrees
	rC := ringToAlgebraMap(g, 1); --turn C into an A algebra
	newC := rC#0; --grab the ring
	flatC := flattenVarDegrees((flattenRing(newC))#0); --remove extraneous variable degrees
	newgAlg := (rC#1)*g; --get the map to the new ring
	newg := map(flatC, A, sub(matrix newgAlg, flatC)); --but we need to actually map to the one without funny degrees
	BigBC := flattenVarDegrees((flattenRing(newB ** newC))#0); --this is the first attempt at a big ring,
							--it is just the tensor over A, the problem is it might have too many minimal primes

	BtoBigBC := map(BigBC, flatB);  --the map from B to BigBC
	CtoBigBC := map(BigBC, flatC);  --the map from B to BigBC
	ambB := ambient flatB; --get the ambient ring of our real flatB
	minPrimeListBigBC := apply(minimalPrimes(ideal BigBC), q -> sub(q, BigBC));
		--thus we grab the minimal primes of BigBC
	rawMinPrimeListC := minimalPrimes(ideal flatC);
	minPrimeListC := new MutableHashTable from apply(#(rawMinPrimeListC), i->{i,  sub(rawMinPrimeListC#i, flatC) } );
		--and the minimal primes of C

	newPrimeList := new MutableHashTable;
	invPrime := null;
	i := 0;
	flag := false;
	--the following loop goes through the minimal prime list of BigBC.  It removes a minimal prime from the minimal
	--prime list of C each time it finds a match.  When C runs out of primes, the loop should exit.
	--newPrimeList should store the list of the minimal primes
	while ((#minPrimeListC > 0) and (#minPrimeListBigBC > 0)) do (
		invPrime = preimage(CtoBigBC, minPrimeListBigBC#0); --take the first prime of BigBC and pull it back to B
		i = 0;
		flag = true;
		while( flag and (i < #minPrimeListC) ) do(
			if (invPrime == minPrimeListC#((keys minPrimeListC)#i) ) then ( --check if that pulled back prime is in minPrimeList for B
				flag = false;
				newPrimeList#(#newPrimeList) = minPrimeListBigBC#0; --add it to the newPrimeList
				remove(minPrimeListC, ((keys minPrimeListC)#i));
			);
			i = i+1
		);
		if flag then error "findOverring: The tensor product has too many minimal primes.  One of the maps is not integral.";
		minPrimeListBigBC = drop(minPrimeListBigBC, {0,0});--drop the prime we just compared on
	);
	--do a check to make sure every prime got covered.
	--once we have a list of minimal primes lying over primes of B, we just intersect them.
	if (#minPrimeListC > 0) then error "findOverring: Not every prime in target g is covered by a minimal prime, are you sure that f is integral?";
	newZero := intersect(values newPrimeList); --this is the new zero ideal of our big ring.
	{(flattenRing(BigBC/newZero))#0, --return the overring,
		newf, --the map A->newlabeledB,
		newg, --the map A->newlabeledC,
		map(flatB, target f, sub(matrix rB#1, flatB)), --the map oldB -> newlabeledB (an iso)
		map(flatC, target g, sub(matrix rC#1, flatC)) --the map oldC -> newlabeledC (an iso)
	}
);

--this was formerly ringToAlgebraMapV2
--given a map of rings A->B this writes B = A[...]/?.  Useful for tensoring rings together.
ringToAlgebraMap = method()
--the first input is the map, the second is the integer on which the map is based
--it returns the target of the map and then the map from the old ring to the new one
ringToAlgebraMap(RingMap) := List => (ff) -> (
	ringToAlgebraMap(ff, 1)
)

ringToAlgebraMap(RingMap, ZZ) := List => (ff, nn) -> (
--STRATEGY
	--0.  Write A = QQ[x1, ..., xd]/J
--1.  let [YY1, ..., YYn] correspond to variables of B
--2.  form A[YY1, ..., YYn],
--3.  Map to B, sending YY's to the corresponding generators of B.
--    A[YY1, ..., YYn] -> B, where the xi go to wherever they originally go, and the YYs go to the generators.
--4.  Let I be the kernel, then QQ[x1, ..., xd, YY1, ..., YY_n]/I is isomorphic to B.  And we have a canonical map
--    A to A[YY1, ..., YY_n]/I, variables go to variables.
	A:=source ff;
	B:=target ff;
	currentListB:=gens ambient B;    --get the variables of B
	nextListB:=listOfVariableCreator(currentListB, nn); --rename the variables of B (using the nn given to avoid collisions)
	currentListA:=gens ambient A; --get the variables of A (we leave these alone)
	newBamb := (coefficientRing A)[currentListA | nextListB]; --this is QQ[x1, ..., xd, YY1, ..., YYn]
	varTargets := apply(currentListA, tt -> ff(sub(tt, A))) | apply(currentListB, tt -> sub(tt, B)); --these are where the
									--variables of newBamb get mapped to in the old ring B.
	mapToB := map(B, newBamb/sub(ideal A, newBamb), varTargets); --now we map our new ring to the old one
	I := ker mapToB; --compute the kernel (this is bad, we should be doing this manually)
	--TODO MAKE THE PREVIOUS LINE FASTER TO EXECUTE
	I1 := sub(I, newBamb) + sub(ideal A, newBamb); --create the formal ideal
	realBamb := A[nextListB]; --now adjoin the variables to A really
	newB := realBamb/sub(I, realBamb); --mod out by the formal ideal
	BtoNewB := map(newB, B, first entries sub(vars realBamb, newB)); --this is the final and real map
	{newB, BtoNewB} --return the A algebra A[...]/? and the map B->A[...]/? (a ring isomorphism)
);

--given a multigraded ring, this flattens the multigrading into a single grading
--it just sums the terms of the multigrading
flattenVarDegrees = method()
flattenVarDegrees(Ring) := (R1) -> (
	KK := coefficientRing R1;
	varList := gens ambient R1;
	degListOld := degrees ambient R1;
	I1 := ideal R1;
	degListNew := apply(degListOld, zL -> {sum zL} );
	S2 := KK[varList, Degrees=>degListNew];
	(S2 / sub(I1, S2))
);




--this was formerly called shrunkOverring
--given f : A -> A^{SN} and A -> B (both finite injective), this should find A^{SN} \cap B
--it REQUIRES that B is seminormal (in practice, B is actually normal)

intersectSeminormalizationAndExtension = method()

--we pass it ff : A -> A^{SN}, gg : A -> B, and an integer used to avoid variable collisions
intersectSeminormalizationAndExtension(RingMap, RingMap, ZZ) := (ff,gg, varInteger) -> (
--STRATEGY
--We form ASN \oplus B \to D (where D is an appropriate overring whose primes line up with those of B)
--We then compute the kernel of that map,
--and finally view that map as a ring.
	fullOver := findOverring(ff,gg); --now we have A^{SN} -> D and B -> D (variables are setup so these maps should be canonical)
	D := fullOver#0;
	f := fullOver#1; --A to ASN (relabeled)
	g := fullOver#2; --A to B (relabeled)
	ASN := target f;
	B := target g;
	A := source f;
	assert(A === source g);
	ASNtoD := map(D, ASN);
	BtoD := map(D, B);
	AtoD := ASNtoD * f; --we write down all the maps in sight

	pushASNtoD := pushFwd(ASNtoD); --now we view D as ASN module
	pushBtoD := pushFwd(BtoD); --and D as a B module
	pushf := pushFwd(f); --likewise ASN
	pushg := pushFwd(g); --and B as A modules
	pushAtoD := pushFwd(AtoD); --finally D as an A module
	ASNoverA := pushf#0; --we grab those modules
	BoverA := pushg#0;
	DoverA := pushAtoD#0;
	moduleMapAtoASN := (pushf#2)(sub(1, ASN)); --we have a map A->ASN as A modules
	moduleMapAtoB := (pushg#2)(sub(1, B)); --and a map A->B as A modules
	listGensASNinD := apply(first entries (pushf#1), tt -> (pushAtoD#2)(ASNtoD(tt))); --we take the generators of
					--ASN as an A module, viewed as elements of the ring ASN, and look at their images in D, as a D module!
	moduleMapASNtoD := map(DoverA, ASNoverA,  matrix fold( (a,b)->(a|b), listGensASNinD)); --that list we just created
	--is used to create a map of A-modules, ASN->D
	--now we do the same thing with B->D
	listGensBinD :=  apply(first entries (pushg#1), tt -> (pushAtoD#2)(BtoD(tt)));
	moduleMapBtoD := map(DoverA, BoverA,  matrix fold( (a,b)->(a|b), listGensBinD )); --this creates an A-module map
	--B->D
	moduleMapASNsumBtoDoverA := moduleMapBtoD|((-1)*moduleMapASNtoD);--we direct sum these two modules and create their common map to D
	K := ker moduleMapASNsumBtoDoverA; --we found our goal, unfortunately its a module...
	--at this point we have found the intersection.  However, we need to rewrite it as a ring.
	ASNsumBtoB :=  (source moduleMapASNsumBtoDoverA)^[0]; --projection onto the first coordinate
	KtoBoverA := ASNsumBtoB*(inducedMap(source moduleMapASNsumBtoDoverA, K));--now we concatenate with A->B
	targetSubmodule := trim image KtoBoverA; --we just want to make the ring generated by this submodule of B
	newVarsTargets := first entries ((pushg#1)*(g(gens targetSubmodule))); --so we grab these module generators
	newVariableNameList := apply(#newVarsTargets, i -> "YY" | toString(i) | "Y" | toString(varInteger));
	AwithBonusVars := (flattenRing (A[newVariableNameList]))#0; --we create a new ring with some extra vars

	AvarsInB := first entries g(vars A); --we find where the regular variables of A go.
	h := map(B, AwithBonusVars, newVarsTargets | AvarsInB ); --finally, we make the ring map from A[..] -> B
	unprunedAnswer := (flattenRing(AwithBonusVars / (ker h)))#0;
	prunedAnswer := prune unprunedAnswer; --there will definitely be some extra unwanted variables, so remove them
	AtoUnprunedAnswer := map(unprunedAnswer, A);
	--next is the map from A to the intersection-
	answerPart1 := (unprunedAnswer.minimalPresentationMap)*AtoUnprunedAnswer; --this is A -> prunedAnswer
	unprunedAnswerToB := map(B, unprunedAnswer, first entries (h(vars AwithBonusVars)));
	answerPart2 := (inverse(fullOver#4))*unprunedAnswerToB*(unprunedAnswer.minimalPresentationMapInv);
	{
		answerPart1, --return the map from source ring to the intersection, and
		answerPart2 --the map from the intersection to B (= target of gg)
	}
);

--*************************************
----NORMALIZATION REDUX----
--*************************************

--the following function should normalize a general ring
betterNormalizationMap=method(Options => { Variable => Yy, Strategy => {}  });

betterNormalizationMap(Ring):=o->(R1) -> (
---STRATEGY
--we take the quotient by each minimal primes
--normalize each one, produce the map from the original ring to the product of these normalizations
--the tricky part is we need to construct a map R -> \prod_i (R/q_i)^N
--we do this by keeping track of idempotents
	S1 := ambient R1;
	minPrimeList := minimalPrimes ideal R1; --grab the minimal primes of our ring
	--if there is only one ring, we just return that
	if (#minPrimeList == 1) then (
	integralClosure(R1, Variable=>o.Variable, Strategy=>o.Strategy);
		return icMap(R1);
	);
	quotientDomainList := apply(minPrimeList, qq -> (flattenRing(S1/qq))#0);  --form the quotients by each
	mapR1ToQuotientList := apply(quotientDomainList, R1q -> map(R1q, R1) ); --record the map from our ring to each quotient
	apply(quotientDomainList, R1q -> integralClosure(R1q, Variable=>o.Variable, Strategy=>o.Strategy));
	normalizationMapList := apply(quotientDomainList, R1q -> icMap(R1q)); --normalize each quotient (recording the map)
	normalizedRingList := apply(normalizationMapList, fi -> target fi); --list those normalized rings
	normalizedRing := ringProduct(normalizedRingList); --take the product of said normalizations
	normalizationMapFromR1List := apply(#quotientDomainList, i -> (normalizationMapList#i)*(mapR1ToQuotientList#i) ); --form the maps from R1 to each normalized quotient.

	--now comes the tricky part
	--first, the following creates a list of maps
	--(R/q_j)^N -> \prod_i (R/q_i)^N
	--which would be the right (non-ring) map except for the fact that we aren't multiplying by the appropriate idempotent
	--  (we'll multiply by said idempotent later).
	fakeMapList := apply(#normalizedRingList, jj -> map(normalizedRing#0, normalizedRingList#jj, (normalizedRing#2)#jj) );--these are not real ring maps

	--now we make some function closures
	--these take elements (perhaps of R or R/q_j)
	--sub them into (R/q_j)
	--map them to (R/q_j)^N
	--apply the maps from fakeMapList to them to get elements
	--multiply them by appropriate idempotents
	--in other words, this gives us a function which tells us where elements of R go in in \prod_i (R/q_i)^N
	--(well, rather the sum of the images in the list should do that)
	R1qToNormalizedRingList := apply(#quotientDomainList, jj -> (tt -> ((normalizedRing#1)#jj)*( (fakeMapList#jj)((normalizationMapList#jj)(sub(tt, source (normalizationMapList#jj)) )) ) ) ); --not a real map either, this is the "map" from each R1q to the normalizedRing, but it should work for the variables
	--

	--now we construct the image of each variable of R1 inside the quotients
	--the following is a list of lists, each entry is the image of the variables of R1 in R1/q_i
	varImagesInQuotients := apply(#quotientDomainList, jj -> apply(gens R1, vv -> (mapR1ToQuotientList#jj)(vv) ) );

	varImagesInNormalizedRing := apply(#quotientDomainList, jj -> apply(varImagesInQuotients#jj, zz -> (R1qToNormalizedRingList#jj)(zz)));

	--finally, we sum the images of the variables of R in \prod_i (R/q_i)^N
	summedVarImagesInNormalizedRing := apply(entries transpose matrix varImagesInNormalizedRing, ll -> sum(ll));
	--thus we construct the map R -> \prod_i (R/q_i)^N
	R1ToRealNormalization := map(normalizedRing#0, R1, summedVarImagesInNormalizedRing);
	--we prune and trim the big normalization
	prunedNormalization := prune( normalizedRing#0);
	trimmedNormalization := trim prunedNormalization;

	--finally, we build the map R1 -> (pruned trimmed normalization),
	nearlyFinalMap := map(trimmedNormalization, R1, sub(matrix(((normalizedRing#0).minimalPresentationMap)*R1ToRealNormalization), trimmedNormalization));

	myVar := o.Variable;
	if (o.Variable === null) then (myVar = "Ww") else (myVar = toString(myVar));
	renamedNorm := varRenamer(myVar, trimmedNormalization);
	(renamedNorm#1)*(nearlyFinalMap)
);

--**********************************
--the following function takes a list of rings and returns their product.
--It returns three items
-- a) the product of the rings
-- b) the idempotent elements defining each part of the ring (subbed into the product)
-- c) a list of lists showing where each each variable of the original rings goes in the product
ringProduct = method(Options => { Variable => null  })
ringProduct(List):=o->(listRings) ->(
--STRATEGY
--given a list of rings R_i = A[x_{i,1}, ..., x_{i, n_i}] (with the same coefficient ring A)
--we form A[x_{1,1}, ..., x_{t, n_t}, e_1, ..., e_t]
--modulo some relations
--   a) definitely the relations of the original rings R_i
--   b) e_i^2 = e_i (idempotents)
--   c) e_i*x_{i, j} = x_{i,j}
--   d) e_i*e_{i'} = 0
--   e') (note this implies e_i*x_{i', j} = 0)
	if (#listRings < 1) then error "ringProduct: Expected more than zero rings.";
	if (#listRings == 1) then return {listRings#0, sub(1, listRings#0), gens (listRings#0)};

	myCoeffRing := coefficientRing(listRings#0);
	if (any(listRings, RR1 -> not (coefficientRing RR1 === myCoeffRing) ) ) then error "ringProd: Expected all rings to have the same coefficient ring.";

	numRings:=#listRings;
	newListRings := apply(numRings, count-> ringRenamer(ringRenamer(listRings#count, count), count) );
	--first we make a new list of rings, with variables renamed to avoid collisions
	listVars := apply(numRings, count->gens ambient (newListRings#count) );
	--make a list of all the variables (well, a list of lists)
	idems := apply(numRings, count->"e"|count);
	--make a list of idempotents
	moreCompleteList := flatten flatten apply(numRings, count->{listVars#count, idems#count} );

	unQuotiented := myCoeffRing[moreCompleteList]; --make the ring out of all these variables
	totalVars := gens unQuotiented; --get the variable names of our new ring

	--now that the ring has actually been created, we need to get the corresponding variables
	modVarList:=totalVars;
	listOfListsOfVars := new MutableList from 0..(numRings-1);
	listOfIdems := new MutableList from 0..(numRings-1);
	tempList:=null;
	--next we break up the variables into the appropriate sublists
	--and put them into the listofListsOfVars and listOfIdems we just made
	for count from 0 to numRings-1 do (
		tempList=take(modVarList, #(listVars#count));
		modVarList=take(modVarList, -(#modVarList-#tempList));
		listOfIdems#count = modVarList#0;
		modVarList=take(modVarList, -(#modVarList-1));
		listOfListsOfVars#count = tempList;
	);
	--next we break up the variables into the appropriate sublists
	--and put them into the listofListsOfVars and listOfIdems we just made

	--next let's build the relations
	--first idempotents
	relations:={};
	megaIdeal:=sub(ideal(), unQuotiented);
	for count from 0 to numRings-1 do (
		for count1 from count to numRings-1 do (
			--we run two loops
			if(count1==count) then( --if i == j...
				megaIdeal=megaIdeal+ideal( flatten{(listOfIdems#count)^2-(listOfIdems#count)});
				--e^2 - e (do the idempotent relation)
				megaIdeal=megaIdeal+ideal( flatten{apply(listOfListsOfVars#count, zz -> (listOfIdems#count)*zz-zz)});
				--e*x - x (and the idempotent does nothing to x)
			)
			else ( --if i \neq j
				megaIdeal=megaIdeal+ideal( flatten{(listOfIdems#count)*(listOfIdems#count1)});
				--ei*ej = 0 if i \neq j
			);
		);
	);
	megaIdeal = megaIdeal + ideal( sum(toList listOfIdems) - 1); --add these idempotent relations to our big ideal

	--next we grab the old relations
	listOfIdeals:={};
	for count from 0 to numRings-1 do (
		tempMap:=map(unQuotiented, ambient (newListRings#count), listOfListsOfVars#count); --create a map to our new ring, from each old ring.
		megaIdeal=megaIdeal+tempMap(ideal(newListRings#count)); --map the old relations to our new ring
	);
	outputRing:=unQuotiented/(trim(megaIdeal)); --and we are DONE(ish)

	subbedIdempotents :=apply(listOfIdems, tt -> sub(tt, outputRing)); --we get the list of idempotents, in the final quotiented ring
	subbedRingVarTargets := apply(#listOfListsOfVars, jj -> apply(listOfListsOfVars#jj, tt->((sub(tt, outputRing))) ));
		--sub the variables into this ring as well
	renamedProd := null;
	hMap := null;
	if (o.Variable === null) then (
		return {outputRing, subbedIdempotents, subbedRingVarTargets}; )
	else if (class o.Variable === String) then (
		renamedProd = varRenamer(o.Variable, outputRing);
		hMap = renamedProd#1;
		return {
			renamedProd#0, --the product of the rings
			apply(subbedIdempotents, ei -> hMap(ei)), --the idempotent elements defining each part of the ring (subbed into the product)
			apply(subbedRingVarTargets, li -> apply(li,  ti -> hMap(ti))) --a list of lists showing where each each variable of the original rings goes in the product
		};
	)
	else if (class o.Variable === Symbol) then (
		renamedProd = varRenamer(toString(o.Variable), outputRing);
		hMap = renamedProd#1;
		return {
			renamedProd#0,--the product of the rings
			apply(subbedIdempotents, ei -> hMap(ei)), --the idempotent elements defining each part of the ring (subbed into the product)
			apply(subbedRingVarTargets, li -> apply(li,  ti -> hMap(ti))) --a list of lists showing where each each variable of the original rings goes in the product
		};
	);
 )

beginDocumentation()

document {
	Key => Seminormalization,
	Headline => "a package used to seminormalize rings",
	EM "Seminormalization", " is a package which can be used to seminormalize rings or more generally check if a ring is seminormal.  Roughly speaking a ring is non-normal if in the Spec, points are identified and tangent spaces killed.  Seminormal rings are those where only points are identified.  See the following for more discussion: ",
	HREF("https://mathoverflow.net/questions/109395/is-there-a-geometric-intuition-underlying-the-notion-of-normal-varieties/", "MathOverflow question"),
	BR{}, BR{},
	BOLD "Core functions",
	UL {
		{TO "seminormalize", " computes the seminormalization of a ring"},
		{TO "isSeminormal", " checks if a ring is seminormal"},
	},
	"To accomplish this, this we first normalize the ring via ", TO "integralClosure", " and then glue points together via the ", TO "Pullback", " package.  There are some other functions exported which people may also find useful.", BR{}, BR{},
	BOLD "Other useful functions",
	UL {
		{TO "ringProduct", " finds a ring equal to a product of rings"},
		{TO "betterNormalizationMap", " computes the integral closure of reduced rings (that are not necessarily domains)"},
	},
}

doc ///
	Key
		seminormalize
		(seminormalize, Ring)
	Headline
		seminormalize a reduced ring
	Usage
		seminormalize(S)
	Inputs
		S:Ring
	Outputs
		:List
			the first entry of which is the seminormalization of the ring, the second and thirds are maps between the ring, its seminormalization and its normalization
	Description
		Text
			This seminormalizes a reduced ring and outputs a list, the first entry of which is the seminormalized ring, the second is the map from the ring to its seminormalization, and finally the map from the seminormalization to its normalization.
			In our first example, the cusp, the seminormalization and normalization are isomorphic.
		Example
			R = QQ[x,y]/ideal(x^3 - y^2);
			L = seminormalize(R)
			L#0
			target(L#2)
		Text
			The previous example seminormalized a non-seminormal ring.  Let's try a seminormal ring (the pinch point).
		Example
			R = QQ[x,y,z]/ideal(x^2*y-z^2);
			L = seminormalize(R)
			L#0
			target(L#2)
		Text
			We conclude with an example of a ring where the seminormalization, the normalization and the ring itself are all are distinct, the tacnode.
		Example
			R = QQ[x,y]/ideal(y*(y-x^2));
			L = seminormalize(R)
			L#0
			target(L#2)
///

doc ///
	Key
		[betterNormalizationMap, Strategy]
		[seminormalize, Strategy]
	Headline
		controls what strategy is used in calls to integralClosure
	Description
		Text
			These options are passed along whenever the functions call integralClosure.
	SeeAlso
		[integralClosure, Strategy]
///

doc ///
	Key
		[seminormalize, Variable]
		[betterNormalizationMap, Variable]
		[ringProduct, Variable]
	Headline
		set the name for new variables created by the function
	Description
		Text
			This option sets the default variable for new variables created by the above functions.  You must pass it a symbol.  We first give an example of this in the context of seminormalization.
		Example
			A = QQ[a,b]/ideal(a^2-b^5);
			seminormalize(A, Variable=>X)
		Text
			Here is an example where we normalize a non-domain.
		Example
			B = QQ[u,v]/ideal(u*v);
			betterNormalizationMap(B, Variable=>Y)
		Text
			We conclude with an example of taking the product of two rings.
		Example
			C = QQ[x];
			D = QQ[y];
			ringProduct({C,D}, Variable=>z)
///



doc ///
	Key
		findElementMappingToTarget
		(findElementMappingToTarget, RingMap, RingElement)
	Headline
		given a ring map and an element of the target, this find an element from the source whose image is the given one
	Usage
		x = findElementMappingToTarget(phi, y)
	Inputs
		phi: RingMap
		y: RingElement
	Outputs
		x: RingElement
			a ring element mapping to y by phi
	Description
		Text
			Given a ring map $\phi : R \to S$ and $y \in S$, this finds $x \in R$ such that $\phi(x) = y$ (if it exists).  If no such element exists, it throws an error.
		Example
			R = QQ[u,v, w];
			S = QQ[a,b];
			phi = map(S, R, {a, a*b, 0});
			findElementMappingToTarget(phi, a)
			findElementMappingToTarget(phi, sub(0, S))
///


doc ///
	Key
		ringProduct
		(ringProduct, List)
	Headline
		compute the product of a list of rings
	Usage
		M = ringProduct(L)
	Inputs
		L: List
	Outputs
		M: List
			a list with a product of of the rings input, and information about its structure
	Description
		Text
			Given a list of rings, of finite type over the same coefficient ring, this computes a ring isomorphic to a product of the rings.  It returns a list with three entries.  First is the ring.  Second is the list of orthogonal idempotents.  Finally, it lists where the variables of each of the rings in the list go in the new ring.
		Example
			R = QQ[a];
			S = QQ[b];
			T = QQ[c];
			L = ringProduct({R,S})
			ringProduct({R,S,T})
		Text
			The third entry in the list correspond to the elements $(x,0)$ and $(0,y)$ in the product of rings.
		Example
			R = QQ[x];
			S = QQ[y,z];
			L = ringProduct({R,S});
			newx = L#2#0#0;
			newy = L#2#1#0;
			newz = L#2#1#1;
			newx*newy==0
			newx*newz==0
			newy*newz==0
///

doc ///
	Key
		betterNormalizationMap
		(betterNormalizationMap, Ring)
	Headline
		normalizes non domains
	Usage
		phi = betterNormalizationMap(S)
	Inputs
		S: Ring
	Outputs
		phi: RingMap
			the map from the ring to its normalization
	Description
		Text
			Given a reduced ring $S$, this returns the normalization map $S \to S^N$.  It works by computing the normalization after modding out by each minimal prime, and then taking the product of the rings.  This should be compared to the function icMap built into Macaulay2.
		Example
			R = QQ[x,y]/ideal(x*y);
			icMap(R);
			h2 = betterNormalizationMap(R);
			RN = target h2
			radical ideal singularLocus RN
///

doc ///
	Key
		flattenVarDegrees
		(flattenVarDegrees, Ring)
	Headline
		turns a multigraded ring into a singly graded ring
	Usage
		S = flattenVarDegrees(R)
	Inputs
		R: Ring
	Outputs
		S: Ring
	Description
		Text
			This turns a multigraded ring into a singly graded ring, summing the individual degrees.
		Example
			R = QQ[x,y, Degrees=>{{1,2},{2,5}}];
			S = flattenVarDegrees(R);
			vars S
			degrees S
///

doc ///
	Key
		ringToAlgebraMap
		(ringToAlgebraMap, RingMap)
		(ringToAlgebraMap, RingMap, ZZ)
	Headline
		presents the target of a map of rings as an algebra over the source
	Usage
		L = ringToAlgebraMap(f)
		L = ringToAlgebraMap(f, n)
	Inputs
		f: RingMap
		n: ZZ
	Outputs
		L: List
			the first entry is the target of f as an algebra over the source
	Description
		Text
			Given a ringmap map $f: A \to B$, this writes $B$ as $A[...]/J$.  It returns the ring $A[...]/J$ as well as the isomorphim $B \to A[...]/J$.  Consider the first example, a normalization of a cusp.
		Example
			A = QQ[a,b]/ideal(a^2-b^3);
			B = QQ[t];
			f = map(B, A, {t^3, t^2});
			(ringToAlgebraMap(f))#0
			(ringToAlgebraMap(f))#1
		Text
			The second input is used to specify an integer used for new variable enumeration and labeling.  Here is another example where we consider the Frobenius map.
		Example
			A = ZZ/5[x,y,z]/ideal(x^2-y*z);
			B = ZZ/5[X,Y,Z]/ideal(X^2-Y*Z);
			f = map(B, A, {X^5, Y^5, Z^5});
			(ringToAlgebraMap(f))#0
			(ringToAlgebraMap(f, 5))#0
///

doc ///
	Key
		isSeminormal
		(isSeminormal, Ring)
	Headline
		checks if a ring is seminormal
	Usage
		b = isSeminormal(R)
	Inputs
		R: Ring
	Outputs
		b: Boolean
			true if the ring is seminormal
	Description
		Text
			This returns true if the ring is seminormal, otherwise it returns false.
		Example
			R = QQ[x,y]/ideal(y^2 - x^3);
			isSeminormal(R)
			S = ZZ/5[a,b]/ideal(a^2-b^2+a^3)
			isSeminormal(R)
///



doc ///
	Key
		Yy
	Headline
		default symbol for new variables in the seminormalization method
	Description
		Text
			This is the default symbol for new variables created by the functions in this packages, in particular the seminormalize method.
	SeeAlso
		[seminormalize, Variable]
///

TEST /// --#0 check nodes are seminormal
	assert isSeminormal(ZZ/5[x,y]/ideal(y^2-x^3-x^2));
	assert isSeminormal(QQ[x,y]/ideal(y^2-x^3-x^2));
	assert isSeminormal(ZZ/11[x,y]/ideal((x+y)^3 - x*y));
	assert isSeminormal(ZZ/101[x,y]/ideal(x*y));
///

TEST /// --#1 check cusps and tacnodes are not seminormal
	assert(not isSeminormal(ZZ/7[x,y]/ideal(y^2-x^3)));
	assert(not isSeminormal(QQ[x,y]/ideal(y^2-x^5)));
	assert(not isSeminormal(ZZ/5[x,y]/ideal(x*(x-y^2))));
	assert(not isSeminormal(ZZ/101[x,y]/ideal((y-x^2)*(y+x^4))));
///

TEST /// --#2 check pinch points are seminormal, even in char 2)
	assert(isSeminormal(ZZ/2[x,y,z]/(y^2-x^2*z)))
	assert(isSeminormal(QQ[x,y,z]/(y^2-x^2*z)))
///

TEST /// --#3 check SNC is seminormal and 3 lines in A^2 and 4 lines in A^3 is not
	assert(isSeminormal(ZZ/23[x,y,z]/(x*y*z)));
	assert(isSeminormal(QQ[x,y,z]/(x*y,x*z, y*z)));
	assert(isSeminormal(ZZ/101[x,y,u,v]/(x*u, x*v, y*u,y*v)));
	assert(not isSeminormal(QQ[x,y]/ideal(x*y*(x+y))));
	assert(not isSeminormal(ZZ/7[x,y,z]/intersect(ideal(x,y), ideal(x,z), ideal(y,z), ideal(x+y, z))));
///

TEST /// --#4 check an example of Greco and Traverso,
--in particular a SN scheme such that some irreducible component is not SN
	B = ZZ/11[x,y,u,v,e,f];
	I = intersect(ideal(u,v,e-1,f),ideal(x,y,e,f-1));
	A = B/I;
	E = ZZ/11[z1, z2, z3, z4, z5];
	h = map(A, E, {x^3+u, x^2+v, y, u^2-v^3, x*y});
	J = ker h;
	D = E/J;
	assert(isSeminormal(D)); --this should be seminormal, but
	JJ = preimage(h, ideal(sub(f,A)));
	D2 = E/(trim(JJ + J));
	assert(not isSeminormal(D2));
///

TEST /// --#5 do a test that makes the recursion algorithm work a bit more
	A = QQ[x,y];
	B = A/ideal(y*x);
	C = QQ[t2,t3]/ideal(t2^3-t3^2);
	f = map(B, A);
	g = map(B, C, {(x+y)^2, (x+y)^3});
	R = trim prune ((pullback(f,g))#0); --this ring should not be seminormal...
	assert(not isSeminormal(R));
///

TEST /// --#6 do a test that makes the recursion of the algorithm work a bit more
	A = QQ[x,y];
	B = A/ideal(y*x*(x-2*y));
	C = QQ[t];
	f = map(B, A);
	g = map(B, C, {x+y});
	R = trim prune ((pullback(f,g))#0); --this ring should be seminormal...
	assert(isSeminormal(R));
///

TEST /// --#7 check that the non-semi-normal locus is the right set
	A = QQ[x,y];
	B = A/ideal(y^2*x^2);
	C = QQ[t];
	f = map(B, A);
	g = map(B, C, {x+y});
	R = trim prune ((pullback(f,g))#0); --this ring should not be seminormal...
	snList = seminormalize(R);
	RtoRSN = snList#1;
	pushList = pushFwd(RtoRSN);
	myMap = (pushList#2)(sub(1, target RtoRSN));
	cond = radical ann coker myMap;
	assert(dim radical cond == 1);
	assert(#(minimalPrimes cond) == 1);
///

TEST /// --#8 check that the non-semi-normal locus is the right set again
	A = QQ[x,y,z]/ideal(x^2*y-z^2);
	B = A/( (ideal(x^2,y,z)));
	C = QQ[];
	f = map(B, A);
	g = map(B, C, {});
	R = trim prune ((pullback(f,g))#0); --this ring should not be seminormal
	snList = seminormalize(R);
	RtoRSN = snList#1;
	pushList = pushFwd(RtoRSN);
	myMap = (pushList#2)(sub(1, target RtoRSN));
	cond = radical ann coker myMap;
	assert(dim radical cond == 0);
	assert(#(minimalPrimes cond) == 1);
///

end
