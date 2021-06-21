newPackage(
    	"Tropical",
	Version => "1.0",
	Date => "July 2019",
	Authors => {
	    	{Name => "Carlos Amendola", Email => "carlos.amendola@tum.de", HomePage=>""},
	    	{Name => "Kathlen Kohn", Email => "kathlen.korn@gmail.com", HomePage=>""},
  		{Name => "Sara Lamboglia", Email => "lamboglia@math.uni-frankfurt.de", HomePage=>""},
	    	{Name => "Diane Maclagan", Email => "D.Maclagan@warwick.ac.uk", HomePage=>"http://homepages.warwick.ac.uk/staff/D.Maclagan/"},
   		{Name => "Ben Smith", Email => "benjamin.smith-3@manchester.ac.uk", HomePage=>""},
   		{Name => "Jeff Sommars", Email => "sommars1@uic.edu", HomePage=>"http://homepages.math.uic.edu/~sommars"},
    		{Name => "Paolo Tripoli", Email => "paolo.tripoli@nottingham.ac.uk", HomePage=>"https://sites.google.com/view/paolotripoli/home"},
   		{Name => "Magdalena Zajaczkowska", Email => "Magdalena.A.Zajaczkowska@gmail.com", HomePage=>""}
		},
	Headline => "A package for doing computations in tropical geometry",
	Configuration => {
		"path" => "",
		"fig2devpath" => "",
--		"keepfiles" => true,
"keepfiles" => false,
		"cachePolyhedralOutput" => true,
		"tropicalMax" => false,
		"polymakeCommand" =>""
	},
    	OptionalComponentsPresent => true,
        PackageExports => {"gfanInterface","EliminationMatrices","Matroids","Polyhedra"},
	AuxiliaryFiles => true,
--	AuxiliaryFiles => false,
	CacheExampleOutput => true
)


export{
  "TropicalCycle",
  "tropicalCycle",
  "isBalanced",
  "tropicalPrevariety",
  "ComputeMultiplicities",
  "Prime",
  "stableIntersection",
  "tropicalVariety",
  "isTropicalBasis",
  "multiplicities",
  "IsHomogeneous",
  "Symmetry",
  "visualizeHypersurface",
  "Valuation",
  "BergmanFan"
  }


polymakeCommand = (options Tropical)#Configuration#"polymakeCommand"
polymakeOK = polymakeCommand != ""

--Do we want to keep this?
if polymakeOK then << "-- polymake is installed\n" else << "-- polymake not present\n";


------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------


--Polymake visualization for tropical hypersurfaces

--inputs: p prime, x rational number
--outputs: p-adic valuation of x
pAdicVal = (p,x) -> (
    num := numerator(x);
    denom := denominator(x);
    temp := p;
    if num%p===0 then (while (num%temp===0) do (temp=temp*p); return round(log(p,temp)-1));
    if denom%p===0 then (while (denom%temp===0) do (temp=temp*p); return -round(log(p,temp)-1));
    return 0;
)

--inputs: p prime, polyn polynomial with p-adic coefficients
--outputs: polynomial whose coefficients are valuations of coefficients of polyn
pAdicCoeffs := (p,polyn) -> (
    (M,C):=coefficients polyn;
    valuations := transpose matrix{apply(flatten entries C,i->pAdicVal(p,sub(i,QQ)))};
    return (M,valuations);
)

--inputs: parameter specifies the coefficient ring for polyn, polyn polynomial over coefficient ring
--outputss: polynomial whose coefficients are valuations of coefficients of polyn

polynomialCoeffs := (parameter,polyn) -> (
    (M,C):=coefficients(Variables=>delete(parameter,gens class polyn),polyn);
    return (M,transpose matrix{apply(flatten entries C, i->min apply(exponents(polyn),sum))});
)

--inputs: var power of a monomial
--outputs: term with power as coefficient
expToCoeff = (var) -> (
    temp := separate("^",toString(var));
    if (length temp === 1) then return var else return concatenate(temp_1,temp_0);
)

--inputs: polyn macaulay 2 polynomial
--outputs: min of linear polynomials for polymake
toTropPoly = method(TypicalValue=>String)

toTropPoly (RingElement) := (polyn) ->(
    termList := apply(apply(terms polyn,toString),term->separate("*",term));
    tropTerms := apply(apply(apply(termList, term->apply(term,expToCoeff)),term->between("+",term)),concatenate);
    return "min("|concatenate(between(",",tropTerms))|")";
)

--inputs: (termList,coeffs)=coefficients polynomial
--outputs: min of linear polynomials for polymake
toTropPoly (Matrix,Matrix) := (termList,coeffs) ->(
    noCoeffs := sum flatten entries termList;
    termString := apply(apply(terms noCoeffs,toString),term->separate("*",term));
    tropTerms := apply(apply(apply(termString, term->apply(term,expToCoeff)),term->between("+",term)),concatenate);
    withCoeffs := for i when i<numColumns termList list toString((flatten entries coeffs)_i)|"+"|tropTerms_i;
    return "min("|concatenate(between(",",withCoeffs))|")";
)

--outputs: return Min or Max depending on the state of tropcailMax
minmax = () -> (if (Tropical#Options#Configuration#"tropicalMax") then return "Max" else return "Min";)

visualizeHypersurface = method(Options=>{
	Valuation=>null
	})

visualizeHypersurface (RingElement) := o-> (polyn)->(
    polynomial := toTropPoly(polyn);
    if (instance(o.Valuation,Number)) then polynomial = toTropPoly(pAdicCoeffs(o.Valuation,polyn));
    if (instance(o.Valuation,RingElement)) then polynomial = toTropPoly(polynomialCoeffs(o.Valuation,polyn));
    if (instance(o.Valuation,String) and o.Valuation == "constant") then polynomial = toTropPoly(sum flatten entries (coefficients polyn)_0);
    print polynomial;
    filename := temporaryFileName();
    filename << "use application 'tropical';" << endl << "visualize_in_surface(new Hypersurface<"|minmax()|">(POLYNOMIAL=>toTropicalPolynomial(\""|polynomial|"\")));" << close;
    runstring := polymakeCommand | " " |filename | " > "|filename|".out  2> "|filename|".err";
    run runstring;
    removeFile (filename|".err");
    removeFile (filename|".out");
    removeFile (filename);
)


--Example hypersurface
--visualizeHypersurface("min(12+3*x0,-131+2*x0+x1,-67+2*x0+x2,-9+2*x0+x3,-131+x0+2*x1,-129+x0+x1+x2,-131+x0+x1+x3,-116+x0+2*x2,-76+x0+x2+x3,-24+x0+2*x3,-95+3*x1,-108+2*x1+x2,-92+2*x1+x3,-115+x1+2*x2,-117+x1+x2+x3,-83+x1+2*x3,-119+3*x2,-119+2*x2+x3,-82+x2+2*x3,-36+3*x3)")
--opens in browser



--Setting up the data type TropicalCycle

TropicalCycle = new Type of MutableHashTable
TropicalCycle.synonym = "tropical cycle"
TropicalCycle.GlobalAssignHook = globalAssignFunction
TropicalCycle.GlobalReleaseHook = globalReleaseFunction


--basic operations on a tropical cycle

tropicalCycle = method(TypicalValue => TropicalCycle)

tropicalCycle (Fan, List) := (F,mult)->(
    if #maxCones(F) != #mult then error("The multiplicity list has the wrong length");
    T := new TropicalCycle;
    T#"Multiplicities" = mult;
    T#"Fan" = F;
    return T
)


--functions to switch to min-convention

minmaxSwitch = method ()

minmaxSwitch (Fan) := F ->
      fanFromGfan({- rays F, linSpace F, maxCones F ,dim F,Polyhedra$isPure F,isSimplicial F,Polyhedra$fVector F});

minmaxSwitch (TropicalCycle) := T ->(
    tropicalCycle( minmaxSwitch fan T, multiplicitiesReorder({rays (minmaxSwitch fan T),
	    maxCones (minmaxSwitch fan T),-rays  T,maxCones  T,multiplicities T})))



--Decide if a one-dimensional tropical cycle is balanced.
--input: TropicalCycle T, which we assume is 1-dimensional
isBalancedCurves = T ->(
    -- find first integer lattice points on each vector (get list of points)
    -- check whether sum * multiplicity is 0
	-- assuming already have lattice points V = {...}
	m := multiplicities T;
	r := entries transpose rays T;
	(first unique sum(#r, i->(m_i * r_i))) == 0
)

--computes the star of the codimension-one polyhedron P in the tropical cycle Sigma
star (TropicalCycle, Polyhedron) := (Sigma, P) -> (
    	--Create the linear space parallel to P
	--and quotient by it
	--Version for cones
	d:=dim P;
	V:= gens  kernel transpose (rays(P)|linealitySpace(P));
	B:=inverse(rays(P) | linealitySpace(P) | V);
    	--version for polyhedral complex
--    	adjacentCells=select(maxPolyhedra(fan Sigma),sigma->(contains(sigma,P)));
    	--version for fans, as currently implemented
	maxConesSigma:=facesAsCones(0, fan Sigma);
	adjacentCellsPos:=positions(maxConesSigma,sigma->(contains(sigma,P)));
	adjacentCells:=apply(adjacentCellsPos,i->(maxConesSigma_i));
	raysStar:=apply(adjacentCells,sigma->(
    		--Create vector pointing from P into sigma
		--version for Polyhedral complexes
--    		w:=interiorPoint(sigma)-interiorPoint(P);
    	        --version for fans
		w:=sum(rank source rays sigma, i->(rays sigma)_{i}) -
		     sum(rank source rays P, i->(rays P)_{i});
		--Project it onto ker(P)
		w=entries(B*w);
		w=posHull matrix apply(#w-d,i->(w_(i+d)))
	));
    	multsSigma:=multiplicities Sigma;
	multStar:=apply(adjacentCellsPos,i->(multsSigma_i));
	return(tropicalCycle(fan raysStar,multStar));
);


isBalanced = method(TypicalValue => Boolean)

isBalanced (TropicalCycle):= T->(
    if polymakeOK then (
--in polymake, the lineality span (1,...,1) is default.  we embed the
--fans in a higher dimensional fan in case our lineality span does not
--contain (1,...,1)
	C := tropicalCycle(embedFan fan T, multiplicities T);
-- parse object into a polymake script, run polymake and get result back from the same file (which got overwritten by polymake)
	filename := temporaryFileName();
       filename << "use application 'tropical';" << endl << "my $c = "|convertToPolymake(C) <<
	endl << "print is_balanced($c);" << endl;
	filename<<close;
--	filename << "use strict;" << endl << "my $filename = '" << filename << "';" << endl << "open(my $fh, '>', $filename);" << endl;
--	filename << "print $fh is_balanced($c);" << endl << "close $fh;" << endl << close;
	runstring := polymakeCommand | " "|filename | " > "|filename|".out  2> "|filename|".err";
	run runstring;
	removeFile (filename|".err");
	result := get (filename|".out");
	removeFile (filename|".out");
	removeFile (filename);
	if (substring(-4,result)=="true") then return true
	else if  (substring(-5,result)=="false")  then return false
	else if (substring(-1,result)=="1") then return true
	else if (substring(0,result)=="") then return false
	else return "Polymake throws an error";
    )
    else (
	if dim T == 1 then return (isBalancedCurves T) else (
	--loop over all co-dimension 1 faces F of T (use faces(ZZ, PolyhedralObject))
	--for each F, compute star F / lineality space F (can use linSpace, write star of polyhedral complex)
	--use isBalancedCurves to check if balanced for all F
	d:=dim T;
	balanced:=true;
	F:= Polyhedra$faces(1, fan T);
	i:=0;
	while balanced and i<#F do (
--change next line when PolyhedralComplex change is made
	    balanced = isBalancedCurves(star(T,polyhedronFromFace(fan T,F_i)));
	    i=i+1;
	);
    	return balanced;
       );
    );
);

--F is a fan and f is a list describing a face of F
polyhedronFromFace = (F, f) -> (
	L := linealitySpace F;
	R := rays F;
	ambientDim := numgens target rays F;
	origin := transpose matrix{apply(ambientDim, i -> 0)};
	convexHull(origin, R_f, L)
);


--Computing a tropical prevariety

tropicalPrevariety = method(TypicalValue => Fan,  Options => {
--in the future, more strategies not dependent on "gfan" will be available
	Strategy=> "gfan"
	})

tropicalPrevariety (List) := o -> L -> (
	gfanopt:=(new OptionTable) ++ {"tropicalbasistest" => false,"tplane" => false,"symmetryPrinting" => false,
	"symmetryExploit" => false,"restrict" => false,"stable" => false};

--using strategy gfan
    if (o.Strategy=="gfan") then (
    	F:= gfanTropicalIntersection(L, gfanopt);
--gives only the fan and not the fan plus multiplicities which are wrongly computed in gfan
	if (Tropical#Options#Configuration#"tropicalMax" == true) then return F_0 else return minmaxSwitch (F_0))
    else error "options not valid"
)

tropicalPrevariety (List, List) := o -> (L, symmetryList) -> (
	gfanopt:=(new OptionTable) ++ {"tropicalbasistest" => false,"tplane" => false,"symmetryPrinting" => false,
	"symmetryExploit" => true,"restrict" => false,"stable" => false};
--using strategy gfan
    if (o.Strategy=="gfan") then (
    	F:= gfanTropicalIntersection(L, symmetryList, gfanopt);
--gives only the fan and not the fan plus multiplicities which are wrongly computed in gfan
	if (Tropical#Options#Configuration#"tropicalMax" == true) then return F_0 else return minmaxSwitch (F_0))
    else error "options not valid"
)

--Computing a tropical variety
--The input is a matrix R, consisting of the rays of the variety,
--a list of lists M, being the list of maximal cones, and a matrix L giving generators
--for the lineality space.  R has the first lattice point on rays as columns, while L has generators
--for the lineality space as columns.
--output:list of matrices whose rows span the span of each cone.
-- note that ConesOfVariety is a local variable also in findMultiplicities
computeCones=(R,M,L)->(
      ConesOfVariety:={};
      i:=0;
      ConeOfVariety:={};
      if M!={{}} then(
     --this i is going through the maximal cones
      while(i<#M) do(
	  ConeOfVariety={};
	  j:=0;
     --this j is going through the rays inside the maximal cone you are in
	  while(j<#M_i) do(
	      --creates a list of rays
	      ConeOfVariety=append(ConeOfVariety, R_(M_i_j));
	  j=j+1);
      -- creates a matrix whose rows are the rays of the cone
     ConeOfVariety= L|matrix ConeOfVariety;
     -- each cone has to contain also the lineality space since we are not quotienting by it
     -- ConeOfVariety = matrix ConeOfVariety ;
      --add the cone to the list of cones
      	  ConesOfVariety=append(ConesOfVariety,ConeOfVariety);
	  i=i+1;
	);
    )
    else
    (ConesOfVariety={L};
	) ;
    ConesOfVariety
    )


--Compute the multiplicity of the cone spanned by the columns of the matrix M in trop(V(I))
findMultiplicity=(M,I)->(
    --compute vector w in relative interior in order to get the initial ideal in_w(I) with w in the maximal cone M
    w:=flatten entries(sum(numColumns M, j->M_{j}));
    --weight the ring according to this w , we are using leadTerm and max
    --convention since the input fan has not been changed to min
    --convention yet
    S:=ring I;
    R:=newRing(S, MonomialOrder=>{Weights=>w},Global=>false);
    J:=sub(I,R);
    K:=ideal(leadTerm(1,J));
    --you saturate since you don't want the components outside the torus
    initialIdeal:= saturate(sub(K,S), product gens S);
    --this is the the basis of the lattice associated to the toric ideal we are going to compute
--    Basis:= (maxCol( generators kernel transpose M))_0;
      toricIdeal:= ideal apply(entries transpose gens kernel transpose M, u->(
	      mon1:=1_R;
		  mon2:=1_R;
		  scan(numgens ring I,i->(if u_i>0 then mon1=mon1*R_i^(u_i) else mon2=mon2*R_i^(-u_i)));
		  mon1-mon2
         ));
    --this is where we used to use Binomials package
--    toricIdeal:=saturate(latticeBasisIdeal(ring InitialIdeal,Basis),ideal product gens ring I);
    toricIdeal = saturate(toricIdeal,product gens R);
    m:=(degree(initialIdeal)/degree (toricIdeal));
    --return multiplicity m as integer, since it lives currently in QQ
    --if m is an integer (as it should be), then the following command parses it to ZZ
    --otherwise, an errow will be returned "rational number is not an integer"
    lift(m,ZZ)
    )

--input Matrix whose columns are the generators of the cone and the ideal of the variety
--output a list of one number that is the multiplicity
--maths behind it look at exercise 34 chapter 3 Tropical book and [Stu96]
--(Grobner basis and Convex Polytopes )




--input: the ideal of the variety and the fan computed by gfanbruteforce
--output: list with the multiplicities to add to the tropicalCycle
findMultiplicities=(I,T)->(
	ConesOfVariety:=computeCones( rays T,maxCones T, linSpace T);
      --creates a list with matrices that correspond to the maximal cones
	M:= apply(ConesOfVariety, linearSpan->(
       --for each cone computes the multiplicity and adds this to a list of multiplicities
	    findMultiplicity(linearSpan,I))
	    );
	M
)


tropicalVariety = method(TypicalValue => TropicalCycle,  Options => {
	ComputeMultiplicities => true,
	Prime => true,
	IsHomogeneous=>false,
	Symmetry => {}
	})



--Main function to call for tropicalVariety.  Makes no assumption on ideal
tropicalVariety (Ideal) := o -> (I) ->(
    local F;
    local T;
    newSymmetry:= o.Symmetry; --In case of homogenization we adjust the user given symmetries, recorded in the var newSymmetry.

    --If Symmetry present, check user has input permutations with the right length. If newSymmetry is {}, any always returns false.
    M := #(gens ring I);
    if any(newSymmetry, listPermutation ->  #listPermutation != M) then
	error ("Length of permutations should be " | M);

    if o.IsHomogeneous==false then
    (

	--First homogenize, append variable AA to the beginning
    	R := ring I;
    	AA := symbol AA;

	--Extend to a new coefficient ring. The added variable is at the beginning.
	S := first flattenRing(R[AA, Join =>false]);

	J:=substitute(I,S);
	J=homogenize(J,S_0);
	J=saturate(J,S_0);
	--we transform I in J so that the procedure continues as in the homogeneous case
	I=J;

	--If Symmetry present, adjust the symmetry vectors to the right length and shift the values up by one. If not present, this operates on an empty list.
    	--Increase the values of these lists by 1.
	newSymmetry = for listPermutation in newSymmetry list --make a list with the following values
	             apply(listPermutation, j -> j + 1);
        --Prepend a 0.
        newSymmetry = apply(newSymmetry, listPermutation -> prepend(0, listPermutation));
    );
    if (o.Prime== true) then (
	    cone := gfanTropicalStartingCone I;
	    --check if resulting fan would be empty
	    if instance(cone, String) then return cone;
		if(newSymmetry == {}) then
			F= gfanTropicalTraverse cone
		else
			F= gfanTropicalTraverse (cone, "symmetry" => newSymmetry);

	    --check if resulting fan would be empty
	    if (instance(F,String)) then return F;
	    T=tropicalCycle(F_0,F_1))
    else
	--If ideal not prime, use gfanTropicalBruteForce to ensure disconnected parts are not missed at expense of multiplicities
	    (if o.ComputeMultiplicities==false
	     then (
		   F= gfanTropicalBruteForce gfanBuchberger I;
		   --check if resulting fan is empty
		   if (instance(F,String)) then return F;
		   mult := {};
		   i:=0;
		   while(i<#maxCones (F))do(
		       mult=append(mult,{});
		       i=i+1);
		   T=tropicalCycle(F,mult)
		   --note that the output of gfanTropicalBruteForce is a fan and an empty list of multiplicities this is why we have to add the right number of empty multiplicities
    	    	   )
	     else (
		 F= gfanTropicalBruteForce gfanBuchberger I;
		 --check if resulting fan is empty
		 if (instance(F,String)) then return F;
			--call the function tropicalCycle to create a new tropical variety with multiplicities
		 T=tropicalCycle(F,findMultiplicities(I,F))
	 );  );
--Need to fix the paragraph below - why arewe doing all these checks when the
--tropical variety is computed?
    if   o.IsHomogeneous==false  then
	(
	    newRays:=dehomogenise(rays T);
     	    newLinSpace:=gens gb dehomogenise(linealitySpace T);
	    TProperties := {newRays,
			newLinSpace,
			maxCones T,
			dim(T)-1,
			Polyhedra$isPure fan T,
			isSimplicial T,
			drop(fVector T,1)};
	UFan:=fanFromGfan(TProperties);
	U:= tropicalCycle(UFan,multiplicitiesReorder({rays UFan,maxCones UFan,newRays,maxCones T,multiplicities(T)}));
	-- we always want the output to be called T so we change U to T
	T=U;
	);
	if (Tropical#Options#Configuration#"tropicalMax" == true) then return  T  else return minmaxSwitch T
)



--auxiliary function to quotient out the lineality space (1,1,...1) introduced by the homogenisation
--input= matrix whose columns are either the rays or the generators of the lineality space of a fan
--output= matrix whose columns are either the rays or the generators of the linelity space of the fan quotiented by (1,...,1)
dehomogenise=(M) -> (
	vectorList:= entries transpose M;
	dehomog:= new List;
	for L in vectorList do (
		newL := apply(#L-1,i->(L#(i+1)-L#0));
		gcdL := gcd(newL);
		if gcdL==0 then continue;
		newL = newL/gcdL;
		newL = apply(newL,i->(lift(i,ZZ)));
		dehomog = append(dehomog,newL);
	);
	if dehomog=={} then map(ZZ^(#entries(M)-1),ZZ^0,0)
	else transpose matrix dehomog
)

--Check if a list of polynomials is a tropical basis for the ideal they generate

--Current Strategy is using 'gfan'
isTropicalBasis = method(TypicalValue => Boolean,  Options => {
	Strategy=> "gfan"
	})

isTropicalBasis (List) := o -> L -> (
	if (o.Strategy=="gfan") then (
	    gfanopt:=(new OptionTable) ++ {"tropicalbasistest" => true,"tplane" => false,"symmetryPrinting" => false,"symmetryExploit" => false,"restrict" => false,"stable" => false};

if not all(L, a-> isHomogeneous a) then error "Not implemented for non homogeneous polynomials yet";
 	    return gfanTropicalIntersection(L, gfanopt)
	)
	)


stableIntersection = method(TypicalValue =>
TropicalCycle, Options => {Strategy=> if polymakeOK then "atint" else "gfan"})

stableIntersection (TropicalCycle, TropicalCycle) := o -> (T1,T2) -> (
--TODOS:
--4) gfan strategy outputs only a fan, not a tropical cycle
--5) test cases
--6) make tropical an immutable hash table
    if (o.Strategy=="atint") then (
	--in polymake, the lineality span (1,...,1) is default.
	--we embed the fans in a higher dimensional fan in case our lineality span does not contain (1,...,1)
	C1 := tropicalCycle(embedFan fan T1, multiplicities T1);
	C2 := tropicalCycle(embedFan fan T2, multiplicities T2);

	filename := temporaryFileName();
	--ugly declaration of helping strings
	openingStr := "\"_type SymmetricFan\\n\\nAMBIENT_DIM\\n\";";
	dimStr := "\"\\n\\nDIM\\n\";";
	linDimStr := "\"\\n\\nLINEALITY_DIM\\n\";";
	raysStr := "\"\\n\\nRAYS\\n\";";
	nRaysStr := "\"\\nN_RAYS\\n\";";
	linSpaceStr := "\"\\n\\nLINEALITY_SPACE\\n\";";
	orthLinStr := "\"\\n\\nORTH_LINEALITY_SPACE\\n\";";
	fStr := "\"\\n\\nF_VECTOR\\n\";";
	simpStr := "\"\\n\\nSIMPLICIAL\\n\";";
	pureStr := "\"\\n\\nPURE\\n\";";
	coneStr := "\"\\n\\nCONES\\n\";";
	maxConeStr := "\"MAXIMAL_CONES\\n\";";
	weightStr := "\"\\nMULTIPLICITIES\\n\";";
	filename << "use application 'tropical';" << "my $c = "|convertToPolymake(C1) << "my $d = "|convertToPolymake(C2) << "my $i = intersect($c,$d);" << "use strict;" << "my $filename = '" << filename << "';" << "open(my $fh, '>', $filename);" << "print $fh " << openingStr << "print $fh $i->AMBIENT_DIM;" << "print $fh " << dimStr << "print $fh $i->DIM;" << "print $fh " << linDimStr << "print $fh $i->LINEALITY_DIM;" << "print $fh " << raysStr << "print $fh $i->RAYS;" << "print $fh " << nRaysStr << "print $fh $i->N_RAYS;" << "print $fh " << linSpaceStr << "print $fh $i->LINEALITY_SPACE;" << "print $fh " << orthLinStr << "print $fh $i->ORTH_LINEALITY_SPACE;" << "print $fh " << fStr << "print $fh $i->F_VECTOR;" << "print $fh " << simpStr << "print $fh $i->SIMPLICIAL;" << "print $fh " << pureStr << "print $fh $i->PURE;" << "print $fh " << coneStr << "my $cones = $i->CONES;" << "$cones =~ s/['\\>','\\<']//g;" << "print $fh $cones;" << "print $fh " << maxConeStr << "print $fh $i->MAXIMAL_CONES;" << "print $fh " << weightStr << "print $fh $i->WEIGHTS;" << "close $fh;" << close;

	runstring := polymakeCommand | " "|filename | " > "|filename|".out  2> "|filename|".err";
	run runstring;
	result := get filename;
	removeFile (filename|".out");
	removeFile (filename|".err");
	removeFile (filename);
	parsedResult := gfanParsePolyhedralFan(result);
	if instance(parsedResult, String) then return parsedResult;
	(polyfan, mult) := parsedResult;
--now we still need to transform things back to our format:
--1) adjust the rays
	R := rays polyfan;
	row := (entries R)#0;
	ind := -1;
	scan(#row, i -> if ((row#i)==1) then ind = i);
	R = submatrix'(R, {0} , {ind});
--2)adjust the maximal cones
	C := maxCones polyfan;
	C = apply(C, c -> delete(ind, c));
	C = apply(C, c -> apply(c, n -> if (n > ind) then (n-1) else (n) ));
--3)adjust lineality space
	L := linSpace polyfan;
	L = submatrix'(L, {0} , );
--	L = L|(transpose matrix {apply(numgens target L , i -> 1)});
--4)inverse of our function embedFan
	F := unembedFan fan(R,L,C);
	return tropicalCycle (F,mult);
    )
    else if (o.Strategy=="gfan") then (
	F1 := fan(T1);
	m1 := multiplicities(T1);
	F2 := fan(T2);
	m2 := multiplicities(T2);
	return gfanStableIntersection(F1,m1,F2,m2);
    )
    else (
	return "Strategy unknown: Choose 'atint' or 'gfan'";
    );
)

embedFan = F -> (
--embeds a fan into a fan of one dimension higher
--and DOES NOT add the lineality space (1,...,1)
	--1) adjust rays
	rs := rays F;
	numberOfEntries := (numgens target rs)+1;
	rs = entries transpose rs;
	if (#rs != 0) then (
--		rs = apply(rs, s -> s|{-sum s});
		rs = apply(rs, s -> s|{0});
--		numberOfEntries = #first(rs);
		rs = transpose matrix rs;
	) else (
		rs = matrix apply(numberOfEntries, i -> {})
	);
	--2) adjust lineality space
 	ls := entries transpose linSpace F;
	if (#ls != 0) then(
--		ls = apply(ls, s -> s|{-sum s});
		ls = apply(ls, s -> s|{0});
--		ad := ambDim F;
--		ls = ls|{apply(ad+1, i -> 1)};
		ls = transpose matrix ls;
	) else (
		ls = matrix apply(numberOfEntries, i -> {});
	);
	return fan(rs,ls,maxCones F);
)

unembedFan = F -> (
--inverse function to embedFan
	--1) adjust rays
	rs := entries transpose rays F;
	if (#rs != 0) then (
--		rs = apply(rs, s -> apply(s, i -> i-sum(s)/(#s)));
		rs = apply(rs, s -> apply(s, i -> i-last(s)));
		rs = apply(rs, s -> drop(s,-1));
--		rs = apply(rs, s -> apply(s, i -> i*(#s+1)));
		rs = transpose matrix rs;
	) else (
		rs = matrix apply(numgens(target rays F)-1, i -> {});
	);
	--2) adjust lineality space
	ls := entries transpose linSpace F;
	if (#ls != 0) then (
--		ls = apply(ls, s -> apply(s, i -> i-sum(s)/(#s)));
		ls = apply(ls, s -> apply(s, i -> i-last(s)));
		ls = apply(ls, s -> drop(s,-1));
--		ls = apply(ls, s -> apply(s, i -> i*(#s+1)));
		ls = transpose matrix ls;
	) else (
		ls = matrix apply(numgens(target rays F)-1, i -> {});
	);
	return fan(rs,ls,maxCones F);
)

convertToPolymake = (T) ->(
-- converts a tropical cycle into a string, which is a constructor of a tropical cycle in polymake
--
	F := fan(T);
--check if given cycle is empty
	if (dim(F) < 0) then (return "new Cycle<Min>(PROJECTIVE_VERTICES=>[],MAXIMAL_POLYTOPES=>[],WEIGHTS=>[]);";) else (
--if not empty, check if min- or max-convention is used
	str := "new Cycle<";
	if Tropical#Options#Configuration#"tropicalMax" then str=str|"Max" else str=str|"Min";
	rs := entries transpose rays T;
	numberOfRays := #rs;
	ambientDim := ambDim F;
--convert to polymake convention of rays: 1) add origin of the form (1,0,...,0)
	str = str|">(PROJECTIVE_VERTICES=>[[1";
	local ray;
	scan (ambientDim,i -> str = str|",0");
	str = str|"]";
--2) add every ray with a leading 0
	scan (numberOfRays,i -> (
		ray = rs#i;
		str = str|",[0";
		scan (ambientDim,j -> str = str|","|ray#j);
		str = str|"]";
	));
--every cone must now also be spanned by the origin
	str = str|"],MAXIMAL_POLYTOPES=>[";
	maxCs := maxCones(F);
	numberOfMaxCones := #maxCs;
	local cone;
	scan (numberOfMaxCones,i -> (
		cone = maxCs#i;
		str = str|"[0";
		scan (#cone,j -> str = str|","|(cone#j+1));
		str = str|"],";
	));
--delete last comma
	str = substring(0,#str-1,str);
	ls := entries transpose linSpace F;
	if (#ls != 0) then (
		str = str|"],LINEALITY_SPACE=>[";
--add lineality space
		scan (#ls, i -> (
			ray = ls#i;
			str = str|"[0";
			scan(#ray, j -> str = str|","|(ray#j));
			str = str|"],";
		));
--delete last comma
		str = substring(0,#str-1,str);
	);
	str = str|"],WEIGHTS=>[";
--the multiplicities stay unchanged
	mult := multiplicities(T);
	scan (numberOfMaxCones,i -> str = str|mult#i|",");
	str = substring(0,#str-1,str);
	str = str | "]);";
	return str;
	)
)





--functions to get stuff from fans and tropical cycles

rays TropicalCycle:= T->( rays fan T)

cones (ZZ,TropicalCycle):= (i,T)->( cones(i,fan T))

dim TropicalCycle:= T->( dim fan T)

ambDim TropicalCycle:= T->( ambDim fan T)

fVector (TropicalCycle) := T->(Polyhedra$fVector fan T)

fan TropicalCycle := T -> (T#"Fan")

linealitySpace (TropicalCycle):= T->( linSpace fan T)

maxCones (TropicalCycle):= T->( maxCones fan T)

multiplicities = method(TypicalValue => List)

multiplicities (TropicalCycle) := T -> (T#"Multiplicities")

isPure TropicalCycle := Boolean => T->( Polyhedra$isPure(fan(T)))

isSimplicial TropicalCycle:= Boolean => T->( isSimplicial(fan(T)))



--------------------
--Bergman fan code
--------------------


-- BergmanconeC returns the matrix of of generators of the cones
-- corresponding to the chain of flats C it does not check whether C
-- is a chain of flat or not 

BergmanconeC  = (M, C) -> (
    groundSetM:=#M.groundSet;
    L := {};
    for F in C do(
    	  vect:={};
    	  scan(groundSetM, i->(
	    if member(i,F) then vect =  append(vect,1) else vect = append(vect,0);
	  ));
	L = append(L, vect)
	);
   transpose  matrix L
    )

-- BergmanFan returns the fan of a loopless well-defined matroid
-- ground set must be [n]
-- depends on functions above
BergmanFan = (M) -> (
    if ( loops(M) != {} ) then
	    error("The current method only works for loopless matroids");
    E := toList M.groundSet;
    L := {};
    LM := latticeOfFlats M;
    redLM := dropElements(LM, {{}, E});
    redOrdcplx := maximalChains redLM;
    allOnes := apply(E,i->1);
    for C in redOrdcplx do(
	L = append(L, coneFromVData(BergmanconeC(M,C),transpose matrix {allOnes}));
	);
    F:= fan L;
    mults:=apply(#(maxCones F),i->1);
    tropicalCycle(F,mults)    
    )


----------------------------------------------------------------------------

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
beginDocumentation()
doc ///
    Key
    	Tropical
    Headline
    	the main M2 package for tropical computations
    Description
    	Text
	    This is the main M2 package for all tropical computations.
	    This uses Anders Jensen's package gfan, Michael Joswig's
	    package Polymake, and also internal M2 computations.

    	    The package defaults to using the min convention for tropical geometry.
	    To switch to the max convention, reload the package using the command
            loadPackage("Tropical",Configuration=>{"tropicalMax"=>true});

	    The main command is @TO tropicalVariety@.

	    To use the Polymake commands see the @TO "Polymake interface instructions"@.

        Text
            @SUBSECTION "Contributors"@
        Text
	    The following people have also contributed to the package:
	Text
	     @UL {
	       {HREF("https://users.math.yale.edu/~km995/","Kalina Mincheva")},
	       {HREF("http://www.math.unibe.ch/ueber_uns/personen/vargas_de_leon_alejandro/index_ger.html","Alejandro Vargas de Leon")},
	       {HREF("http://www.math.harvard.edu/~cmwang/","Charles Wang")},
	       {HREF("https://math.berkeley.edu/~yelena/", "Yelena Mandelshtam")},
	       {HREF("https://alessioborzi.github.io/", "Alessio Borzì")},
	       {HREF("https://www.linkedin.com/in/timothyxu/", "Timothy Xu")},
	       {HREF{"publish.uwo.ca/~aashra9/","Ahmed Umer Ashraf"}}
    	     }@
///


doc ///
	Key
		visualizeHypersurface
		(visualizeHypersurface,RingElement)
	Headline
		visualize the tropical hypersurface of the given polynomial
	Usage
		visualizeHypersurface(polyn)
		visualizeHypersurface(Valuation=>p,polyn)
		visualizeHypersurface(Valuation=>t,polyn)
	Inputs
		polyn: RingElement
		    polynomial
		Valuation=>Number
		    use p-adic coefficients with given p
		Valuation=>RingElement
		    use coefficients in R[t] with given t
	Description
	    Text
	        This function wraps the Polymake visualization for a
		tropical hypersurface given an input polynomial. The input
		should be entered as a homogeneous polynomial. Running
		this method opens an image in a new browser window. The
		coefficients can be intereted as p-adic coefficients or as
		polynomials via the option @TO Valuation@. Examples are
		commented out because they open a new browser window.
	    Example
	    	--Examples are commented because they open in browser. Uncomment to run.
    	        R=ZZ[x,y,z]
		f=2*x*y+x*z+y*z+z^2
		--visualizeHypersurface(Valuation=>2,f)

		f=2*x^2+x*y+2*y^2+x*z+y*z+2*z^2
		--visualizeHypersurface(f)

		R=ZZ[w,x,y,z]
		f=8*x^2+8*y^2+8*z^2+8*w^2+2*x*y+2*x*z+2*y*z+2*x*w+2*y*w+2*z*w
		--visualizeHypersurface(f)
///


doc ///
    Key
    	TropicalCycle
    Headline
    	a Type for working with tropical cycles
    Description
		Text
			This is the main type for tropical cycles. A TropicalCycle
			consists of a Fan with an extra HashKey Multiplicities,
			which is the list of multiplicities on the maximal cones
			listed in the order that the maximal cones appear in the
			MaxCones list. A TropicalCycle
			is saved as a hash table which contains the Fan and the
			Multiplicities.

///






doc ///
    Key
	tropicalCycle
	(tropicalCycle, Fan, List)
    Headline
    	constructs a TropicalCycle from a Fan and a list with multiplicities
    Usage
    	tropicalCycle(F,mult)
    Inputs
    	F:Fan
		mult:List
    Outputs
    	T:TropicalCycle
    Description
		Text
			This function creates a tropical cycle from a fan and a list of multiplicities.
			The multiplicities must be given in the same order as the maximal cones
			appear in the MaximalCones list.
		Example
			F = fan {posHull matrix {{1},{0},{0}}, posHull matrix {{0},{1},{0}}, posHull matrix {{0},{0},{1}}, posHull matrix {{-1},{-1},{-1}}}
			mult = {1,2,-3,1}
			tropicalCycle(F, mult)
///


doc///
    Key
	isBalanced
	(isBalanced, TropicalCycle)
    Headline
		checks whether a tropical cycle is balanced
    Usage
    	isBalanced T
    Inputs
		T:TropicalCycle
    Outputs
    	B:Boolean
    Description
		Text
			This function checks if a tropical cycle is balanced.
		Example
			QQ[x,y,z]
			V = tropicalVariety(ideal(x+y+z))
			isBalanced V
			F = fan {posHull matrix {{1},{0},{0}}, posHull matrix {{0},{1},{0}}, posHull matrix {{0},{0},{1}}, posHull matrix {{-1},{-1},{-1}}}
			mult = {1,2,-3,1}
			isBalanced (tropicalCycle(F, mult))
///


doc///
	Key
		tropicalPrevariety
		(tropicalPrevariety, List)
		(tropicalPrevariety, List, List)
		[tropicalPrevariety, Strategy]
	Headline
		the intersection of the tropical hypersurfaces
	Usage
		tropicalPrevariety(L)
		tropicalPrevariaty(L,LS)
		tropicalPrevariety(L,Strategy=>S)
		tropicalPrevariety(L,LS,Strategy=>S)
	Inputs
		L:List
		  of polynomials
		LS: List
		  of Symmetries (optional)
		Strategy=>String
		  Strategy (currently only "gfan")
	Outputs
		F:Fan
		  the intersection of the tropical hypersurfaces of polynomials in L
	Description
		Text
			This method intersects the tropical hypersurfaces
			coming from the tropicalizations of the polynomials in the list L.
			If there are symmetries that leave the specified polynomials fixed,
			they can  be specified by passing a list with the symmetries
			as second argument, with the same format as the option  @TO Symmetry@.
		Example
			QQ[x_1,x_2,x_3,x_4]
			L={x_1+x_2+x_3+x_4,x_1*x_2+x_2*x_3+x_3*x_4+x_4*x_1,x_1*x_2*x_3+x_2*x_3*x_4+x_3*x_4*x_1+x_4*x_1*x_2,x_1*x_2*x_3*x_4-1}
			tropicalPrevariety L
			QQ[x_0,x_1]
			tropicalPrevariety({x_0+x_1+1}, {{1,0}})
			QQ[x_0,x_1]
			tropicalPrevariety({x_0+x_1+1,x_0+x_1},Strategy => "gfan")
///



doc///
    Key
      tropicalVariety
      (tropicalVariety, Ideal)
      [tropicalVariety, ComputeMultiplicities]
      [tropicalVariety, Prime]
      [tropicalVariety, IsHomogeneous]
      [tropicalVariety, Symmetry]

    Headline
      the tropical variety associated to an ideal
    Usage
      tropicalVariety(I)
      tropicalVariety(I,ComputeMultiplicities=>true)
      tropicalVariety(I,Prime=>true)
      tropicalVariety(I,IsHomogeneous=>false)
      tropicalVariety(I,Symmetry=>{{...},{...}})
    Inputs
      I:Ideal
        of polynomials
      IsHomogeneous=>Boolean
        that ensures whether the ideal is already homogeneous
      ComputeMultiplicities=>Boolean
        that confirms whether the multiplicities will be computed
      Prime=>Boolean
        that ensures whether the ideal is already prime
      Symmetry=>List
        that records the symmetries of the ideal
    Outputs
        F:TropicalCycle
    Description
       Text
         This method takes an ideal and computes the tropical variety
         associated to it.  By default the ideal is assumed to be
         prime.  If this is not the case the default answer will not
         necessarily give the correct answer.  In this case use the
         optional argument Prime=>false.  By default the
         tropicalVariety command computes multiplicities but setting
         computeMultiplicities=>false turns this off.  This only saves
         time if Prime is set to false.  The ideal I is not assumed to
         be homogeneous.  The optional argument IsHomogeneous=>true
         allows the user to assert that the ideal is homogeneous. If there
	 are symmetries of the ring corresponding to I that leave I fixed,
	 they can be specified with the option @TO Symmetry@.
      Example
       QQ[x,y];
       I=ideal(x+y+1);
       T=tropicalVariety(I);
       rays(T)
       maxCones(T)
       linealitySpace T
       Polyhedra$fVector fan T
       multiplicities(T)
       QQ[x,y,z,w];
       I = ideal(w+x+y+z)
       T = tropicalVariety(I, IsHomogeneous=>true, Symmetry=>{{1,0,2,3},{2,1,0,3},{3,1,2,0}})
       rays(T)
       maxCones(T)
       I=intersect(ideal(x+y+z+w),ideal(x-y,y-z));
       T= tropicalVariety(I,Prime=>false);
       rays(T)
       maxCones(T)
       multiplicities(T)
       linealitySpace T
       QQ[x,y,z,w];
       I=intersect(ideal(x+y+z+1),ideal(x^2-y*z));
       T= tropicalVariety(I,Prime=>false,ComputeMultiplicities=>false);
       rays(T)
       maxCones(T)
       linealitySpace T
       multiplicities(T)

///



doc///
    Key
	stableIntersection
	(stableIntersection,TropicalCycle,TropicalCycle)
	[stableIntersection, Strategy]
    Headline
    	computes the stable intersection of two tropical varieties
    Usage
	stableIntersection(F,G)
	stableIntersection(L,Strategy=>S)
    Inputs
	F:TropicalCycle
	  a tropical cycle
  	G:TropicalCycle
  	  another tropical cycle
	Strategy=>String
	  Strategy ("atint" (polymake) or "gfan")
    Outputs
        T:TropicalCycle
		  a tropical cycle
    Description
    	Text
	    This computes the stable intersection of two tropical
	    cycles.  For details on the definition of stable
	    intersection, see, for example, Section 3.6 of TROPICALBOOK.
	    If a recent enough version of polymake is installed,
	    the Strategy "atint" is default. Otherwise "gfan" will be used,
	    which only computes the fan of the stable intersection
	    without multiplicities.
	Example
	    QQ[x,y,z];
	    I = ideal(x^2+y^2+z^2-1);
	    T1 = tropicalVariety(I);
	    J = ideal(x*y+y*z+x*z+1);
	    T2 = tropicalVariety(J);
	    V = tropicalVariety(I+J);
	    -- W1 =  stableIntersection(T1,T2,Strategy=>"atint");
	    W2 =  stableIntersection(T1,T2,Strategy=>"gfan");
	    -- V#"Fan" == W1#"Fan"
	    -- V#"Multiplicities" == W1#"Multiplicities"
	    V#"Fan" == W2

///


doc///
    Key
	isTropicalBasis
	(isTropicalBasis, List)
	[isTropicalBasis, Strategy]
    Headline
	checks if a list of polynomials is a tropical basis for the ideal they generate
    Usage
	isTropicalBasis(L)
	isTropicalBasis(L,Strategy=>S)
    Inputs
	L:List
	  of polynomials
	Strategy=>String
	    Strategy (currently only "gfan")
    Outputs
	F:Boolean
	    whether the list of polynomials is a tropical basis for the ideal it generates
    Description
	Text
	    This method checks if the intersection of the tropical hypersurfaces associated to the polynomials in the list equals the tropicalization of the variety corresponding to the ideal they generate.
        Example
	    QQ[x,y,z]
	    isTropicalBasis({x+y+z,2*x+3*y-z})
	    isTropicalBasis(flatten entries gens Grassmannian (1,4,QQ[a..l]))
///


doc///
    Key
	multiplicities
	(multiplicities, TropicalCycle)
    Headline
		returns the list of multiplicities on maximal cones in a tropical cycle
    Usage
    	multiplicities(T)
    Inputs
		T:TropicalCycle
    Outputs
    	L:List
    Description
		Text
			This method returns the list of multiplicities on maximal cones in a tropical cycle.
		Example
			QQ[x,y,z]
			V = tropicalVariety(ideal(x+y+z));
			multiplicities V
///

doc///
    Key
	ComputeMultiplicities
    Headline
		option to compute the multiplicities in case they ideal is not prime
    Usage
    	tropicalVariety(I,ComputeMultiplicities=>true)
    Description
		Text
			This option allows to compute the multiplicities in case the ideal I is not prime. In fact the output of gfan
			does not include them and hence they are computed separately by this package. By default the ideal is assumed to be prime.
		Example
			QQ[x,y,z];
			I=ideal(x^2-y^2);
			isPrime I
			T=tropicalVariety(I,Prime=>false,ComputeMultiplicities=>true);
			rays T
			maxCones T
			multiplicities T
///

doc///
    Key
	Prime
    Headline
		option to declare if the input ideal is prime
    Usage
    	tropicalVariety(I,Prime=>false)

    Description
		Text
			By default the ideal is assumed to be prime. If the ideal is not prime then the internal  procedure to compute the tropicalization is different.
			It is used gfan_tropicalbrute force instead of gfan_tropicaltraverse.
		Example
			QQ[x,y,z];
			I=ideal(x^2+y^2-2*x*y);
			isPrime I
			T=tropicalVariety(I,Prime=>false)
///


doc///
    Key
	IsHomogeneous
    Headline
		option to declare if the input ideal is homogeneous
    Usage
    	tropicalVariety(I,IsHomogeneous=>true)

    Description
		Text
			If the option is used than homogeneity of the
			ideal is not tested. By default the ideal is
			always assumed not homogeneous and a test is
			performed before applying the function
			tropicalVariety.
		Example
		          QQ[x,y];
			  I=ideal(x+y+1);
			  T=tropicalVariety (I,IsHomogeneous=>false)
///


doc///
    Key
	Symmetry
    Headline
		option to declare if the input ideal has symmetries
    Usage
    	tropicalVariety(I,Symmetry=>{{..},{..}})

    Description
		Text
			If the option is used, the specified
			symmetries are used in the calculation of the
			tropical variety. For an ideal I of a
			polynomial ring R = KK[x_0 .. x_N], each
			symmetry is a permutation encoded in a list
			\{s_0, s_1, ..., s_N\} of numbers from 0 to N
			which records that swapping the variable x_j
			with the variable x_{s_j} in R leaves the
			ideal I fixed. Exploiting symmetries reduces
			the number of computations needed. The length
			of each symmetry equals the number of
			generators of R, otherwise an error is raised.
		Example
		          QQ[x_0,x_1,x_2];
			  I=ideal(x_0+x_1+x_2+1);
			  T=tropicalVariety (I,Symmetry=>{{1,0,2}, {2,1,0} })
///


doc///
    Key
	(fan,TropicalCycle)
    Headline
	    outputs the fan assocated to the tropical cycle
    Usage
    	fan(T)
    Inputs
	T:TropicalCycle

    Outputs
	F:Fan
	    the fan associated to the  tropical cycle T

    Description
		Text
		        This function outputs the fan associated to the tropical cycle T.
		Example
			QQ[x,y,z]
			T=tropicalVariety (ideal(x+3*y+3));
			fan T
			peek o3#cache


///
doc///
    Key
	(maxCones,TropicalCycle)
    Headline
	computes the maximal cone of a tropical cycle
    Usage
    	maxCones(T)
    Inputs
	T:TropicalCycle

    Outputs
        L:List


    Description
		Text
		        This function computes the maximal cones of the fan associated to the tropical cycle.

		Example
			QQ[x,y,z,w]
			I=ideal(x^2-y*z+w^2,w^3-x*y^3+z^3);
			T=tropicalVariety I;
			maxCones T

///
doc///
    Key
	(isPure,TropicalCycle)
    Headline
	checks whether  a tropical cycle is pure
    Usage
    	isPure(T)
    Inputs
	T:TropicalCycle

    Outputs
	B:Boolean


    Description
		Text
		        This function checks whether the fan associated to the tropical cycle is pure, i.e. if the maximal cones have all the same dimension.
		Example
		        F=fan ({posHull(matrix{{1,2,3},{0,2,0}}),posHull(matrix{{0},{1}})});
			T=tropicalCycle (F,{1,2});
			isPure T

///
doc///
    Key
	(isSimplicial,TropicalCycle)
    Headline
		checks whether a tropical cycle is simplicial
    Usage
    	isSimplicial(T)
    Inputs
	T:TropicalCycle

    Outputs
	B:Boolean


    Description
		Text
		       This function checks if the fan associated to the tropical cycle T is simplicial, i.e. if for each cone the rays generating  it are linearly independent.
		Example
		       F=fan ({posHull(matrix{{1,2,3},{0,2,0}}),posHull(matrix{{0},{1}})});
		       T=tropicalCycle (F,{1,2});
		       isSimplicial T


///
doc///
    Key
	(rays,TropicalCycle)
    Headline
	computes the rays of a tropical cycle
    Usage
    	rays(T)
    Inputs
	T:TropicalCycle

    Outputs
	M:Matrix


    Description
		Text
		        This function computes the rays of the fan associated to the tropical cycle. These are the columns of
		        the output matrix.
		Example
		        QQ[x,y,z,w]
			I=ideal(x^2-y*z+w^2,w^3-x*y^3+z^3);
			T=tropicalVariety I;
			rays T

///
doc///
    Key
	(dim,TropicalCycle)
    Headline
		computes the dimension of a tropical cycle
    Usage
    	dim(T)
    Inputs
	T:TropicalCycle

    Outputs
	k:ZZ
	    the dimension of the tropical cycle T

    Description
		Text
	                This function computes the dimension of the fan associated to the tropical cycle T.
		Example
			QQ[x,y,z,w]
			I=ideal(x^2-y*z+w^2,w^3-y^3*x+z^3);
			T=tropicalVariety I;
			dim T

///

doc///
    Key
	(fVector,TropicalCycle)
    Headline
		computes the fVector of  a  tropical cycle
    Usage
    	fVector(T)
    Inputs
	T:TropicalCycle

    Outputs
	L:List
	    the fVector of the fan associated to the  tropical cycle T

    Description
		Text
		       This function computes the fVector of the fan associated to the tropical cycle T.
		Example
			QQ[x,y,z]
			T=tropicalVariety (ideal(x+3*y+3));
			fVector T

///

doc///
    Key
	(ambDim,TropicalCycle)
    Headline
		computes the dimension of the ambient space of a tropical cycle
    Usage
    	ambDim(T)
    Inputs
	T:TropicalCycle

    Outputs
	n:ZZ
	    the dimension of the tropicalCycle T

    Description
		Text
		        This function computes the dimension of the space where the tropical cycle is contained.
		Example
			QQ[x,y,z]
			T=tropicalVariety(ideal(x+y+z));
			ambDim T

///
doc///
    Key
	(cones,ZZ,TropicalCycle)
    Headline
		computes the cones of a tropical cycle
    Usage
    	cones(k,T)
    Inputs
	k:ZZ
	T:TropicalCycle

    Outputs
	L:List
	    the cones of codimension k in T

    Description
		Text
		        This function computes the cone of codimension k of the fan associated to the tropical cycle T.
		Example
			QQ[x,y,z,w,t]
			I=ideal(x^2-y*z+w^2,w^3-y^3*x+z^3,t-w+x);
			T=tropicalVariety I;
			cones(2,T)

///

doc///
    Key
	(linealitySpace,TropicalCycle)
    Headline
	computes the lineality space of a  tropical cycle
    Usage
    	linealitySpace(T)
    Inputs
	T:TropicalCycle

    Outputs
	M:Matrix


    Description
		Text
		        This function computes the lineality space of the fan associated to the tropical cycle T. The generators of the lineality space are the columns of the
		        output  matrix
		Example
		        QQ[x,y,z];
			I=ideal(x-y);
			T=tropicalVariety I;
			L=linealitySpace T

///


doc///
   Key
       "Polymake interface instructions"
   Headline
       instructions for loading Polymake with this package.
   Description
       Text
       	   The software program Polymake is not distributed with
       	   Macaulay2, so to use the Polymake commands the user needs
       	   to install Polymake on their own machine, and tell
       	   Macaulay2 where to find it.  This is done with the
       	   Configuration option "polymakeCommand".  The default is
       	   that this is empty, which means that Polymake options will
       	   not be used.  To tell the package where your copy of Polymake is installed, use either
	   loadPackage("Tropical",Configuration=>\{"polymakeCommand"=>"YOUR COMMAND"\}), or
	   edit the init-Tropical.m2 file (created after you install the package)
	   by changing "polymakeCommand" => "", into "polymakeCommand" => "YOUR COMMAND"

	   On a Mac, the default value for YOUR COMMAND is
	   /Applications/polymake.app/Contents/MacOS/polymake.start
	   and the init-Tropical.m2 file is usually in ~/Library/Application Support/Macaulay2.

	   On Unix, the default value for YOUR COMMAND is
           /usr/bin/polymake
	   and the init-Tropical.m2 file is usually in ~/.Macaulay2.
	   If polymake is installed in a nonstandard location, you can
	   find YOUR COMMAND with the terminal command "which polymake".

	   This package should work with Polymake versions > 3.2, and has been tested up to 4.2.
///

doc///
        Key
	  BergmanFan
	Headline
	    the Bergman fan of a matroid
	Usage
	    BergmanFan(M)
	Inputs
	    M:Matroid
	Outputs
	    T:TropicalCycle
	Description
	    Text
	    	Computes the Bergman fan of a matroid, with the fine
	    	fan structure.  This uses the Matroids package; the
	    	input should be a matroid in the sense of that
	    	package.  The output is a tropical cycle T whose
	    	underlying fan is the fine fan structure in the sense
	    	of Ardila-Klivans on the Bergman fan of the matroid.
	    	This has underlying simplicial complex the order
	    	complex of the lattice of flats of the matroid M.
		
    	    	If the ground set of the matroid has size n, then the
    	    	fan given is in R^n, so there is always a lineality
    	    	space of dimension at least one.
	    Example
	        M=uniformMatroid(2,3);
		T=BergmanFan(M);
		rays T
		maxCones T
		linealitySpace T
///

	      

----- TESTS -----

-----------------------
--tropicalCycle
-----------------------
TEST ///

F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
assert((tropicalCycle(F,{1,1,1}))#"Fan"== F)
assert((tropicalCycle(F,{1,1,1}))#"Multiplicities"== {1,1,1})

///

-----------------------
--isTropicalBasis
-----------------------

TEST ///
assert(isTropicalBasis (flatten entries gens Grassmannian(1,4,QQ[a..l]))==true)
R:=QQ[x,y,z]
assert( not isTropicalBasis({x+y+z,2*x+3*y-z}))
///


-----------------------
--getters
-----------------------

--rays


TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((rays T)==( matrix{{0,0,0},{1,-1,0},{0,-1,1}}))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((rays(T))==(matrix {{}, {}, {}}))
///

--maxCones
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert(sort(maxCones T)==( sort({{0,1},{0,2},{1,2}})))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((maxCones(T))==({{}}))
///



--dim
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((dim T)==( 3))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((dim T)==(1))
///

--ambDim

TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((ambDim T)==( 3))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((ambDim T)==(3))
///


--fVector
TEST///
T1:=new TropicalCycle
G:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T1#"Multiplicities" ={1,1,1};
T1#"Fan" = G;
assert((fVector T1)==( {0, 1, 3, 3}))
G=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T1#"Multiplicities" ={1};
T1#"Fan" = G;
assert((fVector T1)==({0,1}))
///



--linealitySpace
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((linealitySpace T)==( matrix{{1},{1},{1}}))
F=fan(matrix{{1,0,-1},{0,1,-1}},map(ZZ^2,ZZ^0,0),{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((linealitySpace T)==(0))
///

--multiplicities
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((multiplicities T)==( {1,1,1}))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((multiplicities T)==({1}))
///


--fan
TEST///
T=new TropicalCycle
F=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((fan T)==(F))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((fan T)==(F))
///

--cones
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert((cones(1,T))==({{}}))
assert((cones(2,T))==({{0},{1},{2}}))
assert((cones(3,T))==({{0,1},{0,2},{1,2}}))
F:=fan(map(ZZ^3,ZZ^0,0),matrix{{1},{1},{1}},{{}})
T#"Multiplicities" ={1};
T#"Fan" = F;
assert((cones(1,T))==({{}}))
///







-----------------------
--isBalanced
-----------------------

TEST///

F=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}});
T= tropicalCycle(F,{1,1,1});
assert(isBalanced T)

T2 = tropicalCycle(F,{1,2,3});
assert(not isBalanced T2);

///

TEST///

R=QQ[x,y,z];
V = tropicalVariety(ideal(x+y+z))
assert(isBalanced V)

G = V;
G#"Multiplicities" = {1,2,1}
assert(not isBalanced G)

///

TEST///

S = QQ[x,y];
V = tropicalVariety(ideal(x+y+1))
assert(isBalanced V)
///

TEST///

F = fan {posHull matrix {{1},{0},{0}}, posHull matrix {{0},{1},{0}}, posHull matrix {{0},{0},{1}}, posHull matrix {{-1},{-1},{-1}}}
mult = {1,2,-3,1}
assert(not isBalanced (tropicalCycle(F, mult)))

///



-----------------------
--tropicalPrevariety
-----------------------

TEST///
QQ[x,y,z,w]
F=tropicalPrevariety({x+y+z+w,x^2+y*z})
assert((rays F) == matrix {{1,1,-1},{5,-3,-1},{-3,5,-1},{-3,-3,3}})
///

TEST///
QQ[x,y,z,w]
F=tropicalPrevariety({x+y+z+w,x^2+y*z}, {{0,2,1,3}})
assert((rays F) == matrix {{1,1,-1},{5,-3,-1},{-3,5,-1},{-3,-3,3}})
///


TEST///
QQ[x,y]
F=tropicalPrevariety({x+y+1}, {{1,0}})
assert((rays F) == matrix {{1,-1,0},{0,-1,1}})
///

-----------------------
--stableIntersection
-----------------------

TEST///

F=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}});
T= tropicalCycle(F,{1,1,1});
T2= stableIntersection(T,T,Strategy=>"gfan")
assert(dim T2 == 3)
assert(maxCones T2 == {{1, 2}, {0, 2}, {0, 1}})

///


if polymakeOK then (
TEST///

F1:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}});
T1:= tropicalCycle(F1,{1,1,1});
R1:= stableIntersection(T1,T1,Strategy=>"atint");
R2:= stableIntersection(T1,T1,Strategy=>"gfan");
assert(R1#"Fan" == R2)
assert(dim R2 == 3)
assert(maxCones R2 == {{1, 2}, {0, 2}, {0, 1}})

R =QQ[x,y,z,t];
I=ideal(x+y+z+t);
J=ideal(4*x+y-2*z+5*t);
T1 = stableIntersection(tropicalVariety(I),tropicalVariety(J));
T2 = tropicalVariety(I+J);
assert(T1#"Fan" == T2#"Fan")
assert(T1#"Multiplicities" == T2#"Multiplicities")

R = QQ[x,y,z];
I = ideal(x^2+y^2+z^2-1);
T1 = tropicalVariety(I);
J = ideal(x*y+y*z+x*z+1);
T2 = tropicalVariety(J);
V = tropicalVariety(I+J);
W1 =  stableIntersection(T1,T2,Strategy=>"gfan");
W2 =  stableIntersection(T1,T2,Strategy=>"atint");
assert(V#"Fan" == W1)
assert(V#"Fan" == W2#"Fan")
assert(V#"Multiplicities" == W2#"Multiplicities")

///
)

-----------------------
--tropicalVariety
-----------------------


TEST///
QQ[x,y,z,w]
--homogeneous
I=ideal(x^2+y^2+z^2)
T:=tropicalVariety(I)
assert ((rays T)==(matrix {{2, -1, -1},{-1, 2, -1}, {-1, -1, 2}, {0, 0, 0}}))
assert((linealitySpace T)==( matrix {{1, 0}, {1, 0}, {1, 0}, {0, 1}}))
assert((multiplicities T)==( {2,2,2}))
assert((maxCones T)==( {{0},{1},{2}}))
--homogeneous and binomial
I=ideal(x^2+x*z)
T=tropicalVariety (I,Prime=>false)
assert ((rays T)==(0 ))
assert((linealitySpace T)==( matrix {{0, 1, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}}))
assert((maxCones T)==( {{}}))
assert((multiplicities T)==( {1}))
T=tropicalVariety (I,Prime=>false,ComputeMultiplicities=>false)
assert ((rays T)==(0))
assert((linealitySpace T)==( matrix {{0, 1, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}}))
assert((maxCones T)==( {{}}))
assert((multiplicities T)==( {{}}))
--non homogeneous
I=ideal(x^2-y*z+w^2,w^3-y^3*x+z^3)
T=tropicalVariety(I)
assert((rays T)== (matrix {{-6, -3, 0, 1, 3, 6}, {-5, -2, -1, 1, 2, 5}, {-7, -3, -1, 1, 4, 7}, {-6, -3, -1, 1, 3, 7}}))
assert((linealitySpace T)==(0))
assert((maxCones T)==( {{0, 1}, {0, 2}, {0, 5}, {1, 3}, {2, 3}, {1, 4}, {2, 4}, {3, 5}, {4, 5}}))
assert((multiplicities T)==( {2, 1, 1, 6, 3, 2, 1, 3, 1}))
QQ[x,y,z]
--non homogeneous
I=ideal(x*y-y+1)
T=tropicalVariety(I)
assert ((rays T)== (matrix {{-1, 0, 3}, {1, -3, 0}, {0, -1, 1}}))
assert((linealitySpace T)==( matrix {{0}, {0}, {1}} ))
assert((maxCones T)==( {{1}, {0}, {2}}))
assert((multiplicities T)==( {1, 1, 1}))
--symmetry and homogeneous
QQ[x,y, z]
I=ideal(x^2+y^2+z^2)
G=tropicalVariety(I, Symmetry=>{{1, 0, 2}, {1, 2, 0}, {2, 0, 1}})
assert ((rays G)==(matrix {{2, -1, -1},{-1, 2, -1}, {-1, -1, 2}}))
--symmetry and non-homogeneous
QQ[x,y]
G=tropicalVariety(ideal(x+y+1), Symmetry=>{{1,0}})
assert((rays G) == matrix {{1,-1,0},{0,-1,1}})

///








-----------------------
--findMultiplicities
--findMultiplicity
--computeCones
-----------------------





-----------------------
--isPure
-----------------------
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert(isPure(T)==(true))
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0},{1,2}})
T#"Multiplicities" ={1,1};
T#"Fan" = F;
assert(isPure(T)==(false))
///



-----------------------
--isSimplicial
-----------------------
TEST///
T:=new TropicalCycle
F:=fan(matrix{{0,0,0},{1,0,-1},{0,1,-1}},matrix{{1},{1},{1}},{{0,1},{0,2},{1,2}})
T#"Multiplicities" ={1,1,1};
T#"Fan" = F;
assert(isSimplicial(T)==(true))
F:=fan(matrix{{0,0,0,0},{1,0,-1,2},{0,1,-1,1}},matrix{{1},{1},{1}},{{0,1,2},{0,1,3},{0,2,3},{1,2,3}});
T#"Multiplicities" ={1,1,1,1};
T#"Fan" = F;
assert(isSimplicial(T)==(false))
///


-----------------------
--BergmanFan
-----------------------
TEST///
U24 = uniformMatroid(2,4)
F = BergmanFan U24
ray = rays F
mC = maxCones F
l = sort(flatten(mC))
assert(ray*transpose(matrix({{1,1,1,1}})) == 0)
assert(l == toList(0..3))
assert(isBalanced(F)==(true))
///

TEST///
A=transpose matrix{{1,0,0},{1,1,0},{1,2,0},{1,0,1},{1,2,1},{1,0,2}};
M=matroid(A);
T=BergmanFan M;
R=rays T;
assert(rank source R ==17)
assert(dim(T)==rank(A))
///


-----------------------
--convertToPolymake
-----------------------


-----------------------
--star
-----------------------

TEST ///
R=QQ[x,y,z];
I=ideal(x+y+z+1);
T=tropicalVariety(I);
P=convexHull(matrix{{0},{0},{0}}, matrix{{1},{0},{0}});
starT=star(T,P);
R=rays starT;
assert(rank source R == 3);
assert(dim starT== 1);
R=QQ[x,y,z,w];
I=ideal(x+y+z+w);
T=tropicalVariety(I);
P=convexHull(matrix{{0},{0},{0},{0}}, matrix{{1},{0},{0},{0}}, matrix{{1},{1},{1},{1}});
starT=star(T,P);
assert(dim starT== 1);
///




end


restart
uninstallPackage "Tropical"

installPackage "Tropical"
check "Tropical"

