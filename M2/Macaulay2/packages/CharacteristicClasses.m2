-- -*- coding: utf-8 -*-

newPackage(
     	"CharacteristicClasses",
	Version =>"2.0",
    	Date => "October 24, 2015",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "martin.helmer@berkeley.edu", 
		  HomePage => "https://math.berkeley.edu/~mhelmer/"},
	      {Name => "Christine Jost", 
		  Email => "christine.e.jost@gmail.com"}},
    	Headline => "CSM classes, Segre classes and the Euler characteristic for some subschemes of smooth complete toric varieties",
	Keywords => {"Intersection Theory"},
    	DebuggingMode => false,
	PackageImports => { "Elimination", "PrimaryDecomposition", "NormalToricVarieties"},
	Configuration => { "pathToBertini" => ""},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Computing characteristic classes and the topological Euler characteristic of complex projective schemes",
	     "acceptance date" => "5 June 2015",
	     "published article URI" => "http://msp.org/jsag/2015/7-1/p04.xhtml",
	     "published code URI" => "http://msp.org/jsag/2015/7-1/jsag-v7-n1-x04-CharacteristicClasses.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/CharacteristicClasses.m2",
	     "release at publication" => "82375d8c668f3acf1d825b8ba991081769fba742",	    -- git commit number in hex
	     "version at publication" => "1.1",
	     "volume number" => "7",
	     "volume URI" => "http://msp.org/jsag/2015/7-1/"
	     }
    	);
    

-- Check the ~/.Macaulay2/init-CharacteristicClasses.m2 file for the absolute path.
bertini'path = (options CharacteristicClasses).Configuration#"pathToBertini";
if not instance(bertini'path,String) then error "expected configuration option pathToBertini to be a string."
--Exported functions/variables
export{"Segre",
   "CSM",
   "Euler",
   "Chern",
   "ChowRing",
   "ClassInChowRing",
   "ClassInToricChowRing",
   "ToricChowRing",
   "isMultiHomogeneous",
   "MultiProjCoordRing",
   "CheckToricVarietyValid",
   "Output",
   "HashForm",
   "HashFormXL",
   "ChowRingElement",
   "Method",
   "InclusionExclusion",
   "DirectCompleteInt",
   "InputIsSmooth",
   "IndsOfSmooth",
   "CheckSmooth",
   "CompMethod",
   "ProjectiveDegree",
   "PnResidual",
   "bertini",
   "bertiniCheck"
   }

MultiProjCoordRing=method(TypicalValue=>Ring);
MultiProjCoordRing (Symbol,List):=(x,l)->(
    kk:=ZZ/32749;
    return MultiProjCoordRing(kk,x,l);
    );
MultiProjCoordRing (Ring,List):=(kk,l)->(
    x:=symbol x;
    return MultiProjCoordRing(kk,x,l);
    );
MultiProjCoordRing (List):=(l)->(
    x:=symbol x;
    kk:=ZZ/32749;
    return MultiProjCoordRing(kk,x,l);
    );
MultiProjCoordRing (Ring, Symbol,List):=(kk,x,l)->(
    if not isField(kk) then(
	<<"The coefficient ring must be a field, using the default field kk=ZZ/32749"<<endl;
	kk=ZZ/32749;
	);
    totalDim:=sum(l);
    m:=length(l);
    numVars:=totalDim+m;
    degs:={};
    ind:=0;
    for n in l do (
	for i from 0 to n do(
	    degs=append(degs,OneAti(m,ind));
	    );
	ind=ind+1;
	);
    return kk[x_0..x_(numVars-1),Degrees=>degs];
    );
ClassInChowRing=method(TypicalValue=>RingElement);
ClassInChowRing (QuotientRing,RingElement) :=(A,f)->(
    d:=degree f;
    m:=numgens(A);
    if not isMultiHomogeneous(f) then error "Requires Homogeneous Input"<<endl;
    if length(d)!=m then(
	error "The degree length of the input polynomial does not match the input ring"<<endl;
	return 0;
	);
    return sum(m,i->d_i*A_i);
    );

ClassInToricChowRing=method(TypicalValue=>RingElement);
ClassInToricChowRing (QuotientRing,RingElement) :=(A,f)->(
    return substitute(f,A);
    );

---------------------------------------------------------
--This function computes the Chern or Chern Fulton class
--
--Input: An Ideal I, of if the ambient spaces is a Toric variety
--an ideal and a NormalToricVariety. Optionally the 
--associated chow ring, or toric chow ring may be input so 
--that the output is returned in this ring
--
--Output: If V=V(I) (in a applicable toric variety X) is smooth 
--the Chern class c(V)=c(TV)*[V] is output,
--if the input is not smooth the Chern-Fulton class (CF) is returned
--
--Computed as CF(V)=c(TX)*s(V,X).
-- (s(V,X) denoting the Segre class of the subscheme V)
--
--Optionally the output may be returned in the form of a MutableHashTable 
--using the option: Output=>HashForm
--with the following keys:
--"Segre"=s(V,X)
--::"SegreList"-List for of "Segre"
--"CF"="Chern"=Chern-Fulton class (Chern class if V smooth)
--"G" the class of the 'Projective Degrees', see [7] (or [5])
--::"Glist"-List form of G
--
---------------------------------------------------------
Chern=method(TypicalValue=>RingElement,Options => {CompMethod=>ProjectiveDegree,Output=>ChowRingElement});
Chern (Ideal,Symbol) :=opts->(I,h)->(
    if opts.CompMethod==PnResidual or opts.CompMethod==bertini  then(
	if degreeLength(ring(I))==1 then(
	    (chernList, ambientDim):= internalChernClassList(I, CompMethod => opts.CompMethod);
	    return output (chernList, ambientDim, h);
	    )
	else(
	    <<"The input computational method is not valid for rings with degree length greater than 1"<<endl;
	    <<"The standard method will be used instead"<<endl;
	    );
	);
    return Chern(ChowRing(ring(I),h),I);
    );
Chern (Ideal) :=opts->(I)->(
    if (opts.CompMethod==PnResidual or opts.CompMethod==bertini) then(
	H:=symbol H;
	return Chern(I,H,CompMethod=>opts.CompMethod,Output=>opts.Output);
	);
    return Chern(ChowRing(ring(I)),I,Output=>opts.Output);
    );
Chern (QuotientRing, Ideal) :=opts->(ChRing, I)->(
    if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    B:=flatten entries sort basis ChRing;
    ns:=degree last B;
    n:=sum(ns);
    m:=length ns;
    R:=ring I;
    ChernTR:=product(m,i->(1+(basis(OneAti(m,i),ChRing))_0_0)^(ns_i+1));
    csm:=0;
    if opts.Output==HashForm then(
	csm=Segre(ChRing,I,Output=>opts.Output);
	csm#"CF"=ChernTR*csm#"Segre";
	csm#"Chern"=csm#"CF";
	)
    else(
	csm=ChernTR*Segre(ChRing,I);
	);
    return csm;
    );
Chern (NormalToricVariety,Ideal) :=opts-> (TorVar,I)->(
    return Chern(ToricChowRing(TorVar),TorVar,I);
    );
Chern (QuotientRing, NormalToricVariety,Ideal) :=opts-> (ChRing, TorVar,I)->(
    if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    R:=ring I;
    ChernTR:=substitute(product(numgens(R),i->(1+R_i)),ChRing);
    csm:=0;
    if opts.Output==HashForm then(
	csm=Segre(ChRing,TorVar,I,Output=>opts.Output);
	csm#"CF"=ChernTR*csm#"Segre";
	csm#"Chern"=csm#"CF";
	)
    else(
	csm=ChernTR*Segre(ChRing,TorVar,I);
	);
    return csm;
    );

---------------------------------------------------------
--This function checks if the input is a toric variety for
--which the methods described in 
--
--Input: A NormalToricVariety 
--
--Output: true/false, true if the input if the methods of
-- are applicable, false otherwise. 
--
---------------------------------------------------------
CheckToricVarietyValid=method(TypicalValue=>Boolean);
CheckToricVarietyValid NormalToricVariety:=X->(
   -- needsPackage "NormalToricVarieties";
    Value:=true;
    for i from 0 to #(rays(X))-1 do (
	if not isNef(X_i) then(
	    return false;
	    );
	);
    if length(primaryDecomposition ideal X)!=(#(rays(X))-dim(X)) then(
	Value=false;
	);
    return Value;
);

---------------------------------------------------------
--This function checks if the input is Homogeneous with
--respect to the grading
--
--Input: An ideal or a polynomial in a polynomial ring R
--
--Output: true/false, true if the input is homogeneous with 
--respect to the grading on R
--
---------------------------------------------------------
isMultiHomogeneous=method(TypicalValue=>Boolean);
isMultiHomogeneous Ideal:=I->(
    Igens:=flatten entries gens(I);
    d:=0;
    fmons:=0;
    for f in Igens do(
	fmons=flatten entries monomials(f);
	if length(fmons)>1 then(
	    d=degree(first(fmons));
	    for mon in fmons do(
		if degree(mon)!=d then(
		    <<"Input term below is not homogeneous with respect to the grading"<<endl;
		    <<f<<endl;
		    return false;
		    );
		);
	    
	    );
	);
    return true;
    
    );
isMultiHomogeneous RingElement:=f->(
    return isMultiHomogeneous(ideal(f));
    );

ToricChowRing=method(TypicalValue=>QuotientRing);
ToricChowRing NormalToricVariety:=TorVar->(
    needsPackage "NormalToricVarieties";
    assert isSimplicial TorVar;
    --First build Chow ring, need Stanley-Reisner Ideal (SR) and the ideal
    -- generated by the linear relations of the rays (J)
    --See Cox, Little, Schenck Th. 12.5.3 and comments after proof
    R:=ring(TorVar);
    A:=0;
    --For simplical toric var. Lemma 3.5 of Euler characteristic of coherent sheaves on simplicial torics via the Stanley-Reisner ring
    -- (and probably other sources) tell us that the SR ideal is the Alexander
    --dual of the toric irrelevant ideal 
    --SR:=dual monomialIdeal TorVar;
    P:=primaryDecomposition ideal TorVar;
    SR:=ideal for p in P list product flatten entries gens p;
    F:=fan TorVar;
    Fd:=dim(F);
    --Build ideal generated by linear relations of the rays
    Jl:={};
    for j from 0 to dim(F)-1 do(
	Jl=append (Jl,sum(length rays(TorVar), i->(((rays TorVar)_i)_j)*R_i ));
    	);
    J:=ideal(Jl);
    --Chow ring
    if isSmooth(TorVar) then(
	--if smooth our Chow ring should be over ZZ
	 --C:=QQ[gens R, Degrees=>degrees R, Heft=>heft R];
	 C:=ZZ[gens R];
         A=C/substitute(SR+J,C);
	 )
     else (error "Calculations for subschemes of singular toric varieties are not implemented yet";return 0;);
    --Generators (as a ring) of the quotient ring representation of the chow ring correspond to 
    --the divisors associated to the rays in the fan Theorem 12.5.3. Cox, Little, Schenck and 
    --comments above
    return A;
    
    
    );

---------------------------------------------------------
--This function builds the Chow ring of a product of 
--projective spaces. 
--
--Input: A graded polynomial ring which is the coordinate ring
--of a product of projective spaces
--
--Output: A quotient ring which represents the Chow ring of the 
--product of projective spaces P=P^{n_1} x ....xP^{n_m} as
--A=ZZ[h_1,...,h_m]/(h_1^{n_1+a},...,h_m^{n_m+1})
-- with h_j denoting the (pushforward of) the rational equivalence 
--class of a hyperplane in the projective space P^{n_j}.
--
--optionally one may choose a different symbol to represent
--the hyperplane classes
---------------------------------------------------------
ChowRing=method(TypicalValue=>QuotientRing);
ChowRing (Ring):=(R)->(
    h:=symbol h;
    return ChowRing(R,h);
    );
ChowRing (Ring,Symbol):=(R,h)->(
    Rgens:=gens R;
    Rdegs:=degrees R;
    degd:=0;
    eqs:=0;
    ChDegs:=unique Rdegs;
    m:=length ChDegs;
    C:=ZZ[h_1..h_m,Degrees=>ChDegs];
    K:={};
    inds:={};
    rg:=0;
    ns:={};
    temp:=0;
    for d in ChDegs do(
	temp=0;
	for a in Rdegs do(
	    if d==a then temp=temp+1;
	    );
	ns=append(ns,temp);
	);
    
    for i from 0 to length(ChDegs)-1 do(
	K=append(K,C_(i)^(ns_i));
	);
    K=substitute(ideal K,C);
    A:=C/K;
    return A;
);
Euler = method(TypicalValue => RingElement,Options => {Method=>InclusionExclusion,CompMethod=>ProjectiveDegree,InputIsSmooth=>false,Output=>ChowRingElement,IndsOfSmooth=>{}});

Euler Ideal:=opts->I->(
    if opts.CompMethod==PnResidual or opts.CompMethod==bertini then(
	if degreeLength(ring(I))==1 then(
	    return internalEuler(I, CompMethod => opts.CompMethod);
	    )
	else(
	    <<"The input computational method is not valid for rings with degree length greater than 1"<<endl;
	    <<"The standard method will be used instead"<<endl;
	    );
	);    
    A:=ChowRing(ring I);
    B:=last flatten entries sort basis A;
    csm:=0;
    EC:=0;
    if opts.InputIsSmooth==true then(
	if opts.Output==HashForm then(
	    EC=Chern(A,I,CompMethod=>opts.CompMethod,Output=>opts.Output);
	    EC#"Euler"=EC#"CSM"_(B)
	    )
	else(
	    csm=Chern(A,I,CompMethod=>opts.CompMethod);
	    EC=csm_(B);
	    );
	)
    else (
	if opts.Output==HashForm then(
	    EC=CSM(A,I,Method=>opts.Method,Output=>opts.Output);
	    EC#"Euler"=EC#"CSM"_(B)
	    )
	else(
	    csm=CSM(A,I,Method=>opts.Method,IndsOfSmooth=>opts.IndsOfSmooth);
	    EC=csm_(B);
	    );	
	);
    return EC;
    );

Euler (NormalToricVariety,Ideal):=opts->(TorVar,I)->(   
    A:=ToricChowRing TorVar;
    B:=last flatten entries sort basis A;
    csm:=0;
    EC:=0;
    if opts.InputIsSmooth==true then(
	if opts.Output==HashForm then(
	    EC=Chern(A,TorVar,I,CompMethod=>opts.CompMethod,Output=>opts.Output);
	    EC#"Euler"=EC#"CSM"_(B)
	    )
	else(
	    csm=Chern(A,I,CompMethod=>opts.CompMethod);
	    EC=csm_(B);
	    );
	)
    else (
	if opts.Output==HashForm then(
	    EC=CSM(A,TorVar,I,Method=>opts.Method,Output=>opts.Output,IndsOfSmooth=>opts.IndsOfSmooth);
	    EC#"Euler"=EC#"CSM"_(B)
	    )
	else(
	    csm=CSM(A,TorVar,I,Method=>opts.Method,IndsOfSmooth=>opts.IndsOfSmooth);
	    EC=csm_(B);
	    );	
	);
    return EC;
    );

Euler RingElement:=opts->csm->(
    A:=ring csm;
    B:=last flatten entries sort basis A;
    EC:=csm_(B);
    return EC;
    );

CSM = method(TypicalValue => RingElement,Options => {CompMethod=>ProjectiveDegree,Method=>InclusionExclusion,CheckSmooth=>true,Output=>ChowRingElement,IndsOfSmooth=>{},InputIsSmooth=>false});

CSM NormalToricVariety :=opts->TorVar->(
    A:=ToricChowRing(TorVar);
    return CSM(A,TorVar,CheckSmooth=>opts.CheckSmooth);
    );

CSM (QuotientRing, NormalToricVariety) :=opts->(A,TorVar)->(
    --Generators (as a ring) of the quotient ring representation of the Chow ring correspond to 
    --the divisors associated to the rays in the fan Theorem 12.5.3. Cox, Little, Schenck and 
    --comments above
    L:=gens(A);
    Trays:=rays TorVar;
    csm:=1_A;
    Rmat:=0;
    prodj:=0;
    --The following implements the method described 
    --in Barthel, Brasselet, and Fieseler.
    --Lemma 12.5.2 of Cox, Little, Schenck is used to find the chow ring class of the 
    --orbit closure from divisors
    --if the toric variety is smooth the multiplicity is 1. 
    Ssets:=0;
    indsubsets:=0;
    TorVarIsSmooth:=false;
    if opts.CheckSmooth==true then TorVarIsSmooth=isSmooth(TorVar);
    if  TorVarIsSmooth then(
     	for i from 1 to dim(TorVar) do(
	    indsubsets=subsets((0..numgens(A)-1),i);
	    Ssets=for l in indsubsets list L_l;
	    csm=csm+sum(0..(length(Ssets)-1),j-> product(Ssets_j));	
	    );
	)
    else(
	for i from 1 to dim(TorVar) do(
	    indsubsets=subsets((0..numgens(A)-1),i);
	    Ssets=for l in indsubsets list L_l;
	    --csm=csm+sum(0..(length(Ssets)-1),j-> mult(transpose matrix Trays_(indsubsets_j))*product(Ssets_j));
	    for j from 0 to length(Ssets)-1 do(
	    	Rmat=transpose matrix Trays_(indsubsets_j);
	    	prodj=product(Ssets_j);
		if prodj!=0 then(
		    csm=csm+ mult(Rmat)*prodj;
		    );
	    	);		
	    );
     	);
    return csm;
)


CSM (NormalToricVariety, Ideal):= opts->(TorVar,I)->(
    needsPackage "NormalToricVarieties";
    return CSM(ToricChowRing(TorVar),TorVar,I,Method=>opts.Method,Output=>opts.Output,IndsOfSmooth=>opts.IndsOfSmooth,InputIsSmooth=>opts.InputIsSmooth);
    );

CSM (QuotientRing,NormalToricVariety, Ideal):= opts->(ChRing,TorVar,I)->(
    needsPackage "NormalToricVarieties";
    KnownCSM:= new MutableHashTable;
    return CSM(ChRing,TorVar,I,KnownCSM,Method=>opts.Method,Output=>opts.Output,IndsOfSmooth=>opts.IndsOfSmooth,InputIsSmooth=>opts.InputIsSmooth);
    );

CSM (Ideal, Symbol) :=  opts -> (I,hyperplaneClass) -> (
        if opts.CompMethod==PnResidual or opts.CompMethod==bertini  then(
	    if degreeLength(ring(I))==1 then(
		(csmList, ambientDim):= internalCSMClassList(I, CompMethod => opts.CompMethod);
		return output (csmList, ambientDim,hyperplaneClass);
		)
	    else(
		<<"The input computational method is not valid for rings with degree length greater than 1"<<endl;
		<<"The standard method will be used instead"<<endl;
		);
	    );
	if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
	KnownCSM:=new MutableHashTable; 
        return CSM(ChowRing(ring(I),hyperplaneClass),I,KnownCSM,Method=>opts.Method,Output=>opts.Output,IndsOfSmooth=>opts.IndsOfSmooth,InputIsSmooth=>opts.InputIsSmooth);
	)

CSM Ideal:=opts->I->(
    if opts.CompMethod==PnResidual or opts.CompMethod==bertini then(
	 H:=symbol H;
	 return CSM(I,H,CompMethod=>opts.CompMethod)
	);
    if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    KnownCSM:=new MutableHashTable; 
    return CSM(ChowRing(ring I),I,KnownCSM,Method=>opts.Method,Output=>opts.Output,IndsOfSmooth=>opts.IndsOfSmooth,InputIsSmooth=>opts.InputIsSmooth);
    );

CSM (QuotientRing,Ideal):=opts->(Chring,I)->(
    KnownCSM:=new MutableHashTable; 
    return CSM(Chring,I,KnownCSM,Method=>opts.Method,Output=>opts.Output,IndsOfSmooth=>opts.IndsOfSmooth,InputIsSmooth=>opts.InputIsSmooth);
    );
CSM (QuotientRing,Ideal,MutableHashTable):=opts->(ChRing,I,KnownCSMVals)->(
    if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    R:=ring I;
    if opts.InputIsSmooth==true then(
	return Chern(ChRing,I,Output=>opts.Output);
	);
    B:=flatten entries sort basis ChRing;
    ns:=degree last B;
    n:=sum(ns);
    m:=length ns;
    KnownCSMVals#"m"=m;
    --R:=ring I;
    degs:=unique degrees R;
    tempId:={};
    PDl:={};
    for d in degs do(
	tempId={};
	for y in gens(R) do(
	    if degree(y)==d then(
		tempId=append(tempId,y);
		);
	    );
	PDl=append(PDl,ideal(tempId));
	);
    ChernTR:=product(m,i->(1+(basis(OneAti(m,i),ChRing))_0_0)^(ns_i+1));
    return CSMMain(ChRing,I,{PDl,n,ChernTR},KnownCSMVals,opts.Method,opts.Output,opts.IndsOfSmooth);
    );
CSM (QuotientRing,NormalToricVariety,Ideal,MutableHashTable):=opts->(ChRing,TorVar,I,KnownCSMVals)->(
    needsPackage "NormalToricVarieties";
    if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    R:=ring I;
    ChernTR:=substitute(product(numgens(R),i->(1+R_i)),ChRing);
    irel:=ideal TorVar;
    PDl:=primaryDecomposition irel;
    n:=dim(TorVar);
    KnownCSMVals#"TorVar"=TorVar;
    return CSMMain(ChRing,I,{PDl,n,ChernTR},KnownCSMVals,opts.Method,opts.Output,opts.IndsOfSmooth);
    );

---------------------------------------------------------
--This function computes the Segre class
--
--Input: An Ideal I, or if the ambient spaces is a Toric variety
--an ideal and a NormalToricVariety. Optionally the 
--associated chow ring, or toric chow ring may be input so 
--that the output is returned in this ring
--
--Output: If V=V(I) (in a applicable toric variety X) is smooth 
--the Segre class s(V,X) of the subscheme V is output,
--
-- For most situations computations are performed in the internal method 
-- SegreMain, this employs the theoretical method described in [5,7]. 
-- Optionally CompMthod=>PnResidual or CompMthod=>bertini can be specified
-- in such a case the method described in [1] is used and the computation
-- is performed in the method internalSegre.
--
--Optionally the output may be returned in the form of a MutableHashTable 
--using the option: Output=>HashForm
--with the following keys:
--"Segre"=s(V,X)
--::"SegreList"-List for of "Segre"
--"G" the class of the 'Projective Degrees', see [7] (or [5])
--::"Glist"-List form of G
--
---------------------------------------------------------

Segre = method(TypicalValue => RingElement,Options => {Output=>ChowRingElement,CompMethod=>ProjectiveDegree});

Segre (NormalToricVariety, Ideal):= opts->(TorVar,I)->(
    return Segre(ToricChowRing(TorVar),TorVar,I,Output=>opts.Output);
    );

Segre (QuotientRing,NormalToricVariety, Ideal):= opts->(ChRing,TorVar,I)->(
    if not isMultiHomogeneous(I) then error"Reqires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    A:=ChRing;
    R:=ring I;
    irel:=ideal TorVar;
    PDl:=primaryDecomposition irel;
    n:=dim(TorVar);
    degI:= degrees I;
    transDegI:= transpose degI;
    len:= length transDegI;
    maxDegs:= for i from 0 to len-1 list max transDegI_i;
    maxexs:=flatten exponents first flatten entries monomials random(maxDegs,R);
    alpha:=sum(numgens(R),i->maxexs_i*substitute(R_i,A));
    return SegreMainToric(ChRing,TorVar,I,PDl,{alpha,n},opts.Output);
    );

Segre (QuotientRing,Ideal):=opts->(ChRing,I) ->(
    R:=ring I;
    degs:=unique degrees R;
    tempId:={};
    PDl:={};
    for d in degs do(
	tempId={};
	for y in gens(R) do(
	    if degree(y)==d then(
		tempId=append(tempId,y);
		);
	    );
	PDl=append(PDl,ideal(tempId));
	);
    Output:=opts.Output;
    return(SegreMainProjective(ChRing,I,PDl,Output));
    );
Segre (Ideal, Symbol) :=  opts -> (I,hyperplaneClass) -> (
        if opts.CompMethod==PnResidual or opts.CompMethod==bertini  then(
	if degreeLength(ring(I))==1 then(
	    (segreList, ambientDim):= internalSegreClassList(I, CompMethod=> opts.CompMethod));
	    return output (segreList, ambientDim,hyperplaneClass);
	    )
	else(
	    <<"The input computational method is not valid for rings with degree length greater than 1"<<endl;
	    <<"The standard method will be used instead"<<endl;
	    );
	return(Segre(ChowRing(ring(I),hyperplaneClass),I,Output=>opts.Output));
     );
Segre Ideal:=opts->I ->(
        if opts.CompMethod==PnResidual or opts.CompMethod==bertini  then(
	    H:=symbol H;
	    return Segre(I,H,CompMethod=>opts.CompMethod)
	);
    return(Segre(ChowRing(ring(I)),I,Output=>opts.Output));
    );

-- There is no test for the above functions using CompMethod=>Bertini as Bertini does not need to
-- be installed on every system that runs Macaulay2. However, the function bertiniCheck()
-- checks whether the commands Segre, Chern, CSM and Euler work when using Bertini
-- instead of symbolic computations.
bertiniCheck = () -> (
    
    setRandomSeed 24;
    x := symbol x; y := symbol y; z := symbol z; w := symbol w;
    
    -- smooth example for Segre and ChernClass
    R := QQ[x,y,z,w];
    I := minors(2,matrix{{x,y,z},{y,z,w}});
    totalSegre := Segre(I, CompMethod=>bertini);
    assert( totalSegre == 3*( (ring(totalSegre))_0 )^2 - 10*( (ring(totalSegre))_0 )^3 );
    totalChern := Chern(I, CompMethod=>bertini);
    assert( totalChern == 3*( (ring(totalChern))_0 )^2 + 2 * ((ring(totalChern))_0)^3 );
    
    -- singular example for CSM Class and Euler
    S := QQ[x,y,z];
    J := ideal(x^3 + x^2*z - y^2*z);
    totalCSM := CSM(J, CompMethod=>bertini);
    assert( totalCSM == 3*( (ring(totalCSM))_0 ) + 1*( (ring(totalCSM))_0 )^2 );
    eulerCharacteristic := Euler(J, CompMethod=>bertini);
    assert( eulerCharacteristic == 1 );
    
    print "Test passed for the option CompMethod=>bertini for the commands Chern, Segre, CSM and Euler.";
    
    )  
---------------------------
--Internal functions 
---------------------------

mult=RayMatrix->(
    r:=rank RayMatrix;
    return multr(RayMatrix,r);
    )

--Find the multiplicity (or index) of a simplicial cone defined by the
-- rays given by the columns of the input RayMatrix pg. 300
-- Cox, Little, Schenck.
--pg 66-68, and others.... 
--Find the multiplicity (or index) of a simplicial cone defined by the
-- rays given by the columns of the input RayMatrix pg. 300
-- Cox, Little, Schenck. 

multr=(RayMatrix,r)->(
    m:=RayMatrix;
    if m==0 then return 0;
    if (numRows(m)==1 or numColumns(m)==1) then( return 1);
    --r:=rank m;
    if r<numRows(m) then(
	m=transpose groebnerBasis(transpose(m), Strategy=>"MGB");
	);
    if r<numColumns(m) then(
	--this shouldn't be reached when using mult in csm/euler calc
	m= groebnerBasis(m, Strategy=>"MGB");
	);
    if numRows(m)==numColumns(m) then(	
	mymult:=abs(determinant(m,Strategy =>Cofactor));
	return mymult;
	)
    else (
	error "multiplicity computation error";
	return 0;
	);
    
    )

SegreMainToric = (ChRing,TorVar,I,PDl,AlphNList,Output)->(
    if not isMultiHomogeneous(I) then error"Reqires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    alpha:=AlphNList_0;
    n:=AlphNList_1;
    return SegreMain(ChRing,I,PDl,{alpha,n,Output});
    );
SegreMainToric2 = (ChRing,TorVar,I,PDl,Output)->(
    if not isMultiHomogeneous(I) then error"Reqires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    irel:=ideal TorVar;
    n:=dim(TorVar);
    A:=ChRing;
    R:=ring I;
    degI:= degrees I;
    transDegI:= transpose degI;
    len:= length transDegI;
    maxDegs:= for i from 0 to len-1 list max transDegI_i;
    maxexs:=flatten exponents first flatten entries monomials random(maxDegs,R);
    alpha:=sum(numgens(R),i->maxexs_i*substitute(R_i,A));
    return SegreMain(ChRing,I,PDl,{alpha,n,Output});
    );

SegreMainProjective=(ChRing,I,PDl,Output)->(
    if not isMultiHomogeneous(I) then error "Requires Homogeneous Input, try saturating by the irrelevant ideal"<<endl;
    R:=ring I;
    A:=ChRing;
    B:=flatten entries sort basis A;
    degs:=unique degrees R;
    n:=numgens(R)-length(degs);
    degI:= degrees I;
    transDegI:= transpose degI;
    len:= length transDegI;
    maxDegs:= for i from 0 to len-1 list max transDegI_i;
    deg1B:={};
    for w in B do if sum(degree(w))==1 then deg1B=append(deg1B,w);
    m:=length degs;
    alpha:=sum(length deg1B,i->(basis(OneAti(m,i),A))_0_0*maxDegs_i); 
    return SegreMain(A,I,PDl,{alpha,n,Output});
    );

SegreMain = (ChRing,I,PDl,alphaANDn)->(
    -- take care of the special cases I = (0) and I = (1) 
    alpha:=alphaANDn_0;
    n:=alphaANDn_1;
    Output:=alphaANDn_2;
    R:=ring I;
    kk:=coefficientRing R;
    A:=ChRing;     
    zdim:=0;
    B:=flatten entries sort basis A;
    for b in B do(
	if sum(flatten(exponents(b)))==n then zdim=b;
	);
    seg:=0;
    gbI:=groebnerBasis(I, Strategy=>"MGB");
    codimI:= codim ideal leadTerm gbI;
    if ideal(gbI)==ideal(0_R) then return 1_A;
    t1:=symbol t1;
    S:=kk[gens R, t1];
    dimI:=n-codimI;
    gensI:= delete(0_R,flatten sort entries gens I);
    exmon:=0;
    degI:= degrees I;
    m:=length unique degrees R;
    transDegI:= transpose degI;
    len:= length transDegI;
    maxDegs:= for i from 0 to len-1 list max transDegI_i;
    J:= for i from 1 to n list sum(gensI,g -> g*random(kk)*random(maxDegs-degree(g),R));
    RdList:={};
    GList:={};
    Jd:=0;
    JT:=0;
    c:={};
    v:=0;
    ve:=0;
    K:=0;
    Ls:=0;
    LA:=0;
    gbWt2:=0;
    tall2:=0;
    Yiota:=0;
    ValuesTable:=new MutableHashTable;
    n2:=n;--min(n,numgens(I));
    if codimI<=n then( 
	GList=for iota from 0 to (codimI-1) list alpha^(iota);
	for iota from codimI to n2 do(
	    Jd=substitute(ideal take(J,iota),S);
	    JT=ideal (1-t1*substitute(sum(gensI,g -> g*random(kk)),S));
    	    c={};
    	    for w in B do if sum(flatten(exponents(w)))==iota then c=append(c,w);
    	    Yiota=0;
    	    for w in c do(
	    	Ls=0;
		LA=0;
	    	K=0;
	    	v=zdim//w;
		ve=flatten exponents(v);
		--In this case we are working with the toric representation
		--and may use exponents directly
		if length(ve)==numgens(R) then(
	    	    for i from 0 to length(ve)-1 do(
	    	    	if ve_i!=0 then (
		    	    Ls=Ls+sum(ve_i,j->ideal(random(degree(R_i),R)));
		    	    );
	    	    	);
		)
	        else(
		    --Projective representation, exponents associated with degrees ring
		    for i from 0 to length(ve)-1 do(
			if ve_i!=0 then (
			    Ls=Ls+sum(ve_i,j->ideal(random(OneAti(m,i),R)));
			    );
			);
		    );
		for p in PDl do (
		    LA=LA+ideal(1-sum(numgens(p),i->random(kk)*p_i));
		    );
	    	K=Jd+JT+substitute(Ls,S)+substitute(LA,S);
	    	gbWt2 = groebnerBasis(K, Strategy=>"F4");
            	tall2 = numColumns basis(cokernel leadTerm gbWt2);
	    	Yiota=Yiota+tall2*w;
	    	);
    	    GList=append(GList,Yiota);
    	    );
	for l from n2 to n-1 do(
	    GList=append(GList,0);
	    );
	--the following performs the Aluffi tensor notation comp
	--GxOMD:=sum(0..n,i->GList_i//((1+cOMaxDegs)^i));
	temp3:=1;
	GxOMD:=0;
	ValuesTable#"Glist"=GList;
	ValuesTable#"G"=sum(GList);
	tayAlph:=1;
	temp4:=1;
	ind:=1;
	while temp4!=0 do(
	    temp4=alpha*temp4;
	    tayAlph=tayAlph+(-1)^ind*temp4;
	    ind=ind+1;
	    );
	for i from 0 to n do(
	    GxOMD=GxOMD+GList_i*(temp3);
	    temp3=temp3*tayAlph;
	    );
	seg=1-(GxOMD*tayAlph);
	ValuesTable#"Segre"=seg;
	tseg:=terms(seg);
	tot:=0;
	use R;
    	if  Output==HashForm then( 
	    segList:={};
	    for i from 0 to n do(
	    tot=0_A;
	    for f in tseg do(
		if sum(flatten(exponents(f)))==i then(
		    tot=tot+f
		    );
	    );
	    segList=append(segList,tot);
	    );
	    use R;
	    ValuesTable#"SegreList"=segList;
	    return ValuesTable;	
	    )
	else (
	    use R;
	    return seg
	    )
	)
    else(
	segList=for i from 0 to n list 0_A;
	seg=0_A;
	if Output==HashForm then(
	    use R;
	    ValuesTable#"SegreList"=segList;
	    ValuesTable#"Segre"=seg;
	    ValuesTable#"Glist"=segList;
	    ValuesTable#"G"=seg;
	    return ValuesTable;
	    )
	 else (
	     use R;
	     return seg;
	     );
	);
);




CSMInEx = (ChRing,I,RingInfoList,KnownCSMVals,Output)->(
    R:=ring I;
    A:=ChRing;
    PDl:=RingInfoList_0;
    n:=RingInfoList_1;
    ChernTR:=RingInfoList_2;
    gensI:=flatten entries gens I;
    gbI:=groebnerBasis(I, Strategy=>"MGB");
    if ideal(gbI)==ideal 1_R then return 0_A;
    if ideal(gbI)==ideal 0_R then return ChernTR;
    --B:=flatten entries sort basis A;
    m:=0;
    if KnownCSMVals#?"m" then m=KnownCSMVals#"m";
    SegY:=0;
    SegV:=0;
    SegYH:=0;
    V:=0;
    K:=0;
    J:=0;
    csmInput:=false;
    csmValsComputed:=new MutableHashTable;
    vf:=0;
    csm:=0;
    f:=0;
    csm2:=0;
    Isubsets:=delete({},subsets(numgens(I)));
	for ind in Isubsets do(
	    csmInput=false;
	    if KnownCSMVals#?ind then(
		if instance(KnownCSMVals#ind,A) then(
		    csmInput=true;
		    );
		);
	    f=gensI_ind;
	    if csmInput then(
		csm=csm+(-1)^(length(f)+1)*KnownCSMVals#ind;
		) 
	    else(	
	    	K=radical ideal product(f);
	    	J=ideal(delete(0_R,flatten entries jacobian K));
	    	if KnownCSMVals#?"TorVar" then(
		    vf=flatten exponents first flatten entries monomials K_0;
	    	    V=sum(numgens(R),i->vf_i*substitute(R_i,A));
		    SegYH=SegreMainToric2(A,KnownCSMVals#"TorVar",J,PDl,HashForm);
		    SegY=SegYH#"SegreList";  
		    if Output==HashFormXL then(
			csmValsComputed#("G(Jacobian)"|toString(ind))=SegYH#"G";
			csmValsComputed#("Segre(Jacobian)"|toString(ind))=SegYH#"Segre";
			);
		    ) 
	    	else (
		    vf=degree K_0;
	    	    V=sum(length vf, i-> vf_i*(basis(OneAti(m,i),A))_0_0);
		    SegYH=SegreMainProjective(A,J,PDl,HashForm);
		    SegY=SegYH#"SegreList";
		    if Output==HashFormXL then(
			csmValsComputed#("G(Jacobian)"|toString(ind))=SegYH#"G";
			csmValsComputed#("Segre(Jacobian)"|toString(ind))=SegYH#"Segre";
			);
		    );
	    	SegV=V//(1+V);
	    	csm2=(-1)^(length(f)+1)*(ChernTR*(SegV+sum(n+1,i->sum(n+1-i,j->binomial(n-i,j)*(-V)^j*(-1)^(n-j-i)*SegY_(n-i-j)))) );
	    	csm=csm+csm2;
		csmValsComputed#ind=csm2*(-1)^(length(f)+1);
		);
	    );
	csmValsComputed#"CSM"=csm;
	use R;
	if Output==HashForm or Output==HashFormXL then return csmValsComputed else return csm;
    );

CSMCompleteInt= (ChRing,I,RingInfoList,KnownCSMVals,Output,SmoothInds)->(
    R:=ring I;
    irelId:=irrell(R);
    PDl:=RingInfoList_0;
    n:=RingInfoList_1;
    ChernTR:=RingInfoList_2;
    A:=ChRing;
    gbI:=groebnerBasis(I, Strategy=>"MGB");
    if ideal(gbI)==ideal 1_R then return 0_A;
    if ideal(gbI)==ideal 0_R then return ChernTR;
    codimI:= codim ideal leadTerm gbI;
    J2:=0;
    Z:=0;
    J:=0;
    W:=0;
    hyper:=0;
    cont2:=false;
    Sing:=0;
    cont:=true;
    SingInds:={};
    SegY:=0;
    Ssets:={};
    gensI:=flatten entries gens I;
    r:=length(gensI);
    codimJ2:=0;
    m:=0;
    if KnownCSMVals#?"m" then m=KnownCSMVals#"m";
    csm:=0;
    if codimI!=length(gensI) then(
        <<"The input ideal does not define a complete intersection, the option 'DirectComleteInt' may not be used."<<endl;
        <<"Using Inclusion/Exclusion instead."<<endl;
	return CSM(ChRing,I);
	)
    else(
	if KnownCSMVals#?"SmoothPart" or KnownCSMVals#?"SingularPart" or (SmoothInds!={}) then(
	    cont2=true;
	    if KnownCSMVals#?"SmoothPart" then(
		SingInds=toList(set(0..(r-1))-set(KnownCSMVals#"SmoothPart"));
		Z=ideal gensI_(KnownCSMVals#"SmoothPart");
		)
	    else if KnownCSMVals#?"SingularPart" then(
		SingInds=KnownCSMVals#"SingularPart";
		Z=ideal gensI_(toList(set(0..(r-1))-set(SingInds)));
		)
	    else (
		 SingInds=toList(set(0..(r-1))-set(SmoothInds));
		 Z=ideal gensI_(SmoothInds);
		 );
	    )
	else(	    
	    for j from 1 to r-1 do(
                    if cont then (
			Ssets=subsets(set(0..(r-1)),r-j);
                        for s in Ssets do(
                            W=ideal gensI_(toList(s));
			    J2=saturate(minors(#s,jacobian W)+W,irelId);
			    codimJ2:= codim ideal leadTerm groebnerBasis(J2, Strategy=>"MGB");
                            if codimJ2>n then (
                                SingInds=toList(set(0..(r-1))-s);
                                Z=W;
				cont=false;
				cont2=true;
                                break;
                                );
                            );
                        )
                    else(
                        break; 
                        );   
                    );        
                
	    );
	Sing=gensI_(SingInds);
	Ssets=subsets(SingInds)-set{{}};
	if cont2 then(
	    Vlist:={};
	    dv:=0;
	    if KnownCSMVals#?"TorVar" then(
		for f in gensI do(
		    dv=flatten exponents first flatten entries monomials f;
		    Vlist=append(Vlist,sum(numgens(R), i-> dv_i*substitute(R_i,A)));
		    );
		)
	    else(
		for f in gensI do(
		    dv=degree f;
	    	    Vlist=append(Vlist,sum(length dv, i-> dv_i*(basis(OneAti(m,i),A))_0_0));
		    );
		);
	    CE:=product(r,j->(1+Vlist_j));
	    Vr:=0;
	    V2:=0;
	    for s in Ssets do(
		hyper=product(gensI_(toList s));
		J=saturate(minors(numgens(Z)+1,jacobian(Z+ideal(hyper)))+Z+ideal(hyper),irelId);
		if KnownCSMVals#?"TorVar" then(
		    dv=flatten exponents first flatten entries monomials Sing_0;
		    Vr=sum(numgens(R), i-> dv_i*substitute(R_i,A));
		    SegY=(SegreMainToric2(A,KnownCSMVals#"TorVar",J,PDl,HashForm))#"SegreList";
		    )
		else(
		    dv=degree Sing_0;
		    Vr=sum(length dv, i-> dv_i*(basis(OneAti(m,i),A))_0_0);
		    SegY=(SegreMainProjective(A,J,PDl,HashForm))#"SegreList";
		    );
	        V2=product(r,j->Vlist_j);
	        if r==1 then return (ChernTR*V2)//CE;
	        CEotimesL:=sum(0..r,i->(-1)^i*(1+Vr)^(r-i)*sum(select(terms CE, q->sum(degree(q))==i)));
	        segstuff:=sum(0..n,i->((-1)^i*SegY_(i))//( (1+Vr)^i) );
	        cfj:=(ChernTR//CE)*V2;
	        milnor:=(ChernTR//CE)*(-1)^(r+1)*CEotimesL*(segstuff);
	        csm=csm+(-1)^(#(s)+1)*(cfj+milnor);
	        );
	    use R;
	    return csm;
	    )
	else(
	    <<"Input does not satisfy assumptions, using inclusion/exclusion instead"<<endl;
	    CSM(ChRing,I);
	    );
	);
    
    );
CSMMain = (ChRing,I,RingInfoList,KnownCSMVals,Methd,Output,SmoothInds)->(
    if not instance(SmoothInds,List) then(
	<<"The given input option IndsOfSmooth will be ignored"<<endl
	<<"The input option IndsOfSmooth must be of type list"<<endl;
	SmoothInds={};
	 );
    if Methd==DirectCompleteInt and (numgens(I)>1) then(
	return CSMCompleteInt(ChRing,I,RingInfoList,KnownCSMVals,Output,SmoothInds);
    )
    else (
	return CSMInEx(ChRing,I,RingInfoList,KnownCSMVals,Output);
	);
    
    )
OneAti=(dl,i)->(
    vec:={};
    for j from 0 to dl-1 do(
	if j==i then vec=append(vec,1) else vec=append(vec,0);
	);
    return vec;
    )

irrell=R->(    
    Rgens:=gens R;
    Rdegs:=degrees R;
    bloks:=unique Rdegs;
    irId:=ideal 1_R;
    elList:={};
    for a in bloks do(
	elList={};
	for r in Rgens do(
	    if degree(r)==a then(
		elList=append(elList,r);
		);
	    );
	irId=irId*ideal(elList)
	
	);
    return irId;
    )
-- The functions internalSegreClassList, internalChernClassList and internalCSMClassList call 
-- other internal functions which do the actual work. 
internalSegreClassList = {CompMethod => ProjectiveDegree} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I, opts.CompMethod);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Segre classes
     return internalSegre(localI, CompMethod => opts.CompMethod);
     )
internalChernClassList = {CompMethod => ProjectiveDegree} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I,opts.CompMethod);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Chern classes
     return internalChern(localI, CompMethod => opts.CompMethod);
     )
internalCSMClassList = {CompMethod => ProjectiveDegree} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I,opts.CompMethod);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Chern-Schwartz-MacPherson classes
     return internalCSM(localI, CompMethod => opts.CompMethod);
     )
-- The function internalEuler checks and prepares the input, just as for example
-- internalSegreClassList. It then computes the Chern-Schwartz-MaxPherson-classes
-- of the input using internalCSM and returns the top Chern-Schwartz-MacPherson-
-- class, which equals the topological Euler characteristic
internalEuler = {CompMethod => ProjectiveDegree} >> opts -> I -> (
     -- check that the input is a homogeneous ideal in a polynomial ring over a field
     checkUserInput(I,opts.CompMethod);
     -- trim the ideal and make it an ideal over a ring only used internally
     localI := prepare I;
     -- compute the Chern-Schwartz-MacPherson classes and return the degree of the top class
     return last first internalCSM(localI, CompMethod => opts.CompMethod);
     )

-- The function internalSegre is one of the two main functions in this package which do the actual 
-- computation of the Segre classes. It uses the algorithm described in [1].
-- Computing the degrees of the residuals as defined in [1] is the heart of the algorithm. This
-- is done by the subroutine residualDegs.
-- Notation: This algorithm computes the degrees of the Segre classes s_0(Z,P^k), ..., s_n(Z,P^k) of an
-- n-dimensional closed subscheme Z of P^k. The subscheme Z is given by a homogeneous ideal I in the 
-- polynomial ring R.
-- Input:  I, a homogeneous ideal in a polynomial ring over a field
-- Output: segreList, a list containing the degrees of the Segre classes of Proj(R/I) = Z
--         ambientDim, the dimension k of the ambient space Proj(R)=P^k 
internalSegre = {CompMethod => ProjectiveDegree} >> opts -> I -> (
    
     -- Obtain:
     -- the ring R 
     -- the dimension of the ambient space and
     -- the dimension n of Z
     
     R := ring I;
     ambientDim := dim Proj R;
     dimension := dim Proj(R/I) ;
      -- initialize segreList as an empty list
     segreList:= {};
     -- take care of the special cases I = (0) and I = (1)
     if I == ideal(0_R) then (
	  segreList = {1} | toList( ambientDim:0 );
	  return (segreList,ambientDim);
	  );
     if I == ideal(1_R) then (
	  segreList = {};
	  return (segreList,ambientDim);
	  ); 

      -- For the nonspecial cases, obtain:
     -- a list of the generators of I sorted by degree
     -- the maximal degree of the generators of I and
     -- a generator of I with minimal degree     
     
     gensI := flatten entries sort gens I;
     maxDeg := first max degrees I; 
     minDegGen := first gensI;
     
     if(opts.CompMethod==ProjectiveDegree) then (
        S:=ring I;
        m:=numgens I;
        kk:=coefficientRing S;
        n:=numgens S-1; 
        h := symbol h;   
        ChowRingPn:=ZZ[h]/(h^(n+1));
        d:=first max degrees I; 
        use(ChowRingPn);
       
        g:=internalProjectiveDegree(I);
        poly:=sum(0..n,s->g_s*h^s*(1+d*h)^(n-s));
        segreclass:=1 - poly * sum(0..n,i->binomial(n+i,i)*(-d*h)^i);
        for a in listForm segreclass do (segreList={a_1}|segreList );
        
      )
     else (  
    
   
    
    
     -- Pick random elements in I of degree maxdeg, one more than the dimension of the ambient space, store in the list f.
     f := for i from 1 to (ambientDim + 1) list sum( gensI, g -> g * random(maxDeg - first(degree(g)), R) );      
     
     -- Compute the degree of the residual of Z in the intersection of d hypersurfaces, where d = codimension of Z, ... , dimension of the ambient space.
     -- Depends on the strategy (ResidualSymbolic/Bertini).
     degR := residualDegs(f, ambientDim, dimension, minDegGen, CompMethod => opts.CompMethod);  
         
     
     -- The for loop computes the degrees of the Segre classes of Z using the degrees of the residuals
     for d from (ambientDim - dimension) to ambientDim do (

     	  -- Using the degree of the residual, compute the degree of the pth Segre class, where p = d - codimension of Z.
	  p := d - (ambientDim - dimension);
	  degSegreClass := maxDeg^d - degR_(d - ambientDim + dimension) - sum( 0..(p-1), i -> binomial(d,p-i)*maxDeg^(p-i)*segreList_i );

	  segreList = append(segreList, degSegreClass);

	  );  );
     
     return (segreList, ambientDim); 
    
     
     )


-- The function residualDegs is the other one of the two main functions in this package which do the actual 
-- computation of the Segre classes. It computes the degrees of the residuals as defined in [1].
-- The option CompMethod determines which method is used to compute the degrees of the residuals.
-- ResidualSymbolic uses Groebner bases to compute the saturation of ideals.
-- Bertini uses the regenerative cascade as developed in [3] and implemented in Bertini [2].
residualDegs = {CompMethod => ProjectiveDegree} >> opts -> (f, ambientDim, dimension,minDegGen) -> (
     
     R := ring first f;	  
     degR :={};
     
     if (opts.CompMethod == PnResidual) then (

  	  for d from (ambientDim - dimension) to ambientDim do (
	       -- Obtain the ideal J of the intersection of d hypersurfaces containing Z, where d = codimension of Z, ..., dimension of the ambient space.
	       J := ideal(take(f,d));

	       -- Compute the residual of Z in the intersection of the d hypersurfaces, using saturation. Compute the degree of the residual. 
	       -- Remark: Instead of saturating with the ideal I of the scheme Z, we saturate with a hypersurface containing Z of minimal degree.
	       --         This gives the same result with sufficiently high probability and speeds up calculations considerably.

	       residual := saturate(J,minDegGen);
	       -- Take care of the special case where the residual is the irrelevant ideal when computing the degree
	       degR = append(degR, if residual != ideal vars R then degree residual else 0);
	       ) 
	  );
     
     if (opts.CompMethod == bertini) then (

	  -- write Bertini input file

	  -- configuration 
	  outConfig := "CONFIG \n" | "OUTPUTLEVEL: 0; \n" | "TRACKTYPE: 1; \n" | "USEREGENERATION: 1; \n" | "MAXNORM: 1e8; \n" | "SECURITYMAXNORM: 1e8; \n" |"END; \n \n";
	  outVarGroup := "hom_variable_group ";
	  -- variables
	  variables := flatten entries vars R;
	  for i from 0 to (length(variables)-2) do outVarGroup = outVarGroup | toString(variables_i) | ", ";
	  outVarGroup = outVarGroup | toString(last variables) | "; \n";
	  -- functions
	  outFunctionDecl := "function "; 
	  for i from 0 to (length(f)-2) do outFunctionDecl = outFunctionDecl | "f" | toString(i) | ", ";
	  outFunctionDecl = outFunctionDecl | "f" | toString(length(f)-1) | "; \n \n";
	  outFunctions := "";
	  for i from 0 to (length(f)-1) do outFunctions = outFunctions | "f" | toString(i) | "=" | replace("ii","I",  toString(f_i) ) | "; \n";
	  outInput := "INPUT \n" | outVarGroup | outFunctionDecl |  outFunctions | "END; \n";

     	  
	  out := outConfig | outInput;

	  -- create input file, write it
	  filename := getFilename();

          g := openOut(filename);
	  g << out;
	  close g;


	  -- run Bertini
	  execstr := "cd /tmp ;" | bertini'path | "bertini " | filename | " > " | getFilename();
	  ret := run(execstr);
	  if ret =!= 0 then  error("error occurred while executing external program Bertini. Make sure that Bertini v1.3 or higher is installed and configured.");

	  -- Read output file "regenSummary". Remove the first two lines and the last one. 
	  -- Furthermore remove the lines corresponding to codimensions less than the codimension of the variety,
	  -- these are not relevant. The degrees of the residuals are then the numbers in the 5th column.
	  degR = apply(drop(drop(lines(get "/tmp/regenSummary"),1 + ambientDim-dimension),-1), myString->value( (separate(" ", myString))_5 ) );

	  -- If some the residuals are empty, we have to add zeros manually.

	  for i from 1 to dimension + 1 - #degR do degR = degR | {0};

	 );
     
     degR
     
     );

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists filename  do filename = temporaryFileName();
     rootPath | filename)


-- The function internalChern calls internalSegre to compute the Segre classes of the given subscheme of P^k. From these it computes the
-- Chern-Fulton classes using a simple formula (see for example [1]). The Chern-Fulton classes are identical to the Chern classes if the scheme 
-- is a smooth variety.
-- Input:  I, a homogeneous ideal in a polynomial ring over a field
-- Output: chernList, a list containing the degrees of the Chern classes of Proj(R/I)
--         ambientDim, the dimension k of the ambient space Proj(R)=P^k 
internalChern = {CompMethod => ProjectiveDegree} >> opts -> I -> (
     
     -- Obtain:
     -- the ring R
     -- the dimension of the ambient space and
     -- the dimension n of Z
     R := ring I;
     ambientDim := dim Proj R;
     dimension := dim Proj(R/I) ;
     

     -- take care of the special cases I = (0) and I = (1) 
     if I == ideal(0_R) then (
	  chernList := apply(0..dimension, i-> binomial(dimension+1, i));
	  return (chernList,ambientDim);
	  );
     if I == ideal(1_R) then (
	  chernList = {};
	  return (chernList,ambientDim);
	  ); 

     (segreList,ambientDimDummy) := internalSegre(I, CompMethod => opts.CompMethod); 
     chernList = for i from 0 to dimension list sum( 0..i, p -> binomial( ambientDim + 1, i-p )*segreList_p );
     return  (chernList, ambientDim)
        
     )

-- The function internalCSM computes the Chern-Schwartz-MacPherson class of a projective variety given by
-- an ideal I, using an exclusion-inclusion principle and the function internalCSMhyp, which computes the
-- Chern-Schwartz-MacPherson classes of a hypersurface.
-- Input:  I, a homogeneous ideal in a polynomial ring over a field
-- Output: csmList, a list containing the degrees of the Chern-Schwartz-MacPherson classes of Proj(R/I)
--         ambientDim, the dimension k of the ambient space Proj(R)=P^k 
internalCSM = {CompMethod => ProjectiveDegree} >> opts -> I -> (
     
     -- Compute the dimension of the ambient space 
     -- and the codimension of V(I)
     ambientDim := numgens ring I - 1;
     coDimension := ambientDim - (dim I - 1);
     
     -- obtain ring of ambient space and the dimension of I
     R := ring I;
     dimension := dim Proj(R/I) ;
     
     -- take care of the special cases I = (0) and I = (1) 
     if I == ideal(0_R) then (
	  csmList := apply(0..dimension, i-> binomial(dimension+1, i));
	  return (csmList,ambientDim);
	  );
     if I == ideal(1_R) then (
	  csmList = {};
	  return (csmList,ambientDim);
	  ); 
          
     -- compute the Chern-Schwartz-MacPherson class of V(I) from the Chern-Schwartz-MacPherson classes of
     -- hypersurfaces containing V(I), with the help of exclusion-inclusion
     csmList = toList( ambientDim+1:0 );
     for subset in drop(subsets first entries gens I, 1) do (
	  csmList = csmList + (-1)^(length subset - 1) * (internalCSMhyp( product subset, CompMethod=>opts.CompMethod) );
	  );
     -- remove leading zeros
     csmList = drop(csmList, coDimension);
     return  (csmList, ambientDim)
        
     )
     
     --The main calculation is done here
-- Input:
--    I - homogeneous polynomial ideal  defining a scheme in Proj(R)=P^k    
--    
-- Output:
    -- A sequence of projective degrees (g_0,...,g_k)
internalProjectiveDegree = (I) -> (    
S:=ring I;
m:=numgens I;
kk:=coefficientRing S;
n:=numgens S-1;
gbI:=MyGb(I,"MGB");
dimI := dim ideal leadTerm gbI - 1;
t:=symbol t;
R3:=kk[gens S,t];
J:=substitute(I,R3) ;
njac:=numgens J;
g:=new MutableList from {0..n}; 
g#0=1;
Pol:=0;
d:=first max degrees I;
Xs:=0;
EqT:=0;
Wt:=0;
Wg:=0;
Affx:=0;
tall:=0;
Sgens := (gens R3)_{0..n};
val:=n-dimI;
gbWt:=0;
for k from 1 to n do (
if k<val then (g#k=(d)^k) else (
              Pol=sum ( k,jj-> ideal sum(njac,i->random(kk)*J_i*substitute(random(d-(degree(J_i))_0,S),R3)));
             Xs=sum((n-k),jj->ideal sum(numgens S,i->random(kk)*Sgens_i));
             Affx=ideal( sum(numgens S,i->random(kk)*Sgens_i)-1);
            EqT=ideal( sum((numgens J),i->(1-t*random(kk)*J_i)));
            
             Wt=Pol+Xs+Affx+EqT;
             gbWt = MyGb(Wt,"F4"); 
             tall= numColumns basis(cokernel leadTerm gbWt);
             
              g#k=tall;
              ); 
              );
             
              ProjSeq:= toSequence g;
              return ProjSeq
     )

-- The function internalCSMhyp computes the Chern-Schwartz-MacPherson class of a hypersurface
-- using the algorithm from [4].
-- Input:  p, a homogeneous element of a polynomial ring over a field
-- Output: csmList, a list containing the degrees of the Chern-Schwartz-MacPhersn classes of Proj(R/ideal(p)) 
internalCSMhyp = {CompMethod => ProjectiveDegree} >> opts -> p -> (
     
     -- Compute:
     -- the ideal singP of the singular locus of V(p)
     -- the dimension of the ambient space,
     -- the dimension of the singular locus and
     -- the maximal degree maxDegSingP of its generators
     singP := ideal jacobian ideal p;
     ambientDim := numgens ring singP - 1;
     dimension := dim singP - 1;
     maxDegSingP := first max degrees singP;
     g := {};
     singP=prepare singP;
     -- compute the integers s tilde related to the Segre classes of singP
   
     
      -- if projective degree call projective degree
     if(opts.CompMethod==ProjectiveDegree) then (
     gensI := flatten entries sort gens singP; 
     minDegGen := first gensI;
     gs:=internalProjectiveDegree(singP);
     g=toList gs;
     )
     --if residual Jost do this
     else(
     (s, ambientDimDummy) := internalSegre(singP, CompMethod => opts.CompMethod);
     stilde := {-1} | toList( (ambientDim - dimension - 1):0 ) | s;
      
     for i from 0 to ambientDim do 
          g = g | {- stilde#i - sum(0..(i-1), j -> binomial(i,j) * (-maxDegSingP)^(i-j) * g_j) };
    
     );
     -- compute the shadow of the graph of singP
    
     -- compute the Chern-Schwartz-MacPherson classes of V(p) from the shadow of the graph of singP
     for i from 0 to ambientDim list 
          binomial(ambientDim+1, i) - sum(0..i, j-> (-1)^j * g#j * binomial(ambientDim-j, i-j))
     )


-- The function checkUserInput checks that the given ideal I is a homogeneous ideal in a polynomial ring over a field, with a suitable coefficient field.
checkUserInput = (I,CompMethod) -> (
     
        
     -- Is the ring a polynomial ring?
     if not isPolynomialRing ring I then error "the ideal needs to be defined over a polynomial ring.";
     
     -- Is the ideal homogeneous?
     if not isHomogeneous I then error "the ideal has to be homogeneous.";
     
     -- Is the coefficient ring a field (to make dimension command work)?
     if not isField coefficientRing ring I then error "the coefficient ring needs to be a field.";
     
     -- The saturation part of the ResidualSymbolic version will not work with real or complex coefficients.
     if  (CompMethod == PnResidual or CompMethod ==ProjectiveDegree)and any( {ComplexField,RealField}, myField -> instance( coefficientRing ring I, myField ) ) then error "the Symbolic algorithms (ResidualSymbolic and ProjectiveDegree) do not work with real or complex coefficients.";
  
     -- The numeric version only works with rational, real or complex coefficients.
     if  CompMethod == bertini and not( coefficientRing ring I === QQ or any( {ComplexField,RealField}, myField -> instance( coefficientRing ring I, myField ) ) )  then  error "the numeric algorithm only works with rational or complex coefficients."; 
      
     )


-- The function prepare does two things to prepare the later computations. At first, it trims the ideal I, taking away
-- nonnecessary generators. Then it creates a ring only used internally and an ideal in it isomorphic to I and returns this ideal. This 
-- step is done to avoid possible later conflicts in the choice of variables.
prepare = I -> (

     --trim I
     localI := trim I;     
     
     -- rename variables
     numGen := numgens ring localI;
     coeffRing := coefficientRing ring localI;
     z := symbol z;
     internalR := coeffRing[z_1 .. z_numGen];
     renamingMap := map(internalR, ring localI, {z_1 .. z_numGen});
     return renamingMap localI;
     )

-- The function output turns a list of degrees of characteristic classes into a polynomial in the Chow ring of the ambient space P^k.
-- This ring is generated by the hyperplane class.
-- Input:  segreList, a list {deg s_0, ..., deg s_n} of integers
--         ambientDim, the dimension k of ambient space P^k
--         hyperplaneClass, the symbol for the hyperplane class
-- Output: the polynomial (deg s_0)*hyperplaneClass^ambientDim + ... + (deg s_n)*hyperplaneClass^(ambientDim - n)
output = (segreList,ambientDim,hyperplaneClass) -> (
     -- produce the Chow ring ZZ[hyperplaneClass]/(hyperplaneClass^ambientDim+1)
     tempRing := ZZ[hyperplaneClass];
     outputRing := tempRing / ideal((tempRing_0)^(ambientDim+1));
     -- obtain the dimension n
     dimension := #segreList-1;
     -- create the polynomial (deg s_0)*hyperplaneClass^ambientDim + ... + (deg s_n)*hyperplaneClass^(ambientDim - n)
     return  sum(0..dimension, i -> segreList_i * (outputRing_0)^(ambientDim - dimension + i))
     )

-*
MyGb is a wrapper function for the M2 groebner basis command which 
uses the fastest available GB algorithm depending on the users system and on the field over which they are working
*-

MyGb =(I,stdgy)->(
    gbI:=0;
    try (gbI = groebnerBasis(I, Strategy=>stdgy)) else ( gbI= groebnerBasis(I));
    return gbI;
    )

----------------------------------------------
-- Documentation
---------------------------------------------

beginDocumentation()

doc ///
     Key
     	  CharacteristicClasses
     Headline
     	  Chern classes and other characteristic classes of subschemes of certain smooth toric varieties, including products of projective spaces
     Description
     	  Text
	       The package CharacteristicClasses provides commands to compute the Chern class, Chern-Schwartz-MacPherson class Segre class and 
               Euler characteristic of closed subschemes of certain smooth complete varieties, including products of projective 
               spaces \PP^{n_1} x ... x \PP^{n_m}. In particular the methods of this package are applicable for toric varieties for which all Cartier divisors 
               are numerically effective (nef), see [7] for details. For simplicity a method (@TO CheckToricVarietyValid@) is provided which allows the user
               to determine if these methods can be applied to a given object of class NormalToricVariety. Note that to perform computations involving toric 
               varieties it is required that the package NormalToricVarieties is also loaded. 

               More precisely the CharacteristicClasses package computes the pushforward of the respective classes to the Chow ring of either a product 
               of projective space or of the appropriate toric variety. In the case where the input is an ideal I (in the appropriate graded coordinate ring)
               defining a subscheme V of \PP^{n_1} x ... x \PP^{n_m} the characteristic class is returned as an element of the Chow ring 
               A^*(\PP^{n_1} x ... x \PP^{n_m})=\ZZ[h_1,...,h_m]/(h_1^{n_1+1},...,h_m^{n_m+1}); here h_i represents (the pullback of) the rational equivalence 
               class of a hyperplane in \PP^i. In the case where V is a subscheme of a smooth toric variety X_{\Sigma} with total coordinate ring (that is Cox ring) R
               the characteristic classes will be represented as elements of the Chow ring of X, A^*(X_{\Sigma})=R/(I+J) where I is the Stanley-Reisner Ideal of the 
               fan \Sigma  and J is the ideal defined by linear relations among the rays. See Theorem 12.5.3 of "Toric varieties" by Cox, Little and Schenck.

               If V is smooth, then by definition the (total) Chern classes of V is the Chern classes of the tangent bundle T_V, that is c(V)=c(T_V)\cap [V]. 
               The Chern classes are cycles in the Chow ring of V, i.e., linear combinations of subvarieties of V modulo rational equivalence. 
               
               In practice all cycle classes will be represented in terms of integers multiplied by hyperplane classes. Consider, for example, a hypersurface
               V=V(f) in \PP^{n_1} x ... x \PP^{n_m} where f has multi-degree (d_1,...,d_m), then [V]=d_1h_1+...+d_mh_m. This extends linearly to linear 
               combinations of cycles. Computing the Chern class of V is equivalent to computing the pushforward of the Chern classes to 
               the Chow ring of the ambient space. Also by definition, the Segre classes of V a subscheme of X are the Segre classes 
	       of V in X, that is the Segre classes of the normal cone to V in X, C_VX. For definitions of the concepts used so far see, for example, 
               "Intersection Theory" by W. Fulton. Chern-Schwartz-MacPherson (CSM) classes are a generalization of Chern classes of smooth schemes to possibly 
               singular schemes with nice functorial properties including the a relation to the Euler characteristic.
	       
	       The functions computing characteristic classes in this package can have several different types of output, with the default form being objects of type QuotientRingElelement, that is elements in the appropriate Chow ring. See the function documentation for more details.  
	       
	       This implementation offers several different algorithms to compute characteristic classes. For the general case of subschemes of smooth 
               toric varieties or the case of products of projective spaces \PP^{n_1} x ... x \PP^{n_m} (with m>1) the computational method used is 
               CompMethod=>ProjectiveDegree. These methods, in the toric case, are described in [7]. In the case of projective space see also [5].
               The main computational step of this approach is the computation of the projective degrees. This can be done symbolically, using 
               Gr&ouml;bner bases, or numerically using a package such as Bertini, however only the symbolic implementation is offered at present.

               To compute the CSM class the default method is inclusion-exclusion, which uses the inclusion-exclusion property of CSM classes to compute the CSM
               class for codimension greater than one (this is the option Method=>InclusionExclusion). When V is a complete intersection subscheme of an applicable
               toric variety then CSM(V) may also be computed Method=>DirectCompleteInt may also be used; this method is described in [6] and [7] and may offer a
               performance improvement in some applicable cases, particularly in projective space. 

               In the special case where the ambient space is \PP^n the computational methods CompMethod=>PnResidual and CompMethod=>bertini may be used. These 
               methods are described in [1, 2, 8]. The main step in this approach is the computation of the residuals. This can be done 
               symbolically, using Gr&ouml;bner bases, and numerically, using the regenerative cascade implemented in Bertini. The regenerative
	       cascade is described in [3].   
	       
	       All algorithms are probabilistic but will succeed with high probability. In the case of the symbolic implementation of the ProjecvtiveDegree method 
               practical experience and algorithm testing indicate that a finite field with over 25000 elements is more than sufficient, i.e.
               using the finite field kk=ZZ/25073 the experiential chance of failure with the ProjectiveDegree algorithm on a variety of examples
               was less than 1/2000. Using kk=ZZ/32749 resulted in no failures in over 10000 attempts of several different examples. 
               Read more under @TO "probabilistic algorithm"@.
	       
               References: \break
               [1] David Eklund, Christine Jost, Chris Peterson. A method to compute Segre classes, Journal of Algebra and Its Applications 12(2), 2013 \break
               [2] Daniel J. Bates, Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler. Bertini: Software for Numerical Algebraic Geometry, available at http://www.nd.edu/~sommese/bertini \break
	       [3] Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler. Regenerative cascade homotopies for solving polynomial systems, Applied Mathematics and Computation 218(4), 2011 \break
	       [4] Christine Jost. An algorithm for computing the topological Euler characteristic of complex projective varieties, submitted, arXiv:1301.4128 [math.AG] \break
	       [5] Martin Helmer. Algorithms to compute the topological Euler characteristic, Chern-Schwartz-Macpherson class and Segre class of projective varieties. Journal of Symbolic Computation, 2015. Preprint on arXiv at arXiv:1402.2930. \break
	       [6] Martin Helmer. A Direct Algorithm to Compute the Topological Euler Characteristic and Chern-Schwartz-MacPherson Class of Projective Complete Intersection Varieties. (2014). arXiv preprint arXiv:1410.4113. \break
	       [7] Martin Helmer. An Algorithm to Compute the Topological Euler Characteristic, the Chern-Schwartz-MacPherson Class and the Segre class of Subschemes of Some Smooth Complete Toric Varieties. (2015). arXiv preprint arXiv:1508.03785 \break
	       [8]Sandra Di Rocco, David Eklund, Chris Peterson, and Andrew J. Sommese. Chern numbers of smooth varieties via homotopy continuation and intersection theory. Journal of symbolic computation 46, no. 1 (2011): 23-33.
///


doc ///
     Key
     	  Segre
	  [Segre, Output, CompMethod]
	  (Segre,Ideal)
	  (Segre,QuotientRing, Ideal)
	  (Segre, NormalToricVariety,Ideal)
	  (Segre, QuotientRing, NormalToricVariety,Ideal)
	  (Segre, Ideal, Symbol)	  
     Headline
     	  The Segre class of a subscheme
     Usage
     	  Segre I
	  Segre(A,I)
	  Segre(X,J)
	  Segre(Ch,X,J)
     Inputs
     	  I:Ideal
	    a multi-homogeneous ideal in a graded polynomial ring over a field defining a closed subscheme V of \PP^{n_1}x...x\PP^{n_m}
	  A:QuotientRing
	    A=\ZZ[h_1,...,h_m]/(h_1^{n_1+1},...,h_m^{n_m+1}) quotient ring representing the Chow ring of \PP^{n_1}x...x\PP^{n_m}, this ring should be
	    built using the @TO ChowRing@ command		
          J:Ideal
	    in the graded polynomial ring which is coordinate ring of the Normal Toric Variety X
          X:NormalToricVariety
	    which is the ambient space which contains V(J)
	  Ch:QuotientRing
	    the Chow ring of the toric variety X, Ch=(ring J)/(SR+LR) where SR is the Stanley-Reisner ideal of the fan defining X and LR is the linear relations
	    ideal, this ring should be built using the @TO ToricChowRing@ command
	  CompMethod=>"ProjectiveDegree"
	    this algorithm may be used for subschemes of any applicable toric variety (this may be checked using the @TO CheckToricVarietyValid@ command)
	  CompMethod=>"PnResidual"
	    this algorithm may be used for subschemes of \PP^n only  
	  Output=>"ChowRingElement"
	    returns a RingElement in the Chow ring of the appropriate ambient space 
	  Output=>"HashForm"
	    HashForm returns a MutableHashTable containing the following keys: "G" (the polynomial with coefficients of the hyperplane classes representing the projective degrees), "Glist" (the list form of "G") , "Segre" (the total Segre class of the input),"SegreList" (the list form of "Segre")       	 
     Outputs
     	  :RingElement
	   the pushforward of the total Segre class of the scheme V defined by the input ideal to the appropriate Chow ring
     Description
     	  Text
	       For a subscheme V of an applicable toric variety X this command computes the push-forward of the total Segre class s(V,X) of V in X to the Chow ring of X.
	  Example
	       setRandomSeed 72;
	       R = ZZ/32749[w,y,z]
	       Segre(ideal(w*y),CompMethod=>PnResidual)
	       A=ChowRing(R)
	       Segre(A,ideal(w^2*y,w*y^2))	  
	  Text
     	       Now consider an example in \PP^2 \times \PP^2, if we input the Chow ring A the output will be returned in the same ring. To ensure proper function of the methods we build the Chow ring using the @TO ChowRing@ command. We may also return a MutableHashTable.
	  Example
	       R=MultiProjCoordRing({2,2})
	       r=gens R
	       A=ChowRing(R)
	       I=ideal(r_0^2*r_3-r_4*r_1*r_2,r_2^2*r_5)
	       Segre I
	       s1=Segre(A,I)
	       SegHash=Segre(A,I,Output=>HashForm)
	       peek SegHash
	       s1==SegHash#"Segre"
	  Text 
	       In the case where the ambient space is a toric variety which is not a product of projective spaces we must load the NormalToricVarieties package and must also input the toric variety. If the toric variety is a product of projective space it is recommended to use the form above rather than inputting the toric variety for efficiency reasons. 
	  Example
	       needsPackage "NormalToricVarieties"
	       Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
               Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	       X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	       CheckToricVarietyValid(X)
	       R=ring(X)
               I=ideal(R_0^4*R_1,R_0*R_3*R_4*R_2-R_2^2*R_0^2)
	       Segre(X,I)
	       Ch=ToricChowRing(X)
	       s3=Segre(Ch,X,I)
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO CompMethod@ to bertini will do the main computations numerically, provided
	       Bertini is  @TO2 {"configuring Bertini", "installed and configured"}@. Note that the bertini option
	       is only available for subschemes of \PP^n.
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///
     


doc ///
     Key
     	  Chern
	  [Chern, Output, CompMethod]
	  (Chern,Ideal)
	  (Chern,QuotientRing, Ideal)
	  (Chern, NormalToricVariety,Ideal)
	  (Chern, QuotientRing, NormalToricVariety,Ideal)
	  (Chern, Ideal, Symbol)	  
     Headline
     	  The Chern class
     Usage
     	  Chern I
	  Chern(A,I)
	  Chern(X,J)
	  Chern(Ch,X,J)
     Inputs
     	  I:Ideal
	    a multi-homogeneous ideal in a graded polynomial ring over a field defining a closed subscheme V of \PP^{n_1}x...x\PP^{n_m}
	  A:QuotientRing
	    A=\ZZ[h_1,...,h_m]/(h_1^{n_1+1},...,h_m^{n_m+1}) quotient ring representing the Chow ring of \PP^{n_1}x...x\PP^{n_m}, this ring should be
	    built using the @TO ChowRing@ command		
          J:Ideal
	    in the graded polynomial ring which is coordinate ring of the Normal Toric Variety X
          X:NormalToricVariety
	    which is the ambient space that we are working in
	  Ch:QuotientRing
	    the Chow ring of the toric variety X, Ch=(ring J)/(SR+LR) where SR is the Stanley-Reisner ideal of the fan defining X and LR is the linear relations
	    ideal, this ring should be built using the @TO ToricChowRing@ command
	  CompMethod=>"ProjectiveDegree"
	    this algorithm may be used for subschemes of any applicable toric variety (this may be checked using the @TO CheckToricVarietyValid@ command)
	  CompMethod=>"PnResidual"
	    this algorithm may be used for subschemes of \PP^n only, see @TO CompMethod@  
	  Output=>"ChowRingElement"
	    returns a RingElement in the Chow ring of the appropriate ambient space 
	  Output=>"HashForm"
	    returns a MutableHashTable containing the following keys: "Chern" (the Total Chern class),"G" (the polynomial with coefficients of the hyperplane classes representing the projective degrees), "Glist" (the list form of "G") , "Segre" (the total Segre class of the input),"SegreList" (the list form of "Segre") ,  "CF" (the total Chern-Fulton class)    
	 
     Outputs
     	  :RingElement
	   the pushforward of the total Chern class of the input to the Chow ring of the appropriate ambient space

Description
     	  Text
	       For a non-singular n-dimensional subscheme V of an applicable toric variety X, this command computes the push-forward of the total Chern class of V to the Chow ring of X. The output is an element of the Chow ring of X, that is a polynomial in the hyperplane classes h_1,...,h_m if X=\PP^{n_1}\times... \times \PP^{n_m} is a product of projective space. Otherwise it is a polynomial in R/(SR+LR) where R is the Cox ring, SR the Stanely-Reilser Ideal and LR the ideal generated by linear relations among the rays.
	  Example
	       setRandomSeed 438;
	       R = QQ[v,y,z,w]
	       B = matrix{{v,y,z},{y,z,w}}
	       Chern minors(2,B)
	       Chern(minors(2,B),CompMethod=>PnResidual)  	  
	  Text
	       The 2x2-minors of the matrix A form the ideal of the twisted cubic. It is well-known that its degree is 3 and its genus is 0. The calculations confirm that deg c_1 = 2-2g = 2 and deg  c_0 = 3. 
	       It is also possible to input the Chow ring of \PP^3 so that the answers are returned in this ring allowing us to check equality:
	  Example
	       A=ChowRing(R)
	       c1=Chern(A, minors(2,B)) 
	       cHash=Chern(A,minors(2,B),Output=>HashForm)
	       peek cHash
	       c1==cHash#"Chern"
	  Text 
	       Now consider an example in \PP^2 \times \PP^2, if we input the Chow ring A the output will be returned in the same ring. We may also return a MutableHashTable.
	  Example
	       R=MultiProjCoordRing({2,2})
	       r=gens R
	       I=ideal(r_0^2*r_3-r_4*r_1*r_2,r_2^2*r_5)
	       Chern I
	  Text 
	       In the case where the ambient space is a toric variety which is not a product of projective spaces we must load the NormalToricVarieties package and must also input the toric variety. If the toric variety is a product of projective space it is recommended to use the form above rather than inputting the toric variety for efficiency reasons. Below we verify that the Chern-Fulton class equals the Chern class for this smooth subvariety. 
	  Example
	       needsPackage "NormalToricVarieties"
	       Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
               Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	       X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	       CheckToricVarietyValid(X)
	       R=ring(X)
               I=ideal(R_0^4*R_1,R_0*R_3*R_4*R_2-R_2^2*R_0^2)
	       Chern(X,I)
	       Ch=ToricChowRing(X)
	       s3=Chern(Ch,X,I) 
	       s3==(product(gens(Ch),a->a+1)*Segre(Ch,X,I))          
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO CompMethod@ to bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@. Note that the bertini and PnResidual options may
	       only be used for subschemes of \PP^n. 
	       
	       The command Chern actually computes the push-forward of the total @EM {"Chern-Fulton class"}@ of the subscheme V of an applicable toric variety X. The Chern-Fulton class is one of several generalizations of Chern classes to possibly singular subschemes. It is defined as c_{CF}(V) = c(T_{X}|_V) \cap s(V,X). For non-singular schemes, the Chern-Fulton class coincides with the Chern class of the tangent bundle. So for non-singular input, the command will compute just the usual Chern class.
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///

doc ///
     Key
         ChowRing
	 (ChowRing,Ring)
	 (ChowRing,Ring,Symbol)
     Headline
         Computes the Chow ring of a product of projective spaces m projective spaces given the coordinate ring 
     Usage 
         ChowRing R
     Inputs
         R:Ring
	   the graded coordinate ring of the product of projective spaces \PP^{n_1}\times \cdots \times \PP^{n_m} 
     Outputs
         :QuotientRing
	   the Chow ring A=\ZZ[h_1,...,h_m]/(h_1^{n_1+1},...,h_m^{n_m+1}) of a product of projective spaces \PP^{n_1}\times \cdots \times\PP^{n_m}
Description
     Text
         This method computes the Chow ring A=\ZZ[h_1,...,h_m]/(h_1^{n_1+1},...,h_m^{n_m+1}) of a product of projective spaces \PP^{n_1}\times \cdots \times\PP^{n_m}. It is needed for input into the methods @TO Segre@, @TO Chern@ and @TO CSM@ to ensure that these methods return results in the same ring. We give an example of the use of this method to work with elements of the Chow ring of \PP^3x\PP^4.
     Example
         R=MultiProjCoordRing({3,4})
	 A=ChowRing(R)
	 I=ideal(random({1,0},R));
	 K=ideal(random({1,1},R));
	 c=Chern(A,I)
	 s=Segre(A,K)
	 s-c
	 s*c
     Text
         We may also specify the variable to be used for the Chow ring.
     Example
         A2=ChowRing(R,symbol v)
	 describe A2	 	 
///     
     
doc ///
     Key
         ClassInChowRing
	 (ClassInChowRing,QuotientRing, RingElement)
     Headline
         Gives the class of a hypersurface in the associated Chow ring of a product of projective spaces
     Usage 
         ClassInToricChowRing(A,f)
     Inputs
         A:QuotientRing
	   the Chow ring of \PP^{n_1} x...x\PP^{n_1} 
	 f:RingElement
	   an element of the coordinate ring of \PP^{n_1} x...x\PP^{n_1}
     Outputs
         :RingElement
	   the class of V(f), [V(f)] in the Chow ring of \PP^{n_1} x...x\PP^{n_1}
Description
     Text
         This method finds the class [V(f)] of the hypersurface V(f) where f is a polynomial in the graded coordinate ring of \PP^{n_1} x...x\PP^{n_1}. The class [V(f)] is an element of the Chow ring of  \PP^{n_1} x...x\PP^{n_1}. Consider \PP^3x\PP^4x\PP^1
     Example
	 R=MultiProjCoordRing({3,4,1})
	 A=ChowRing(R)
	 f=random({1,1,1},R);
	 ClassInChowRing(A,f)
///

doc ///
     Key
         ToricChowRing
	 (ToricChowRing,NormalToricVariety)
     Headline
         Computes the Chow ring of a normal toric variety
     Usage 
         ToricChowRing X
     Inputs
         R:NormalToricVariety
	   A normal toric variety
     Outputs
         :QuotientRing
Description
     Text
         Let X be a toric variety with total coordinate ring (Cox ring) R. This method computes the Chow ring  Chow ring Ch=R/(SR+LR) of X; here SR is the Stanley-Reisner ideal of the corresponding fan and LR is the ideal of linear relations amount the rays. It is needed for input into the methods @TO Segre@, @TO Chern@ and @TO CSM@ in the cases where a toric variety is also input to ensure that these methods return results in the same ring. We give an example of the use of this method to work with elements of the Chow ring of a toric variety
     Example
         needsPackage "NormalToricVarieties"
	 Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
         Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	 X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	 R=ring X
	 Ch=ToricChowRing(X)
	 describe Ch
	 r=gens R
	 I=ideal(random({1,0},R))
	 K=ideal(random({1,1},R))
	 c=Chern(Ch,X,I)
	 s=Segre(Ch,X,K)
	 s-c
	 s*c
/// 
doc ///
     Key
         ClassInToricChowRing
	 (ClassInToricChowRing,QuotientRing, RingElement)
     Headline
         Gives the class of a hypersurface in the associated Chow ring of a toric variety
     Usage 
         ClassInToricChowRing(Ch,f)
     Inputs
         Ch:QuotientRing
	   the Chow ring of a normal toric variety X
	 f:RingElement
	   an element of the coordinate ring of the toric variety X
     Outputs
         :RingElement
	   the class of V(f), [V(f)] in the Chow ring of X
Description
     Text
         This method finds the class [V(f)] of the hypersurface V(f) where f is a polynomial in the graded coordinate ring of a toric variety X. The class [V(f)] is an element of the Chow ring of X.
     Example
         needsPackage "NormalToricVarieties"
	 Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
         Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	 X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	 R=ring X
	 Ch=ToricChowRing(X)
	 f=random({1,0},R)
	 ClassInToricChowRing(Ch,f)
///

doc ///
     Key
         isMultiHomogeneous
	 (isMultiHomogeneous,Ideal)
	 (isMultiHomogeneous,RingElement)
     Headline
         Checks if an ideal is homogeneous with respect to the grading on its ring (i.e. multi-homogeneous in the multi-graded case)
     Usage
         isMultiHomogeneous I
	 isMultiHomogeneous f
     Inputs
         I:Ideal
	   an ideal in a graded or multi-graded ring
	 f:RingElement
	   a element in a graded or multi-graded ring
     Outputs
         :Boolean
Description 
     Text
        Tests if the input Ideal or RingElement is Homogeneous with respect to the grading on the ring. Homogeneous input is required for all methods to compute characteristic classes.
     Text
        This method works for ideals in the graded coordinate rings of toric varieties, and hence for products of projective spaces. These can be created directly, or using methods the @TO MultiProjCoordRing@ method of this package, or with methods from the NormalToricVarieties Package.
     Example
         R=MultiProjCoordRing({1,2,1})
	 x=gens(R)
	 I=ideal(x_0^2*x_3-x_1*x_0*x_4,x_6^3)
	 isMultiHomogeneous I
	 isMultiHomogeneous ideal(x_0*x_3-x_1*x_0*x_4,x_6^3)
     Text 
         Note that for an ideal to be multi-homogeneous the degree vector of all monomials in a given generator must be the same.	 
///

doc ///
     Key
         MultiProjCoordRing
	 (MultiProjCoordRing,List)
	 (MultiProjCoordRing,Ring,List)
	 (MultiProjCoordRing,Symbol,List)
	 (MultiProjCoordRing,Ring,Symbol,List)
     Headline
         A quick way to build the coordinate ring of a product of projective spaces
     Usage 
         MultiProjCoordRing Dims
	 MultiProjCoordRing (CoeffRing,Dims)
	 MultiProjCoordRing (var,Dims)
	 MultiProjCoordRing (CoeffRing,var,Dims)
     Inputs
         Dims:List
	   representing the dimensions of the projective spaces, i.e. {n_1,...,n_m} corresponds to \PP^{n_1} x.... x \PP^{n_m}
         CoeffRing:Ring
	   the coefficient ring of the graded polynomial ring to be built by the method, by default this is \ZZ/32749
	 var:Symbol
	   to be used for the intermediates of the graded polynomial ring to be built by the method  
     Outputs
         :Ring
	   the graded coordinate ring of the \PP^{n_1} x.... x \PP^{n_m} where {n_1,...,n_m} is the input list of dimensions
Description
     Text
         Computes the graded coordinate ring of the \PP^{n_1} x.... x \PP^{n_m} where {n_1,...,n_m} is the input list of dimensions. This method is used to quickly build the coordinate ring of a product of projective spaces for use in computations.
     Example
         S=MultiProjCoordRing(QQ,symbol z,{1,3,3})
	 degrees S
         R=MultiProjCoordRing {2,3}
	 coefficientRing R
         describe R
	 A=ChowRing R
	 describe A
         Segre(A,ideal random({1,1},R))
///

doc ///
     Key
         CheckToricVarietyValid
	 (CheckToricVarietyValid,NormalToricVariety)
     Headline
         Checks if the input normal toric variety X is a valid choice for an ambient space when computing characteristic classes of subschemes V of X
     Usage 
         CheckToricVarietyValid X
     Inputs
         X:NormalToricVariety
       	   a normal toric variety which is a candidate for an ambient space in which to perform characteristic class computations
     Outputs
         :Boolean
Description
     Text
         Note that if you are working with subvarieties of some product of projective spaces \PP^{n_1}\times \cdots \times \PP^{n_m} then the ambient space is a valid choice for use with the ChacteristicsClasses package and there is no need to load the NormalToricVarieties Package or to check validity. For other cases the CheckToricVarietyValid method returns true if the input toric variety X may be used as an ambient space for other characteristic class computations, i.e. if this method returns true we may use methods such as CSM(X,I), Chern(X,I) and Segre(X,I) for I an ideal in the coordinate ring of X. We will see an example of a valid toric variety which is not a product of projective spaces and a smooth toric variety which is not valid.
     Example
         needsPackage "NormalToricVarieties"
	 Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
         Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	 X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	 CheckToricVarietyValid(X)
	 R=ring(X)
         I=ideal(R_0^4*R_1,R_0*R_3*R_4*R_2-R_2^2*R_0^2)
	 Segre(X,I)
	 W = smoothFanoToricVariety(4,123)
	 CheckToricVarietyValid(W)
     Text
         Even if we can not perform computations on subschemes we may still compute the CSM class of the toric variety itself using the @TO CSM@ command.	 
     Example
         Ch=ToricChowRing W
         CSM W    
         CSM(Ch,W)
///

doc ///
     Key
     	  CSM
	  [CSM, Output, CompMethod,Method,CheckSmooth,IndsOfSmooth,InputIsSmooth]
	  (CSM,Ideal)
	  (CSM,QuotientRing, Ideal)
	  (CSM,QuotientRing, Ideal,MutableHashTable)
	  (CSM, NormalToricVariety,Ideal)
	  (CSM, QuotientRing, NormalToricVariety,Ideal)
	  (CSM, QuotientRing, NormalToricVariety,Ideal,MutableHashTable)
	  (CSM, Ideal, Symbol)
	  (CSM,NormalToricVariety)
	  (CSM,QuotientRing,NormalToricVariety)
	  [CSM, CompMethod]
	  [CSM, Method]
	  [CSM, CheckSmooth]
	  [CSM, InputIsSmooth]
	  [CSM, IndsOfSmooth]
     Headline
     	  The Chern-Schwartz-MacPherson class
     Usage
     	  CSM I
	  CSM(A,I)
	  CSM(A,I,M)
	  CSM(X,J)
	  CSM(Ch,X,J)
	  CSM(Ch,X,J,M)
	  CSM X
	  CSM(Ch,X)
	  CSM(I,h)
     Inputs
     	  I:Ideal
	    a multi-homogeneous ideal in a graded polynomial ring over a field defining a closed subscheme V of \PP^{n_1}x...x\PP^{n_m}
	  A:QuotientRing
	    A=\ZZ[h_1,...,h_m]/(h_1^{n_1+1},...,h_m^{n_m+1}) quotient ring representing the Chow ring of \PP^{n_1}x...x\PP^{n_m}, this ring should be
	    built using the @TO ChowRing@ command		
          J:Ideal
	    an ideal in the graded polynomial ring which is coordinate ring of the Normal Toric Variety X
          X:NormalToricVariety
	    which is the ambient space containing V(J)
	  Ch:QuotientRing
	    the Chow ring of the toric variety X, Ch=(ring J)/(SR+LR) where SR is the Stanley-Reisner ideal of the fan defining X and LR is the linear relations
	    ideal, this ring should be built using the @TO ToricChowRing@ command
	  h:Symbol
	    to be used as the intermediate for the Chow ring (this may only be used for subschemes of \PP^{n_1}x...x\PP^{n_m})
	  M:MutableHashTable
	    containing known CSM classes of hypersurfaces appearing in the inclusion-exclusion procedure
	  CompMethod => "ProjectiveDegree"
	    this is the default algorithm used for the main computational steps in the computation
	  CompMethod => "PnResidual"
	    this algorithm may be used for subschemes of \PP^n only  
	  Method => "InclusionExclusion"
	    this is the default method and is applicable for all inputs
	  Method => "DirectCompleteInt"
	    this method may provide a performance improvement when the input is a complete intersection, if the input is not a complete intersection a warning will be given and the InclusionExclusion option will be used instead
	  CheckSmooth => 
	    this option is only used when computing the CSM class of a input toric variety X (not of a subscheme), if true it checks if the toric variety is smooth before computing its CSM class, this will lead to faster computation in the smooth cases
	  InputIsSmooth=>
	    this option has values true/false and tells the method whether to assume the input ideal defines a smooth scheme, and hence to call the method Chern instead for reduced run time, alternatively the Chern function can be used directly
	  Output=>"ChowRingElement"
	    the type of output to return, "ChowRingElement" is default and returns a RingElement in the Chow ring of the appropriate ambient space 
	  Output=>"HashForm"
	    the type of output to return, HashForm returns a MutableHashTable containing the key "CSM" (the CSM class), and keys of the form \{0\},\{1\},\{2\},...,\{0,1\},\{0,2\} ....\{0,1,2\}... and so on which correspond to the indices of the possible subsets of the generators of the input ideal, for each set of indices the CSM class of the hypersurface given by the product of all polynomails in the corresponding set of generators is stored, there is no extra cost to using this option
          IndsOfSmooth=>
	    this option may speed up the run time when using the DirectCompleteInt Method if the user knows additional information about the input ideal, see @TO IndsOfSmooth@ 
     Outputs
     	  :RingElement
	   the pushforward of the CSM class to the Chow ring of the appropriate ambient space
Description
     	  Text
	       For a non-singular n-dimensional subscheme V of an applicable toric variety X, this command computes the push-forward of the total Chern class of V to the Chow ring of X. The output is an element of the Chow ring of X, that is a polynomial in the hyperplane classes h_1,...,h_m if X=\PP^{n_1}\times\PP^{n_m} is a product of projective space. Otherwise it is a polynomial in R/(SR+LR) where R is the Cox ring, SR the Stanely-Reilser Ideal and LR the ideal generated by linear relations among the rays.
	  Example
	       kk=ZZ/32749;
	       R=kk[x_0..x_4]
	       I=ideal(random(1,R),random(2,R),x_0*x_2-x_3*x_0);
               CSM(ideal I_0,CompMethod=>PnResidual)
	       csmI=CSM(I)
	       A=ring csmI
	       csmIHash=CSM(A,I,Output=>HashForm);
	       csmIHash#{0,1}==CSM(A,ideal(I_0*I_1))
	  Text 
	       Note that the ideal above is a complete intersection, thus we may change the method option which may speed computation in some cases. We may also note that the ideal generated by the first 2 generators of I defines a smooth scheme and input this information into the method.
	  Example
	       csmI==CSM(A,I,Method=>DirectCompleteInt)
	       CSM(A,I,Method=>DirectCompleteInt,IndsOfSmooth=>{0,1})
	  Text 
	       Now consider an example in \PP^2 \times \PP^2, if we input the Chow ring A the output will be returned in the same ring. We may also return a MutableHashTable.
	  Example
	       R=MultiProjCoordRing({2,2})
	       A=ChowRing(R)
	       r=gens R
	       K=ideal(r_0^2*r_3-r_4*r_1*r_2,r_2^2*r_5)
	       time csmK=CSM(A,K)
	       csmKHash= CSM(A,K,Output=>HashForm)
	       csmK==csmKHash#"CSM"
	       CSM(A,ideal(K_0))==csmKHash#{0}	    
	  Text
	       Suppose we have already computed some of CSM classes of hypersurfaces involved in the inclusion-exclusion procedure, then we may input these to be used by the CSM function. In the example below we input the CSM class of V(K_0) (that is of the hypersurface defined by the first polynomial generating K) and the CSM class of the hypersurface defined by the product of the generators of K.
	  Example
	       m=new MutableHashTable;
	       m#{0}=csmKHash#{0}
	       m#{0,1}=csmKHash#{0,1}
	       time CSM(A,K,m)
	  Text 
	       In the case where the ambient space is a toric variety which is not a product of projective spaces we must load the NormalToricVarieties package and must also input the toric variety. If the toric variety is a product of projective space it is recommend to use the form above rather than inputting the toric variety for efficiency reasons. 
	  Example
	       needsPackage "NormalToricVarieties"
	       Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
               Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	       X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	       csmX=CSM X
	       Ch=ring csmX
	       CheckToricVarietyValid(X)
	       R=ring(X)
               I=ideal(R_0^4*R_1,R_0*R_3*R_4*R_2-R_2^2*R_0^2)
	       CSM(X,I)
	       CSM(Ch,X,I)
          Text
               This function may also compute the CSM class of a normal toric variety defined by a fan. In this case a combinatorial method is used. This method is accessed with the usual CSM command with either only a toric variety or a toric variety and a Chow ring as input. In this case we only require that the input toric variety is complete and simplicial (in particular we do not need it to be smooth).
          Example
               needsPackage "NormalToricVarieties"
               U = hirzebruchSurface 7
               Ch=ToricChowRing(U)
               CSM U
               csm1=CSM(Ch,U)                 
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO CompMethod@ to bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@. Note that the bertini and PnResidual options may
	       only be used for subschemes of \PP^n. 
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
   
///

doc ///
     Key
     	  Euler
	  [Euler, Output, CompMethod,Method,CheckSmooth,IndsOfSmooth,InputIsSmooth]
	  (Euler,Ideal)
	  (Euler, NormalToricVariety,Ideal)
	  (Euler, RingElement)	  
     Headline
     	  The Euler Characteristic 
     Usage
     	  Euler I
	  Euler(X,J)
	  Euler csm
     Inputs
     	  I:Ideal
	    a multi-homogeneous ideal in a graded polynomial ring over a field defining a closed subscheme V of \PP^{n_1}x...x\PP^{n_m}	
          J:Ideal
	    an ideal in the graded polynomial ring which is coordinate ring of the Normal Toric Variety X
          X:NormalToricVariety
	    a normal toric variety which is the ambient space that we are working in
	  csm:RingElement
	    the CSM class of some variety V
	  CompMethod => "ProjectiveDegree"
	    applicable for all cases where the methods in the package may be used
	  CompMethod => "PnResidual"
	    this algorithm may be used for subschemes of \PP^n only  
	  Method => "InclusionExclusion"
	    applicable for all inputs
	  Method => "DirectCompleteInt"
	    this method may provide a performance improvement when the input is a complete intersection, if the input is not a complete intersection inclusion/exclusion it will return an error
	  InputIsSmooth=>
	    this option has values true/false and tells the method whether to assume the input ideal defines a smooth scheme, and hence to call the method Chern instead for reduced run time, alternatively the Chern function can be used directly
	  Output=>
	    the type of output to return the default output is an integer
	  Output=>"HashForm"
	    the type of output to return, HashForm returns a MutableHashTable containing the key "CSM" (the CSM class), and keys of the form  \{0\},\{1\},\{2\},...,\{0,1\},\{0,2\} ....\{0,1,2\}... and so on which correspond to the indices of the possible subsets of the generators of the input ideal, for each set of indices the CSM class of the hypersurface given by the product of all polynomails in the corresponding set of generators is stored, there is no extra cost to using this option
          IndsOfSmooth=>
	    this option may speed up the run time when using the DirectCompleteInt Method if the user knows additional information about the input ideal, see @TO IndsOfSmooth@
     Outputs
     	  :RingElement
	   the Euler characteristic 
Description
     	  Text
	       For a subscheme V of an applicable toric variety X, this command computes the Euler characteristic 
	  Example
	       kk=ZZ/32749;
	       R=kk[x_0..x_4]
	       I=ideal(random(1,R),random(2,R))
	       time Euler(I,InputIsSmooth=>true)
	       time Euler I
	       EulerIHash=Euler(I,Output=>HashForm);
	       A=ring EulerIHash#"CSM"
	       EulerIHash#{0,1}==CSM(A,ideal(I_0*I_1))
	       J=I+ideal(x_0*x_2-x_3*x_0)
	  Text 
	       Note that the ideal J above is a complete intersection, thus we may change the method option which may speed computation in some cases. We may also note that the ideal generated by the first 2 generators of I defines a smooth scheme and input this information into the method. This may also improve computation speed. 	   
	  Example
	       time Euler(J,Method=>DirectCompleteInt)
	       time Euler(J,Method=>DirectCompleteInt,IndsOfSmooth=>{0,1})
	  Text 
	       Now consider an example in \PP^2 \times \PP^2.
	  Example
	       R=MultiProjCoordRing({2,2})
	       r=gens R
	       K=ideal(r_0^2*r_3-r_4*r_1*r_2,r_2^2*r_5)
	       EulerK=Euler(K)
	       csmK= CSM(K)
	       EulerK==Euler(csmK)	       
	  Text 
	       In the case where the ambient space is a toric variety which is not a product of projective spaces we must load the NormalToricVarieties package and must also input the toric variety. If the toric variety is a product of projective space it is recommended to use the form above rather than inputting the toric variety for efficiency reasons. 
	  Example
	       needsPackage "NormalToricVarieties"
	       Rho = {{1,0,0},{0,1,0},{0,0,1},{-1,-1,0},{0,0,-1}}
               Sigma = {{0,1,2},{1,2,3},{0,2,3},{0,1,4},{1,3,4},{0,3,4}}
	       X = normalToricVariety(Rho,Sigma,CoefficientRing =>ZZ/32749)
	       CheckToricVarietyValid(X)
	       R=ring(X)
               I=ideal(R_0^4*R_1,R_0*R_3*R_4*R_2-R_2^2*R_0^2)
	       csmI=CSM(X,I)
	       EulerI=Euler(X,I)
	       Euler(csmI)==EulerI 	                 
	  Text
	       All the examples were done using symbolic computations with Gr\"obner bases. Changing the
	       option @TO CompMethod@ to bertini will do the main computations numerically, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@. Note that the bertini and PnResidual options may
	       only be used for subschemes of \PP^n. 
	       
	       Observe that the algorithm is a probabilistic algorithm and may give a wrong answer with a small but nonzero probability. Read more under 
	       @TO "probabilistic algorithm"@.
///



doc ///
     Key
     	  "configuring Bertini"
     Description
     	  Text
	       Using the numeric version of any command in the package CharacteristicClasses needs version 1.3 or higher of Bertini
	       to be installed. Download and installation of Bertini are explained at the @HREF {"http://www.nd.edu/~sommese/bertini/","Bertini homepage"}@. 
	       
	       Bertini should be installed in a directory in the user's PATH. As an alternative you can tell
	       the package how to find Bertini. Usually, when the package is installed, a file called {\tt init-CharacteristicClasses.m2} is created automatically in the user's
	       @TO2 {"applicationDirectory", "application directory"}@. See also the option {\tt Configuration} under @TO "newPackage"@.
	       In the file {\tt init-CharacteristicClasses.m2}, replace {\tt ""} in  the line {\tt "pathToBertini" => ""}
	       by the path to Bertini in quotation marks, for example {\tt "pathToBertini" => "/usr/local/BertiniLinux64&#95;v1.3.1/"}. The / at the end is important.	
	       Windows users should use the path relative to the cygwin directory, for example {\tt "/usr/local/BertiniWindows32&#95;v1.3.1/"} if Bertini is installed under 
	       {\tt pathToTheCygwinDirectory&#92;cygwin&#92;usr&#92;local&#92;BertiniWindows32&#95;v1.3.1 }.
	       
	       To check whether Bertini is working properly with the functions in the package CharacteristicClasses, use @TO "bertiniCheck"@.

///

doc ///
    Key
    	bertiniCheck
    Headline
     	  Checks whether the numerical version of the algorithms using Bertini works
    Usage
     	  bertiniCheck()
    Description
    	Text
		The functions @TO Chern@, @TO Segre@, @TO CSM@ and @TO Euler@ have the option @TO CompMethod@,
		which can be any of @TO ProjectiveDegree@, @TO PnResidual@, or @TO bertini@. The option "bertini" uses the external program Bertini, which might not
		be installed on the user's system. The function bertiniCheck checks whether Bertini is properly installed and configured. See
		also @TO "configuring Bertini"@.	    
///
  


doc ///
     Key
     	  "probabilistic algorithm"
     Description
     	  Text
	       The algorithms used for the computation of characteristic classes are probabilistic. Theoretically, they calculate the classes 
	       correctly for a general choice of certain polynomials. That is, there is an open 
               dense Zariski set for which the algorithm yields the correct class, i.e., the correct class is calculated with probability 1. 
               However, since the implementation works over a discrete probability space there is a very small, but non-zero, probability of not 
               computing the correct class. 
               Skeptical users should repeat calculations several times to increase the probability of computing the correct class.

               In the case of the symbolic implementation of the ProjecvtiveDegree method practical experience and algorithm testing indicate that a finite field with over 25000 elements is more than sufficient to expect a correct result with high probability, i.e.
               using the finite field kk=ZZ/25073 the experiential chance of failure with the ProjectiveDegree algorithm on a variety of examples
               was less than 1/2000. Using the finite field kk=ZZ/32749 resulted in no failures in over 10000 attempts of several different examples. 
	       
	       We illustrate the probabilistic behaviour with an example where the chosen random seed leads to a wrong result in the first calculation. 
	  Example
	       setRandomSeed 121;
   	       R = QQ[x,y,z,w]
   	       I = minors(2,matrix{{x,y,z},{y,z,w}})
   	       Chern (I,CompMethod=>PnResidual)  
     	       Chern (I,CompMethod=>PnResidual)  
	       Chern (I,CompMethod=>PnResidual)
	       Chern(I,CompMethod=>ProjectiveDegree)  	       
///

doc ///
     Key 
          CompMethod
	  ProjectiveDegree
          PnResidual
	  bertini
     Description
     	  Text
	       The option CompMethod determines which algorithm is used for the main computational steps of the calculation. This option map be used with methods @TO CSM@, @TO Segre@, @TO Chern@ , and @TO Euler@. Note, however, that CompMethod can only be set to PnResidual and bertini when the input ideal defines a subscheme of a projective space \PP^n. In all other cases this option will be ignored and ProjectiveDegree will be used automatically.  
	  Example
	       R = ZZ/32749[r,y,z,w];
	       Chern( minors(2,matrix{{r,y,z},{y,z,w}}), CompMethod=>ProjectiveDegree)  
	  Text  
	       There are three algorithms which can be used, ProjectiveDegree, PnResidual, and Bertini. When choosing the ProjectiveDegree 
	       option, the main step is the computation of projective degrees, for which Gr\"obner basis methods will be used. When choosing 
	       ResidualSymbolic, Gr\"obner basis methods will be used to compute so-called residuals. These computations can also be done 
	       numerically using the regenerative cascade implemented in Bertini. This is done by choosing the option bertini, provided
	       Bertini is @TO2 {"configuring Bertini", "installed and configured"}@.  	      
          Example
               R=ZZ/32749[v_0..v_5];
               I=ideal(4*v_3*v_1*v_2-8*v_1*v_3^2,v_5*(v_0*v_1*v_4-v_2^3));
               time CSM(I,CompMethod=>ProjectiveDegree)
               time CSM(I,CompMethod=>PnResidual)
	       codim I
               S=QQ[s_0..s_3];
	       K=ideal(4*s_3*s_2-s_2^2,(s_0*s_1*s_3-s_2^3));
	       time CSM(K,CompMethod=>ProjectiveDegree)
	       time CSM(K,CompMethod=>PnResidual)
	  Text
	      The options PnResidual and bertini may only be used for subschemes of a single projective space of fixed dimension. For subschemes of products of projective spaces and for subschemes of toric varieties only the ProjectiveDegree option is available and other options will be ignored by the methods @TO Segre@, @TO CSM@, @TO Chern@ and @TO Euler@.
	  Example
	      R=MultiProjCoordRing({1,2,2})
	      I=ideal(R_0*R_1*R_3-R_0^2*R_2)
	      Segre I
	      Segre(I,CompMethod=>ProjectiveDegree)
///

doc ///
     Key 
          Method
	  InclusionExclusion
	  DirectCompleteInt
     Description
     	  Text
	       The option Method is only used by the commands @TO CSM@ and @TO Euler@ and only in combination with @TO CompMethod@=>ProjectiveDegree. The Method InclusionExclusion will always be used with @TO CompMethod@ PnResidual or bertini. When the input ideal is a complete intersection one may, potentially, speed up the computation by setting Method=> DirectCompleteInt. The option Method is only used by the commands @TO CSM@ and @TO Euler@ and only in combination with @TO CompMethod@=>ProjectiveDegree. The Method InclusionExclusion will always be used with @TO CompMethod@ PnResidual or bertini. 
	  Example
	       R = ZZ/32749[x_0..x_6]
	       I=ideal(random(2,R),random(1,R),R_0*R_1*R_6-R_0^3);
	       time CSM I
	       time CSM(I,Method=>DirectCompleteInt)
	  Text 
	      When using the DirectCompleteInt method one may potentially further speed up computation time by specifying what subset of the generators of the input ideal define a smooth subscheme (if this is known), see @TO IndsOfSmooth@.
///

doc ///
     Key 
          Output
	  ChowRingElement
	  HashForm
	  HashFormXL
     Description
     	  Text
	       The option Output is only used by the commands @TO CSM@, @TO Segre@, @TO Chern@ and @TO Euler@ to specify the type of output to be returned to the used. This option will be ignored when used with @TO CompMethod@ PnResidual or bertini. The option will also be ignore when @TO Method@=>DirectCompleteInt is used. The default output for all these methods is ChowRingElelment which will return an element of the appropriate Chow ring. All methods also have an option HashForm which returns additional information computed by the methods during their standard operation. 
	  Example
	       R = ZZ/32749[x_0..x_6]
	       A=ChowRing(R)
	       I=ideal(random(2,R),R_0*R_1*R_6-R_0^3);
	       csm=CSM(A,I,Output=>HashForm)
	       peek csm
	       CSM(A,ideal I_0)==csm#{0}
	       CSM(A,ideal(I_0*I_1))==csm#{0,1}
	       c=Chern( I, Output=>HashForm)  
	       peek c
	       seg=Segre( I, Output=>HashForm) 
	       peek seg
	       eu=Euler( I, Output=>HashForm) 
	       peek eu
	       
	  Text  
	       The MutableHashTable returned with the option Output=>HashForm contains different information depending on the method with which it is used. Additionally if the option @TO InputIsSmooth@ is used then the hash table returned by the methods Euler and CSM will be the same as that returned by Chern. When using the @TO CSM@  command in the default configurations (that is @TO Method@=>InclusionExclusion, @TO CompMethod@=>ProjectiveDegree) there is the additional option to set Output=>HashFormXL. This returns all the usual information that Output=>HashForm would for this configuration with the addition of the projective degrees and Segre classes of singularity subschemes generated by the hypersurfaces considered in the inclusion/exclusion procedure, that is in finding the CSM class of all hypersurfaces generated by taking a product of some subsets of generators of the input ideal. Note that, since the CSM class of a subscheme equals the CSM class of its reduced scheme, or equailiently for us the CSM class corresponding to an ideal I equals the CSM class of the radical of I, then internally we always work with radical ideals (for efficiency reasons). Hence the projective degrees and Segre classes computed internally will be those of the radical of an ideal defined by a polynomial which is a product of some subset of the generators. We illustrate this with an example below.
          Example
	      csmXLhash=CSM(A,I,Output=>HashFormXL)
	      peek csmXLhash
	      K=ideal I_0*I_1;
	      CSM(A,radical K)==CSM(A,K)
	      J=ideal jacobian radical K;
	      segJ=Segre(A,J,Output=>HashForm)
	      csmXLhash#("G(Jacobian)"|toString({0,1}))==segJ#"G"
	      csmXLhash#("Segre(Jacobian)"|toString({0,1}))==segJ#"Segre"
///

doc ///
     Key 
          IndsOfSmooth
     Description
     	  Text
	       The option IndsOfSmooth is only used by the commands @TO CSM@, and @TO Euler@ in combination with the option Method=>DirectCompletInt. When used this option may allow the user to speed up the computation by telling giving the method a list of indices for the generators of the input ideal that, when taken together, define a smooth subscheme of the ambient space. This option will be ignored otherwise. 
	  Example
	       R = MultiProjCoordRing({2,2})
	       I=ideal(R_0*R_1*R_3-R_0^2*R_3,random({0,1},R),random({1,2},R));
	       time CSM(I,Method=>DirectCompletInt)
	       time CSM(I,Method=>DirectCompletInt,IndsOfSmooth=>{1,2})
///

doc ///
     Key 
          InputIsSmooth
     Description
     	  Text
	       The option InputIsSmooth is only used by the commands @TO CSM@, and @TO Euler@. If the input ideal is known to define a smooth subscheme setting this option to true will speed up computations (it is set to false by default).	  
	  Example
	       R = ZZ/32749[x_0..x_4];
	       I=ideal(random(2,R),random(2,R),random(1,R));
	       time CSM I
	       time CSM(I,InputIsSmooth=>true)
	       
	  Text
	       Note that one could, equivalently, use the command @TO Chern@ instead in this case.  
	  Example
	       time Chern I       
///

doc ///
     Key 
          CheckSmooth
     Description
     	  Text
	       The option CheckSmooth is only used by the commands @TO CSM@ and only when computing the CSM class of a toric variety. It is set to true by default. When true it will check if the toric variety is smooth before proceeding, if it is this will speed up computation; however checking for smoothness does take some time.
	  Example
	       needsPackage "NormalToricVarieties"
               U = toricProjectiveSpace 7
	       time CSM U
               time CSM(U,CheckSmooth=>false)
	       
///
--------------------------------------------------------
-- Tests
--------------------------------------------------------
 


 
TEST ///
   setRandomSeed 24;
   R = QQ[v,y,z,w];
   I = minors(2,matrix{{v,y,z},{y,z,w}});
   totalSegre = Segre(I,CompMethod=>PnResidual);
   assert( totalSegre == 3*( (ring(totalSegre))_0 )^2 - 10*( (ring(totalSegre))_0 )^3 );
   totalChern = Chern(I,CompMethod=>PnResidual);
   assert( totalChern == 3*( (ring(totalChern))_0 )^2 + 2 * ((ring(totalChern))_0)^3 );
///

TEST ///
-*
   restart
   needsPackage "CharacteristicClasses"
*-
   R = ZZ/32749[x_0..x_4];
   I = ideal(random(1,R),random(1,R),x_0^2*x_3-x_4*x_1*x_0);
   A=ChowRing(R);
   csm1 = CSM(A,I);
   csm2=CSM(A,I,Method=>DirectCompleteInt);
   assert( csm1== csm2 );
///

TEST ///
-*
   restart
   needsPackage "CharacteristicClasses"
*-
    n=4;
    kk=ZZ/32749;
    R=kk[x_0..x_n];
    A=ChowRing(R);
    I = ideal random(R^1, R^{-2,-2});
    csm = CSM(I);
    A = ring csm;
    seg = Segre(A,I);
    assert((1+A_0)^(n+1) * seg == csm);
    assert(Euler(csm) == 8);
    --FORMULA FROM FULTON FOR SEGRE
    segCI= product(0..numgens(I)-1,i->((degree(I_i))_0*A_0//(1+(degree(I_i))_0*A_0)));
    assert(seg==segCI);
///

TEST ///
-*
   restart
   needsPackage "CharacteristicClasses"
   installPackage "CharacteristicClasses"
*-
    R=MultiProjCoordRing({2,2});
    A=ChowRing(R);
    I=ideal(random({1,1},R),R_0*R_3^2-R_1*R_4*R_3);
    csmH=CSM(A,I,Output=>HashForm);
    csmD=CSM(A,I,Method=>DirectCompleteInt,IndsOfSmooth=>{0});
    m=new MutableHashTable;
    m#{1}=csmH#{1};
    m#{0,1}=csmH#{0,1};
    V=ClassInChowRing(A,I_0);
    seg=Segre(A,ideal(I_0));
    assert(csmD==csmH#"CSM");
    assert(csmH#"CSM"==CSM(A,I,m));
    assert(Euler(csmH#"CSM")==7);
    assert(csmH#{0}==Chern(A,ideal(I_0)));
    assert(seg==(V//(1+V)));    
///
-------------------------------------------------------
-- References
------------------------------------------------------
-- [1] David Eklund, Christine Jost, Chris Peterson. A method to compute Segre classes, Journal of Algebra and Its Applications 12(2), 2013
-- [2] Daniel J. Bates, Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler. Bertini: Software for Numerical Algebraic Geometry, available at http://www.nd.edu/~sommese/bertini
-- [3] Jonathan D. Hauenstein, Andrew J. Sommese, Charles W. Wampler. Regenerative cascade homotopies for solving polynomial systems, Applied Mathematics and Computation 218(4), 2011
-- [4] Christine Jost. An algorithm for computing the topological Euler characteristic of complex projective varieties, submitted, arXiv:1301.4128 [math.AG]
-- [5] Martin Helmer. Algorithms to compute the topological Euler characteristic, Chern-Schwartz-Macpherson class and Segre class of projective varieties. Journal of Symbolic Computation, 2015. Preprint on arXiv at arXiv:1402.2930.
-- [6] Martin Helmer. A Direct Algorithm to Compute the Topological Euler Characteristic and Chern-Schwartz-MacPherson Class of Projective Complete Intersection Varieties. (2014). arXiv preprint arXiv:1410.4113.
-- [7] Martin Helmer. An Algorithm to Compute the Topological Euler Characteristic, the Chern-Schwartz-MacPherson Class and the Segre class of Subschemes of Some Smooth Complete Toric Varieties. (2015). arXiv preprint arXiv:1508.03785
----------------------------------------------------------------------------------------------


