///
restart
needsPackage "WeierstrassSemigroups"
uninstallPackage "WeierstrassSemigroups"  
elapsedTime installPackage "WeierstrassSemigroups"  -- 33.3846s elapsed
viewHelp "WeierstrassSemigroups"
check "WeierstrassSemigroups"
uninstallPackage "WeierstrassSemigroups"
///

newPackage(
         "WeierstrassSemigroups",
         Version => "1.0",
         Date => "June 4, 2026",
         Headline => "Compute smoothing families for Weierstrass semigroups",
         Authors => {{ Name => "David Eisenbud", Email => "de@berkeley.edu", HomePage => "http://eisenbud.github.io"},
	             { Name => "Frank-Olaf Schreyer", Email => "schreyer@math.uni-sb.de", HomePage => "https://www.math.uni-sb.de/ag/schreyer"}},
         Keywords => {"Algebraic Geometry"},
	 PackageExports => {"FastMinors",
	     HomologicalAlgebraPackage,
	     "Permutations",
	     "PrimaryDecomposition",
	     "NumericalSemigroups",
	     "VersalDeformations",
	     "RandomPoints"},
	 AuxiliaryFiles => false,
         DebuggingMode => false,
	 Keywords =>{"Algebraic Geometry"}
         )

     export {
	 "getListOfIdeals",
	 "checkSmoothnessOfOneParameterFamilies",
	 "checkFlatnessOfOneParameterFamilies",
	 "HowToStartCollecting",
	 "collectWithVersalDeformations",
	 "getSmoothingFamilyWithVersalDeformation",
	  --"getSmoothingFamilyFromDeformation",
	 "satisfiesDegreeCondition1",
	 "satisfiesDegreeCondition2",
	 "hasExactSubcomplex",
	 "hilbertBurchMatrices",
	 "hilbertBurchConditions",
	 "displaySyzygyMatrices",
	 "give1683Format",
	 "depthCondition1",
	 --"degreeMatrices",
	 "prepareInitialPositionList",
	 --"positioListToArray",
	 --"degreeArray",
	 "restrictedUnfolding",
	 "makeUnfolding",
	 "flatteningRelations",
	 "getFromDisk",
	 "pruneFamily",
	 "getFlatFamily",
	 "findPoint",
	 --"checkSmoothness",
	 "getSmoothingFamily",
	 --"getSmoothingFamilies",
         --"fasterBound",
	 --"produceSmoothingFamilies",
	 "solvingFlatteningRelations",
	 "isSmoothingFamily",
	 "clearDenominators",
	 "makeRange",
	 "appendFamily",
	 "getRangeOfOneParameterFamily",
	 "getParameterFamily",
	 "getOneParameterFamily",
	 "improveFamily",
	 --"isSmoothAffineCurve",
	 "findCompleteIntersection",
	 "smoothnessWithReductions",
	 "testBound",
	 "testCongruences",
	 "testRange",
	 "toDoList",
	 "collectByBound",
	 "collectByCongruences",
	 "collectByRange",
	 -- option keys
	 "CoeffSize",
	 "CoeffBound",
	 --"Congruence",
	 "Bound",
	 "BaseField",
	 "HighestOrder1"
	 }

-* Code section *-

getListOfIdeals=method(Options=>{Verbose=>0})

getListOfIdeals(List,String) := o -> (LL,doneData) -> (
    X:=doneData;
    Y:=openDatabase X;
    if o.Verbose>0 then <<"#keys Y = "<<#keys Y<< ", #LL = "<<#LL<<endl;
    R:=null;I:=null;
    listOfIdeals:=apply(LL,L->(R=value Y#(toString L|"ring");
	I=value (Y#(toString L|"ideal"))));
    close Y;
    assert(all(listOfIdeals,I->isHomogeneous I));
    listOfIdeals)

checkFlatnessOfOneParameterFamilies=method(Options=>{Verbose=>0})
checkFlatnessOfOneParameterFamilies(List,String) := o -> (LL,doneData) -> (
    listOfIdeals := getListOfIdeals(LL,doneData,Verbose => o.Verbose-1);
    I:=null;L:=null;coorectSpecialFiber:=null;flat:=null;correctSpecialFiber:=null;
    flatnessTests := apply(listOfIdeals,J->(L=flatten drop(degrees ring J,-1);
	I=semigroupIdeal L;
	correctSpecialFiber = sub(J,vars ring I|matrix{{0}})==I;
	flat = betti res(J,LengthLimit=>2)== betti res(I,LengthLimit=>2);
        correctSpecialFiber and flat));
   all flatnessTests)

checkSmoothnessOfOneParameterFamilies=method(Options=>{Verbose=>0,BaseField=>ZZ/nextPrime 10^7})

checkSmoothnessOfOneParameterFamilies(List,String) := o -> (LL,doneData) -> (
    listOfIdeals := getListOfIdeals(LL,doneData);--,Verbose => o.Verbose-1);
    smooth:=null;J:=null;L:=null;
    toDoAgain := {};
    apply(toList(0..#listOfIdeals-1),i->(
	J=listOfIdeals_i;
	if o.Verbose>0 then (<<"case = " <<i<<endl;
	elapsedTime smooth=smoothnessWithReductions(J,Verbose=>o.Verbose-1,BaseField=>o.BaseField)) else (
	   smooth=smoothnessWithReductions(J,Verbose=>o.Verbose-1,BaseField=>o.BaseField));
    if not smooth then (
	L=flatten drop(degrees ring J,-1);
	toDoAgain=append(toDoAgain,L);
        if o.Verbose>0 then (
	    << "The semigroup " << L<< " has to be  checked again. " << endl););
     )); 
    (#toDoAgain==0, toDoAgain)
    )

///
restart
needsPackage"WeierstrassSemigroups"
LL=getFromDisk "fam8";#LL
LL1=LL|{{2,3}}
doneData="fam8.dbm"
Js=getListOfIdeals(LL1,doneData,Verbose=>1);
checkFlatnessOfOneParameterFamilies(LL,doneData,Verbose=>2)
checkSmoothnessOfOneParameterFamilies(LL,doneData,Verbose=>2)
(answer,toDoAgain)=checkSmoothnessOfOneParameterFamilies(LL,doneData,Verbose=>1,BaseField=>ZZ/5)
checkSmoothnessOfOneParameterFamilies(toDoAgain,doneData,Verbose=>2,BaseField=>ZZ/7)
checkSmoothnessOfOneParameterFamilies(LL,doneData,Verbose=>2,BaseField=>ZZ/5)
///
    


collectWithVersalDeformations=method(Options=>{BaseField=>ZZ/nextPrime 10^7,
	Bound=>1,CoeffSize=>1,CoeffBound=>10^4,Verbose=>0,HighestOrder1=>20})

collectWithVersalDeformations(List,ZZ,String,String) := o -> (LL,b,done,doneData) -> (
    alreadyDone := getFromDisk (done);
    LLStillToDo:=select(LL,L->not member(L, alreadyDone));
    A:=null;L:=null;successes:=0;
    apply(#LLStillToDo, i -> (L=LLStillToDo_i;
	    <<endl;
	    << "case " << i <<" from " <<#LLStillToDo <<", semigroup = " << L <<endl;
	    elapsedTime A=getSmoothingFamilyWithVersalDeformation(L,Bound=>b,CoeffSize=>o.CoeffSize,
		CoeffBound=>o.CoeffBound,Verbose=>o.Verbose,HighestOrder1=>o.HighestOrder1);
	    if A_0 then ( successes=successes+1;
		if o.Verbose>0 then (<<"successes = "<<successes<<endl;);
		appendFamily(L,A_1,done,doneData))));
    )


getSmoothingFamilyWithVersalDeformation=method(Options=>{BaseField=>ZZ/nextPrime 10^7,
	Bound=>1,CoeffSize=>1,CoeffBound=>10^4,Verbose=>0,HighestOrder1=>20})


getSmoothingFamilyWithVersalDeformation(List) := o -> L ->(
    I:=semigroupIdeal(L,"BaseField"=>o.BaseField);
    T1:=cotangentCohomology1 I;
    T1=cotangentCohomology1(min flatten degrees source T1,-o.Bound,I);
    T2:=cotangentCohomology2 I;
    F0:=gens I;
    if o.Verbose >0 then (
        elapsedTime (F,R,G,C) := versalDeformation(F0,T1,T2,DegreeBound=>2,Verbose=>o.Verbose,HighestOrder=>o.HighestOrder1)) else (
        (F,R,G,C) = versalDeformation(F0,T1,T2,DegreeBound=>2,Verbose=>o.Verbose,HighestOrder=>o.HighestOrder1));
    fam:=sum F;
    base:=ideal sum G;
    kk:=coefficientRing ring I;
    A:=kk[gens ring base,Degrees=>flatten degrees ring base];
    J:=sub(base,A);
    S:=ring I;
    SA:=kk[gens S|gens A,Degrees=>degrees S|degrees A];
    family:=sub(fam,SA);
    return getSmoothingFamilyFromDeformation(I,J,family))


getSmoothingFamilyFromDeformation=method(Options=>{Verbose=>0,CoeffSize=>1,CoeffBound=>10^4})

getSmoothingFamilyFromDeformation(Ideal,Ideal,Matrix) := o -> (I,base,fam) -> (
    (J1,family1) := pruneFamily(I,base,fam);--,Verbose=>o.Verbose);
    if o.Verbose >1 then (<< "time to decompose:" <<endl;
	cJ1:= reverse decompose J1;) else (cJ1= reverse decompose J1;);
    ccJ1:=apply(cJ1,c->codim c);S:=ring I;
    if o.Verbose >0 then (
    << "number of components = " <<#cJ1 << ", codimension of components = " <<ccJ1 <<endl;);
    good:=false;
    fiber:=matrix{{1_S}};fib:=null;fiber1:=null;pos:=null;posa:=null;fact1:=null;
    c:=o.CoeffSize;J3:=null;family2:=null; StFinite:=null;fibFinite:=null;J2:=null;
    comps:={};
    L:=degrees S;kk:=coefficientRing S;
    z:=symbol z;
    St:=kk[gens S|{z},Degrees=>degrees S|{1}];
    apply(#cJ1,k->( if not good then ( J2= cJ1_k;
		if o.Verbose>1 then <<flush<<endl;
		if isSmoothingFamily(L,I,family1,J2) then (
	    comps=append(comps,k);
            family2=family1%sub(J2,ring family1);
	    J3=trim flatteningRelations(I,ring family2,family2);
	    if o.Verbose>1 then <<"component number = "<< k <<", codim J3 = "
	    << codim J3 <<", numgens J3 = "<< numgens J3 <<endl;
	    if o.Verbose>1 then (elapsedTime if numgens J3 != 0 then (
		(worked,fiber):=solvingFlatteningRelations(J3,family2,I,CoeffBound=>o.CoeffBound,CoeffSize=>o.CoeffSize) ) else (
		fiber1=sub(family2,(vars ring family2)_{0..#L-1}|
		    matrix{ apply(numgens ring family2-#L,i->random(2*c-1)-c)});
		(worked,fiber)=clearDenominators(fiber1,CoeffBound=>o.CoeffBound));) else (
	        if numgens J3 != 0 then (
		(worked,fiber)=solvingFlatteningRelations(J3,family2,I,CoeffBound=>o.CoeffBound,CoeffSize=>o.CoeffSize) ) else (
		fiber1=sub(family2,(vars ring family2)_{0..#L-1}|
		    matrix{ apply(numgens ring family2-#L,i->random(2*c-1)-c)});
		(worked,fiber)=clearDenominators(fiber1,CoeffBound=>o.CoeffBound)););
            if worked then (
	    fib=ideal homogenize(sub(fiber,St),last gens St);
	    assert(betti syz gens fib == betti syz gens I);
	    StFinite:=kk[gens St, Degrees=>degrees St];
	    fibFinite=sub(fib,StFinite);
	    good=smoothnessWithReductions(fibFinite,Verbose=>o.Verbose);	    
	    );));
	));
    --error "debug";
    if o.Verbose >0 then <<" smoothing components numbers = " << comps <<endl;
    flat:=false;
    Sz:=QQ[gens ring fib,Degrees=>degrees ring fib];
    fib=sub(fib,Sz);
    if good then if o.Verbose>2 then (elapsedTime flat=(betti syz gens fib==betti syz gens I);
	<<"flat = " << flat << endl;) else (flat=betti syz gens fib==betti syz gens I);
    if good and flat then (true,fib) else (false,null)
    )


give1683Format=method()

give1683Format(List) := L -> (
    if #L != 4 then error "expected a semigroup with 4 generators";
    if gcd L =!=1 then return false;
    fL := res semigroupIdeal L;
    if rank fL_3 =!= 3 or rank fL_1 =!= 6 then
         false else true)


give1683Format(ZZ,ZZ,ZZ,ZZ) :=(a,b,c,d) -> (
    L := {a,a+b,a+b+c,a+b+c+d};
    give1683Format L)

give1683Format(List,List,List,List) := (r1,r2,r3,r4) -> (
    LL := flatten flatten flatten for a in r1 list
    for b in r2 list
     for c in r3 list
      for d in r4 list(
	L := {a,a+b,a+b+c,a+b+c+d};
        if give1683Format(a,b,c,d) then L else continue
   )
)

satisfiesDegreeCondition1=method()

satisfiesDegreeCondition1(List) := L -> (
    fL := res semigroupIdeal L;
    if rank fL_3 =!= 3 or rank fL_1 =!= 6 then
         return (false);
    m3 := matrix apply(flatten degrees fL_2,d->apply(flatten degrees fL_3,e->e-d));
    if m3_(4,0)<0  then true else false)

satisfiesDegreeCondition2=method()

satisfiesDegreeCondition2(List) := L -> (
    fL := res semigroupIdeal L;
    if rank fL_3 =!= 3 or rank fL_1 =!= 6 then
         return (false);
    m2 := matrix apply(flatten degrees fL_1,d->apply(flatten degrees fL_2,e->e-d));
    if  m2_(4,2) <min L then true else false)

hasExactSubcomplex=method()
hasExactSubcomplex(List) := L -> (
    I := semigroupIdeal L;
    fI := res I;
    J := ideal apply(4,i->I_i);
    fJ:=res J;
    if not (rank fJ_1==4 and rank fJ_2 == 4) then return false;
    degrees fJ_2 == (degrees fI_2)_{0..3} and degrees fJ_3 == (degrees fI_3)_{0})

hilbertBurchMatrices = method()

hilbertBurchMatrices(List) := L -> (
    I:=semigroupIdeal L;
    J := ideal apply(4,i->I_i);
    fJ := res J;
    rowIndices:=select(4,i->numgens trim ideal (fJ.dd_2^{i})==2);
    columnIndices:=null;m3x2:=null;m1x2:=null;
    apply(rowIndices,i->(columnIndices=select(4,j-> (ideal (fJ.dd_2)_(i,j))==0);
	m3x2 = (fJ.dd_2^(select(4,i'->i'=!= i)))_columnIndices;
	m1x2 = (fJ.dd_2^{i}_(select(4,j->not member(j,columnIndices)
	)));
        directSum(m3x2,m1x2)))
    )

hilbertBurchConditions = method()
hilbertBurchConditions(List) := L -> (
    (m4x4s:=hilbertBurchMatrices L;
    all(m4x4s,m-> codim minors(3,m)==2))
)

displaySyzygyMatrices=method(Options=>{Verbose=>true})

displaySyzygyMatrices(List) := o -> L -> (
    I := semigroupIdeal L;
    kk := coefficientRing ring I;
    S :=kk[(gens ring I)_{1,2,3,0},Degrees=>(degrees ring I)_{1,2,3,0}];
    y:= symbol y; w := symbol w;
    Syw:=kk[(gens ring I)_1,y,w,(gens ring I)_0,Degrees=>(degrees ring I)_{1,2,3,0}];    
    I = trim sub(sub(I,S),vars Syw);
    fI := res I;
    assert(rank fI_1==6 and rank fI_2==8);
    A:={
	(transpose fI.dd_1|fI.dd_2)||(matrix{{0},{0},{0}}|transpose fI.dd_3),
	(genus L,matrix apply(flatten degrees fI_1,d->
	    apply(flatten degrees fI_2,e->e-d)),
	 matrix apply(flatten degrees fI_2,d->
	    apply(flatten degrees fI_3,e->e-d)))};
   if o.Verbose then <<netList A <<endl;
   A)
  
depthCondition1 = method()

depthCondition1(List):= L -> (
    I:= semigroupIdeal L;
    fI:=res I;
    codim minors(2,fI.dd_3^{4..7})
)


prepareInitialPositionList=method(Options=>{Verbose=>false})


prepareInitialPositionList(List,ZZ) := o -> (L,b) -> (
    I := ideal semigroupRing(L,"BaseField"=>ZZ/nextPrime 10^4);
    (A,unfolding) := makeUnfolding(I);
    xs:=(support unfolding)_{0..#L-1};
    as := apply(numgens I,i-> select(support unfolding_{i},m->not member(m,xs)));
    --(as := apply(numgens I,i-> drop(support unfolding_{i},#L)));
    if o.Verbose then (<<"degrees of the variables in the universal unfolding:"<< endl;
	<<netList apply(numgens I,i->apply(as_i, m->(degree m)_0)) << endl);
    --optBound := optimalBound(L,Verbose=>o.Verbose,Probably=>o.Probably);
    rL := apply(#as,i->select(as_i,m->(degree m)_0> b));
    if o.Verbose then (<<"degrees of the variables after restriction"<<endl;
	<<netList apply(#rL,i->apply(rL_i,m->(degree m)_0))<<endl);
    positionList:=apply(flatten rL,m->position(flatten as, n-> m==n));
    positionList)


positionListToArray=method(Options=>{Verbose=>false})
positionListToArray(List,List):= o -> (L,positionList) -> (
    I := ideal semigroupRing(L,BaseField=>ZZ/nextPrime 10^4);
    (A,unfolding) := makeUnfolding(I);
    xs:=(support unfolding)_{0..#L-1};
    as := apply(numgens I,i-> select(support unfolding_{i},m->not member(m,xs)));

    arrayBoundaries:= apply(numgens I+1,i->sum(i,j->#as_j));
    positionArray:=apply(numgens I,i->select(positionList,j->(arrayBoundaries_i<=j and
		j < arrayBoundaries_(i+1))));
    if o.Verbose then (<<"positionArray = "<< endl;
	<<netList apply(numgens I,i->positionArray_i) << endl;);
    positionArray
    )

positionArrayToList=method()
positionArrayToList(List):= positionArray -> flatten positionArray


degreeArray=method()
degreeArray(List,List) := (L,positionArray) -> (
     I := ideal semigroupRing(L,"BaseField"=>ZZ/nextPrime 10^4);
     (A,unfolding) := makeUnfolding(I);
     xs:=(support unfolding)_{0..#L-1};
     as := apply(numgens I,i-> select(support unfolding_{i},m->not member(m,xs)));
     apply(numgens I,i->apply(positionArray_i,j->(degree (flatten as)_j)_0))
     )
     

showRestrictedFamily=method()
showRestrictedFamily(List,List) := (L,leftPositions) -> (
    I := ideal semigroupRing(L,BaseField=>ZZ/nextPrime 10^4);
    (A,unfolding) := makeUnfolding(I);
    xs:=(support unfolding)_{0..#L-1};
    as := apply(numgens I,i-> select(support unfolding_{i},m->not member(m,xs)));
    as1 := flatten as;
    --(as := apply(numgens I,i-> drop(support unfolding_{i},#L))); --perhaps has to be corrected
    restrictionList := apply((flatten as)_leftPositions,m->sub(m,A));
    superflousVars:=ideal(vars A%ideal restrictionList);
    runfolding:= unfolding%sub(ideal superflousVars,ring unfolding);
    kk:=coefficientRing ring I;
    SB:=kk[support runfolding,Degrees=>apply(support runfolding,m->degree m)];
    runfolding = sub(runfolding,SB);
    --(B1,runfolding1):=restrictedUnfolding(I,leftPositions)
    (J,family) := getFlatFamily(I,A,unfolding,restrictionList);
    (J1,family1) := pruneFamily(I,J,family);
    bs:=(entries((vars SB)_{#L..numgens SB-1}))_0;
    B:=kk[bs,Degrees=>apply(bs,m->degree m)];
    J2:=trim flatteningRelations(I,B,runfolding);
    (J1,family1,runfolding,J2))


restrictedUnfolding = method() -- to be improved
restrictedUnfolding(Ideal,List) := (I,positionList) -> (
    S := ring I;
    (A,unfolding) := makeUnfolding I;
    kk := coefficientRing A;
    listInA:=(gens A)_positionList;   
    sub1 := matrix{apply(numgens A,i->if member(i,positionList) then (gens A)_i else 0)};
    A' :=  kk[support sub1,Degrees=>apply(support sub1,m->(degree m)_0)];    
    SA' := kk[gens S|gens A',Degrees=>apply((gens ring I|gens A'),m->(degree m)_0)];
    runfolding := sub(unfolding,sub(vars S,SA')|sub(sub1,SA'));
    (A',runfolding)
    )
 

makeUnfolding=method(Options =>
      {Verbose => false,
      BaseField => ZZ/(nextPrime 10^4)})

makeUnfolding Ideal := o-> I ->(
    if not degreeLength ring I == 1 or
       not isHomogeneous I or
       I != trim I then
       error "expected N-graded homogeneous ideal
       given with minimal set of generators";
--    gbI := gb(I,ChangeMatrix =>true);
--    chMat := getChangeMatrix gbI;
    S := ring I;
    kk := coefficientRing S;
    degs := flatten degrees source gens I;
    unfoldingTerms := flatten for i from 0 to max degs-1 list (b:=basis(i,S/I); if b==0 then
	continue else (entries b))_0;
    unfoldingTerms2 := apply(degs,d->select(unfoldingTerms, t-> (degree t)_0 < d));
    a := symbol a;
    avars := flatten apply(#degs,i->apply(#unfoldingTerms2_i,j->a_{i,j}));
    adegs := flatten apply(#degs,i->apply(unfoldingTerms2_i,t->degs_i-(degree t)_0));
    A := kk[avars,Degrees=>adegs];
    avars= reverse sort(gens A,y->degree y);
    adegs=apply(avars,y->(degree y)_0);
    A = kk[avars,Degrees=>adegs];
    SA := kk[gens S|gens A,Degrees=>degrees S|degrees A];
    avars = apply (#degs,i->apply(#unfoldingTerms2_i,j->a_{i,j}));
    unfoldingTerms3 := matrix{apply(#degs,i->sum(#unfoldingTerms2_i,j->
	    sub(a_{i,j},SA)*sub((unfoldingTerms2_i)_j,SA)))}; 
    unfolding := sub(gens I,SA)+unfoldingTerms3;
    (A,unfolding)
    )

makeUnfolding List := o-> L -> (
        I:= trim ideal semigroupRing(L,"BaseField"=>o.BaseField);
	makeUnfolding I)

flatteningRelations=method()
flatteningRelations(Ideal,Ring, Matrix) := (I,A,unfolding) -> (
    gbI:=gb(I,ChangeMatrix=>true);
    S := ring I;
    SA := ring unfolding;
    chMat:=getChangeMatrix gbI;
    unfoldingGB := unfolding*sub(chMat,SA);
    -- can one use the build in gb algorithm to compute the
    -- flattening relations faster
    unfGBf:=forceGB unfoldingGB;
    ldT := flatten entries leadTerm unfoldingGB;
    s0:=syz sub(gens gbI,SA);
    testSyzygy1:=unfoldingGB*s0;
    testSyzygy2:=testSyzygy1%unfGBf;
    u1:=null;
    ma := max flatten degrees source syz leadTerm gens gbI;
    rems := reverse flatten for i from 0 to ma list (b:=basis(i,S^1/I); if b==0 then  continue else (entries b))_0;
    us := apply(rems,u->(u1=contract(sub(u,SA),testSyzygy2);testSyzygy2-sub(u,SA)*u1;
	u1));
    relsA:=sub(ideal(flatten us),A);
    relsA
     )


getFromDisk = method()
getFromDisk String := name -> drop(toList value get name,-1)

getFlatFamily=method(Options =>
      {Verbose => false,
      BaseField => ZZ/(nextPrime 10^4)})


getFlatFamily(Ideal,Ring,Matrix,List) :=  o -> (I,A,unfolding,restrictionList) -> (
    --Input: I, the ideal of the semigroup
    --       A coordinate ring of 
    --       unfolding, a matrix defining a family over kk[gens ring I|gens A]
    --       restrictionList, aList of variables 
    --Output: I, the ideal of the semigroup
    --        J1 the ideal of the flattening relations in A
    --        family the ideal in  SA defining
    --        the flat family over (Spec A/J1)
    SA:=ring unfolding;
    S:= ring I;
    --restrictionList
    runfolding:=unfolding%sub(ideal (vars A%sub(ideal restrictionList,A)),SA);
    if o.Verbose then (
	 <<"flatteningRelations"<<endl<<flush;
          elapsedTime J:=flatteningRelations(I,A,runfolding);
	  ) else (
	 J=flatteningRelations(I,A,runfolding)
	 );
    mA:= max flatten degrees A;
    if o.Verbose then (
	<<"next gb" << endl<<flush;
	elapsedTime gbJ:=forceGB gens gb(J,DegreeLimit=>mA);) else (
	gbJ=forceGB gens gb(J,DegreeLimit=>mA););
    varsAmodJ:=vars A%gbJ;
    J1:=sub(J,varsAmodJ);    
    family:=sub(runfolding,sub(vars S,SA)|sub(varsAmodJ,SA));
    if J1==0 then assert (betti syz family==betti syz gens I);
    (J1,family))



getFlatFamily(Ideal,Ring,Matrix) := o ->(I,A,runfolding) -> (
    --Input: I, the ideal of the semigroup
    --       A coordinate ring of 
    --       a matrix defining a family over kk[gens ring I|gens A]
    --Output: I, the ideal of the semigroup
    --        J1 the ideal of the flattening relations in A
    --        family the ideal in  SA defining
    --        the flat family over (Spec A/J1)
    assert(#gens ring runfolding==#gens A + #gens ring I);
    SA:=ring runfolding;
    S:= ring I;
    if o.Verbose then (
	 <<"flatteningRelations"<<endl<<flush;
          elapsedTime J:=flatteningRelations(I,A,runfolding);
	  ) else (
	 J=flatteningRelations(I,A,runfolding)
	 );
    mA:= max flatten degrees A;
    gbJ:=null;
    if o.Verbose then (
	<<"next gb" << endl<<flush;
	elapsedTime gbJ=forceGB gens gb(J,DegreeLimit=>mA);) else (
	gbJ=forceGB gens gb(J,DegreeLimit=>mA););
    varsAmodJ:=(matrix{support runfolding})%sub(gens gbJ,SA);
    J1:=trim sub(J,((sub(varsAmodJ,ring J))_{#gens S..#gens SA-1}));-- to be corrected   
    family:=sub(runfolding,sub(varsAmodJ,SA));
    if J1==0 then assert (betti syz family==betti syz gens I);
    (J1,family))

pruneFamily=method(Options=>{Verbose=>false})
pruneFamily(Ideal,Ideal,Matrix) := o -> (I,J,family) ->(
    -- Input: I, the ideal of the semigroup
    --       J ideal of flattening relations
    --       a matrix defining a family over kk[gens ring I|gens ring J]
    -- Output: J1 pruned flattening relations     
    --        family1 pruned family
    S:=ring I;
    kk:=coefficientRing S;
    SA:=kk[support family,Degrees=>apply(support family, m->degree m)];
    family1:=sub(family,SA);
    as:=drop(support family,numgens S);
    A:=kk[as,Degrees=>apply(as,m->degree m)];
    J1:=if o.Verbose then (<<"time for prune J = " <<flush<<endl;
	elapsedTime prune trim sub(J,A)) else (prune trim sub(J,A));
    if J1 == 0 or
     sub(gens J1,A)%(ideal gens A)^2== 0 then
     family2:=family1 else (
	if o.Verbose then (<<"time to reduce family1= "<<flush<<endl;
	    elapsedTime family2=family1%sub(J1,SA)) else (family2=family1%sub(J1,SA)));

    SA':=kk[support family2,Degrees=>apply(support family2, m->degree m)];   
    family3:=sub(family2,SA');
    as=drop(support family3,numgens S);
    A=kk[as,Degrees=>apply(as,m->degree m)];
    --assert(#support J1+#support I==#support family 3);
    J3:=prune trim sub(J1,A);
    (J3,family3)
    )

findPoint = method(Options => {Verbose => false})--,"IgnoreGrading" => true })
-- this assumes that
-- c == sub(ideal prune(ring c/c), ring c))
findPoint(Ideal) := o -> (J) -> (
    --need to be revisited, looks different than the corrected versions below
    c:= radical J;
    if degree c =!=degree J then
    <<"taking radicals makes a difference in findPoints"<<flush <<endl;
    S := ring c;
    kk := coefficientRing S;
    rc:=radical c;
    c1 := prune rc;  
    R := ring c;
    A1 := vars R % rc;
    if c1==0 then(
	point := sub(A1,random(R^1,R^(numgens R)));
	assert(sub(c, point) == 0);
	return sub(point,kk);
	);
    if o.Verbose then << "has to solve" <<flush<< endl;

    R1 := kk[gens ring c1];
    cR1 := radical sub(c1, R1);
    p := null;count := 2;
    if o.Verbose then (
    
    elapsedTime while (count < 10 and (
	p = first randomPoints(count,cR1);
	if p == {} then
            (<< "No point was found by randomPoints"<<endl; return null);
        product flatten p ==0)) do (count = count+1);
        if count == 10 then return null

    ) else (

    while (count < 10 and (
	p = first randomPoints(count,cR1);
	product flatten p ==0)) do(count = count+1);
    if count == 10 then return null
    );

    if p == {} then
    (<< "No point was found by randomPoints"<<endl; return null);

    point = sub(matrix{ p},kk);

    assert(sub(cR1, point) == 0);
    keyVars := apply(gens R1, x -> sub (x,S));
    subList := apply(#keyVars, i -> (keyVars_i => point_(0,i)));
    point1 := sub(sub(A1, subList), kk);
    assert(sub(c, point1) == 0);
return point1
)
-*
findPoint(Ideal,Number,Number) := o -> (J,a,b) -> ( -- a seems not to be used
    c:= radical J;
    if degree c =!=degree J then
    <<"taking radicals makes a difference in findPoints"<<flush <<endl;
    S := ring c;
    kk := coefficientRing S;
    rc:=radical c;
    c1 := prune rc;  
    R := ring c;
    A1 := vars R % rc;
    if c1==0 then(
	point := sub(A1,random(R^1,R^(numgens R)));
	assert(sub(c, point) == 0);
	return sub(point,kk);
	);
    if o.Verbose then << "has to solve" <<flush<< endl;

    R1 := kk[gens ring c1];
    cR1 := radical sub(c1, R1);
    p := {}; numberOfPoints := 1;
    if o.Verbose then (
	elapsedTime while (numberOfPoints < b and (
	   p = randomPoints(numberOfPoints,cR1,BruteForceAttempts => 0); 
	   p == {} or product first p ==0)) do (numberOfPoints = numberOfPoints+1);
        if numberOfPoints == b then(
           (<< "No point was found by randomPoints"<<endl; return null);
           return null);
    ) else (

    while (numberOfPoints < b and (
	p = randomPoints(numberOfPoints,cR1,BruteForceAttempts => 0); 
	p == {} or product first p ==0)) do (numberOfPoints = numberOfPoints+1);
    if numberOfPoints == b then return null;

    );

    point = sub(matrix{first p},kk);

    assert(sub(cR1, point) == 0);
    keyVars := apply(gens R1, x -> sub (x,S));
    subList := apply(#keyVars, i -> (keyVars_i => point_(0,i)));
    point1 := sub(sub(A1, subList), kk);
    assert(sub(c, point1) == 0);
return point1
)
*-

smoothnessWithReductions=method(Options=>{Verbose=>0,BaseField=>ZZ/nextPrime 10^4})

smoothnessWithReductions(Ideal) := o -> J -> (
    L:= flatten drop(degrees ring J,-1);
    St:= ring J;
    fiber:=sub(J,last gens St =>1);   
    if char St > 0 then kk:=coefficientRing St else kk=o.BaseField;
    ng:=numgens fiber;
        Sfinite:=kk[support fiber];
    fiberFinite:=sub(fiber,Sfinite);
    jac:=jacobian fiberFinite;
    if o.Verbose > 0 then <<"semigroup = "<< L << flush<<endl;
    I:=semigroupIdeal(L,"BaseField"=>o.BaseField);
    nL:=#L;
    assert(dim fiberFinite==1);

column := findCompleteIntersection(I,Strategy=>"front") ;
fewMinors := minors(#L-1,jac_column,Strategy=>Cofactor);
    singF := trim (fiberFinite +ideal(gens fewMinors %fiberFinite));
if singF == ideal 1_Sfinite then return true;
if o.Verbose>1 then <<"dim and degree singF = "<< (dim singF,degree singF) <<endl;

column = findCompleteIntersection(I,Strategy=>"back");
    fewMinors = minors(#L-1,jac_column,Strategy=>Cofactor);
    singF = trim (singF +ideal(gens fewMinors %fiberFinite));
if singF == ideal 1_Sfinite then return true;
if o.Verbose>1 then  <<"dim and degree singF = "<< (dim singF,degree singF) <<endl;
   
column = findCompleteIntersection(I,Strategy=>"random");
    fewMinors = minors(#L-1,jac_column,Strategy=>Cofactor);
    singF = trim (singF +ideal(gens fewMinors %fiberFinite));
if singF == ideal 1_Sfinite then return true;
if o.Verbose>0 then
<<"dim and degree singF = "<< (dim singF,degree singF) <<endl;

    if o.Verbose>1 then (
    <<"time to decompose: " <<endl;
    elapsedTime badPoints:=decompose singF;
    <<apply(badPoints,P->degree P)<<endl;
    ) else ( badPoints=decompose singF);
    SP:=null;jacP:=null;cok:=null;
    all(badPoints,P->(
	    if o.Verbose>1 then <<"degree P = " <<degree P <<endl;
	    SP=Sfinite/P;
    jacP=sub(jac,SP);
    cok=prune coker jacP;
    if o.Verbose>2 then (
	<< betti cok<<endl;);
    numgens cok == 1))
)


findCompleteIntersection=method(Options=>{Strategy=>"front"})

findCompleteIntersection(Ideal) := o -> I -> (
    ng:=numgens I;
    cd:=codim I;
    column := {};
    ci:= ideal (gens I)_column;ci1:=null;
   
    if o.Strategy=="front" then (
	a:=0;
	for i from 1 to cd do (
            while ( ci1=ci+ideal I_a;
                not codim ci1==i and a <ng-1) do (
		 a=a+1);
	    if a<ng then (column = append(column,a);
	    ci=ideal (gens I)_column;
	    if a <ng-1 then a=a+1;
	    --<<column<<endl;
	    );
	););

    if o.Strategy=="back" then (
	a=ng-1;
	for i from 1 to cd do (
	    while ( ci1=ci+ideal I_a;
                not codim ci1==i and a>0) do (
		 a=a-1);
	    if a>-1 then ((column = append(column,a);
	    ci=ideal (gens I)_column;);
	    if a>0 then a=a-1;
	    --<<column<<endl;
	    );
	);
    );
 
    if o.Strategy=="random" then (
       a= random ng;
       testValues:=toList(0..ng-1);
       for i from 1 to cd do (
	   while ( ci1=ci+ideal I_a;
                not codim ci1==i and #testValues >0) do (
		testValues=delete(a,testValues);
	        if #testValues>0 then a=testValues_(random( #testValues));
	    testValues=delete(a,testValues));
	    column = append(column,a);
	    ci=ideal (gens I)_column;
	    --<<column<<endl;	    
	    );
    );
    column
    )






improveFamily=method(Options=>{Verbose=>0})
improveFamily(Ideal) := o -> J -> (
    <<flush <<endl;
    L:= flatten drop(degrees ring J,-1);
    if o.Verbose >0 then << "semigroup = " << L <<endl;
    I:=semigroupIdeal(L,"BaseField"=>QQ);
    if o.Verbose >1 then elapsedTime (J1,family1):=getParameterFamily J else (
	(J1,family1)=getParameterFamily J);
    SJ := ring family1/sub(J1,ring family1);
    assert(betti syz sub(family1,SJ) == betti syz gens I);-- check flatness
    cJ1:=decompose J1;
    if o.Verbose >1 then <<"number of Components = "<<#cJ1 << endl;
    smooth:=null;fams:=null;fib0:=null;b:=null;
    while (
      fams=for J2 in cJ1 list (
	fib0=getOneParameterFamily(J,J2,family1,10);
	--"p=20 means one out of 20 coefficients is chosen to be nonzero"
	if o.Verbose >1 then (elapsedTime smooth=smoothnessWithReductions(fib0);
	     <<"smooth =" << smooth <<endl;) else (
	     smooth=smoothnessWithReductions(fib0); );
	if smooth then return fib0;
	(smooth,fib0));
      all(fams, (b,fib) ->not b)) do (<<"semigroup = " <<L <<" repeat" <<endl;);
    (b,fib0)=(select(fams,(b,fib)->b))_0;
    if o.Verbose >0 then <<"smooth = "<< b <<endl;
    fib0)





getRangeOfOneParameterFamily=method()
getRangeOfOneParameterFamily(Ideal):= J -> (
    Sz:=ring J;
    L:=flatten drop(degrees  Sz ,-1);
    I:=semigroupIdeal(L,"BaseField"=>QQ);
    (A,unfolding):=makeUnfolding I;
    SA:=ring unfolding;
    z:=last gens ring J;
    eq:=null;termsOfeq:=null;termsOfEq:=null;usedMonomials:=null;
    varsList:=flatten for i from 0 to rank source unfolding-1 list (
	eq=J_i;
	termsOfeq=transpose (coefficients(eq,Variables=>{z}))_1;
	termsOfEq=termsOfeq_{0..rank source termsOfeq-2};
	usedMonomials=sub((coefficients termsOfEq)_0,SA);
	(entries sub(contract(usedMonomials,unfolding_{i}),A))_0
    );
    sort unique(varsList/degree))


getParameterFamily=method()
getParameterFamily(Ideal):= J -> (
    Sz:=ring J;
    L:=flatten drop(degrees  Sz ,-1);
    I:=semigroupIdeal(L,"BaseField"=>QQ);
    (A,unfolding):=makeUnfolding I;
    SA:=ring unfolding;
    z:=last gens ring J;
    eq:=null;termsOfeq:=null;termsOfEq:=null;usedMonomials:=null;
    varsList:=flatten for i from 0 to rank source unfolding-1 list (
	eq=J_i;
	termsOfeq=transpose (coefficients(eq,Variables=>{z}))_1;
	termsOfEq=termsOfeq_{0..rank source termsOfeq-2};
	usedMonomials=sub((coefficients termsOfEq)_0,SA);
	(entries sub(contract(usedMonomials,unfolding_{i}),A))_0
    );
   posList:=apply(varsList,a->position(gens A,b->a==b));
   (A1,runf):=restrictedUnfolding(I,posList);
   J1:=trim flatteningRelations(I,A1,runf);
   fam1:=runf%sub(J1,ring runf);
   J2:=ideal prune(A1/J1) ;
   SJ:= ring fam1/sub(J2,ring fam1);
   assert(betti syz sub(fam1,SJ) == betti syz gens I);
   (J2,fam1)
   )

getOneParameterFamily=method(Options=>{CoeffSize=>1, Verbose => 0})
getOneParameterFamily(Ideal,Ideal,Matrix,ZZ) := o -> (J,J2,family1,p) -> (
	 family2:=family1%sub(J2,ring family1);
	 Sz := ring J;
	 c:=1;--o.CoeffSize;
	 SA:=QQ[support family2,Degrees=>apply(support family2,a->degree a)];
	 family2=sub(family2,SA);
	 I':=semigroupIdeal(flatten drop(degrees ring J,-1) ,"BaseField"=>QQ);
         S:=ring I';
	 if J2 != 0 then (
		
	    J3:=trim flatteningRelations(I',ring family2,family2);
	    if o.Verbose >0 then <<"codim J3 = " << codim J3 <<", numgens J3 = "<< numgens J3 <<endl;        
	    cd:=codim J3;
	    J3a:=ideal (gens J3)_{0..cd-1};
	    avars:=support J3;
	    Lvars:=subsets(avars,cd);
	    Lvarsc:=apply(Lvars, va->select(avars,av->not member(av,va)));
	    pos:=select(Lvarsc,va->gens J3a%sub(ideal va,ring J3a)==0);
	    eq1:=null; eq2:=null;
	    
	    posa:=select(pos,pos1-> (
		    eq1=sub(ideal apply(pos1,m->m-random(2*c-1)-c),ring J3);
		    eq2=ideal (gens J3a%sub(eq1,ring J3));
		    eq2 !=ideal(1_(ring eq2))));
	     <<"#pos = " <<#pos<<", #posa = "<<#posa<<endl;
	     if #posa == 0 then (<<"solving approach did not work" <<endl;return (false,null));
	     j:=0;
	     while (
		 eq1=sub(ideal apply( posa_j,m->m-random(2*c-1)-c),ring J3);
		 eq2=ideal (gens J3%sub(eq1,ring J3));
	         eq:=eq1+eq2;
		not all apply(support J3,m->dim ideal (m%eq)==-1) and j< #posa-1) do j=j+1;
	     if j == #posa then return (null);
	     if not gens J3%eq ==0 or dim eq == -1 then (
	         << "condition 2 of solving approach is not satisfied" <<endl;
		 return null;);
	     fiber1:=family2%sub(eq,ring family2);	    
	     ) else
	 fiber1=family2;
	 --gens ring family2
	 --support fiber1, gens ring family2
	     fiber:=sub(fiber1,vars S|
		    matrix { apply(
			    #gens ring family2 - numgens S
			    ,i->
			    if random(p)>1 then 0 else random(2*c-1)-c)}
		    );

	     fib:=ideal homogenize(sub(fiber, Sz),last gens Sz);

             assert(betti res fib == betti res J);
	     fib)

testBound=method(Options=>{Verbose=>0,CoeffSize=>1})
testBound(List,ZZ) := o -> (L,b) -> (
	range:=drop(makeRange(L,{1}),b);
        A:=getSmoothingFamily(L,range,CoeffSize=>o.CoeffSize)
	)

testCongruences=method(Options=>{Verbose=>0,CoeffSize=>1})
testCongruences(List,List) := o -> (L,congruences) -> (
	range:= makeRange(L,congruences);
        A:=getSmoothingFamily(L,range,CoeffSize=>o.CoeffSize)
	)

testRange=method(Options=>{Verbose=>0,CoeffSize=>1})
testRange(List,List) := o -> (L,range) -> (
        A:=getSmoothingFamily(L,range,CoeffSize=>o.CoeffSize)
	)


    




collectByBound=method(Options=>{Verbose=>0,CoeffSize=>1})

collectByBound(List,ZZ,String,String) := o -> (LL,b,done,doneData) -> (
    alreadyDone := getFromDisk (done);
    LLStillToDo:=select(LL,L->not member(L, alreadyDone));
    A:=null;L:=null;successes:=0;
    apply(#LLStillToDo, i -> (L=LLStillToDo_i;
	    <<endl;
	    << "case " << i <<" from " <<#LLStillToDo <<", semigroup = " << L <<endl;
	    elapsedTime A=testBound(L,b,CoeffSize=>o.CoeffSize);
	    if A_0 then ( successes=successes+1;
		if o.Verbose>0 then (<<"successes = "<<successes<<endl;);
		appendFamily(L,A_1,done,doneData))));
    )

collectByCongruences=method(Options=>{Verbose=>0,CoeffSize=>1})

collectByCongruences(List,List,String,String) := o -> (LL,congruences,done,doneData) -> (
    alreadyDone := getFromDisk (done);
    LLStillToDo:=select(LL,L->not member(L, alreadyDone));
    A:=null;L:=null;
    apply(#LLStillToDo, i -> (L=LLStillToDo_i;
	    <<endl;
	    << "case " << i <<" from " <<#LLStillToDo <<", semigroup = " << L <<endl;
	    elapsedTime A=testCongruences(L,congruences,CoeffSize=>o.CoeffSize);
	    if A_0 then appendFamily(L,A_1,done,doneData)));
    )

collectByRange=method(Options=>{Verbose=>0,CoeffSize=>1})

collectByRange(List,List,String,String) := o -> (LL,range,done,doneData) -> (
    alreadyDone := getFromDisk (done);
    LLStillToDo:=select(LL,L->not member(L, alreadyDone));
    A:=null;L:=null;
    apply(#LLStillToDo, i -> (L=LLStillToDo_i;
	    <<endl;
	    << "case " << i <<" from " <<#LLStillToDo <<", semigroup = " << L <<endl;
	    elapsedTime A=testRange(L,range,CoeffSize=>o.CoeffSize);
	    if A_0 then appendFamily(L,A_1,done,doneData)));
    )

toDoList=method()
toDoList(ZZ) := g -> (
    allg:=findSemigroups g;
    select(allg,L->not isKnownExample L)
    )

toDoList(ZZ,ZZ) := (m,g) -> (
    allg:=findSemigroups(m,g);
    select(allg,L->not isKnownExample L)
    )





solvingFlatteningRelations=method(Options=>{Verbose=>0,BaseField=> ZZ/nextPrime 10^7,
	CoeffSize=>1,CoeffBound=>10^4})
solvingFlatteningRelations(Ideal,Matrix,Ideal) := o -> (J3,family2,I) -> (
	        --SJ:= ring J3/J3;
		--assert(betti syz sub(family2,SJ) == betti syz gens I);-- check flatness
		cd:=codim J3;
		J3a:=ideal (gens J3)_{0..cd-1};
                c:=o.CoeffSize; 
		avars:=support J3;
		Lvars:=subsets(avars,cd);
		Lvarsc:=apply(Lvars, va->select(avars,av->not member(av,va)));
		pos:=select(Lvarsc,va->gens J3a%ideal va==0);
		eq1:=null; eq2:=null;
		posa:=select(pos,pos1-> (
			eq1=sub(ideal apply(pos1,m->m-random(2*c-1)-c),ring J3);
		        eq2=ideal (gens J3a%sub(eq1,ring J3));
			eq2 !=ideal(1_(ring eq2))));
		<<"# of coordinate linear subspace of the base = " <<#pos <<endl;
		<<"# of linear subsets which leading to a point ="<<#posa<<endl;
  --		posa=posa;
  --if #posa == 0 then (error"solving approach did not work"; <<endl;return (false,null));
                if #posa == 0 then (<<"solving approach did not work" <<endl;return (false,null));
	        j:=0;
		while (
		    eq1=sub(ideal apply( posa_j,m->m-random(2*c-1)-c),ring J3);
		    eq2=ideal (gens J3%sub(eq1,ring J3));
	            eq:=eq1+eq2;
		not all apply(support J3,m->dim ideal (m%eq)==-1) and j< #posa-1) do j=j+1;
	        if j == #posa then return (false,null);
		if not gens J3%eq ==0 or dim eq == -1 then (
		    << "condition 2 of solving approach is not satisfied" <<endl;
		    return (false,null););
		fiber1:=family2%sub(eq,ring family2);
		--error "debug";
		S := ring I;
		fiber1=sub(fiber1,vars S|matrix { apply(#gens ring family2-numgens ring I,i->random(2*c-1)-c)});
		(worked,fiber):=clearDenominators(fiber1,CoeffBound=>o.CoeffBound);
		--error " debug";
		return (worked,fiber)
		)

clearDenominators=method(Options=>{CoeffBound=> 10^4})
clearDenominators(Matrix) := o -> fiber1 -> (    
    factors:={2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 27, 30, 32, 36, 40, 45, 48, 54, 60, 64, 72, 80, 90, 96, 108, 120, 128,
      135, 144, 160, 180, 192, 216, 240, 256};
    r:=rank source fiber1;g:=null;j:=null;w:=null;ma:=null;g0:=null;
    wgs:=apply(r,i->(g0=fiber1_{i};g=g0;
	    j=0;
	    while (ma=max (apply(entries (coefficients g)_1_0,c->sub(c,ZZ))/abs);
	          ma > o.CoeffBound and j <#factors ) do (g=factors_j*g0; j=j+1);
            if j==#factors then w=false else w=true;
	    (w,ideal g)));
    worked := all apply(wgs,(w,g)->w);
    if not worked then <<"clearing denominators did not work"<<flush<<endl;
    if worked then return (worked,gens sum(wgs,(w,g)->ideal g)) else return (false,gens sum(wgs,(w,g)->ideal g))
    )

isSmoothingFamily=method(Options=>{Verbose=>0})
isSmoothingFamily(List,Ideal,Matrix,Ideal) := o -> (L,I,family1,J2) -> (
    family2:=family1%sub(J2,ring family1);
    J3:=trim flatteningRelations(I,ring family2,family2);
	    --elapsedTime point=findPoint J3;
    if o.Verbose >0 then <<" codim J3 = "<< codim J3 << ", numgens J3 = "<< numgens J3<< endl;
    if o.Verbose >0 then elapsedTime (point:=findPoint J3;<<" time to find a point:"<<endl;) else
                    point = findPoint J3
		    ;
    if class point === Nothing then return false;
    fiber1:=sub(family2,(vars ring family2)_{0..#L-1}|
		    point_{#L..rank source point -1});
    z:=symbol z;
    kk:=coefficientRing ring I;
    StFinite:=kk[gens ring I|{z}, Degrees=>L|{1}];	    
    fibFinite:=ideal homogenize(sub(fiber1,StFinite),last gens StFinite);
    assert(betti syz gens fibFinite == betti syz gens I);
    smooth:=smoothnessWithReductions(fibFinite);
    smooth)

     
getSmoothingFamily=method(Options=>{BaseField=> ZZ/(nextPrime 10^7),CoeffSize=>1,
	CoeffBound=>10^4,Verbose=>0})

getSmoothingFamily(List,ZZ) := o -> (L,b) -> (
    I:=semigroupIdeal(L,"BaseField"=>o.BaseField);
    S:=ring I;
    z:=symbol z;
    St:=QQ[gens S|{z}, Degrees=>degrees S|{1}];
    kk:=coefficientRing S;
    (A,unfolding):=makeUnfolding I;
    ma:=max flatten unique (gens A/degree);
    range:=toList(b+1..ma);
    restrictionList:=select(flatten gens A, m->member((degree m)_0, range));
    (J,family) := getFlatFamily(I,A,unfolding,restrictionList);
    (J1,family1) := pruneFamily(I,J,family);--,Verbose=>o.Verbose);
    if o.Verbose >1 then (<< "time to decompose:" <<endl;
	cJ1:= reverse decompose J1;) else (cJ1= reverse decompose J1;);
    ccJ1:=apply(cJ1,c->codim c);
    if o.Verbose >0 then (
    << "number of components = " <<#cJ1 << ", codimension of components = " <<ccJ1 <<endl;);
    good:=false;
    fiber:=matrix{{1_S}};fib:=null;fiber1:=null;pos:=null;posa:=null;fact1:=null;
    c:=o.CoeffSize;J3:=null;family2:=null; StFinite:=null;fibFinite:=null;J2:=null;
    comps:={}; 
    apply(#cJ1,k->( if not good then ( J2= cJ1_k;
		if o.Verbose>1 then <<flush<<endl;
		if isSmoothingFamily(L,I,family1,J2) then (
	    comps=append(comps,k);
            family2=family1%sub(J2,ring family1);
	    J3=trim flatteningRelations(I,ring family2,family2);
	    if o.Verbose>1 then <<"component number = "<< k <<", codim J3 = "
	    << codim J3 <<", numgens J3 = "<< numgens J3 <<endl;
	    if o.Verbose>1 then (elapsedTime if numgens J3 != 0 then (
		(worked,fiber):=solvingFlatteningRelations(J3,family2,I,CoeffBound=>o.CoeffBound,CoeffSize=>o.CoeffSize) ) else (
		fiber1=sub(family2,(vars ring family2)_{0..#L-1}|
		    matrix{ apply(numgens ring family2-#L,i->random(2*c-1)-c)});
		(worked,fiber)=clearDenominators(fiber1,CoeffBound=>o.CoeffBound));) else (
	        if numgens J3 != 0 then (
		(worked,fiber)=solvingFlatteningRelations(J3,family2,I,CoeffBound=>o.CoeffBound,CoeffSize=>o.CoeffSize) ) else (
		fiber1=sub(family2,(vars ring family2)_{0..#L-1}|
		    matrix{ apply(numgens ring family2-#L,i->random(2*c-1)-c)});
		(worked,fiber)=clearDenominators(fiber1,CoeffBound=>o.CoeffBound)););
            if worked then (
	    fib=ideal homogenize(sub(fiber,St),last gens St);
	    assert(betti syz gens fib == betti syz gens I);
	    StFinite:=kk[gens St, Degrees=>degrees St];
	    fibFinite=sub(fib,StFinite);
	    good=smoothnessWithReductions(fibFinite,Verbose=>o.Verbose);	    
	    );));
	));
    --error "debug";
    if o.Verbose >0 then <<" smoothing components numbers = " << comps <<endl;
    flat:=false;
    if good then if o.Verbose>2 then (elapsedTime flat=(betti syz gens fib==betti syz gens I);
	<<"flat = " << flat << endl;) else (flat=betti syz gens fib==betti syz gens I);
    if good and flat then (true,fib) else (false,null)
    )

getSmoothingFamily(List,List) := o -> (L,range) -> (
    I:=semigroupIdeal(L,"BaseField"=>o.BaseField);
    S:=ring I;
    z:=symbol z;
    St:=QQ[gens S|{z}, Degrees=>degrees S|{1}];
    kk:=coefficientRing S;
    (A,unfolding):=makeUnfolding I;
    restrictionList:=select(flatten gens A, m->member((degree m)_0, range));
    (J,family) := getFlatFamily(I,A,unfolding,restrictionList);
    (J1,family1) := pruneFamily(I,J,family);--,Verbose=>o.Verbose);
    if o.Verbose >0 then elapsedTime (cJ1:=decompose J1;<<"time to decompose J1 : "<<endl;) else
                    cJ1=decompose J1;
    cJ1=reverse cJ1;
    ccJ1:=apply(cJ1,c->codim c);
    if o.Verbose>1 then (
    << "number of components = " <<#cJ1 << ", codimension of components = " <<ccJ1 <<endl;
    );
    good:=false;
    fiber:=matrix{{1_S}};fib:=null;fiber1:=null;pos:=null;posa:=null;fact1:=null;
    c:=o.CoeffSize;J3:=null;family2:=null; StFinite:=null;fibFinite:=null;J2:=null;
    --J2=cJ1_1,c=2
    comps:={};
    apply(#cJ1,k->( if not good then ( J2= cJ1_k;
		if o.Verbose>0 then (<<flush<<endl;
		<<"component number = "<< k <<endl;);
		if isSmoothingFamily(L,I,family1,J2) then (
	    
	    comps=append(comps,k);
            family2=family1%sub(J2,ring family1);
	    if o.Verbose>0 then (
	    << "deformation weights = " << drop(support family2,#L)/degree <<endl;);
	    J3=trim flatteningRelations(I,ring family2,family2);
	    if o.Verbose>1 then (<<"codim J3 = "<< codim J3
		<<", numgens J3 = "<< numgens J3 <<endl;);
	    -*if codim J3 == 2 and numgens J3 ==3 then (
	    --<< "support J3 = " << toString support J3 <<flush <<endl;
	    --<< "ideal J3 = " << toString J3 <<flush<<endl;
	    fJ3:=res J3;
	    <<betti fJ3 <<flush<<endl;
	    << "Hilbert-Burch = " << fJ3.dd_2 << endl;
	    --J3a:=ideal fJ3.dd_2_{0};
	    --J3=trim ideal(gens J3 % J3a);
	    --assert(numgens J3==0);
	    --family2=family2%J3a;
	    );
	    *-
	    --if o.Verbose>1 then (
	             if numgens J3 != 0 then (
		(worked,fiber):=solvingFlatteningRelations(J3,family2,I,
		    CoeffBound=>o.CoeffBound,CoeffSize=>o.CoeffSize) ) else (
		fiber1=sub(family2,(vars ring family2)_{0..#L-1}|
		    matrix{ apply(numgens ring family2-#L,i->random(2*c-1)-c)});
		(worked,fiber)=clearDenominators(fiber1,CoeffBound=>o.CoeffBound));
            if worked then (
	    fib=ideal homogenize(sub(fiber,St),last gens St);
	    assert(betti syz gens fib == betti syz gens I);
	    StFinite:=kk[gens St, Degrees=>degrees St];
	    fibFinite=sub(fib,StFinite);
	    good=smoothnessWithReductions(fibFinite,Verbose=>o.Verbose);	    
	    );));
	));
    --error "debug";
    if o.Verbose >0 then (<<" smoothing components numbers = " << comps <<endl;
    if  good then flat:=elapsedTime (betti syz gens fib)==betti syz gens I;
	<<"flat = " << flat << endl) else
         if  good then flat =(betti syz gens fib)==betti syz gens I;
    if good and flat then (true,fib,comps) else (false,null,comps)
    )




    

makeRange=method(Options=>{Verbose=>false})
makeRange(List,List) := o -> (L,congruences) -> (
    I:= semigroupIdeal L;
    ma:= max flatten degrees source gens I;
    rangeall:=toList(1..ma);
    range := select(rangeall,d->not all(congruences, j-> not d%j==0));
    range)


appendFamily=method()

appendFamily(List,Ideal,String,String) := (L, fib, name, dataName) -> (       
    familyData:=openDatabaseOut dataName;
    familyData#(toString L|"ring") = toExternalString (ring fib); 
    familyData#(toString L|"ideal") = toString (fib);
    close familyData;
    openOutAppend name;
    name<<L;
    name << ", ";
    name<<close; 
    )
   

     
checkSmoothness=method(Options=>{Verbose=>0})	    
checkSmoothness(Ideal) := o -> fib -> (
    St:=ring fib;
    relJac:=(jacobian fib)^{0..numgens St-2};
    nL:=numgens St-1;
    ng:=numgens fib;
    countMax:=min(2*floor(binomial(ng,nL-1)/3),400);
    column:=toList(0..nL-2);
    mat:=relJac_column;
    fewMinors:=minors(nL-1,mat);
    singF:=ideal mingens (fib+fewMinors);
    if o.Verbose==1 then (
	elapsedTime singF=saturate(singF,last gens St);
	<<" numgens singF = " << numgens singF<<flush<<endl;
	) else (singF=saturate(singF,last gens St));
    columnSets:={column};
    count:=0;
    if o.Verbose==1 then (<<"time to check smoothness:"<<endl;
    elapsedTime while (not singF==ideal 1_St) and count<countMax do (
	count=count+1;
	while (
	    while (
		column=sort apply(nL-1,i->random(ng));
		#(unique column) < nL-1)
	    do ();
            member(column,columnSets))
	do ();
	columnSets=append(columnSets,column);
	--if o.Verbose then (<<"size of columnSets = " << #columnSets <<flush<<endl);
	mat=relJac_column;
	fewMinors=minors(nL-1,mat);
	singF=ideal mingens (singF+fewMinors);
	singF=saturate(singF,last gens St);
        if o.Verbose==1 then ( 
	<<" numgens singF = " << numgens singF<<flush<<endl;
	    );
	);) else (
      while (not singF==ideal 1_St) and count<countMax do (
	count=count+1;
	while (
	    while (
		column=sort apply(nL-1,i->random(ng));
		#(unique column) < nL-1)
	    do ();
            member(column,columnSets))
	do ();
	columnSets=append(columnSets,column);
	--if o.Verbose then (<<"size of columnSets = " << #columnSets <<flush<<endl);
	mat=relJac_column;
	fewMinors=minors(nL-1,mat);
	singF=ideal mingens (singF+fewMinors);
	singF=saturate(singF,last gens St);
        if o.Verbose==1 then ( 
	<<" numgens singF = " << numgens singF<<flush<<endl;
	    );
	););
    if singF==ideal 1_St then return true else (
    if numgens singF<=numgens St+1 then (
    singF=trim(singF+minors(nL-1,relJac%singF));
    singF=saturate(singF,last gens St);
    if o.Verbose==2 then <<" numgens singF = " << numgens singF<<flush<<endl;);
    if o.Verbose==1 then <<" numgens singF = " << numgens singF<<flush<<endl;
	if numgens singF ==1 then <<"singF = " <<singF<<endl;	    
        singF==ideal 1_St))


    -* Documentation section *-
beginDocumentation()
document {
Key => WeierstrassSemigroups,
Headline => "Compute smoothing families for Weierstrass semigroups",
   "This package contains methods to compute 
    one parameter, positively graded
    smoothing families over QQ for certain numerical semigroup rings.
    
    By Pinkham's Theorem a semigroup is a Weierstrass semigroup if and only if
    the semigroup ring has a graded smoothing in negative T^1 directions. ",

   PARA{},
     SUBSECTION "Deformations",
     UL{
	TO makeUnfolding,
	TO flatteningRelations,
	TO getFlatFamily,
	TO pruneFamily
        },
     SUBSECTION "Finding points",
     UL{
	TO solvingFlatteningRelations,
	TO clearDenominators,
	TO findPoint,
	TO getRangeOfOneParameterFamily,
	TO getParameterFamily,
	TO improveFamily,
	TO getSmoothingFamilyWithVersalDeformation
        },
    SUBSECTION "Smoothness",
    UL{
        TO smoothnessWithReductions,
	TO findCompleteIntersection
        },
    SUBSECTION "Collecting",
    UL{
        TO collectByBound,
	TO collectWithVersalDeformations,
	TO appendFamily,
	TO HowToStartCollecting
        },
    SUBSECTION "Checking flatness and smoothness of a database of families",
    UL{
        TO getListOfIdeals,
        TO checkFlatnessOfOneParameterFamilies,
	TO checkSmoothnessOfOneParameterFamilies,
        },
    SUBSECTION "non-Weierstrass semigroups",
    UL{
        TO give1683Format,
	TO satisfiesDegreeCondition1,
	TO satisfiesDegreeCondition2,
	TO displaySyzygyMatrices,
	TO hilbertBurchConditions,
	TO depthCondition1
        },
    SUBSECTION "References",
       "Pinkham, Henry C.,
       Deformations of algebraic varieties with G_m action,
       Ast{'e}risque textbf{20} (1974), pp 1 - 131,
       Soci{'e}t{'e} Math{'e}matique de France (SMF), Paris",
    
    SUBSECTION "SeeAlso",
    UL {
	TO NumericalSemigroups,
	 },
    
}

document {
Key => HowToStartCollecting,
Headline => "How to start collecting",
"We have installed two methods to start to collect smoothing families for a list LL of semigroups. ",

   PARA{},
     SUBSECTION "Collecting",
     UL{
	TO collectByBound,
	TO collectWithVersalDeformations,
	TO appendFamily},
   PARA{},
   " In both cases we have two files 'doneLL' and 'doneData' which contains
the cases already done. The first file 'doneLL' is a list of semigroups where we store
all semigroups in our collecting process where we found a smoothing one-parameter family over QQ.
The second file 'doneData' is a .dbm file containing a data base of rings and ideals.
To get these data we use:",

     SUBSECTION "Communicating with the hard disk",
        "X='doneLL'",PARA{},
	"Y='doneData.dbm'",PARA{},
	"openOutAppend X",PARA{},
	    "X << ',' ",PARA{},
        "X<<close;",PARA{},
	"doneLL=getFromDisk X;#doneLL",PARA{},
	"Y=openDatabase Xdbm",PARA{},
	"#keys Y",PARA{},
	"keys Y",PARA{},
	"listOfIdeals=apply(doneLL,L->(R=value Y#(toString L|'ring');
	        I=value (Y#(toString L|ideal))));",PARA{},
        "close Y",PARA{},
 SUBSECTION "Continue collecting",
      "LL=toDoList 13",PARA{},
      "b=12;collectByBound(LL,b,'doneLL','dataData.dbm' ",PARA{},
      "or with",PARA{},
      "collectWithVersalDeformations(LL,b,'doneLL','dataData.dbm' ",PARA{},
      "one can continue the collecting. One can interrupt the collecting any time, 
       and continue later without losing data." ,
 SUBSECTION "Replacing quotes",
     " In the above the simple quotes ' have to be replaced by the double quotes, 
       which indicate the begin and end of a string",


    }





doc ///
Key
 restrictedUnfolding
 (restrictedUnfolding, Ideal, List)
Headline
 Compute a restricted unfolding
Usage
 (A,runfolding)=restrictedUnfolding(I,positionList)
Inputs
 I:Ideal
   ideal of semigroup ring
 positionList:List
   list of positions of the unfolding parameters to be used in the restricted unfolding
Outputs
 A: Ring
   coordinate ring of the restricted unfolding
 runfolding: Matrix
   equations of the restricted unfolding
Description
  Text 
   Given ideal c the functions adds random linear equations L to c to obtain
   1-dimensional ideal. Since the ground field is finite, decompose the ideal c+L
   will lead to a point with positive probability. Thus repeating will lead to success.
  Example
   L={5,6,8}
   I=ideal semigroupRing(L,"BaseField"=>ZZ/nextPrime 10^4);
   (A,unfolding)=makeUnfolding I;
   gens A/degree
   numgens A
   positionList=toList(0..30)
   (A,runfolding)=restrictedUnfolding(I,positionList)
   (J,family)=getFlatFamily(I,A,runfolding)
   support runfolding
   support family
///



doc ///
Key
 makeUnfolding
 (makeUnfolding, Ideal)
 (makeUnfolding, List) 
 [makeUnfolding, BaseField]
 [makeUnfolding, Verbose ]
Headline
 Makes the universal homogeneous unfolding of an ideal with positive degree parameters
Usage
 (A,unfolding) = makeUnfolding I
 (A,unfolding) = makeUnfolding sgrp
Inputs
 I:Ideal
 sgrp:List
  generators of a semigroup
Outputs
 A: Ring
   algebra of unfolding parameters
 unfolding: Matrix
   equations of the unfolding
Description
  Text
   Given a (quasi)homogeneous ideal in a ring S = kk[x_0..x_n]
   the function creates a positively graded polynomial ring A = kk[a_{i,j}]
   and computes the unfolding of I as an ideal 
   of SA = kk[x_0..x_n, a_{i,j}]. This can be used as a step in computing the
   semi-universal deformation of the affine cone defined by I.

   In the case of

   makeUnfolding sgrp

   the routine first forms the ideal of the semigroup ring, and applies makeUnfolding to this.
  Example
   L={4,5,7}
   I := semigroupIdeal L;
   (A,unfolding):= makeUnfolding I;
   S=ring I
   fI=res I
   degs=flatten (gens A/degree)
   n=floor(max degs/2+3)
   restricted=ideal select(gens A, y-> (degree y)_0<n);
   SA=ring unfolding
   runfolding=unfolding%sub(restricted,SA);
   transpose runfolding
   J=flatteningRelations(I,A,runfolding);
   cJ=decompose J;#cJ
   ideal prune (A/J)
   family=runfolding%sub(J,SA);
  Text
   This is a flat family!
  Example
   betti res ideal family == betti res I
   fiber=ideal sub(family,vars S|random(S^1,S^(numgens A)));
   singFiber=radical ideal gens gb (fiber+minors(codim I,jacobian fiber))
  Text
   Thus the family is a smoothing of S/I so
   the semigroup L in the example is a Weierstrass semigroup by Pinkham's thesis.
SeeAlso
 flatteningRelations
///

doc ///
Key
 flatteningRelations
 (flatteningRelations, Ideal, Ring, Matrix)
Headline
 Compute the flattening relations of an unfolding
Usage
 J= flatteningRelation(I,A,unfolding)
Inputs
 I:Ideal
  homogeneous with respect to a possibly nonstandard NN-grading
 A: Ring
  the ring of parameters of the unfolding
 unfolding: Matrix
  an unfolding of gens I
Outputs
 J: Ideal
   of A
Description
  Text
   Given the tuple (I,A,unfolding) the function computes the flattening relations
   via the set of Buchberger test syzygies.
   The procedure terminates since the parameters
   of A have positive degree, and the unfolding is homogeneous.
  Example
   L={4,6,7}
   I = trim semigroupIdeal L;
   (A,unfolding)=makeUnfolding I
   S=ring I
   fI=res I
   degs=flatten (gens A/degree)
   n=floor(max degs/2)+3
   restricted=ideal select(gens A, y-> (degree y)_0<n);
   SA=ring unfolding
   runfolding=unfolding%sub(restricted,SA);
   transpose runfolding
   J=flatteningRelations(I,A,runfolding);
   cJ=decompose J;#cJ
   ideal prune (A/J)
   family=runfolding%sub(J,SA);
   betti res ideal family == betti res I
  Text
   Thus this is a flat family!
  Example
   fiber=ideal sub(family,vars S|random(S^1,S^(numgens A)));
   singFiber=radical ideal gens gb (fiber+minors(codim I,jacobian fiber))
  Text
   Thus the family is a smoothing of S/I so
   the semigroup L in the example is a Weierstrass semigroup by Pinkham's thesis.

SeeAlso
 makeUnfolding
 ///


     doc ///
       Key
        getFlatFamily
	prepareInitialPositionList
	(prepareInitialPositionList,List,ZZ)
        (getFlatFamily, Ideal,Ring,Matrix)
        (getFlatFamily, Ideal,Ring,Matrix,List)
        [getFlatFamily, BaseField]
        [getFlatFamily, Verbose]
	[prepareInitialPositionList, Verbose]
       Headline
        Compute the flat family depending on a subset of parameters of the universal unfolding
       Usage
        (J,family)=getFlatFamily(I,A,runfolding)
	(J,family)=getFlatFamily(I,A,unfolding,variableList)
	initialList=prepareInitialPositionList(L,b)
       Inputs
        I:Ideal
         of a semigroup ring
	A:Ring
	 coordinate ring of the base of the restricted unfolding
        runfolding:Matrix
	 equations of the restricted unfolding
        variableList: List
	 list of variables to be used in the restricted unfolding
	b:ZZ
	 bound on the degree of the parameters
       Outputs
        J: Ideal
	 ideal of flattening relations
	family: Matrix
	 equations of the reduced equations
	initialList: List
	  list of positions of the desired variables
       Description
        Text
	 We compute the flattening relations.
	Example
	 L = {4,5,6}
	 genus L
	 I=ideal semigroupRing(L,"BaseField"=>ZZ/nextPrime 10^4)
	 (A,unfolding)=makeUnfolding I;
	 (J,family)=getFlatFamily(I,A,unfolding);
	 betti J
	 support unfolding
	 support family
	 family_(0,0)
	 gens ring family
	 --ramdomFiber(I,J,family)
	 support family
	 support family /degree
        Text
	 In the second version we restrict to a subset of the variables
	Example
	 b=5
	 initialList=prepareInitialPositionList(L,b)
	 as = apply(numgens I,i-> drop(support unfolding_{i},#L))
         as1=apply(flatten as,m->sub(m,A))
	 restrictionList=as1_initialList
	 (J1,family1)=getFlatFamily(I,A,unfolding,restrictionList)
	 (J2,family2)=pruneFamily(I,J1,family1)
	 --isARandomFiberSmooth(I,J2,family2)
      SeeAlso
	 makeUnfolding
	 flatteningRelations
	 getFlatFamily
	 
    ///

     doc ///
       Key
        pruneFamily
        (pruneFamily,Ideal,Ideal,Matrix)
	[pruneFamily,Verbose]
       Headline
        Present the family with fewest number of variables
       Usage
        (J1,family1)=pruneFamily(I,J,family)
       Inputs
        I:Ideal
         of a semigroup ring
	J:Ideal
	 ideal of flattening relations of the family 
        family:Matrix
	 equations of the family
       Outputs
        J1: Ideal
	 pruned ideal of J
	family1: Matrix
	 equations of reduced family
       Description
        Text
	 If a generator of J has a variable as lead term then
	 this variable can be removed from the presentation of (ring J/J).
	 At the same time we remove this variable from the equations of
	 the family.	 
	Example
	 L={5,6,8,9}
         genus L
	 I=ideal semigroupRing(L,"BaseField"=>ZZ/nextPrime 10^4);
	 (A,unfolding)=makeUnfolding I;
	 b = genus L-1; 
	 as=apply(numgens I,i->a=drop(support unfolding_{i},#L));
	 rL=apply(#as,i->select(as_i,m->(degree m)_0> b));
	 restrictionList=apply(flatten rL,m->sub(m,A));
	 elapsedTime (J,family)=getFlatFamily(I,A,unfolding,restrictionList);
	 leadTerm J
	 (J1,family1)=pruneFamily(I,J,family);
	 leadTerm J1	 
      SeeAlso
	 makeUnfolding
	 flatteningRelations
	 getFlatFamily
	 prune
    ///



doc ///
Key
 findPoint
 (findPoint, Ideal)
 [findPoint, Verbose ]
Headline
 Find a kk-rational point in a variety
Usage
 point=findPoint c
Inputs
 I:Ideal
Outputs
 B: Matrix
   coordinates of a point in the finite ground field
Description
  Text 
   Given ideal c the functions adds random linear equations L to c to obtain
   1-dimensional ideal. Since the ground field is finite, decompose the ideal c+L
   will lead to a point with positive probability. Thus repeating will lead to success.
  Example
    kk=ZZ/101
    R=kk[x_0..x_6]
    c=ideal random(R^1,R^{2:-1,2:-2})
    B=findPoint c
    sub(c,B)==0
 
///



doc ///
Key
   smoothnessWithReductions
   (smoothnessWithReductions, Ideal)
   [smoothnessWithReductions,BaseField]
   [smoothnessWithReductions,Verbose]
Headline
   Check smoothness by using reductions to points
Usage
   answer = smoothnessWithReductions J  
Inputs
   J:Ideal
     of a one parameter family 
Outputs
    answer:Boolean
      true if the fiber is smooth
Description
  Text
    Given a homogeneous ideal J in S[z]=kk[x,z] which defines a flat family of affine curves
    we check smoothness of the general fiber at z=1.
    The basic idea is to compute some minors of the jacobian matrix which
    intersect the curve in a zero-dimensional scheme singF.
    Then using decompose singF to reduce the check at points of the finitely
    many maximal ideals p. Since Sp=S/p is a field the rank of the jacobian matrix restricted to Sp
    can be computed by checking the number of generators of the cokernel, since cokernel represents the Zariski tangent
    space at p of the original curve.
    This is much cheaper then computing all or enough minors of the jacobian matrix.
  Example
    L={5,6,8}
    genus L
    R=QQ[x_0, x_1, x_3, z,Degrees=>{5, 6, 8, 1}]
    J=ideal(x_0^2*x_1-x_3^2-x_0^2*z^6-x_3*z^8-x_1*z^10+z^16,x_1^3-x_0^2*x_3-x_1^2*z^6-x_0*x_1*z^7-x_0^2*z^8-x_3*z^10-x_1*z^12+x_0*z^13,x_0^4-x_1^2*x_3+x_0*x_3*z^7+x_3*z^12-z^20)
    elapsedTime smoothnessWithReductions(J,Verbose=>2)
  Text
    The intermediate output dim and degree singF = (0, 4) says that after computing some
    minors of the jacobian matrix, we detect that the curve is smooth away
    from the zero dimensional scheme defined by singF of degree 4.
  Text
    The function checkSmoothness takes longer, some times much longer.
    
SeeAlso


///

doc ///
Key
   findCompleteIntersection
   (findCompleteIntersection,Ideal)
   [findCompleteIntersection,Strategy]
Headline
   Find complete intersection defined by some of the generators
Usage
   column = findCompleteIntersection I  
Inputs
   I:Ideal
     a semigroup ideal
Outputs
    column:List
      a list of codim I generators of the ideal I which defines a complete intersection 
Description
  Text
    We pick positions one by one generators of I, which increase the codimension by one until reached codim I.
    
  Example   
    L={7, 8, 17, 19, 20}
    I=semigroupIdeal L
    numgens I
    columnFront=findCompleteIntersection I
    ci=ideal (gens I)_columnFront
    codim ci == codim I
  Text
    There are three strategies "front", "back" and "random".
  Example
    columnBack=findCompleteIntersection(I,Strategy=>"back")
    ci2=ideal (gens I)_columnBack
    codim ci2 == codim I
    columnRandom=findCompleteIntersection(I,Strategy=>"random")
    ci3=ideal (gens I)_columnRandom
    codim ci3 == codim I

SeeAlso
   semigroupIdeal

Caveat
  It is possible that the function does not find a complete intersection of generators.
  In that case a too short List is returned.
///
-* for CannedExample in improveFamily
  Example
    R=QQ[x_0..x_1, x_3, x_5..x_6, z, Degrees => {7..8, 17, 19..20, 1}]
    J=ideal(x_1^3-x_0*x_3+x_1*z^16+x_0*z^17,x_1*x_5-x_0*x_6+x_0^2*z^13+x_1*z^19-x_0*z^20,x_0^4-x_1*x_6+x_0*x_1*z^13
      +x_0^2*z^14-x_1*z^20,x_1^2*x_3-x_0^2*x_5-x_5*z^14+x_3*z^16+x_1^2*z^17+x_0*x_1*z^18+x_0^2*z^19,x_3^2-x_0^2*x_6
      +x_0^3*z^13-x_6*z^14+x_1^2*z^18+2*x_0*x_1*z^19-x_0^2*z^20+x_0*z^27-2*z^34,x_3*x_5-x_1^2*x_6+x_0*x_1^2*z^13-x_
      6*z^16-x_5*z^17+x_3*z^19-x_1^2*z^20+x_0*z^29-2*z^36,x_0^3*x_1^2-x_3*x_6+x_0*x_3*z^13+x_0*x_1^2*z^14+x_0^3*z^
      16+x_6*z^17-x_3*z^20+z^37,x_0^3*x_3-x_5^2+x_0^2*x_1*z^16+x_0^3*z^17+x_6*z^18-x_0*z^31+2*z^38,x_0^2*x_1*x_3-x_
      5*x_6+x_0*x_5*z^13+x_0*x_1^2*z^16+x_0^2*x_1*z^17+x_0^3*z^18+x_6*z^19-x_5*z^20+z^39,x_0^3*x_5-x_6^2+2*x_0*x_6*
      z^13+x_0*x_5*z^14+x_0^3*z^19-2*x_6*z^20-x_0^2*z^26+3*x_0*z^33-z^40)
    L=flatten drop(degrees R,-1)
    J1=improveFamily(J)
    J_*/size
    J1_*/size
    (M,C)=coefficients gens J;
    unique (entries flatten C)_0
    (M1,C1)=coefficients gens J1;
    unique (entries flatten C1)_0
*-
doc ///
Key
   improveFamily
   (improveFamily,Ideal)
   [improveFamily,Verbose]
Headline
   Find a 1-parameter smoothing family with perhaps smaller number of terms and coefficients
Usage
   J1 = improveFamily J  
Inputs
   J:Ideal
     a one-parameter smoothing family of a semigroup ideal
Outputs
   J:Ideal
     a one-parameter smoothing family of a semigroup ideal
Description
  Text
    We first compute the flat family of ideals which uses the same terms as J
    using getParameterFamily.
    We then choose a point in a smoothing component of the base which uses hopefully fewer terms and
    smaller coefficients by using getOneParameterFamily
    
  CannedExample
    i1 : R=QQ[x_0..x_1, x_3, x_5..x_6, z, Degrees => {7..8, 17, 19..20, 1}]

    o1 = R

    o1 : PolynomialRing
    i2 : J=ideal(x_1^3-x_0*x_3+x_1*z^16+x_0*z^17,x_1*x_5-x_0*x_6+x_0^2*z^13+x_1*z^19-x_0*z^20,x_0^4-x_1*x_6+x_0*x_1*z^13
       +x_0^2*z^14-x_1*z^20,x_1^2*x_3-x_0^2*x_5-x_5*z^14+x_3*z^16+x_1^2*z^17+x_0*x_1*z^18+x_0^2*z^19,x_3^2-x_0^2*x_6
       +x_0^3*z^13-x_6*z^14+x_1^2*z^18+2*x_0*x_1*z^19-x_0^2*z^20+x_0*z^27-2*z^34,x_3*x_5-x_1^2*x_6+x_0*x_1^2*z^13-x_
       6*z^16-x_5*z^17+x_3*z^19-x_1^2*z^20+x_0*z^29-2*z^36,x_0^3*x_1^2-x_3*x_6+x_0*x_3*z^13+x_0*x_1^2*z^14+x_0^3*z^
       16+x_6*z^17-x_3*z^20+z^37,x_0^3*x_3-x_5^2+x_0^2*x_1*z^16+x_0^3*z^17+x_6*z^18-x_0*z^31+2*z^38,x_0^2*x_1*x_3-x_
       5*x_6+x_0*x_5*z^13+x_0*x_1^2*z^16+x_0^2*x_1*z^17+x_0^3*z^18+x_6*z^19-x_5*z^20+z^39,x_0^3*x_5-x_6^2+2*x_0*x_6*
       z^13+x_0*x_5*z^14+x_0^3*z^19-2*x_6*z^20-x_0^2*z^26+3*x_0*z^33-z^40)

                 3             16      17                 2 13      19      20 
    o2 = ideal (x  - x x  + x z   + x z  , x x  - x x  + x z   + x z   - x z  ,
                 1    0 3    1       0      1 5    0 6    0       1       0    
     ------------------------------------------------------------------------
      4               13    2 14      20   2      2        14      16    2 17
     x  - x x  + x x z   + x z   - x z  , x x  - x x  - x z   + x z   + x z  
      0    1 6    0 1       0       1      1 3    0 5    5       3       1   
     ------------------------------------------------------------------------
            18    2 19   2    2      3 13      14    2 18         19    2 20
     + x x z   + x z  , x  - x x  + x z   - x z   + x z   + 2x x z   - x z  
        0 1       0      3    0 6    0       6       1        0 1       0   
     ------------------------------------------------------------------------
          27     34          2        2 13      16      17      19    2 20  
     + x z   - 2z  , x x  - x x  + x x z   - x z   - x z   + x z   - x z   +
        0             3 5    1 6    0 1       6       5       3       1     
     ------------------------------------------------------------------------
        29     36   3 2               13      2 14    3 16      17      20  
     x z   - 2z  , x x  - x x  + x x z   + x x z   + x z   + x z   - x z   +
      0             0 1    3 6    0 3       0 1       0       6       3     
     ------------------------------------------------------------------------
      37   3      2    2   16    3 17      18      31     38   2             
     z  , x x  - x  + x x z   + x z   + x z   - x z   + 2z  , x x x  - x x  +
           0 3    5    0 1       0       6       0             0 1 3    5 6  
     ------------------------------------------------------------------------
          13      2 16    2   17    3 18      19      20    39   3      2  
     x x z   + x x z   + x x z   + x z   + x z   - x z   + z  , x x  - x  +
      0 5       0 1       0 1       0       6       5            0 5    6  
     ------------------------------------------------------------------------
           13        14    3 19       20    2 26       33    40
     2x x z   + x x z   + x z   - 2x z   - x z   + 3x z   - z  )
       0 6       0 5       0        6       0        0

    o2 : Ideal of R
    i3 : L=flatten drop(degrees R,-1)

    o3 = {7, 8, 17, 19, 20}

    o3 : List
    i4 : J1=improveFamily(J)

    #pos = 1, #posa = 1
    #pos = 2, #posa = 2

                 3             16      17                 2 13      19   4       
    o4 = ideal (x  - x x  + x z   + x z  , x x  - x x  + x z   + x z  , x  - x x 
                  1    0 3    1       0      1 5    0 6    0       1      0    1 6
     ------------------------------------------------------------------------
            13    2 14   2      2        14      16    2 17        18  
     + x x z   + x z  , x x  - x x  - x z   + x z   + x z   + x x z   +
        0 1       0      1 3    0 5    5       3       1       0 1     
     ------------------------------------------------------------------------
      2 19   2    2      3 13      14    2 18         19      27    34      
     x z  , x  - x x  + x z   - x z   + x z   + 2x x z   + x z   - z  , x x 
      0      3    0 6    0       6       1        0 1       0            3 5
     ------------------------------------------------------------------------
        2        2 13      16      17      19      29    36   3 2         
     - x x  + x x z   - x z   - x z   + x z   + x z   - z  , x x  - x x  +
        1 6    0 1       6       5       3       0            0 1    3 6  
     ------------------------------------------------------------------------
          13      2 14    3 16      17   3      2    2   16    3 17      18  
     x x z   + x x z   + x z   + x z  , x x  - x  + x x z   + x z   + x z   -
      0 3       0 1       0       6      0 3    5    0 1       0       6     
     ------------------------------------------------------------------------
        31    38   2                   13      2 16    2   17    3 18  
     x z   + z  , x x x  - x x  + x x z   + x x z   + x x z   + x z   +
      0            0 1 3    5 6    0 5       0 1       0 1       0     
     ------------------------------------------------------------------------
        19   3      2         13        14    3 19    2 26      33
     x z  , x x  - x  + 2x x z   + x x z   + x z   - x z   + x z  )
      6      0 5    6     0 6       0 5       0       0       0

    o4 : Ideal of R
    i5 : J_*/size

    o5 = {4, 5, 5, 7, 9, 9, 8, 7, 9, 9}

    o5 : List
    i6 : J1_*/size

    o6 = {4, 4, 4, 7, 8, 8, 6, 7, 7, 7}

    o6 : List
    i7 : (M,C)=coefficients gens J;
    i8 : unique (entries flatten C)_0

    o8 = {0, 1, -1, 2, -2, 3}

    o8 : List
    i9 : (M1,C1)=coefficients gens J1;
    i10 : unique (entries flatten C1)_0

    o10 = {0, 1, -1, 2}

    o10 : List

Caveat
  It is possible that the function does not find a smooth fiber, which results in a message
  that these example needs to be repeated.
    
SeeAlso
   getParameterFamily
   getOneParameterFamily

///
-* for CannedExample in getParameterFamily
  Example
    R=QQ[x_0..x_1, x_3, x_5..x_6, z, Degrees => {7..8, 17, 19..20, 1}]
    J=ideal(x_1^3-x_0*x_3+x_1*z^16+x_0*z^17,x_1*x_5-x_0*x_6+x_0^2*z^13+x_1*z^19-x_0*z^20,x_0^4-x_1*x_6+x_0*x_1*z^13
      +x_0^2*z^14-x_1*z^20,x_1^2*x_3-x_0^2*x_5-x_5*z^14+x_3*z^16+x_1^2*z^17+x_0*x_1*z^18+x_0^2*z^19,x_3^2-x_0^2*x_6
      +x_0^3*z^13-x_6*z^14+x_1^2*z^18+2*x_0*x_1*z^19-x_0^2*z^20+x_0*z^27-2*z^34,x_3*x_5-x_1^2*x_6+x_0*x_1^2*z^13-x_
      6*z^16-x_5*z^17+x_3*z^19-x_1^2*z^20+x_0*z^29-2*z^36,x_0^3*x_1^2-x_3*x_6+x_0*x_3*z^13+x_0*x_1^2*z^14+x_0^3*z^
      16+x_6*z^17-x_3*z^20+z^37,x_0^3*x_3-x_5^2+x_0^2*x_1*z^16+x_0^3*z^17+x_6*z^18-x_0*z^31+2*z^38,x_0^2*x_1*x_3-x_
      5*x_6+x_0*x_5*z^13+x_0*x_1^2*z^16+x_0^2*x_1*z^17+x_0^3*z^18+x_6*z^19-x_5*z^20+z^39,x_0^3*x_5-x_6^2+2*x_0*x_6*
      z^13+x_0*x_5*z^14+x_0^3*z^19-2*x_6*z^20-x_0^2*z^26+3*x_0*z^33-z^40);
    L=flatten drop(degrees R,-1)
    (base,family)=getParameterFamily J;
    numgens base
    cbase=decompose base
    J_0
    family_(0,0)
    J_*/size
    (ideal family)_*/size
    J_4
    family_(0,4)
    support family
    support family/degree 

*-


doc ///
Key
   getParameterFamily
   (getParameterFamily,Ideal)
Headline
   Compute the parametric family which uses the same terms as J
Usage
   (base,family) = getParameterFamily J  
Inputs
   J:Ideal
     of a one-parameter smoothing family of a semigroup ideal
Outputs
   base:Ideal
     of flatteness relations
   family:Matrix
     a matrix containing the generators of the parametric family   
Description
  Text
    We compute the flat family of ideals which uses the same terms as J.
    The ideal base contains the flatness relations for the coefficients of the family.        
  CannedExample    
    i1 : R=QQ[x_0..x_1, x_3, x_5..x_6, z, Degrees => {7..8, 17, 19..20, 1}]

    o1 = R

    o1 : PolynomialRing
    i2 : J=ideal(x_1^3-x_0*x_3+x_1*z^16+x_0*z^17,x_1*x_5-x_0*x_6+x_0^2*z^13+x_1*z^19-x_0*z^20,x_0^4-x_1*x_6+x_0*x_1*z^13
       +x_0^2*z^14-x_1*z^20,x_1^2*x_3-x_0^2*x_5-x_5*z^14+x_3*z^16+x_1^2*z^17+x_0*x_1*z^18+x_0^2*z^19,x_3^2-x_0^2*x_6
       +x_0^3*z^13-x_6*z^14+x_1^2*z^18+2*x_0*x_1*z^19-x_0^2*z^20+x_0*z^27-2*z^34,x_3*x_5-x_1^2*x_6+x_0*x_1^2*z^13-x_
       6*z^16-x_5*z^17+x_3*z^19-x_1^2*z^20+x_0*z^29-2*z^36,x_0^3*x_1^2-x_3*x_6+x_0*x_3*z^13+x_0*x_1^2*z^14+x_0^3*z^
       16+x_6*z^17-x_3*z^20+z^37,x_0^3*x_3-x_5^2+x_0^2*x_1*z^16+x_0^3*z^17+x_6*z^18-x_0*z^31+2*z^38,x_0^2*x_1*x_3-x_
       5*x_6+x_0*x_5*z^13+x_0*x_1^2*z^16+x_0^2*x_1*z^17+x_0^3*z^18+x_6*z^19-x_5*z^20+z^39,x_0^3*x_5-x_6^2+2*x_0*x_6*
       z^13+x_0*x_5*z^14+x_0^3*z^19-2*x_6*z^20-x_0^2*z^26+3*x_0*z^33-z^40)

                 3             16      17                 2 13      19      20 
    o2 = ideal (x  - x x  + x z   + x z  , x x  - x x  + x z   + x z   - x z  ,
                 1    0 3    1       0      1 5    0 6    0       1       0    
     ------------------------------------------------------------------------
      4               13    2 14      20   2      2        14      16    2 17
     x  - x x  + x x z   + x z   - x z  , x x  - x x  - x z   + x z   + x z  
      0    1 6    0 1       0       1      1 3    0 5    5       3       1   
     ------------------------------------------------------------------------
            18    2 19   2    2      3 13      14    2 18         19    2 20
     + x x z   + x z  , x  - x x  + x z   - x z   + x z   + 2x x z   - x z  
        0 1       0      3    0 6    0       6       1        0 1       0   
     ------------------------------------------------------------------------
          27     34          2        2 13      16      17      19    2 20  
     + x z   - 2z  , x x  - x x  + x x z   - x z   - x z   + x z   - x z   +
        0             3 5    1 6    0 1       6       5       3       1     
     ------------------------------------------------------------------------
        29     36   3 2               13      2 14    3 16      17      20  
     x z   - 2z  , x x  - x x  + x x z   + x x z   + x z   + x z   - x z   +
      0             0 1    3 6    0 3       0 1       0       6       3     
     ------------------------------------------------------------------------
      37   3      2    2   16    3 17      18      31     38   2             
     z  , x x  - x  + x x z   + x z   + x z   - x z   + 2z  , x x x  - x x  +
           0 3    5    0 1       0       6       0             0 1 3    5 6  
     ------------------------------------------------------------------------
          13      2 16    2   17    3 18      19      20    39   3      2  
     x x z   + x x z   + x x z   + x z   + x z   - x z   + z  , x x  - x  +
      0 5       0 1       0 1       0       6       5            0 5    6  
     ------------------------------------------------------------------------
           13        14    3 19       20    2 26       33    40
     2x x z   + x x z   + x z   - 2x z   - x z   + 3x z   - z  )
       0 6       0 5       0        6       0        0

    o2 : Ideal of R
    i3 : L=flatten drop(degrees R,-1)

    o3 = {7, 8, 17, 19, 20}

    o3 : List
    i4 : (base,family)=getParameterFamily J;
    i5 : numgens base

    o5 = 4
    i6 : cbase=decompose base

                                                                            
    o6 = {ideal (a      , a      , a      ), ideal (a      a       - a      a   
                 {1, 3}   {2, 3}   {0, 2}           {0, 2} {2, 3}    {0, 1} {1,
     ------------------------------------------------------------------------
                                           2
       , a      a       - a      a      , a       - a      a      , a   
     3}   {3, 4} {2, 3}    {1, 2} {1, 3}   {0, 2}    {1, 2} {1, 3}   {0,
     ------------------------------------------------------------------------
       a       - a      a      , a      a       - a      a      )}
     1} {0, 2}    {1, 2} {2, 3}   {3, 4} {0, 1}    {1, 2} {0, 2}

    o6 : List
    i7 : J_0

          3             16      17
    o7 = x  - x x  + x z   + x z
          1    0 3    1       0

    o7 : R
    i8 : family_(0,0)

         3
   o8 = x  - x x  + x a       + x a
         1    0 3    0 {0, 1}    1 {0, 2}

   o8 : QQ[x ..x , x , x ..x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a      , a      , a       , a       , a      , a      , a      , a      , a       , a       , a      , a      , a      , a       , a       , a       , a       , a      , a      , a      ]
            0   1   3   5   6   {9, 0}   {8, 0}   {7, 0}   {6, 0}   {5, 0}   {4, 0}   {9, 1}   {7, 1}   {5, 1}   {4, 1}   {9, 3}   {9, 8}   {8, 7}   {6, 6}   {5, 5}   {4, 3}   {2, 2}   {1, 1}   {9, 9}   {8, 8}   {5, 6}   {4, 4}   {3, 3}   {1, 2}   {8, 9}   {7, 8}   {4, 5}   {3, 4}   {8, 10}   {7, 9}   {6, 8}   {5, 7}   {3, 5}   {0, 1}   {8, 11}   {7, 10}   {6, 9}   {5, 8}   {3, 6}   {0, 2}   {9, 14}   {6, 11}   {4, 8}   {3, 7}   {2, 3}   {9, 15}   {8, 14}   {6, 12}   {5, 11}   {4, 9}   {2, 4}   {1, 3}
   i9 : J_*/size

   o9 = {4, 5, 5, 7, 9, 9, 8, 7, 9, 9}

   o9 : List
   i10 : (ideal family)_*/size

   o10 = {4, 5, 5, 7, 10, 10, 8, 8, 9, 10}

   o10 : List
   i11 : J_4

          2    2      3 13      14    2 18         19    2 20      27     34
   o11 = x  - x x  + x z   - x z   + x z   + 2x x z   - x z   + x z   - 2z
          3    0 6    0       6       1        0 1       0       0

   o11 : R
   i12 : family_(0,4)

          2    2      2                          2           2                 
   o12 = x  - x x  + x a       + 2x x a       + x a       - a       - x a      
          3    0 6    0 {1, 1}     0 1 {1, 2}    1 {3, 4}    {0, 1}    6 {2, 3}
      -----------------------------------------------------------------------
                          3
      + a      a       + x a       + x a      a
         {1, 1} {2, 3}    0 {1, 3}    0 {2, 3} {1, 3}

   o12 : QQ[x ..x , x , x ..x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a      , a      , a       , a       , a      , a      , a      , a      , a       , a       , a      , a      , a      , a       , a       , a       , a       , a      , a      , a      ]
             0   1   3   5   6   {9, 0}   {8, 0}   {7, 0}   {6, 0}   {5, 0}   {4, 0}   {9, 1}   {7, 1}   {5, 1}   {4, 1}   {9, 3}   {9, 8}   {8, 7}   {6, 6}   {5, 5}   {4, 3}   {2, 2}   {1, 1}   {9, 9}   {8, 8}   {5, 6}   {4, 4}   {3, 3}   {1, 2}   {8, 9}   {7, 8}   {4, 5}   {3, 4}   {8, 10}   {7, 9}   {6, 8}   {5, 7}   {3, 5}   {0, 1}   {8, 11}   {7, 10}   {6, 9}   {5, 8}   {3, 6}   {0, 2}   {9, 14}   {6, 11}   {4, 8}   {3, 7}   {2, 3}   {9, 15}   {8, 14}   {6, 12}   {5, 11}   {4, 9}   {2, 4}   {1, 3}
   i13 : support family

   o13 = {x , x , x , x , x , a      , a      , a      , a      , a      , a   
           0   1   3   5   6   {1, 1}   {1, 2}   {3, 4}   {0, 1}   {0, 2}   {2,
      -----------------------------------------------------------------------
        , a      }
      3}   {1, 3}

   o13 : List
   i14 : support family/degree

   o14 = {{7}, {8}, {17}, {19}, {20}, {20}, {19}, {18}, {17}, {16}, {14}, {13}}

   o14 : List

SeeAlso
   improveFamily
   getOneParameterFamily
///

-* for CannedExample in getOneParameterFamily
  Example
    R=QQ[x_0..x_1, x_3, x_5..x_6, z, Degrees => {7..8, 17, 19..20, 1}]
    J=ideal(x_1^3-x_0*x_3+x_1*z^16+x_0*z^17,x_1*x_5-x_0*x_6+x_0^2*z^13+x_1*z^19-x_0*z^20,x_0^4-x_1*x_6+x_0*x_1*z^13
      +x_0^2*z^14-x_1*z^20,x_1^2*x_3-x_0^2*x_5-x_5*z^14+x_3*z^16+x_1^2*z^17+x_0*x_1*z^18+x_0^2*z^19,x_3^2-x_0^2*x_6
      +x_0^3*z^13-x_6*z^14+x_1^2*z^18+2*x_0*x_1*z^19-x_0^2*z^20+x_0*z^27-2*z^34,x_3*x_5-x_1^2*x_6+x_0*x_1^2*z^13-x_
      6*z^16-x_5*z^17+x_3*z^19-x_1^2*z^20+x_0*z^29-2*z^36,x_0^3*x_1^2-x_3*x_6+x_0*x_3*z^13+x_0*x_1^2*z^14+x_0^3*z^
      16+x_6*z^17-x_3*z^20+z^37,x_0^3*x_3-x_5^2+x_0^2*x_1*z^16+x_0^3*z^17+x_6*z^18-x_0*z^31+2*z^38,x_0^2*x_1*x_3-x_
      5*x_6+x_0*x_5*z^13+x_0*x_1^2*z^16+x_0^2*x_1*z^17+x_0^3*z^18+x_6*z^19-x_5*z^20+z^39,x_0^3*x_5-x_6^2+2*x_0*x_6*
      z^13+x_0*x_5*z^14+x_0^3*z^19-2*x_6*z^20-x_0^2*z^26+3*x_0*z^33-z^40);
    L=flatten drop(degrees R,-1)
    (base,family)=getParameterFamily J;
    J1=getOneParameterFamily(J,base,family,4)
    J_*/size
    J1_*/size
*-


doc ///
Key
   getOneParameterFamily
   (getOneParameterFamily,Ideal,Ideal,Matrix,ZZ)
   [getOneParameterFamily,CoeffSize]
   [getOneParameterFamily,Verbose]
Headline
   Compute a one parameter smoothing family
Usage
   fib = getOneParameterFamily(J,base,family,p)  
Inputs
   J:Ideal
     of a one-parameter smoothing family of a semigroup ideal
   family:Matrix
     a parametric family which uses the same terms as J
   base:Ideal
      flattening relations
   p:ZZ
    about 1 out of p free parameters are chosen nonzero
Outputs
   fib:Ideal
      of a one-parameter smoothing family  
Description
  Text
    We compute a one-parameter smoothing family which uses the same terms as the matrix family.
  CannedExample    
    i1 : R=QQ[x_0..x_1, x_3, x_5..x_6, z, Degrees => {7..8, 17, 19..20, 1}]

    o1 = R

    o1 : PolynomialRing
    i2 : J=ideal(x_1^3-x_0*x_3+x_1*z^16+x_0*z^17,x_1*x_5-x_0*x_6+x_0^2*z^13+x_1*z^19-x_0*z^20,x_0^4-x_1*x_6+x_0*x_1*z^13
       +x_0^2*z^14-x_1*z^20,x_1^2*x_3-x_0^2*x_5-x_5*z^14+x_3*z^16+x_1^2*z^17+x_0*x_1*z^18+x_0^2*z^19,x_3^2-x_0^2*x_6
       +x_0^3*z^13-x_6*z^14+x_1^2*z^18+2*x_0*x_1*z^19-x_0^2*z^20+x_0*z^27-2*z^34,x_3*x_5-x_1^2*x_6+x_0*x_1^2*z^13-x_
       6*z^16-x_5*z^17+x_3*z^19-x_1^2*z^20+x_0*z^29-2*z^36,x_0^3*x_1^2-x_3*x_6+x_0*x_3*z^13+x_0*x_1^2*z^14+x_0^3*z^
       16+x_6*z^17-x_3*z^20+z^37,x_0^3*x_3-x_5^2+x_0^2*x_1*z^16+x_0^3*z^17+x_6*z^18-x_0*z^31+2*z^38,x_0^2*x_1*x_3-x_
       5*x_6+x_0*x_5*z^13+x_0*x_1^2*z^16+x_0^2*x_1*z^17+x_0^3*z^18+x_6*z^19-x_5*z^20+z^39,x_0^3*x_5-x_6^2+2*x_0*x_6*
       z^13+x_0*x_5*z^14+x_0^3*z^19-2*x_6*z^20-x_0^2*z^26+3*x_0*z^33-z^40);

    o2 : Ideal of R
    i3 : L=flatten drop(degrees R,-1)

    o3 = {7, 8, 17, 19, 20}

    o3 : List
    i4 : (base,family)=getParameterFamily J;
    i5 : J1=getOneParameterFamily(J,base,family,4)
       #pos = 2, #posa = 2

                  3             16      17                 2 13      19   4       
    o5 = ideal (x  - x x  + x z   + x z  , x x  - x x  + x z   + x z  , x  - x x 
                  1    0 3    1       0      1 5    0 6    0       1      0    1 6
     ------------------------------------------------------------------------
            13    2 14   2      2        14      16    2 17        18  
     + x x z   + x z  , x x  - x x  - x z   + x z   + x z   + x x z   +
        0 1       0      1 3    0 5    5       3       1       0 1     
     ------------------------------------------------------------------------
      2 19   2    2      3 13      14    2 18         19      27    34      
     x z  , x  - x x  + x z   - x z   + x z   + 2x x z   + x z   - z  , x x 
      0      3    0 6    0       6       1        0 1       0            3 5
     ------------------------------------------------------------------------
        2        2 13      16      17      19      29    36   3 2         
     - x x  + x x z   - x z   - x z   + x z   + x z   - z  , x x  - x x  +
        1 6    0 1       6       5       3       0            0 1    3 6  
     ------------------------------------------------------------------------
          13      2 14    3 16      17   3      2    2   16    3 17      18  
     x x z   + x x z   + x z   + x z  , x x  - x  + x x z   + x z   + x z   -
      0 3       0 1       0       6      0 3    5    0 1       0       6     
     ------------------------------------------------------------------------
        31    38   2                   13      2 16    2   17    3 18  
     x z   + z  , x x x  - x x  + x x z   + x x z   + x x z   + x z   +
      0            0 1 3    5 6    0 5       0 1       0 1       0     
     ------------------------------------------------------------------------
        19   3      2         13        14    3 19    2 26      33
     x z  , x x  - x  + 2x x z   + x x z   + x z   - x z   + x z  )
      6      0 5    6     0 6       0 5       0       0       0

    o5 : Ideal of R
    i6 : J_*/size

    o6 = {4, 5, 5, 7, 9, 9, 8, 7, 9, 9}

    o6 : List
    i7 : J1_*/size

    o7 = {4, 4, 4, 7, 8, 8, 6, 7, 7, 7}

    o7 : List
    
SeeAlso
   improveFamily
   getParameterFamily
///

doc ///
Key
   getRangeOfOneParameterFamily
   (getRangeOfOneParameterFamily,Ideal)
Headline
   Compute the range of degrees of a one parameter family
Usage
   range = getRangeOfOneParameterFamily J
Inputs
   J:Ideal
     of a one-parameter smoothing family of a semigroup ideal
Outputs
   range:List
      range of degrees of the 1-parameter family
Description
  Text
    We compute range of degrees of a one parameter family
  Example
   L={5,6,8}
    genus L
    R=QQ[x_0, x_1, x_3, z,Degrees=>{5, 6, 8, 1}]
    J=ideal(x_0^2*x_1-x_3^2-x_0^2*z^6-x_3*z^8-x_1*z^10+z^16,x_1^3-x_0^2*x_3-x_1^2*z^6-x_0*x_1*z^7-x_0^2*z^8-x_3*z^10-x_1*z^12+x_0*z^13,x_0^4-x_1^2*x_3+x_0*x_3*z^7+x_3*z^12-z^20)
    range=getRangeOfOneParameterFamily J
SeeAlso
   getParameterFamily
///

-* for CannedExample in testBound
Example  
    L={7,8,17,19,20}
    (answer,J,comp)=testBound(L,12)
    range=drop(flatten getRangeOfOneParameterFamily J,-5)    
    (answer1,J1,comp)=testRange(L,range,CoeffSize=>2)
    J_*/size
    J1_*/size
    congruences={6}
    (answer,J,comp)=testCongruences(L,congruences,Verbose=>2)

*-

doc ///
Key
   testBound
   testCongruences
   testRange
   (testBound,List,ZZ)
   (testCongruences,List,List)
   (testRange,List,List)
   [testBound,Verbose]
   [testBound,CoeffSize]
   [testCongruences,CoeffSize]
   [testRange,CoeffSize]
Headline
   Test whether b is bound for the semigroup L and compute a 1-parameter smoothing family if yes
Usage
   (answer,J,comp) = testBound(L,b)
Inputs
   L:List
     a of generators of a semigroup
   b:ZZ
    bound for the degrees of the parameters
Outputs
   answer:Boolean
      true if a smoothing family was found
   J:Ideal
     ideal of a 1-parameter smoothing family of the semigroup ring of L
   comp: List
     of component numbers which are smoothing families
Description
  Text
    We check whether there exists a smoothing component for the restricted unfolding with variables
    of degree >b (or degree congruent 0 mod d for some d in congruences
	or variable with degree in the range).
    If the answer is yes, then we compute such smoothing family over QQ.
    This however might fail if the coefficient size is too small or the random choices in
    solvingFlatteningRelations are bad. In that case J will be null.        
  CannedExample
    i1 : L={7,8,17,19,20}

    o1 = {7, 8, 17, 19, 20}

    o1 : List
    i2 : (answer,J,comp)=testBound(L,12)
    -- the behavior of random(List) will change soon; use shuffle(List) instead
    # of coordinate linear subspace of the base = 21
    # of linear subsets which leading to a point =21

                        3             16      17                 2 13      19  
    o2 = (true, ideal (x  - x x  + x z   + x z  , x x  - x x  + x z   + x z   -
                    1    0 3    1       0      1 5    0 6    0       1     
         ------------------------------------------------------------------------
             20   4               13    2 14      20   2      2        14      16
          x z  , x  - x x  + x x z   + x z   - x z  , x x  - x x  - x z   + x z  
           0      0    1 6    0 1       0       1      1 3    0 5    5       3   
         ------------------------------------------------------------------------
            2 17        18    2 19   2    2      3 13      14    2 18         19
         + x z   + x x z   + x z  , x  - x x  + x z   - x z   + x z   + 2x x z  
            1       0 1       0      3    0 6    0       6       1        0 1   
         ------------------------------------------------------------------------
            2 20      27     34          2        2 13      16      17      19  
         - x z   + x z   - 2z  , x x  - x x  + x x z   - x z   - x z   + x z   -
            0       0             3 5    1 6    0 1       6       5       3     
         ------------------------------------------------------------------------
          2 20      29     36   3 2               13      2 14    3 16      17  
         x z   + x z   - 2z  , x x  - x x  + x x z   + x x z   + x z   + x z   -
          1       0             0 1    3 6    0 3       0 1       0       6     
        ------------------------------------------------------------------------
            20    37   3      2    2   16    3 17      18      31     38   2    
         x z   + z  , x x  - x  + x x z   + x z   + x z   - x z   + 2z  , x x x 
          3            0 3    5    0 1       0       6       0             0 1 3
        ------------------------------------------------------------------------
                       13      2 16    2   17    3 18      19      20    39   3  
         - x x  + x x z   + x x z   + x x z   + x z   + x z   - x z   + z  , x x 
           5 6    0 5       0 1       0 1       0       6       5            0 5
        ------------------------------------------------------------------------
            2         13        14    3 19       20    2 26       33    40
         - x  + 2x x z   + x x z   + x z   - 2x z   - x z   + 3x z   - z  ), {0})
            6     0 6       0 5       0        6       0        0

    o2 : Sequence
    i3 : range=drop(flatten getRangeOfOneParameterFamily J,-5)

    o3 = {13, 14, 16, 17, 18, 19, 20, 26, 27, 29, 31, 33, 34}

    o3 : List
    i4 : (answer1,J1,comp)=testRange(L,range,CoeffSize=>2)
    # of coordinate linear subspace of the base = 6
    # of linear subsets which leading to a point =4

                     3               16       17                  2 13   4  
    o4 = (true, ideal (2x  - 2x x  + 6x z   + 3x z  , x x  - x x  + 3x z  , x  -
                     1     0 3     1        0      1 5    0 6     0      0  
     ------------------------------------------------------------------------
                  13     2 14    2       2         13        14        16  
     x x  + 4x x z   + 2x z  , 8x x  - 8x x  - 8x z   - 16x z   + 24x z   +
      1 6     0 1        0       1 3     0 5     6         5         3     
     ------------------------------------------------------------------------
            18      2 19        26    2     2        3 13        14        17
     27x x z   + 18x z   + 24x z  , 8x  - 8x x  + 16x z   - 16x z   - 12x z  
        0 1         0         0       3     0 6      0         6         3   
     ------------------------------------------------------------------------
          2 18          19       26        27            2         2 13  
     + 27x z   + 18x x z   - 8x z   + 32x z  , 2x x  - 2x x  + 6x x z   -
          1         0 1        1         0       3 5     1 6     0 1     
     ------------------------------------------------------------------------
         16       17        29    3 2                 13       2 14     3 16
     6x z   - 3x z   + 18x z  , 2x x  - 2x x  + 8x x z   + 4x x z   + 6x z  
       6        5         0       0 1     3 6     0 3        0 1        0   
     ------------------------------------------------------------------------
           17    3       2      2   16        18        19         31 
     + 3x z  , 8x x  - 8x  + 24x x z   + 27x z   + 18x z   - 108x z  ,
         6       0 3     5      0 1         6         5          0    
     ------------------------------------------------------------------------
       2                      13        2 16      3 18        19   3      2  
     8x x x  - 8x x  + 24x x z   + 24x x z   + 27x z   + 18x z  , x x  - x  +
       0 1 3     5 6      0 5         0 1         0         6      0 5    6  
     ------------------------------------------------------------------------
           13         14      2 26
     7x x z   + 2x x z   - 12x z  ), {0})
       0 6        0 5         0

    o4 : Sequence
    i5 : J_*/size

    o5 = {4, 5, 5, 7, 9, 9, 8, 7, 9, 9}

    o5 : List
    i6 : J1_*/size

    o6 = {4, 3, 4, 8, 9, 6, 6, 6, 6, 5}

    o6 : List
    i7 : congruences={6}

    o7 = {6}

    o7 : List
    i8 : (answer,J,comp)=testCongruences(L,congruences,Verbose=>2)

    o8 = (false, , {})

    o8 : Sequence
SeeAlso
   solvingFlatteningRelations
   
///

-* for cannedExample in solvingFlatteningRelations

  Example  
    L={7,8,17,19,20}
    I=semigroupIdeal L;
    (answer,J,comp)=testBound(L,12);
    (base1,family1)=getParameterFamily J;
    base=last decompose base1;
    family=family1%sub(base,ring family1);
    (worked,fiber)=solvingFlatteningRelations(base,family,I)
*-

doc ///
Key
   solvingFlatteningRelations
   (solvingFlatteningRelations,Ideal,Matrix,Ideal)
   [solvingFlatteningRelations,CoeffBound]
   [solvingFlatteningRelations,CoeffSize]
   [solvingFlatteningRelations,BaseField]
   [solvingFlatteningRelations,Verbose]
   
Headline
   Solving the flatttening relations over QQ
Usage
   (worked,fiber)=solvingFlatteningRelations(base,family,I)
Inputs
   base:Ideal
     irreducible ideal of flattening relations
   family:Matrix
    matrix of generators of the family
   I:Ideal
     semigroup ideal
Outputs
   worked:Boolean
      true if a smoothing family was found
   fiber:Ideal
     ideal of a 1-parameter smoothing family 
Description
  Text
    We look for subsets of the source consisting of all but codim base many elements
    which define a linear subspace of the base. Substituting random small values for these
    variables, might lead to an ideal which defines a point in the base.
    There is some intermediate output. 
  CannedExample
    i1 : L={7,8,17,19,20}

    o1 = {7, 8, 17, 19, 20}

    o1 : List
    i2 : I=semigroupIdeal L;

                   ZZ
    o2 : Ideal of -----[x ..x , x , x ..x ]
                  10007  0   1   3   5   6
    i3 : (answer,J,comp)=testBound(L,12);
      # of coordinate linear subspace of the base = 21
      # of linear subsets which leading to a point =21
    i4 : (base1,family1)=getParameterFamily J;
    i5 : base=last decompose base1;

    o5 : Ideal of QQ[a      , a      , a      , a      , a      , a      , a      ]
                      {1, 1}   {1, 2}   {3, 4}   {0, 1}   {0, 2}   {2, 3}   {1, 3}
    i6 : family=family1%sub(base,ring family1);

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          10
    o6 : Matrix (QQ[x ..x , x , x ..x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a      , a      , a       , a       , a      , a      , a      , a      , a       , a       , a      , a      , a      , a       , a       , a       , a       , a      , a      , a      ])  <-- (QQ[x ..x , x , x ..x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a      , a      , a       , a       , a      , a      , a      , a      , a       , a       , a      , a      , a      , a       , a       , a       , a       , a      , a      , a      ])
                     0   1   3   5   6   {9, 0}   {8, 0}   {7, 0}   {6, 0}   {5, 0}   {4, 0}   {9, 1}   {7, 1}   {5, 1}   {4, 1}   {9, 3}   {9, 8}   {8, 7}   {6, 6}   {5, 5}   {4, 3}   {2, 2}   {1, 1}   {9, 9}   {8, 8}   {5, 6}   {4, 4}   {3, 3}   {1, 2}   {8, 9}   {7, 8}   {4, 5}   {3, 4}   {8, 10}   {7, 9}   {6, 8}   {5, 7}   {3, 5}   {0, 1}   {8, 11}   {7, 10}   {6, 9}   {5, 8}   {3, 6}   {0, 2}   {9, 14}   {6, 11}   {4, 8}   {3, 7}   {2, 3}   {9, 15}   {8, 14}   {6, 12}   {5, 11}   {4, 9}   {2, 4}   {1, 3}             0   1   3   5   6   {9, 0}   {8, 0}   {7, 0}   {6, 0}   {5, 0}   {4, 0}   {9, 1}   {7, 1}   {5, 1}   {4, 1}   {9, 3}   {9, 8}   {8, 7}   {6, 6}   {5, 5}   {4, 3}   {2, 2}   {1, 1}   {9, 9}   {8, 8}   {5, 6}   {4, 4}   {3, 3}   {1, 2}   {8, 9}   {7, 8}   {4, 5}   {3, 4}   {8, 10}   {7, 9}   {6, 8}   {5, 7}   {3, 5}   {0, 1}   {8, 11}   {7, 10}   {6, 9}   {5, 8}   {3, 6}   {0, 2}   {9, 14}   {6, 11}   {4, 8}   {3, 7}   {2, 3}   {9, 15}   {8, 14}   {6, 12}   {5, 11}   {4, 9}   {2, 4}   {1, 3}
    i7 : (worked,fiber)=solvingFlatteningRelations(base,family,I)
      # of coordinate linear subspace of the base = 2
      # of linear subsets which leading to a point =2

    o7 = (true, | x_1^3-x_0x_3+x_1+x_0 x_1x_5-x_0x_6+x_0^2+x_1-x_0
     ------------------------------------------------------------------------
     x_0^4-x_1x_6+x_0x_1+x_0^2-x_1
     ------------------------------------------------------------------------
     x_1^2x_3-x_0^2x_5-x_5+x_3+x_1^2+x_0x_1+x_0^2
     ------------------------------------------------------------------------
     x_3^2-x_0^2x_6+x_0^3-x_6+x_1^2+2x_0x_1-x_0^2+x_0-2
     ------------------------------------------------------------------------
     x_3x_5-x_1^2x_6+x_0x_1^2-x_6-x_5+x_3-x_1^2+x_0-2
     ------------------------------------------------------------------------
     x_0^3x_1^2-x_3x_6+x_0x_3+x_0x_1^2+x_0^3+x_6-x_3+1
     ------------------------------------------------------------------------
     x_0^3x_3-x_5^2+x_0^2x_1+x_0^3+x_6-x_0+2
     ------------------------------------------------------------------------
     x_0^2x_1x_3-x_5x_6+x_0x_5+x_0x_1^2+x_0^2x_1+x_0^3+x_6-x_5+1
     ------------------------------------------------------------------------
     x_0^3x_5-x_6^2+2x_0x_6+x_0x_5+x_0^3-2x_6-x_0^2+3x_0-1 |)

    o7 : Sequence

SeeAlso
   clearDenominators
   
///

-* for CannedExample in clearDenominators

  Example  
    L={7,8,17,19,20}
    genus L
    I=semigroupIdeal(L,"BaseField"=>QQ);
    (answer,J,comp)=testBound(L,12,Verbose=>1);
    (base1,family1)=getParameterFamily J;
    base=last decompose base1
    family=family1%sub(base,ring family1);
    (worked,fiber)=solvingFlatteningRelations(base,family,I);
    p=nextPrime 10^5
    kk=ZZ/p;
    SzFinite=kk[support J, Degrees=>apply(support J,m->degree m)]
    fibF=sub(fiber,SzFinite);
    fiber=fibF*diagonalMatrix({1/2_kk,2/3_kk,1/5_kk,4/3_kk,1/30_kk,2/5_kk,1/2_kk,1/3_kk,1/2_kk,1/3_kk})
    (worked,fiber1)=clearDenominators fiber
*-

doc ///
Key
   clearDenominators
   (clearDenominators, Matrix)
   [clearDenominators, CoeffBound]
Headline
   Clear denominators
Usage
   (worked,fiber)=clearDenominators(family)
Inputs
   family:Matrix
     a family over a finite large prime field
Outputs
   worked:Boolean
      true if clearing denominators worked
   fiber:Matrix
     matrix of generators of a 1-parameter smoothing family over QQ
Description
  Text
    Since we work at this point over a large prime field we might detect denominators
    for a lift to QQ by multiplying each equation by a factor such that the naive lifts
    are still small.
    The factors we try are have 2,3 and 5 as the only prime factors and are not so small.
  CannedExample    
    i1 : L={7,8,17,19,20}

    o1 = {7, 8, 17, 19, 20}

    o1 : List
    i2 : genus L

    o2 = 12
    i3 : I=semigroupIdeal(L,"BaseField"=>QQ);

    o3 : Ideal of QQ[x ..x , x , x ..x ]
                      0   1   3   5   6
    i4 : (answer,J,comp)=testBound(L,12,Verbose=>1);
    # of coordinate linear subspace of the base = 21
    # of linear subsets which leading to a point =21
    i5 : (base1,family1)=getParameterFamily J;
    i6 : base=last decompose base1

                                                                             
    o6 = ideal (a      a       - a      a      , a      a       - a      a      ,
                 {0, 2} {2, 3}    {0, 1} {1, 3}   {3, 4} {2, 3}    {1, 2} {1, 3} 
     ------------------------------------------------------------------------
      2
     a       - a      a      , a      a       - a      a      , a      a   
      {0, 2}    {1, 2} {1, 3}   {0, 1} {0, 2}    {1, 2} {2, 3}   {3, 4} {0,
     ------------------------------------------------------------------------
        - a      a      )
     1}    {1, 2} {0, 2}

     o6 : Ideal of QQ[a      , a      , a      , a      , a      , a      , a      ]
                      {1, 1}   {1, 2}   {3, 4}   {0, 1}   {0, 2}   {2, 3}   {1, 3}
     i7 : family=family1%sub(base,ring family1);

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          10
     o7 : Matrix (QQ[x ..x , x , x ..x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a      , a      , a       , a       , a      , a      , a      , a      , a       , a       , a      , a      , a      , a       , a       , a       , a       , a      , a      , a      ])  <-- (QQ[x ..x , x , x ..x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a      , a      , a       , a       , a      , a      , a      , a      , a       , a       , a      , a      , a      , a       , a       , a       , a       , a      , a      , a      ])
                      0   1   3   5   6   {9, 0}   {8, 0}   {7, 0}   {6, 0}   {5, 0}   {4, 0}   {9, 1}   {7, 1}   {5, 1}   {4, 1}   {9, 3}   {9, 8}   {8, 7}   {6, 6}   {5, 5}   {4, 3}   {2, 2}   {1, 1}   {9, 9}   {8, 8}   {5, 6}   {4, 4}   {3, 3}   {1, 2}   {8, 9}   {7, 8}   {4, 5}   {3, 4}   {8, 10}   {7, 9}   {6, 8}   {5, 7}   {3, 5}   {0, 1}   {8, 11}   {7, 10}   {6, 9}   {5, 8}   {3, 6}   {0, 2}   {9, 14}   {6, 11}   {4, 8}   {3, 7}   {2, 3}   {9, 15}   {8, 14}   {6, 12}   {5, 11}   {4, 9}   {2, 4}   {1, 3}             0   1   3   5   6   {9, 0}   {8, 0}   {7, 0}   {6, 0}   {5, 0}   {4, 0}   {9, 1}   {7, 1}   {5, 1}   {4, 1}   {9, 3}   {9, 8}   {8, 7}   {6, 6}   {5, 5}   {4, 3}   {2, 2}   {1, 1}   {9, 9}   {8, 8}   {5, 6}   {4, 4}   {3, 3}   {1, 2}   {8, 9}   {7, 8}   {4, 5}   {3, 4}   {8, 10}   {7, 9}   {6, 8}   {5, 7}   {3, 5}   {0, 1}   {8, 11}   {7, 10}   {6, 9}   {5, 8}   {3, 6}   {0, 2}   {9, 14}   {6, 11}   {4, 8}   {3, 7}   {2, 3}   {9, 15}   {8, 14}   {6, 12}   {5, 11}   {4, 9}   {2, 4}   {1, 3}
     i8 : (worked,fiber)=solvingFlatteningRelations(base,family,I);
     # of coordinate linear subspace of the base = 2
     # of linear subsets which leading to a point =2
     i9 : p=nextPrime 10^5

     o9 = 100003
     i10 : kk=ZZ/p;
     i11 : SzFinite=kk[support J, Degrees=>apply(support J,m->degree m)]

     o11 = SzFinite

     o11 : PolynomialRing
     i12 : fibF=sub(fiber,SzFinite);

                          1             10
     o12 : Matrix SzFinite  <-- SzFinite
     i13 : fiber=fibF*diagonalMatrix({1/2_kk,2/3_kk,1/5_kk,4/3_kk,1/30_kk,2/5_kk,1/2_kk,1/3_kk,1/2_kk,1/3_kk})

     o13 = | -50001x_1^3+50001x_0x_3-50001x_1-50001x_0
      -----------------------------------------------------------------------
      33335x_1x_5-33335x_0x_6+33335x_0^2+33335x_1-33335x_0
      -----------------------------------------------------------------------
      -40001x_0^4+40001x_1x_6-40001x_0x_1-40001x_0^2+40001x_1
      -----------------------------------------------------------------------
      -33333x_1^2x_3+33333x_0^2x_5+33333x_5-33333x_3-33333x_1^2-33333x_0x_1-
      -----------------------------------------------------------------------
      33333x_0^2 -23334x_3^2+23334x_0^2x_6-23334x_0^3+23334x_6-23334x_1^2-
      -----------------------------------------------------------------------
      46668x_0x_1+23334x_0^2-23334x_0+46668
      -----------------------------------------------------------------------
      20001x_3x_5-20001x_1^2x_6+20001x_0x_1^2-20001x_6-20001x_5+20001x_3-
      -----------------------------------------------------------------------
      20001x_1^2+20001x_0-40002
      -----------------------------------------------------------------------
      -50001x_0^3x_1^2+50001x_3x_6-50001x_0x_3-50001x_0x_1^2-50001x_0^3-
      -----------------------------------------------------------------------
      50001x_6+50001x_3-50001
      -----------------------------------------------------------------------
      -33334x_0^3x_3+33334x_5^2-33334x_0^2x_1-33334x_0^3-33334x_6+33334x_0+
      -----------------------------------------------------------------------
      33335 -50001x_0^2x_1x_3+50001x_5x_6-50001x_0x_5-50001x_0x_1^2-50001x_0^
      -----------------------------------------------------------------------
      2x_1-50001x_0^3-50001x_6+50001x_5-50001
      -----------------------------------------------------------------------
      -33334x_0^3x_5+33334x_6^2+33335x_0x_6-33334x_0x_5-33334x_0^3-33335x_6+
      -----------------------------------------------------------------------
      33334x_0^2+x_0+33334 |

                          1             10
     o13 : Matrix SzFinite  <-- SzFinite
     i14 : (worked,fiber1)=clearDenominators fiber

     o14 = (true, | x_1^3-x_0x_3+x_1+x_0 2x_1x_5-2x_0x_6+2x_0^2+2x_1-2x_0
      -----------------------------------------------------------------------
      x_0^4-x_1x_6+x_0x_1+x_0^2-x_1
      -----------------------------------------------------------------------
      4x_1^2x_3-4x_0^2x_5-4x_5+4x_3+4x_1^2+4x_0x_1+4x_0^2
      -----------------------------------------------------------------------
      x_3^2-x_0^2x_6+x_0^3-x_6+x_1^2+2x_0x_1-x_0^2+x_0-2
      -----------------------------------------------------------------------
      2x_3x_5-2x_1^2x_6+2x_0x_1^2-2x_6-2x_5+2x_3-2x_1^2+2x_0-4
      -----------------------------------------------------------------------
      x_0^3x_1^2-x_3x_6+x_0x_3+x_0x_1^2+x_0^3+x_6-x_3+1
      -----------------------------------------------------------------------
      x_0^3x_3-x_5^2+x_0^2x_1+x_0^3+x_6-x_0+2
      -----------------------------------------------------------------------
      x_0^2x_1x_3-x_5x_6+x_0x_5+x_0x_1^2+x_0^2x_1+x_0^3+x_6-x_5+1
      -----------------------------------------------------------------------
      x_0^3x_5-x_6^2+2x_0x_6+x_0x_5+x_0^3-2x_6-x_0^2+3x_0-1 |)

    o14 : Sequence

SeeAlso
   solvingFlatteningRelations
   
///
-* for CannedExample in isSmoothingFamily
  Example  
    L={6,8,9,11}
    genus L
    I=semigroupIdeal(L,"BaseField"=>ZZ/nextPrime 10^4);
    (answer,J,comp)=testBound(L,2,Verbose=>1);
    (base1,family1)=getParameterFamily J;
    family=family1%sub(base1,ring family1);
    base=(decompose base1)_2;
    kk=coefficientRing ring I;
    SAFinite=kk[gens ring family,Degrees=>degrees ring family]
    AF=kk[gens ring base,Degrees=>degrees ring base]
    baseF=sub(base,AF)
    familyF=sub(family,SAFinite);
    isSmoothingFamily(L,I,familyF,baseF)
*-

doc ///
Key
   isSmoothingFamily
   (isSmoothingFamily,List,Ideal,Matrix,Ideal)
   [isSmoothingFamily,Verbose]
Headline
   Is the family a smoothing family?
Usage
   smooth=isSmoothingFamily(L,I,family,base) -> (
Inputs
   L:List
     list of generators of a semigroup
   I:Ideal
     semigroup ideal (of L)
   family:Matrix
     a family over a finite large prime field
   base: Ideal
     ideal of flattening relations
Outputs
   smooth:Boolean
      true if the family is a smoothing family
Description
  Text
    Given a irreducible parametric family over a (large) finite field, the functions
    picks a random point in the base and checks whether the corresponding fiber is a smooth.
  CannedExample
    i1 : L={6,8,9,11}

    o1 = {6, 8, 9, 11}

    o1 : List
    i2 : genus L

    o2 = 8
    i3 : I=semigroupIdeal(L,"BaseField"=>ZZ/nextPrime 10^4);

                   ZZ
    o3 : Ideal of -----[x , x ..x , x ]
                  10007  0   2   3   5
    i4 : (answer,J,comp)=testBound(L,2,Verbose=>1);
    i5 : (base1,family1)=getParameterFamily J;
    i6 : family=family1%sub(base1,ring family1);

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  6
    o6 : Matrix (QQ[x , x ..x , x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a       , a      , a      , a      , a      , a       , a      , a      , a       , a       , a      , a      , a      , a      , a       , a      , a      , a      , a       , a       , a       , a      , a      , a       , a       , a      , a      , a      ])  <-- (QQ[x , x ..x , x , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a       , a      , a      , a      , a      , a       , a      , a      , a       , a       , a      , a      , a      , a      , a       , a      , a      , a      , a       , a       , a       , a      , a      , a       , a       , a      , a      , a      ])
                     0   2   3   5   {5, 0}   {4, 0}   {3, 0}   {5, 1}   {2, 0}   {5, 2}   {5, 3}   {4, 1}   {1, 0}   {5, 4}   {4, 2}   {3, 1}   {5, 5}   {3, 2}   {2, 1}   {5, 6}   {4, 4}   {3, 3}   {5, 7}   {4, 5}   {2, 2}   {1, 1}   {5, 8}   {3, 4}   {2, 3}   {0, 1}   {5, 9}   {4, 6}   {3, 5}   {1, 2}   {5, 10}   {4, 7}   {2, 4}   {0, 2}   {5, 11}   {4, 8}   {3, 6}   {2, 5}   {0, 3}   {5, 12}   {4, 9}   {3, 7}   {5, 13}   {4, 10}   {3, 8}   {2, 6}   {1, 5}   {0, 4}   {4, 11}   {3, 9}   {2, 7}   {0, 5}   {5, 15}   {4, 12}   {3, 10}   {2, 8}   {1, 6}   {4, 13}   {3, 11}   {2, 9}   {1, 7}   {0, 6}             0   2   3   5   {5, 0}   {4, 0}   {3, 0}   {5, 1}   {2, 0}   {5, 2}   {5, 3}   {4, 1}   {1, 0}   {5, 4}   {4, 2}   {3, 1}   {5, 5}   {3, 2}   {2, 1}   {5, 6}   {4, 4}   {3, 3}   {5, 7}   {4, 5}   {2, 2}   {1, 1}   {5, 8}   {3, 4}   {2, 3}   {0, 1}   {5, 9}   {4, 6}   {3, 5}   {1, 2}   {5, 10}   {4, 7}   {2, 4}   {0, 2}   {5, 11}   {4, 8}   {3, 6}   {2, 5}   {0, 3}   {5, 12}   {4, 9}   {3, 7}   {5, 13}   {4, 10}   {3, 8}   {2, 6}   {1, 5}   {0, 4}   {4, 11}   {3, 9}   {2, 7}   {0, 5}   {5, 15}   {4, 12}   {3, 10}   {2, 8}   {1, 6}   {4, 13}   {3, 11}   {2, 9}   {1, 7}   {0, 6}
    i7 : base=(decompose base1)_2;

    o7 : Ideal of QQ[a      , a      , a      , a      , a      , a      , a      , a      , a      , a      , a       , a      , a      , a      , a       , a      , a      , a      ]
                      {4, 1}   {4, 2}   {4, 4}   {4, 5}   {0, 1}   {4, 6}   {4, 7}   {4, 8}   {0, 3}   {4, 9}   {4, 10}   {1, 5}   {0, 4}   {0, 5}   {4, 12}   {1, 6}   {1, 7}   {0, 6}
    i8 : kk=coefficientRing ring I;
    i9 : SAFinite=kk[gens ring family,Degrees=>degrees ring family]

    o9 = SAFinite

    o9 : PolynomialRing
    i10 : AF=kk[gens ring base,Degrees=>degrees ring base]

    o10 = AF

    o10 : PolynomialRing
    i11 : baseF=sub(base,AF)

                                                                       
    o11 = ideal (2a       + 2a      a       - 2a      a       + a      a   
	           {4, 4}     {4, 9} {0, 4}     {0, 3} {0, 5}    {0, 5} {4,
      -----------------------------------------------------------------------
                                                                             
         a       + a      a       a      , a      a       + a      a        +
      12} {1, 6}    {0, 4} {4, 12} {1, 7}   {4, 7} {0, 4}    {0, 1} {4, 12}  
      -----------------------------------------------------------------------
                                                          2              
      2a      a      a        + a       a      a       - a      a       +
        {0, 4} {0, 5} {4, 12}    {4, 10} {0, 4} {1, 7}    {0, 4} {1, 7}  
      -----------------------------------------------------------------------
       2                                                                  
      a      a       - a      a       a      , a       - 2a      a       +
       {0, 4} {0, 6}    {0, 3} {4, 12} {0, 6}   {4, 2}     {4, 8} {0, 3}  
      -----------------------------------------------------------------------
        2                                                   2            
      3a       + a      a       + a      a       + 2a      a       + a   
        {0, 3}    {4, 6} {0, 4}    {0, 1} {0, 5}     {0, 4} {0, 5}    {4,
      -----------------------------------------------------------------------
                                                     2       2            
        a       a       - 3a      a       a       + a       a       - a   
      8} {4, 12} {1, 6}     {0, 3} {4, 12} {1, 6}    {4, 12} {1, 6}    {0,
      -----------------------------------------------------------------------
                                            2
        a      a      , 2a      a       + 2a      a       - a      a   
      3} {0, 5} {0, 6}    {0, 1} {0, 4}     {0, 4} {0, 5}    {0, 3} {0,
      -----------------------------------------------------------------------
        a       + a      a      a      )
      5} {1, 6}    {0, 3} {0, 4} {1, 7}

    o11 : Ideal of AF
    i12 : familyF=sub(family,SAFinite);

                         1             6
    o12 : Matrix SAFinite  <-- SAFinite
    i13 : isSmoothingFamily(L,I,familyF,baseF)
 
    o13 = true
  Text
    The intermediate output gives the codimension and number of generators of J3, the final system of equations to solve,
    and the timing for finding a point.
SeeAlso
   getParameterFamily
   testBound
///

doc ///
Key
   collectByBound
   collectByCongruences
   collectByRange
   (collectByBound,List,ZZ,String,String)
   (collectByCongruences,List,List,String,String)
   (collectByRange,List,List,String,String)
   [collectByBound,CoeffSize]
   [collectByBound,Verbose]
   [collectByCongruences,CoeffSize]
   [collectByRange,CoeffSize]
Headline
   Collect 1-parameter families filtered by bound (or congruences or range)
Usage
   collectByBound(LL,b,done,doneData)
   collectByRange(LL,b,range,doneData)
   collectByCongruences(LL,b,congruences,doneData)
Inputs
   LL:List
     list of semigroups
   bound:ZZ
     degree bound
   done:String
     name of a file of done examples
   doneData:String
     name of the dataFile with name doneData.dbm

Outputs

Description
  Text
    We scan the list LL of semigroups L by those which pass testBound(L,b).
    We record the successes ba appending them to the file done and by recording
    the corresponding data for the smoothing families in doneData
    on the hard disk.
  Example  
    LL=(toDoList 8)_{0..2}
    X="fam8"
    Xdbm="fam8.dbm"
    "collectByBound(LL,6,X,Xdbm)";
    range=toList(6..8)
    LL=(toDoList 8)_{4,5}
    "collectByRange(LL,range,X,Xdbm)";
  Text
    Reading and writing to the disk does not work in the documentation.
    Hence we give the command in quotes.
    
SeeAlso
   toDoList
   testBound
///

doc ///
Key
   appendFamily
   (appendFamily,List,Ideal,String,String)
Headline
   Append a one-parameter smoothing family to a data file
Usage
   append(L,J,done,doneData)   
Inputs
   L:List
     generators of a semigroup
   J:Ideal
     ideal of a one parameter smoothing
   done:String
     name of a file of done examples
   doneData:String
     name of the dataFile with name doneData.dbm

Outputs

Description
  Text
    We append a one-parameter smoothing family to a given data file
  Example  
    L={5,7,9}
    genus L
    X="fam8"
    Xdbm="fam8.dbm"
    setRandomSeed("always successful");
    elapsedTime (smooth,J)=getSmoothingFamily(L,6,Verbose=>1)
    elapsedTime smoothnessWithReductions(J,Verbose=>1)
    assert(flatten drop(degrees ring J,-1)==L)   
    "appendFamily(L,J,X,Xdbm)";
  Text
    Reading and writing to the disk does not work in the documentation.
    Hence we give the command in quotes.    
SeeAlso
   getSmoothingFamily
   smoothnessWithReductions
///

doc ///
Key
   collectWithVersalDeformations
   (collectWithVersalDeformations,List,ZZ,String,String)
   [collectWithVersalDeformations,CoeffSize]
   [collectWithVersalDeformations,Verbose]
   [collectWithVersalDeformations,Bound]
   [collectWithVersalDeformations,CoeffBound]
   [collectWithVersalDeformations,BaseField]
   [collectWithVersalDeformations,HighestOrder1]
Headline
   Collect 1-parameter families the using versal deformations
Usage
   (worked,fibz)=collectWithVersalDeformations(LL,b,done,doneData)
Inputs
   LL:List
     list of semigroups
   b:ZZ
     degree bound
   done:String
     name of a file of done examples
   doneData:String
     name of the dataFile with name doneData.dbm

Outputs

Description
  Text
    We scan the list LL of semigroups L by those which pass collectWithVersalDeformations.
    We record the successes by appending them to the file done and by recording
    the corresponding data for the smoothing families in doneData
    on the hard disk.
  Example  
    LL=(toDoList 8)_{0..2}
    X="fam8"
    Xdbm="fam8.dbm"
    "(worked,fibz)=collectWithVersalDeformations(LL,b,done,doneData)";
  Text
    Reading and writing to the disk does not work in the documentation.
    Hence we give the command in quotes.
Caveat
    If the versal deformation computation does not finish with "Solution is polynomial"
    one needs to increase the default value HighestOrder1=>20 to a larger number.
     
SeeAlso
   toDoList
///

doc ///
Key
   getSmoothingFamilyWithVersalDeformation
   (getSmoothingFamilyWithVersalDeformation,List)
   [getSmoothingFamilyWithVersalDeformation,CoeffSize]
   [getSmoothingFamilyWithVersalDeformation,Verbose]
   [getSmoothingFamilyWithVersalDeformation,CoeffBound]
   [getSmoothingFamilyWithVersalDeformation,Bound]
   [getSmoothingFamilyWithVersalDeformation,HighestOrder1]
   [getSmoothingFamilyWithVersalDeformation,BaseField]
Headline
   Get a smoothing family using versal deformations
Usage
   (worked,fibz)=getSmoothingFamilyWithVersalDeformation(L)
   L:List
     list of generators of a semigroup
Outputs
   worked:Boolean
     true if successful
   fibz: Ideal
     the ideal of a 1-parameter smoothing family defined over QQ
Description
  Text
    Using the package VersalDeformations we try compute a smoothing family
  Example
    L = {5,7,11}
    (worked,fibz)=getSmoothingFamilyWithVersalDeformation(L,Verbose=>2,Bound=>4)
Caveat
    If the versal deformation computation does finished with "Solution is polynomial"
    one needs to increase the default value HighestOrder1=>20 to a larger number.
    
   
///

-* for CannedExample in getSmothingFamily
  Example
    L=(toDoList 8)_0
    (smooth,fib)=getSmoothingFamily(L,12,Verbose=>1)
    (smooth,fib)=getSmoothingFamily(L,11,Verbose=>1)
    range=makeRange(L,{4})
    (smooth,fib, comps)=getSmoothingFamily(L,range,Verbose=>1)
    (smooth,fib, comps)=getSmoothingFamily(L,range,Verbose=>2)



*-



doc ///
Key
   getSmoothingFamily
   (getSmoothingFamily,List,ZZ)
   (getSmoothingFamily,List,List)
   [getSmoothingFamily,CoeffBound]
   [getSmoothingFamily,CoeffSize]
   [getSmoothingFamily,BaseField]
   [getSmoothingFamily,Verbose]
Headline
   Get a smoothing family for the semigroup L
Usage
   (smooth,fib)=getSmoothingFamily(L,b)
   (smooth,fib,comps)=getSmoothingFamily(L,range)
Inputs
   L:List
     list of generators of a semigroups
   bound:ZZ
     degree bound
   range:List
     range of degrees
 
Outputs
   smooth: Boolean
     true is a smoothing family was found
   fib: Ideal
     ideal of a one-parameter smoothing family or null
   comps: List
     list of component numbers of the smoothing components discovered
Description
  Text
    We first test whether by the bound b or the range of degrees the 
    restricted unfolding leads to a smoothing family over a finite field.
    If yes and with Verbose>0 then the degrees of the parameters of the parameters
    of the smoothing family are printed.
    In a second step we try to lift the example to characteristic 0 and check smoothness again.
  CannedExample
    i1 : L=(toDoList 8)_0

    o1 = {6, 7, 9, 17}

    o1 : List
    i2 : (smooth,fib)=getSmoothingFamily(L,12,Verbose=>1)
        number of components = 1, codimension of components = {0}
        smoothing components numbers = {}

    o2 = (false, )

    o2 : Sequence
    i3 : (smooth,fib)=getSmoothingFamily(L,11,Verbose=>1)
    number of components = 2, codimension of components = {1, 2}
    semigroup = {6, 7, 9, 17}
    dim and degree singF = (0, 4)
    smoothing components numbers = {1}

                        3    2      12   3    2        15   2               17 
    o3 = (true, ideal (x  - x  - x z  , x  - x x  - x z  , x x  - x x  - x z  ,
                    0    3    0      1    0 3    0      1 3    0 5    0    
     ------------------------------------------------------------------------
        2             15      17   2 2           2 12      17     3    2  
     x x  - x x  + x z   - x z  , x x  - x x  - x z   - x z  , x x  - x  +
      0 3    1 5    3       1      0 1    3 5    1       3      1 3    5  
     ------------------------------------------------------------------------
      2   15       17      27    34
     x x z   - 2x z   - x z   - z  ))
      0 1        5       1

    o3 : Sequence
    i4 : range=makeRange(L,{4})

    o4 = {4, 8, 12, 16, 20, 24, 28, 32}

    o4 : List
    i5 : (smooth,fib, comps)=getSmoothingFamily(L,range,Verbose=>1)
    time to decompose J1 : 
    -- .0169433s elapsed

    component number = 0

    component number = 1
    deformation weights = {{12}, {8}, {8}, {4}, {4}, {4}}
    semigroup = {6, 7, 9, 17}
    dim and degree singF = (0, 8)
    smoothing components numbers = {1}
    -- .00109796s elapsed
    flat = true

                        3    2    2 4      12   3    2        4        8      12 
    o5 = (true, ideal (x  - x  - x z  + x z  , x  - x x  - x z  - x x z  - x z  ,
                    0    3    1      0      1    0 3    5      0 1      3    
         ------------------------------------------------------------------------
           2             2   4        8      16     2                8    2 12 
          x x  - x x  - x x z  - x x z  - x z  , x x  - x x  - 2x x z  - x z  ,
           1 3    0 5    0 1      0 3      1      0 3    1 5     1 3      0    
         ------------------------------------------------------------------------
          2 2                  4     2 8      20     3    2          4         8
         x x  - x x  - 2x x x z  - 2x z  + x z  , x x  - x  - x x x z  - 3x x z 
          0 1    3 5     0 1 3       3      0      1 3    5    0 1 5       3 5  
         ------------------------------------------------------------------------
                  12     2 16      28
        - 3x x x z   - 3x z   + x z  ), {1})
            0 1 3        3       0

    o5 : Sequence
    i6 : (smooth,fib, comps)=getSmoothingFamily(L,range,Verbose=>2)
       time to decompose J1 : 
       -- .0271562s elapsed
       number of components = 2, codimension of components = {2, 1}

       component number = 0

       component number = 1
       deformation weights = {{12}, {8}, {8}, {4}, {4}, {4}}
       codim J3 = 0, numgens J3 = 0
       semigroup = {6, 7, 9, 17}
       dim and degree singF = (0, 8)
       dim and degree singF = (0, 8)
       smoothing components numbers = {1}
       -- .000965479s elapsed
       flat = true

                        3    2    2 4      12   3    2        4        8      12 
    o6 = (true, ideal (x  - x  - x z  + x z  , x  - x x  - x z  - x x z  - x z  ,
                    0    3    1      0      1    0 3    5      0 1      3    
         ------------------------------------------------------------------------
          2             2   4        8      16     2                8    2 12 
         x x  - x x  - x x z  - x x z  - x z  , x x  - x x  - 2x x z  - x z  ,
          1 3    0 5    0 1      0 3      1      0 3    1 5     1 3      0    
         ------------------------------------------------------------------------
          2 2                  4     2 8      20     3    2          4         8
         x x  - x x  - 2x x x z  - 2x z  + x z  , x x  - x  - x x x z  - 3x x z 
          0 1    3 5     0 1 3       3      0      1 3    5    0 1 5       3 5  
        ------------------------------------------------------------------------
                  12     2 16      28
        - 3x x x z   - 3x z   + x z  ), {1})
            0 1 3        3       0

    o6 : Sequence
  Text
    The intermediate output is explained in smoothnessWithReductions.
SeeAlso
   makeRange
   smoothnessWithReductions
///

doc ///
Key
   makeRange
   (makeRange,List,List)
   [makeRange,Verbose]
Headline
   Make a range of degrees for getSmoothingFamily
Usage
   range=makeRange(L,congruences)
Inputs
   L:List
     list of generators of a semigroups
   congruences:List
     ;list of congruences
 
Outputs
   range: List
     a range of degrees
Description
  Text
  Example  
    L={4,5,7}
    congruences={4,6}
    range1=makeRange(L,{4,6})
    elapsedTime (smooth,fib, comps)=getSmoothingFamily(L,range1)
  Text
    The range are degrees r which congruent 0 mod d for some d in the list congruences.
    To get a degree bound rane one can use the following.
  Example
    range2=drop(makeRange(L,{1}),9)
    elapsedTime (smooth,fib, comps)=getSmoothingFamily(L,range2,Verbose=>1)
SeeAlso
   makeRange
   getSmoothingFamily
///

doc ///
Key
   getFromDisk
   (getFromDisk,String)
Headline
   Read a file from the hard disk
Usage
   Ldone=getFromDisk(name)
Inputs
   name:String
     the name of a file
 
Outputs
   Ldone: List
Description
  Text
   Since reading from the hard disk does not work in the documentation, the command is in quotes.
  Example  
   "Ldone=getFromDisk(name)"
SeeAlso

///

doc ///
Key
   toDoList
   (toDoList,ZZ)
   (toDoList,ZZ,ZZ)
Headline
   Make a list of semigroups not previously known to be Weierstrass
Usage
   LL=toDoList g
   LL=toDoList(m,g)
Inputs
   g:ZZ
     the desired genus
   m:ZZ
     the multiplicity
 
Outputs
   LL: List
     of semigropus
Description
  Text
  Example  
   LL8=toDoList 8
   #LL8
   LL610=toDoList(6,9)
   #LL610
SeeAlso

///

doc ///
Key
   BaseField
Headline
   Option keys for various functions
///

doc ///
Key
   Bound
Headline
   Option keys for various functions
///

doc ///
Key
   CoeffBound
Headline
   Option keys for various functions
///

doc ///
Key
   CoeffSize
Headline
   Option keys for various functions
///



doc ///
Key
   HighestOrder1
Headline
   Option keys for various functions
///

-*
--doc ///
Node
  Key
   degreeMatrices
   (degreeMatrices, Complex)
   (degreeMatrices, Ideal)
   (degreeMatrices, List)
  Headline
   degree matrices of the maps in a complex
  Usage
   degmats = degreeMatrices F
   degmats = degreeMatrices I   
   degmats = degreeMatrices L
  Inputs
   F:Complex
   I:Ideal
   L:List
  Outputs
   degmats:List
  Description
    Text
     prints the degree matrices of a complex
    Example
     L = {3,4,5}
     I = semigroupIdeal L
     F = res I
     degreeMatrices F
     degreeMatrices I
     degreeMatrices L
  SeeAlso
   semigroupIdeal
///
*-

  doc ///
  Key
    satisfiesDegreeCondition1
    satisfiesDegreeCondition2
    (satisfiesDegreeCondition1,List)
    (satisfiesDegreeCondition2,List)
  Headline
   Does the semigroup ideal of L satisfies the degree condition for the 3rd respectively 2nd syzgy matrix?
  Usage
   answer = satisfiesDegreeCondition1 L
   answer = satisfiesDegreeCondition2 L
  Inputs
   L:List
     generators of a semigroup with 4 generators
  Outputs
   answer:Boolean
    the answer
  Description
    Text
     Does the resolution of the semigroup ideal of L satisfies the degree condition 1 respectively 2
     for the 2nd or 3rd syzygy matrix?
    Example
     L = {6,9,13,16}
     satisfiesDegreeCondition1 L
     satisfiesDegreeCondition2 L
     displaySyzygyMatrices L;
     L = {6,15,17,20}
     displaySyzygyMatrices L;
     satisfiesDegreeCondition1 L
  ///

doc ///
  Key
    displaySyzygyMatrices
    (displaySyzygyMatrices,List)
    [displaySyzygyMatrices,Verbose]
  Headline
   Display the syzygy matrices
  Usage
   A = displaySyzygyMatrices L
  Inputs
   L:List
     generators of a semigroup with 4 generators
  Outputs
   answer:Boolean
    the answer
  Description
    Text
     Does the resolution of the semigroup ideal of L satisfies the degree condition 1 respectively 2
     for the 2nd or 3rd syzygy matrix?
    Example
     L = {6,9,13,16}
     displaySyzygyMatrices L;
     satisfiesDegreeCondition1 L
     satisfiesDegreeCondition2 L
    Text
     The degree conditions are satisfied. In the next case there are not satisfied.
    Example
     L = {6,15,17,20}
     A=displaySyzygyMatrices(L,Verbose=>false);
     netList A
     satisfiesDegreeCondition1 L
     satisfiesDegreeCondition2 L
  ///

  doc ///
  Key
    hilbertBurchMatrices
    hilbertBurchConditions
    depthCondition1
    hasExactSubcomplex
    (hasExactSubcomplex,List)
    (hilbertBurchMatrices,List)
    (hilbertBurchConditions,List)
    (depthCondition1,List)
  Headline
   Check the depth conditions for the exactness of the 1,4,4,1 subcomplex of the 1,6,8,3 subcomplex
  Usage
   A = hilbertBurchMatrices L
   answer = hilbertBurchConditions L
   d = depthCondition1 L
   answer = hasExactSubcomplex L
  Inputs
   L:List
     generators of a semigroup with 4 generators
  Outputs
   A:List
    a List of Hilbert Burch submatrices of the 2nd syzygy matrix
   answer:Boolean
    the answer
    d:ZZ
     the depth of the 2x2 minors of the 2x4 submatrix of the 3rd syzygy matrix
  Description
    Text
     Exactness of the 1,4,4,1 subcomplex follows from depth condition on the
     Hilbert-Burch matrices
     or if the minors of the 4x2 submatrix of the 3rd syzygy matrix have depth >=2
    Example
     L = {6,9,13,16}
     satisfiesDegreeCondition1 L and satisfiesDegreeCondition2 L
     hilbertBurchMatrices L
     hilbertBurchConditions L
     depthCondition1 L
     hasExactSubcomplex L
    Text
     The degree conditions are satisfied. 
  ///


  
  doc ///
  Key
    give1683Format
    (give1683Format,List)
    (give1683Format,ZZ,ZZ,ZZ,ZZ) 
    (give1683Format,List,List,List,List) 
  Headline
   Does the semigroup ideal of L has a resolution with total betti numbers 1,6,8,3?
  Usage
   answer = give1683Format L
   answer = give1683Format(a,b,c,d)
   goodCases = give1683Format(r1,r2,r3,r4)
  Inputs
   L:List
     generators of a semigroup with 4 generators
   a: ZZ
   b: ZZ
   c: ZZ
   d:ZZ
     test the semigrups generated by {a,a+b,a+b+c,a+b+c+d}
   r1: List
   r2: List
   r3: List
   r4: List
    lists of integer a in r1,...,d in r4 to be tested
  Outputs
   answer:Boolean
    the answer
   goodCases: List
     list of semigroups which passed the test
  Description
    Text
     prints the degree matrices of a complex
    Example
     L = {6,9,13,16}
     I = semigroupIdeal L
     give1683Format L
     give1683Format(6,3,4,3)
     r1={6},r2={3,9},r3={2,4},r4={3}
     give1683Format(r1,r2,r3,r4)
  ///


doc ///
  Key
    getListOfIdeals
    (getListOfIdeals,List,String)
    [getListOfIdeals,Verbose]
  Headline
   Read a list of ideals from a dataBase
  Usage
   Js = getListOfIdeals(LL,name)
  Inputs
   LL:List
     list of semigroups
   name:String
     name of a .dbm file on the disk.
  Outputs
   Js:List
    list of ideals
  Description
   Text
    The database Y=openDatabase name contains an entries Y#(toString L|"ring") and
    Y#(toString L|"ideal") of a ring and an ideal over QQ for every L in LL;
    The function reads the corresponding list of ideals of one parameter smoothing families,
    and checks their homogeneity.
  Caveat
   If for L there are no entries in Y an error occurs.
///

doc ///
  Key
    checkFlatnessOfOneParameterFamilies
    (checkFlatnessOfOneParameterFamilies,List,String)
    [checkFlatnessOfOneParameterFamilies,Verbose]
  Headline
   Check flatness of 1-parameter famiiles
  Usage
   answer=checkFlatnessOfOneParameterFamilies(LL,name)
  Inputs
   LL:List
     list of semigroups
   name:String
     name of a " .dbm" file on the hard disk.
  Outputs
   answer:Boolean
    true if all 1-parameter family are flat families
  Description
   Text
    The database Y=openDatabase name contains an entries Y#(toString L|"ring") and
    Y#(toString L|"ideal") of a ring and an ideal over QQ for every L in LL;
    The function reads  the corresponding list of ideals of one parameter smoothing families,
    and checks whether they areflat families.
  Caveat
   If for L there are no entries in Y an error occurs.
///

doc ///
  Key
    checkSmoothnessOfOneParameterFamilies
    (checkSmoothnessOfOneParameterFamilies,List,String)
    [checkSmoothnessOfOneParameterFamilies,Verbose]
    [checkSmoothnessOfOneParameterFamilies,BaseField]
  Headline
   Check smoothness of 1-parameter families
  Usage
   (answer,toDoAgain)=checkSmoothnessOfOneParameterFamilies(LL,name)
  Inputs
   LL:List
     list of semigroups
   name:String
     name of a " .dbm" file on the hard disk.
  Outputs
   answer:Boolean
    true if all 1-parameter family are smoothing families
   toDoAgain:List
    list of elements L in LL where the smoothness check failed
  Description
   Text
    The database Y=openDatabase name contains an entries Y#(toString L|"ring") and
    Y#(toString L|"ideal") of a ring and an ideal over QQ for every L in LL;
    The function reads the corresponding list of ideals of one parameter smoothing families,
    and checks whether they are indeed smoothing families.
    The smoothness is checked by reduction to a finite field. Picking a bad prime field
    might give a few cases where the test fails. The corresponding semigroups are collected in
    the list toDoAgain.
  Caveat
   If for L there are no entries in Y an error occurs.
///
/// -* Test which requires files on the hard disk *-
restart
needsPackage"WeierstrassSemigroups"
LL=getFromDisk "fam8";#LL
LL1=LL|{{2,3}}
doneData="fam8.dbm"
Y=openDatabase doneData;
keys Y

Js=getListOfIdeals(LL1,doneData,Verbose=>1);
checkFlatnessOfOneParameterFamilies(LL,doneData,Verbose=>2)
checkSmoothnessOfOneParameterFamilies(LL,doneData,Verbose=>2)
(answer,toDoAgain)=checkSmoothnessOfOneParameterFamilies(LL,doneData,Verbose=>1,BaseField=>ZZ/5)
checkSmoothnessOfOneParameterFamilies(toDoAgain,doneData,Verbose=>2,BaseField=>ZZ/7)
checkSmoothnessOfOneParameterFamilies(LL,doneData,Verbose=>2,BaseField=>ZZ/7)
///

-* Test section *-


TEST ///-* makeUnfolding, pruneFamily, getFlatFamily*-
L = {3,4,5}
assert(genus L==2)
I=ideal semigroupRing(L)
(A,unfolding)=makeUnfolding I;
elapsedTime (base,family)=getFlatFamily(I,A,unfolding);
elapsedTime (base1,family1)=pruneFamily(I,base,family);
assert(#support family1==14)

///

TEST /// -* testBound, improveFamily, smoothnessWithReduction *-
L= {5,6,8}
assert(genus L == 6)
setRandomSeed("always the same");
(answer,J,comp) = testBound(L,4,CoeffSize=>2)
assert(getRangeOfOneParameterFamily(J) ==
        {{5}, {6}, {7}, {10}, {12}, {15}, {20}})
assert(smoothnessWithReductions J)
J1=improveFamily(J)
assert(smoothnessWithReductions J1)
assert(getRangeOfOneParameterFamily(J1) == {{10}, {12}, {20}})
assert(J_*/size == {4, 7, 8})
assert(J1_*/size == {3,4,4})
(base2,family2)=getParameterFamily J1
assert(base2==0)
support family2/degree== {{5}, {6}, {8}, {12}, {10}}
SA=QQ[support family2,Degrees=>apply(support family2,m->degree m)]
family3=sub(family2,SA)
Sz=ring J
vars Sz
z=last gens Sz
J2=ideal sub(family3,(vars Sz)_{0..2}|matrix{{-z^12,-z^10}})
smoothnessWithReductions(J2,Verbose=>1)
assert(sub(J1,ring J2)==J2)
///

TEST /// -* satisfiesDegreeCondition1,satisfiesDegreeCondition2,
hilbertBurchConditions,depthCondition1, displaySyzygyMatrices  *-
L = {6,9,13,16}

assert(satisfiesDegreeCondition1 L and satisfiesDegreeCondition2 L)
assert(hilbertBurchConditions L)
L={6,9,10,13}
genus L
assert(genus L==10)
assert(give1683Format L)
assert(satisfiesDegreeCondition2 L)
assert(not satisfiesDegreeCondition1 L)
displaySyzygyMatrices L;
--elapsedTime (answer,J,comp)=testBound(L,1,Verbose=>1)
///

TEST /// -* solvingFlatteningRelations *-
L = {5,7,11}
assert(genus L == 8)
I=semigroupIdeal L;
setRandomSeed("always the same");
elapsedTime (answer,J,comp)=testBound(L,4, Verbose => 2); -- 4.0193s elapsed
assert(answer)
elapsedTime (base1,family1)=getParameterFamily J; -- 2.38625s elapsed
base=last decompose base1;
assert(codim base == 2 and  numgens base == 2)
fbase=res base
assert(rank fbase_1== 2 and rank fbase_2 ==1)
elapsedTime  (worked,fiber)=solvingFlatteningRelations(base,family1,I) -- .0505967s elapsed
assert(worked)
///

TEST /// -* getSmoothingFamilyWithVersalDeformation,*-
L = {5,6,7,8}
assert(
    genus L
    == 5)
setRandomSeed("Always the same")
elapsedTime (worked,fibz)=getSmoothingFamilyWithVersalDeformation L
ring fibz
assert(worked)
assert(fibz_*/size == {7, 6, 10, 9, 9})
elapsedTime J=improveFamily fibz
assert(J_*/size == {6, 5, 7, 5, 5})
///
end--

restart
needsPackage "WeierstrassSemigroups"

installPackage "WeierstrassSemigroups"
viewHelp "WeierstrassSemigroups"

check("WeierstrassSemigroups")


-* computation section*-
-* starting search in genus 13 *-
restart
needsPackage "WeierstrassSemigroups"
LL=toDoList(7,13)
#LL
X="genus13Families.dbm"
Y=openDatabase X
tally keys Y
krings=apply(#keys Y//2,i->(sort(keys Y))_(2*i+1))
tally apply(krings,k->(S=value (Y#k);coefficientRing S))
LL51= apply(krings,k->(S=value (Y#k);flatten drop(degrees S,-1)))
elapsedTime answer=checkFlatnessOfOneParameterFamilies(LL51,"genus13Families.dbm")
elapsedTime (answer,toDoAgain)=checkSmoothnessOfOneParameterFamilies(LL51,
    "genus13Families.dbm",Verbose=>2)
close Y
run "rm done13"
W="done13"
--Z=openOut W
apply(LL51,L->(
   openOutAppend W;
    W<<L;
    W << ", ";
    W<<close;)
)


#getFromDisk W==51


b=13
elapsedTime collectByBound(LL,b,"done13","genus13Families.dbm",Verbose=>2)
elapsedTime collectWithVersalDeformations(LL,b,"done13","genus13Families.dbm",Verbose=>2)

elapsedTime collectByBound(LL,b,"done13","genus13Families.dbm",Verbose=>2)
elapsedTime collectWithVersalDeformations(LL,b,"done13","genus13Families.dbm",Verbose=>2)


-* counting non-Weierstrass semingroup of format 1683 *-

restart
needsPackage "WeierstrassSemigroups"

a=10;
b=10;
c=10;
d=10;
r1=toList(4..6+a)
r2=toList(1..b)
r3=toList(1..c)
r4=toList(1..d)
elapsedTime LL1683=give1683Format(r1,r2,r3,r4); #LL1683 -- 68.8033s elapsed

tally apply(LL1683,L->min L)
elapsedTime LL1441=select(LL1683,L->hasExactSubcomplex L); #LL1441
ta=tally apply(LL1683,L->genus L)
ta1=tally apply(LL1441,L->genus L)
ma=max values ta
gm=first select(keys ta, k->ta#k==ma)
#select(LL1683,L->genus L==gm)
#select(LL1441,L->genus L==gm)
