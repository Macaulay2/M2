newPackage( "SegreClasses",
    Version =>"1.03",
    Date => "May 22, 2022",
    Authors => {
        {Name => "Martin Helmer",
         Email => "mhelmer@ncsu.edu",
         HomePage => "http://martin-helmer.com"},
        {Name => "Corey Harris",
         Email => "Corey.Harris@mis.mpg.de",
         HomePage => "http://coreyharris.name"}
    },
    Headline => "test containment of varieties and computes algebraic multiplicity of subvarieties and Fulton-MacPherson intersection products, via a very general Segre class computation",
    Keywords => {"Intersection Theory"},
    DebuggingMode => false
);

export {
   "chowClass",
   "intersectionProduct",
   "isMultiHom",
   "makeChowRing",
   "makeProductRing",
   "multiplicity",
   "projectiveDegree",
   "projectiveDegrees",
   "containedInSingularLocus",
   "segre",
   "segreDimX",
   "isComponentContained"
}

chowRing = getSymbol "chowRing";
hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"

IntersectionRing = new Type of MutableHashTable
globalAssignment IntersectionRing
intersectionRing = method(TypicalValue => IntersectionRing)
intersectionRing(Ring,ZZ) := (R,ambientdim) -> (
    basisR := flatten entries sort basis R;
    pointClassIndex := position(basisR, w -> sum(flatten exponents(w))==ambientdim);
    codim1basis := select(basisR, w -> sum(degree w)==1 );
    return new IntersectionRing from {
        global ring => R,
        global pointClass => basisR#pointClassIndex,
        global codim1Basis => codim1basis,
        global basis => basisR,
        global ambientDim => ambientdim
    }
)

ring IntersectionRing := R -> ( R.ring )
pointClass = R -> ( R.pointClass )
codim1Basis = R -> ( R.codim1Basis )
ambientDim = R -> ( R.ambientDim )
basis IntersectionRing := R -> ( R.basis )

Scheme = new Type of MutableHashTable
globalAssignment Scheme
toString Scheme := net Scheme := X -> (
    if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary)
    else "a scheme")
Scheme#{Standard,AfterPrint} = X -> (
    << concatenate(interpreterDepth:"o") << lineNumber << " : "
)

scheme = method(TypicalValue => Scheme, Options => {})
scheme Ideal :=  opts -> I -> (
    R := ring I;
    A := makeChowRing(R);
    return new Scheme from {
        global ideal => I,
        global ring => R,
        global coefficientRing => coefficientRing ring I,
        global chowRing => intersectionRing(A, numgens(R)-length unique degrees R)
    }
)
scheme(Ideal,IntersectionRing) :=  opts -> (I,intRing) -> (
    R := ring I;
    return new Scheme from {
        global ideal => I,
        global ring => R,
        global coefficientRing => coefficientRing ring I,
        global chowRing => intRing
    }
)

ideal Scheme := X -> ( X.ideal )
ring Scheme := X -> ( X.ring )
codim Scheme := {} >> opts -> X -> (
    if not X.?codim then ( X.codim = codim ideal leadTerm gb(X) );
    return X.codim
)
gb Scheme := opts -> X -> (
    if not X.?gb then ( X.gb = groebnerBasis(ideal X) );
    return X.gb
)
dim Scheme := X -> (
    if not X.?dim then (
        X.dim = numgens(ring X) - #(unique degrees ring X) - codim(X);
    );
    return X.dim
)
saturate Scheme :=opts ->  X -> (
    if not X.?saturate then (
        X.saturate = saturate(ideal X,product (values partition(degree, gens(ring(X))) / ideal));
    );
    return X.saturate;
)
containedInSingularLocus = method(TypicalValue => Boolean,Options => {Verbose=>false});
containedInSingularLocus (Ideal,Ideal) :=opts-> (IX,IY) -> (
    eXY:=multiplicity(IX,IY);
    if opts.Verbose then <<"e_XY= "<<eXY<<endl;
    return eXY>1;
);
isComponentContained = method(TypicalValue => Boolean,Options => {Verbose=>false});
isComponentContained (Ideal,Ideal) :=opts-> (IX,IY) -> (
    if not isMultiHom(IY) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    R:=ring IY;
    A:=makeChowRing(R);
    kk:=coefficientRing(R);
    Y:=makeMultiHom (IY,ideal 0_R);
    X:=makeMultiHom (IX,ideal 0_(ring IX));
    Theta:=sub(ideal(sum((numgens X),j->random(kk)*X_j)),R);
    F:=ideal sum((numgens Y),j->random(kk)*Y_j);
    Z:=Theta*F;
    s1:=segreDimX(X,Z,A);
    if opts.Verbose then <<"{Lambda(X,Z)}_dim(X)= "<<s1<<endl;
    s2:=segreDimX(X,Theta,A);
    if opts.Verbose then <<"{Lambda(X,Theta)}_dim(X)= "<<s2<<endl;
    return s1!=s2;
);
projectiveDegree = method(TypicalValue => RingElement,Options => {Verbose=>false});
projectiveDegree (Ideal,Ideal,RingElement) :=opts-> (IX,IY,w) -> (
    Y := scheme(IY);
    X := scheme(IX+IY);
    R := ring Y;
    Y.chowRing = intersectionRing(ring w, numgens(R)-length unique degrees R);
    X.chowRing = Y.chowRing;
    --X.equations = IX;

    return projectiveDegree(X,Y,w);
);
projectiveDegree(Scheme,Scheme,RingElement) := opts -> (sX,sY,w) -> (
    A := sX.chowRing;
    X := ideal sX;
    Y := ideal sY;
    R:=ring X;
    kk:=coefficientRing(R);
    Ls:=0;
    wDims:=flatten exponents(A.pointClass//w);
    wTotalDim := sum wDims;
    for i from 0 to length(wDims)-1 do (
        if wDims_i!=0 then (
            Ls=Ls+sum(wDims_i,j->ideal(random(OneAti(degreeLength R,i),R)));
        );
    );
    --TODO: Cache makeMultiHom(X,sY), maybe in sX or sY?
    Wg:=flatten entries gens (makeMultiHom(X,sY)+Ls);
    G:=ideal(for j from 1 to dim(sY)-wTotalDim list sum(Wg,g->random(kk)*g));
    irrelevantIdealsHash := partition(degree, gens R);
    irrelevantIdeals := values irrelevantIdealsHash / ideal;
    LA := sum(irrelevantIdeals, p -> sub(ideal(1-sum(numgens(p),i->random(kk)*p_i)),R));
    pd := 0;
    S:=kk(monoid[gens R,getSymbol "TTT"]);
    TTT := last gens S;
    EqT:=ideal( 1-TTT*sum((numgens X),j->(random(kk)*substitute(X_j,S))));
    ZeroDimGB:=groebnerBasis(sub(Y+Ls+G+LA,S)+EqT, Strategy=>"F4");
    pd = numColumns basis(cokernel leadTerm ZeroDimGB);
    return pd;
);

projectiveDegrees = method(TypicalValue => List,Options => {Verbose=>false});
projectiveDegrees(Ideal,Ideal,QuotientRing) := opts -> (I,J,A) -> (
    if not isMultiHom(I) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    if not isMultiHom(J) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    R :=ring J;
    n:=numgens(R)-length unique degrees R;
    IA := intersectionRing(A,n);
    Y := scheme(J,IA);
    X := scheme(I+J,IA);
    --X.equations = I;
    return projectiveDegrees(X,Y);
)

projectiveDegrees(Ideal,Ideal) := opts -> (I,J) -> (
    R :=ring J;
    if not isMultiHom(I) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    if not isMultiHom(J) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    n:=numgens(R)-length unique degrees R;
    IA := intersectionRing(makeChowRing R,n);
    Y := scheme(J,IA);
    X := scheme(I+J,IA);
    --X.equations = I;
    return projectiveDegrees(X,Y);
)
projectiveDegrees (Scheme,Scheme) := opts -> (X,Y) -> (
    A := X.chowRing;
    projectiveDegreesList := {};
    for i from 0 to dim(X) do (
        for w in A.basis do (
            if sum(flatten exponents(w))==A.ambientDim-i then (
                pd:=projectiveDegree(X, Y, w,opts);
                projectiveDegreesList = append(projectiveDegreesList,pd*w);
            );
        );
    );
    return projectiveDegreesList;
)
segreDimX = method(TypicalValue => RingElement,Options => {Verbose=>false});
segreDimX (Ideal,Ideal,QuotientRing) := opts -> (X,Y,A) -> (
    if not isMultiHom(X) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    if not isMultiHom(Y) then (print "the second ideal is not multi-homogenous, please correct this"; return 0;);
    R :=ring Y;
    n:=numgens(R)-length unique degrees R;
    IA := intersectionRing(A,n);
    sY := scheme(Y,IA);
    sX := scheme(X+Y,IA);
    segreClass:=0_A;
    basisByCodim := partition(i -> sum(flatten exponents i), IA.basis);
    projectiveDegreesList := {};
    for w in IA.basis do (
	if sum(flatten exponents(w))==IA.ambientDim-dim(sX) then (
	    pd:=projectiveDegree(sX, sY, w);
	    projectiveDegreesList = append(projectiveDegreesList,pd*w);
	    );
	);
    --print projectiveDegreesList;
    --find the max multidegree, write it as a class alpha
    i:= codim(sX);
    maxDegs := for d in transpose degrees (X+Y) list max d;
    alpha := sum(length IA.codim1Basis,i->(basis(OneAti(degreeLength R,i),A))_0_0*maxDegs_i);
    RHS:=sum(0..dim sX,i->alpha^(dim sY-i)*chowClass(sY)) - sum(projectiveDegreesList);
    for w in basisByCodim#(i) do (
	L:=(IA.pointClass//w);
        C:=RHS_(w)-(L*(1+alpha)^(dim(sY)-sum(flatten exponents(L)))*segreClass)_(IA.pointClass);
        segreClass=segreClass+C*w;
        );
    --print segreClass;
    return segreClass;
);

multiplicity=method(TypicalValue=>ZZ,Options => {Verbose=>false});
--TODO Add multiplicity (Scheme,Scheme), make this a wrapper
multiplicity (Ideal,Ideal) := opts->(I1,I2) -> (
    if opts.Verbose then print "multiplicity(I,J) computes e_XY where X is the top equidimensional component of V(I) and Y=V(J) (Y is assumed to be irreducible)";
    if not isMultiHom(I1) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    if not isMultiHom(I2) then (print "the second ideal is not multi-homogenous, please correct this"; return 0;);
    R :=ring I2;
    A:=makeChowRing(R);
    n:=numgens(R)-length unique degrees R;
    IA := intersectionRing(A,n);
    sY := scheme(I2,IA);
    sX := scheme(I1+I2,IA);
    clX:=chowClass(I1+I2,A,Strategy=>"multidegree");
    ha:=first flatten entries monomials clX;
    ba:=clX_(ha);
    ga:=projectiveDegree(sX,sY,ha);
            --find the max multidegree, write it as a class alpha
    maxDegs := for d in transpose degrees (I1+I2) list max d;
    alpha := sum(length IA.codim1Basis,i->(basis(OneAti(degreeLength R,i),A))_0_0*maxDegs_i);
    Lambda:=alpha^(dim(sY)-dim(sX))*chowClass(I2,A);
    ca:=Lambda_(ha);
    eXY:=(ca-ga)/ba;
    --<<"g_a= "<<ga<<", Lambda_a= "<<(ca-ga)<<endl; 
    return eXY;
);

chowClass=method(TypicalValue=>RingElement,Options => {Strategy=>"multidegree"});
chowClass Scheme := opts -> X -> (
    -- if not X.?chowClass then X.chowClass = chowClass(ideal X,ring(X.chowRing));
    if X.?chowClass then return X.chowClass;
    R := ring X;
    IA := X.chowRing;
    classI:=0;
    if opts.Strategy=="multidegree" then (
        md:=multidegree saturate X;
        classI=sub(md,matrix{gens ring IA});
	) 
    else(
        irrelevantIdealsHash := partition(degree, gens R);
        irrelevantIdeals := values irrelevantIdealsHash / ideal;
        ZeroDimGB:=0;
        kk:=coefficientRing(R);
        LA := sum(irrelevantIdeals, p -> sub(ideal(1-sum(numgens(p),i->random(kk)*p_i)),R));
        c := for w in IA.basis list if sum(flatten(exponents(w)))==codim X then w else continue;
        for w in c do (
            Ls:=0;
            wDims:=flatten exponents(IA.pointClass//w);
            for i from 0 to length(wDims)-1 do (
                if wDims_i!=0 then (
                    Ls=Ls+sum(wDims_i,j->ideal(random(OneAti(degreeLength R,i),R)));
                );
            );
            ZeroDimGB=ideal groebnerBasis(saturate(X)+Ls+LA, Strategy=>"F4");
            classI=classI+(numColumns basis(cokernel leadTerm ZeroDimGB))*w;
        );
    );
    X.chowClass = classI;
    return X.chowClass;
);
chowClass (Ideal,QuotientRing) := opts -> (I,A) -> (
    if not isMultiHom(I) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    R:=ring I;
    n:=numgens(R)-length unique degrees R;
    IA := intersectionRing(A,n);
    return chowClass(scheme(I,IA),opts);
    --return sub(multidegree I, matrix{ gens A })
)
chowClass (Ideal) := opts -> (I) -> (
    if not isMultiHom(I) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    return chowClass(scheme(I),opts);
    --A:=makeChowRing(ring I);
    --return sub(multidegree I, matrix{ gens A });
);

isMultiHom=method(TypicalValue=>Boolean);
isMultiHom Ideal:=I->(
    Igens:=flatten entries gens(I);
    d:=0;
    fmons:=0;
    for f in Igens do (
        fmons=flatten entries monomials(f);
        if length(fmons)>1 then (
            d=degree(first(fmons));
            for mon in fmons do (
                if degree(mon)!=d then (
                    <<"Input term below is not homogeneous with respect to the grading"<<endl;
                    <<f<<endl;
                    return false;
                );
            );
        );
    );
return true;
);
isMultiHom RingElement:=f->(
    return isMultiHom(ideal(f));
);

makeMultiHom=method(TypicalValue=>Ideal);
makeMultiHom (Ideal,Ideal):=(I,J) -> (
    makeMultiHom(I, scheme(J))
);
makeMultiHom (Ideal,Scheme):=(eqsX,Y)->(
    J := ideal Y;
    I:=eqsX+J;
    R:=ring I;
    n:=numgens(R)-length(unique degrees R);
    kk:=coefficientRing R;
    gensI:= delete(0_R,flatten sort entries gens eqsX);
    homGens:={};
    maxDegs := for d in transpose degrees I list max d;
    curIrel:=0;
    degDif:=0;
    tempfGens:=0;

    irrelevantIdealsHash := partition(degree, gens R);

    for f in gensI do (
        if degree(f)==maxDegs then (
            homGens=append(homGens,f);
        ) else (
            degDif=maxDegs-degree(f);
            tempfGens=ideal(f);
            for i from 0 to #degDif-1 do (
                curIrel=irrelevantIdealsHash#(OneAti(degreeLength R,i));
                tempfGens=tempfGens*ideal(for g in curIrel list g^(degDif_i));
            );
        homGens=join(homGens,flatten entries gens tempfGens);
        );
    );
    return ideal for j from 0 to dim(Y) list sum(homGens,l->l*random(kk)*random(0,4));
    --return ideal homGens;
);

makeProductRing=method(TypicalValue=>Ring);
makeProductRing (Symbol,List):=(x,l)->(
    kk:=ZZ/32749;
    return makeProductRing(kk,x,l);
);
makeProductRing (Ring,List):=(kk,l)->(
    x:=getSymbol "x";
    return makeProductRing(kk,x,l);
);
makeProductRing (List):=(l)->(
    x:=getSymbol "x";
    kk:=ZZ/32749;
    return makeProductRing(kk,x,l);
);
makeProductRing (Ring, Symbol,List):=(kk,x,l)->(
    if not isField(kk) then (
        <<"The coefficient ring must be a field, using the default field kk=ZZ/32749"<<endl;
        kk=ZZ/32749;
    );
    m:=length(l);
    numVars:=sum(l)+m;
    degs:={};
    ind:=0;
    for n in l do (
        for i from 0 to n do (
            degs=append(degs,OneAti(m,ind));
        );
        ind=ind+1;
    );
    return kk(monoid[vars(0..numVars-1),Degrees=>degs]);
);

makeChowRing=method(TypicalValue=>QuotientRing);
makeChowRing (Ring):=(R)->(
    h := getSymbol "H";
    return makeChowRing(R,h);
);
makeChowRing (Ring,Symbol):=(R,h)->(
    Rdegs:=degrees R;
    ChDegs:=unique Rdegs;
    m:=length ChDegs;
    C:= ZZ(monoid[h_1..h_m, Degrees=>ChDegs]);
    K:={};
    ns:={};
    temp:=0;
    for d in ChDegs do (
        temp=0;
        for a in Rdegs do (
            if d==a then temp=temp+1;
        );
        ns=append(ns,temp);
    );

    for i from 0 to length(ChDegs)-1 do (
        K=append(K,C_(i)^(ns_i));
    );
    K=substitute(ideal K,C);
    A:=C/K;
    return A;
);

segre=method(TypicalValue => RingElement,Options => {Verbose=>false});
segre (Ideal,Ideal) :=opts-> (I1,I2) -> (
    A:=makeChowRing(ring(I2));
    return segre(I1,I2,A,opts);
);
segre (Ideal,Ideal,QuotientRing) :=opts->(X,Y,A) -> (
    if not isMultiHom(X) then (print "the first ideal is not multi-homogenous, please correct this"; return 0;);
    if not isMultiHom(Y) then (print "the second ideal is not multi-homogenous, please correct this"; return 0;);

    R :=ring Y;
    n:=numgens(R)-length unique degrees R;
    IA := intersectionRing(A,n);
    sY := scheme(Y,IA);
    sX := scheme(X+Y,IA);

    --find the max multidegree, write it as a class alpha
    maxDegs := for d in transpose degrees (X+Y) list max d;
    alpha := sum(length IA.codim1Basis,i->(basis(OneAti(degreeLength R,i),A))_0_0*maxDegs_i);
    if opts.Verbose then <<"[Y]= "<<chowClass(sY)<<", alpha= "<<alpha<<endl;

    -- All the hard work is hidden in the next line
    projectiveDegreesList := projectiveDegrees(sX,sY);
    if opts.Verbose then <<"Projective degrees= "<<projectiveDegreesList<<endl;

    --build segre class recursively from Proj Degs
    segreClass:=0_A;
    RHS:=sum(0..dim sX,i->alpha^(dim sY-i)*chowClass(sY)) - sum(projectiveDegreesList);
    basisByCodim := partition(i -> sum(flatten exponents i), IA.basis);
    for i from 0 to dim sX do (
        for w in basisByCodim#(codim(sX)+i) do (
            L:=(IA.pointClass//w);
            C:=RHS_(w)-(L*(1+alpha)^(dim(sY)-sum(flatten exponents(L)))*segreClass)_(IA.pointClass);
            segreClass=segreClass+C*w;
            --<<"L= "<<L<<", w= "<<w<<", SegClass= "<<segreClass<<" exp (1+alpha)= "<<(dim(sY)-sum(flatten(exponents(L))))<<endl;
        );
    );
    if opts.Verbose then <<"s(X,Y)= "<<segreClass<<endl;
    return segreClass;
);


intersectionProduct=method(TypicalValue => RingElement,Options => {Verbose=>false});
intersectionProduct (Ideal,Ideal,Ideal) :=opts-> (I1,I2,I3) -> (
    A:=makeChowRing(ring(I2));
    return intersectionProduct(I1,I2,I3,A,opts);
);
intersectionProduct (Ideal,Ideal,Ideal,QuotientRing) :=opts->(Ix,Iv,Iy,A) -> (
    --figure out what ambient (product of) projective space/spaces we are in... need to add code for ambient toric
    R:=ring Ix;
    numFactors := degreeLength R;
    factorDims := factorDimensions(R);
    prodSpaceNs:=join(factorDims,factorDims);
    S:= makeProductRing(prodSpaceNs);
    mapFirstFactor:= map(S,R,take(gens S,numgens(R)));
    mapSecondFactor:= map(S,R,drop(gens S,numgens(R)));
    X:= mapFirstFactor(Ix+Iy);
    Y:= mapSecondFactor(Iv+Iy);

    -- split the vars by factor and take 2x2 minors of the sets of matrices
    factorVars := for d in (unique degrees R) list (partition(degree, gens R))#d;
    D := sum (for l in factorVars list minors(2, matrix{mapFirstFactor \ l,mapSecondFactor \ l}));

    seg:=segre(D,X+Y,opts);
    Aprod:=ring seg;
    temp:={};
    termDegrees := degree \ terms seg;
    segrePullbackCoeffs := new HashTable from (
        for t in terms seg list (
            {apply(take(degree t, numFactors), drop(degree t, numFactors), min), (last coefficients t)_(0,0)}
        )
    );
    segrePullbackTerms := for kvpair in pairs segrePullbackCoeffs list (
        (degList,coeff) := kvpair;
        p := product(apply(gens A, degList, (v,d)->v^d));
        --print(p);
        lift(coeff,ZZ) * p
    );
    segrePullback := sum segrePullbackTerms;

    dimY := sum(factorDims) - codim(Iy);
    expectDim:= dimY - codim(Ix,Iy) - codim(Iv,Iy);
    if opts.Verbose then <<"Segre pullback to diagonal = "<<segrePullback<<endl;
    --cY:=CSM(A,Iy)//chowClass(Iy,A);
    --Find the Chern Class of Y
    B:=flatten entries sort basis A;
    ns:=degree last B;
    n:=sum(ns);
    m:=length ns;
    ChernTR:=product(m,i->(1+(basis(OneAti(m,i),A))_0_0)^(ns_i+1));
    Vlist:={};
    dv:=0;
    gensIy:=flatten entries gens(Iy);
    for f in gensIy do(
    	dv=degree f;
    	Vlist=append(Vlist,sum(length dv, i-> dv_i*(basis(OneAti(m,i),A))_0_0));
    	);
    CE:=product(numgens(Iy),j->(1+Vlist_j));
    cY:=(ChernTR//CE);
    --done with Chern class
    if opts.Verbose then <<"Chern class = "<< cY <<endl;
    intProduct:=cY*segrePullback;
    return sum select(terms intProduct,j->sum(degree(j))==sum(factorDims)-expectDim);
);

----------------------------
-- utility functions
----------------------------

factorDimensions = R -> (
    -- if R = makeProdRing({a,b,c,...})
    -- then factorDimensions(R) = {a,b,c,...}
    return for deg in unique degrees R list (
        (tally degrees R)#deg - 1
    )
)

codim (Ideal,Ideal) := {} >> opts -> (X,Y) -> (
    -- returns codimension of X in Y
    return codim(X+Y) - codim(Y)
)

OneAti=(dl,i)->(
    vec:={};
    for j from 0 to dl-1 do (
        if j==i then vec=append(vec,1) else vec=append(vec,0);
    );
    return vec;
)


beginDocumentation()
multidoc ///

Node 
     Key
     	  SegreClasses
     Headline
	  Tests containment of varieties and computes algebraic multiplicity of subvarieties and Fulton-MacPherson intersection products - via a very general Segre class computation
     Description
     	  Text
	      This package tests containment of (irreducible) varieties and computes Segre classes, algebraic multiplicity, and Fulton-MacPherson intersection products. More generally, for subschemes of  \PP^{n_1}x...x\PP^{n_m}, this package tests if a top-dimensional irreducible component of the scheme associated to an ideal is contained in the scheme associated to another ideal. Specialized methods to test the containment of a variety in the singular locus of another are provided, these methods work without computing the ideal of the singular locus and can provide significant speed-ups relative to the standard methods when the singular locus has a complicated structure. The package works for subschemes of products of projective spaces.
	      The package implements methods described in [1]. More details and relevant definitions can be found in [1].
	      
	      References:\break
	      [1] Corey Harris and Martin Helmer. "Segre class computation and practical applications." arXiv preprint arXiv:1806.07408 (2018). Link: https://arxiv.org/abs/1806.07408.
Node 
    Key
    	segre
	(segre, Ideal,Ideal)
	(segre,Ideal, Ideal,QuotientRing)
    Headline
    	This method computes the Segre class of a scheme X inside a scheme Y, where X,Y are subschemes of some product of projective spaces
    Usage
    	segre(IX,IY)
	segre(IX,IY,A)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	A:QuotientRing
 	    the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This ring can be built by applying @TO makeChowRing@ to the coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
    Outputs
        s:RingElement
	    the Segre class of the subscheme X defined by IX in the subscheme Y defined by IY as a class in the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
    Description 
    	Text
	    For subschemes X,Y of \PP^{n_1}x...x\PP^{n_m} this command computes the Segre class s(X,Y) of X in Y as a class in the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
    	Example
	    R = makeProductRing({3,3})
	    x = gens(R)
	    D = minors(2,matrix{{x_0..x_3},{x_4..x_7}})
	    X = ideal(x_0*x_1,x_1*x_2,x_0*x_2)
	    segre(X,D)
	    A = makeChowRing(R)
	    s = segre(X,D,A)	        
Node 
    Key
    	intersectionProduct
	(intersectionProduct, Ideal,Ideal,Ideal)
	(intersectionProduct,Ideal, Ideal,Ideal,QuotientRing)
    Headline
    	A class in the Chow ring of the ambient space representing the Fulton-MacPherson intersection product of two schemes inside a variety
    Usage
    	intersectionProduct(IX,IV,IY)
	intersectionProduct(IX,IV,IY,A)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IV:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous ideal defining a smooth complete intersection in \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	A:QuotientRing
 	    the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This ring can be built by applying @TO makeChowRing@ to the coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
    Outputs
        intProd:RingElement
	    a class in the Chow ring A of \PP^{n_1}x...x\PP^{n_m} representing the Fulton-MacPherson intersection product of X with V in Y (where X is the scheme associated to IX, etc.).
    Description 
    	Text
	    For subschemes X,V of a smooth complete intersection subvariety Y of \PP^{n_1}x...x\PP^{n_m} this command computes the Fulton-MacPherson intersection product of X with V in Y as a class in the Chow ring of \PP^{n_1}x...x\PP^{n_m}. Note that this command requires that Y is a smooth complete intersection subvariety, however this is not checked internally.  
    	Example
	    R = makeProductRing({3})
	    (x,y,z,w) = toSequence gens R
	    Q = ideal(x*y-z*w)
	    L1 = ideal(x,w)
	    L2 = ideal(y,w)
	    intersectionProduct(L1,L2,Q,Verbose=>true)
	    intersectionProduct(L1,L1,Q)
Node 
    Key
    	projectiveDegrees
	(projectiveDegrees, Ideal,Ideal)
	(projectiveDegrees,Ideal, Ideal,QuotientRing)
    Headline
    	This method computes the projective degrees of a scheme X inside a scheme Y, where X,Y are subschemes of some product of projective spaces
    Usage
    	projectiveDegrees(IX,IY)
	projectiveDegrees(IX,IY,A)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	A:QuotientRing
 	    the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This ring can be built by applying @TO makeChowRing@ to the coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
    Outputs
        pd:List
	    a list of the projective degrees of a subscheme X defined by IX in the subscheme Y defined by IY as classes in the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
    Description 
    	Text
	    For subschemes X,Y of \PP^{n_1}x...x\PP^{n_m} this command computes  a list of the projective degrees of a subscheme X in the subscheme Y as classes in the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
    	Example
	    R = makeProductRing({3,3})
	    x = gens(R)
	    D = minors(2,matrix{{x_0..x_3},{x_4..x_7}})
	    X = ideal(x_0*x_1,x_1*x_2,x_0*x_2)
	    projectiveDegrees(X,D)
	    A = makeChowRing(R)
	    pd = projectiveDegrees(X,D,A)
Node 
    Key
    	projectiveDegree
	(projectiveDegree, Ideal,Ideal,RingElement)
    Headline
    	This method computes a single projective degree of a scheme X inside a scheme Y, where X,Y are subschemes of some product of projective spaces
    Usage
    	projectiveDegree(IX,IY,h)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	h:RingElement
 	    an element of the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This ring can be built by applying @TO makeChowRing@ to the coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
    Outputs
        pd:RingElement
	    a projective degree associated to h of a subscheme X defined by IX in the subscheme Y defined by IY as classes in the Chow ring A of \PP^{n_1}x...x\PP^{n_m}.
    Description 
    	Text
	    For subschemes X, Y of \PP^{n_1}x...x\PP^{n_m} this command computes a projective degree associated to h of a subscheme X in the subscheme Y as classes in the Chow ring of \PP^{n_1}x...x\PP^{n_m}. The value returned is an integer. This method is faster if only one projective degree is needed. 
    	Example
	    R = makeProductRing({3,3})
	    x = gens(R)
	    D = minors(2,matrix{{x_0..x_3},{x_4..x_7}})
	    X = ideal(x_0*x_1,x_1*x_2,x_0*x_2)
	    A = makeChowRing(R)
	    pd = projectiveDegrees(X,D,A)
	    h=A_0^2*A_1^2
	    pdh=projectiveDegree(X,D,h)
	    (sum pd)_h==pdh
	    	    
Node 
    Key
    	segreDimX
	(segreDimX,Ideal, Ideal,QuotientRing)
    Headline
    	This method computes the dimension X part of the Segre class of a scheme X inside a scheme Y, where X,Y are subschemes of some product of projective spaces
    Usage
	segreDimX(IX,IY,A)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	A:QuotientRing
 	    the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This ring can be built by applying @TO makeChowRing@ to the coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
    Outputs
        s:RingElement
	    the dimension X part of the Segre class of the subscheme X defined by IX in the subscheme Y defined by IY as a class in the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
    Description 
    	Text
	    For subschemes X,Y of \PP^{n_1}x...x\PP^{n_m} this command computes the dimension X part of the Segre class s(X,Y) of X in Y as a class in the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This is faster than computing the entire Segre class.
    	Example
	    R = makeProductRing({2,2})
	    x = gens(R)
	    Y = ideal(random({2,2},R));
	    X = Y+ideal(x_0*x_3+x_1*x_4);
	    A = makeChowRing(R)
	    time s = segreDimX(X,Y,A)
	    time segre(X,Y,A)
Node 
    Key
    	chowClass
	(chowClass, Ideal)
	(chowClass,Ideal, QuotientRing)
    Headline
    	Finds the (fundamental) class of a subscheme in the Chow ring of the ambient space
    Usage
    	chowClass(IX)
	chowClass(IX,A)
    Inputs
    	Strategy =>"multidegree"
	    using "prob" uses a probabilistic method which is sometimes faster on large examples
	IX:Ideal
	    an ideal in the multi-graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	A:QuotientRing
 	    the Chow ring of \PP^{n_1}x...x\PP^{n_m}. This ring can be built by applying @TO makeChowRing@ to the coordinate ring of \PP^{n_1}x...x\PP^{n_m}.    
   Outputs
        isMultHom:RingElement
	    the class [X] in A where X is the subscheme associated to IX
   Description
       Text
       	   Given a subscheme X of \PP^{n_1}x...x\PP^{n_m} this method computes [X] in the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
       Example
       	   R=makeProductRing({6})
	   x=gens(R)
	   J=ideal(x_0*x_2-x_4*x_5)
	   clX=chowClass(J,Strategy=>"prob")
	   clX2=chowClass(J,ring(clX))
	   clX==clX2
Node 
    Key
    	isMultiHom
	(isMultiHom, Ideal)
    Headline
    	Tests if an ideal is multi-homogeneous with respect to the grading of its ring
    Usage
    	isMultiHom(X)
    Inputs
    	X:Ideal
	    an ideal in the multi-graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
   Outputs
        isMultHom:Boolean
	    whether the input ideal is multi-homogeneous with respect to the grading of its ring
   Description
       Text
       	   Given an ideal in the coordinate R ring of \PP^{n_1}x...x\PP^{n_m} this method tests if whether the input ideal is multi-homogeneous with respect to the grading on R.     
       Example
       	   R = makeProductRing({1,2})
	   x=gens R
	   degrees R
	   isMultiHom ideal(x_0^2*x_2+x_1*x_2^2)
	   isMultiHom ideal(x_0^2*x_2+x_1^2*x_3)
Node 
    Key
    	multiplicity
	(multiplicity, Ideal,Ideal)
    Headline
    	This method computes the algebraic (Hilbert-Samuel) multiplicity
    Usage
    	multiplicity(IX,IY)
    Inputs
        Verbose=>Boolean
    	IX:Ideal
	    a multi-homogeneous prime ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous primary ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.	    
   Outputs
        eXY:ZZ
	    the algebraic (Hilbert-Samuel) multiplicity e_XY of the variety X associated to IX in the scheme Y associated to IY. 
   Description
       Text
       	   For a subvariety X of an irreducible subscheme Y of \PP^{n_1}x...x\PP^{n_m}  this command computes the algebraic multiplicity e_XY of X in Y. Let R be the coordinate ring of \PP^{n_1}x...x\PP^{n_m}, let O_{X,Y}=(R/I_Y)_{I_X} be the local ring obtained by localizing (R/I_Y) at the prime ideal I_X, and let len denote the length of a local ring.
	   Let M be the unique maximal ideal of O_{X,Y}. The Hilbert-Samuel polynomial is the polynomial P_{HS}(t)=len(O_{X,Y}/M^t) for t large.  
	   In different words, this command computes the leading coefficient of the Hilbert-Samuel polynomial P_{HS}(t) associated to O_{X,Y}.
	   Below we have an example of the multiplicity of the twisted cubic in a double twisted cubic.      
       Example
       	   R = ZZ/32749[x,y,z,w]
	   X = ideal(-z^2+y*w,-y*z+x*w,-y^2+x*z)
	   Y = ideal(-z^3+2*y*z*w-x*w^2,-y^2+x*z)
	   multiplicity(X,Y)
Node 
    Key
    	isComponentContained
	(isComponentContained, Ideal,Ideal)
    Headline
	Tests containment of (irreducible) varieties
    Usage
    	isComponentContained(IX,IY)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
   Outputs
        isCompCont:Boolean
	    whether or not a top-dimensional irreducible (and reduced) component of the scheme X associated to IX is contained in the scheme Y associated to IY   
   Description
       Text
       	   For a subschemes X of an irreducible subscheme Y of \PP^{n_1}x...x\PP^{n_m} this command tests whether or not a top-dimensional irreducible (and reduced) component of X is contained in Y 
       Example
       	   R = makeProductRing({2,2,2})
	   x=(gens R)_{0..2}
	   y=(gens R)_{3..5}
	   z=(gens R)_{6..8}
	   m1=matrix{{x_0,x_1,5*x_2},y_{0..2},{2*z_0,7*z_1,25*z_2}}
	   m2=matrix{{9*z_0,4*z_1,3*z_2},y_{0..2},x_{0..2}}
	   W=minors(3,m1)+minors(3,m2);
	   f=random({1,1,1},R);
	   Y=ideal (z_0*W_0-z_1*W_1)+ideal(f);
	   X=((W)*ideal(y)+ideal(f));
	   time isComponentContained(X,Y)
	   print "we could confirm this with the computation:"
	   B=ideal(x)*ideal(y)*ideal(z)
	   time isSubset(saturate(Y,B),saturate(X,B))
Node 
    Key
    	containedInSingularLocus
	(containedInSingularLocus, Ideal,Ideal)
    Headline
    	This method tests is an irreducible variety is contained in the singular locus of the reduced scheme of an irreducible scheme 
    Usage
    	containedInSingularLocus(IX,IY)
    Inputs
    	Verbose=>Boolean
	IX:Ideal
	    a multi-homogeneous prime ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
	IY:Ideal
	    a multi-homogeneous primary ideal defining a closed subscheme of \PP^{n_1}x...x\PP^{n_m}; @TO makeProductRing@ builds the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
   Outputs
        contSingLoc:Boolean
	    whether or not the variety X associated to IX is contained in the singular locus of the variety associated to the radical of IY 
   Description
       Text
           For a subvariety X of \PP^{n_1}x...x\PP^{n_m} and an irreducible subscheme Y of \PP^{n_1}x...x\PP^{n_m} this command tests whether X is contained in the singular locus of the reduced scheme of Y (i.e. the singular locus of the variety defined by the radical of the ideal defining Y).
       Example
	   n=6
	   R = makeProductRing({n})
	   x=gens(R)
	   m=matrix{for i from 0 to n-3 list x_i,for i from 0 to n-3 list (i+3)*x_(i+3),for i from 0 to n-3 list x_(i+2),for i from 0 to n-3 list x_(i)+(5+i)*x_(i+1)}
	   C=ideal mingens(minors(3,m));
	   P=ideal(x_0,x_4,x_3,x_2,x_1)
	   containedInSingularLocus(P,C)	      
Node 
    Key
    	makeProductRing
	(makeProductRing, List)
	(makeProductRing,Ring ,List)
    Headline
    	Makes the coordinate ring of a product of projective spaces.
    Usage
    	makeProductRing(n)
	makeProductRing(kk,n)
    Inputs
    	n:List
	   a list {n_1,...,n_m} of the dimensions of the m projective spaces in the product \PP^{n_1}x...x\PP^{n_m}.
        kk:Ring
	   the coefficient ring to be used
    Outputs
     	  :Ring
	   the graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
    Description
     	  Text
	       Builds the multi-graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.    
          Example
	    R = makeProductRing(QQ,{3,4})
	    R = makeProductRing({3,4})
	    degrees R
	    gens R
Node 
    Key
    	makeChowRing
	(makeChowRing, Ring)
	(makeChowRing,Ring ,Symbol)
    Headline
    	Makes the Chow ring of a product of projective spaces.
    Usage
    	makeChowRing(R)
	makeChowRing(R,h)
    Inputs
    	n:List
	   a multi-graded coordinate ring of \PP^{n_1}x...x\PP^{n_m}.
        h:Symbol
	   the symbol to be used as the variable in the Chow ring
    Outputs
     	  :QuotientRing
	   the Chow ring of \PP^{n_1}x...x\PP^{n_m}.
    Description
     	  Text
	      Builds the Chow ring Z[H_1,...,H_m]/(H_1^{n_1+1},...,H_m^{n_m+1}) of \PP^{n_1}x...x\PP^{n_m}.  
          Example
	    R = makeProductRing({3,4})
	    R = makeChowRing(R)
	    describe R
///


TEST ///
-- union of coordinate axes in PP3 (diagonal)
-*
restart
installPackage "SegreClasses"
needsPackage "SegreClasses"
*-

R = makeProductRing({3,3})
x = gens(R)
D = minors(2,matrix{{x_0..x_3},{x_4..x_7}})
X = ideal(x_0*x_1,x_1*x_2,x_0*x_2)
A = ZZ[a,b,Degrees=>{{1,0},{0,1}}]/(a^4,b^4)
time s = segre(X,D,A,Verbose=>true)
assert(s == 3*(a^3*b^2+a^2*b^3)-10*(a^3*b^3))
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
R=makeProductRing({6})
x=gens(R)
degrees R
I=ideal(random(2,R),x_0^4-x_1*x_3^3-x_4*x_5^3)
J=ideal(x_0*x_2-x_4*x_5)
chowClass(J,Strategy=>"prob")
assert(multiplicity(I,J,Verbose=>true)==1)
///

TEST ///
-- union of coordinate axes in PP3 (diagonal)
-*
restart
needsPackage "SegreClasses"
*-
R = makeProductRing({3,3})
x = gens(R)
D = minors(2,matrix{{x_0..x_3},{x_4..x_7}})
X = ideal(x_0*x_1,x_1*x_2,x_0*x_2)
pds = projectiveDegrees(X,D,Verbose=>true)
A = ring first pds
(a,b) = toSequence gens A
l = {10*a^3*b^3, 6*a^2*b^3, 6*a^3*b^2, 3*a*b^3, 3*a^2*b^2, 3*a^3*b}
pds = pds  / (i -> sub(i,A))
assert(sort(pds)==sort(l))
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
R=makeProductRing({6})
x=gens(R)
degrees R
I=ideal(random(2,R),x_0^4-x_1*x_3^3-x_4*x_5^3)
J=ideal(x_0*x_2-x_4*x_5)
chowClass(J,Strategy=>"prob")
A = ZZ[h]/(h^7)
assert(segre(I,J,A,Verbose=>true)==16*h^3-96*h^4+448*h^5-1920*h^6)
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
kk=ZZ/32749
R = kk[x,y,z,w];
X = ideal(-z^2+y*w,-y*z+x*w,-y^2+x*z)
Y = ideal(-z^3+2*y*z*w-x*w^2,-y^2+x*z)
multiplicity(X,Y)

assert(multiplicity(X,Y)==2)
time assert(isComponentContained(X,Y,Verbose=>true)==true)
time assert (isComponentContained(Y,X)==true)


///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
-- Cx is a hyperplane section on a smooth quadric surface
-- embedded in the diagonal of P3xP3
R=makeProductRing({3,3})
x=(gens(R))_{0..3}
y=(gens(R))_{4..7}
Qx = ideal (x#0*x#1 - x#2*x#3)
Qy=sub(Qx,matrix{join(y,for i from 4 to 7 list 0)})
D = minors(2,matrix{x,y})
I=ideal(Qx,Qy,D) --Q in the diagonal
Cx=ideal random({1,0},R)
A = ZZ[a,b,Degrees=>{{1,0},{0,1}}]/(a^4,b^4)
s=segre(Cx,I,A,Verbose=>true)
assert(s == 2*(a^3*b^2+a^2*b^3-a^3*b^3))
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
R=makeProductRing({2,1});
x=(gens R)_{0..2};
y=(gens R)_{3..4};
I = ideal (x_0,x_1);  -- choosing a simple point to make things easier
B=ideal(y_0*I_1-y_1*I_0); ---blow up of this point...
E=B+ideal (x_0,x_1);
A = ZZ[a,b,Degrees=>{{1,0},{0,1}}]/(a^3,b^2)
s=segre(E,B,A,Verbose=>true)
assert(s == a^2 + a^2*b)
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
n=6
kk=ZZ/32749
R=kk[x_0..x_n]
A=makeChowRing(R)
X=ideal(x_2*x_3*x_5-5*x_6^2*x_0+3*x_2*x_0*x_1,random(3,R))
Y=ideal mingens(X*X);
time assert(isComponentContained(X,Y,Verbose=>true)==true)
time assert(isComponentContained(Y,X)==true)
Theta=ideal(random(kk)*X_0+random(kk)*X_1)
F=ideal(random(kk)*Y_0+random(kk)*Y_1+random(kk)*Y_2)
time assert(segreDimX(X,Theta,A)==9*A_0^2)
time segreDimX(X,Theta*F,A)
///


TEST ///
-*
restart
needsPackage "SegreClasses"
*-
n=6
R = makeProductRing({n})
x=gens(R)
m=matrix{for i from 0 to n-3 list x_i,for i from 0 to n-3 list (i+3)*x_(i+3),for i from 0 to n-3 list x_(i+2),for i from 0 to n-3 list x_(i)+(5+i)*x_(i+1)}
C=ideal mingens(minors(3,m))
P=ideal(x_0,x_4,x_3,x_2,x_1)
time containedInSingularLocus(P,C)
time assert(containedInSingularLocus(P,C,Verbose=>true)==true)
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
R = makeProductRing({3})
A=makeChowRing(R)
h=A_0
(x,y,z,w) = toSequence gens R
Q = ideal(x*y-z*w)
L1 = ideal(x,w)
L2 = ideal(y,w)
assert(intersectionProduct(L1,L2,Q,A)==h^3)
assert(intersectionProduct(L1,L1,Q,A)==0_A)
///

TEST ///
-*
restart
needsPackage "SegreClasses"
*-
R = makeProductRing({2,2,2})
x=(gens R)_{0..2}
y=(gens R)_{3..5}
z=(gens R)_{6..8}
B=ideal(x)*ideal(y)*ideal(z)
m1=matrix{{x_0,x_1,5*x_2},y_{0..2},{2*z_0,7*z_1,25*z_2}}
m2=matrix{{9*z_0,4*z_1,3*z_2},y_{0..2},x_{0..2}}
W=minors(3,m1)+minors(3,m2)
f=random({1,1,1},R)
Y=ideal (z_0*W_0-z_1*W_1)+ideal(f)
X=((W)*ideal(y)+ideal(f))
assert(isComponentContained(X,Y)==true)
///

end
	    



