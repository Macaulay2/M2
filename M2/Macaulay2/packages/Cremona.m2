
newPackage(
       "Cremona",
	Version => "1.1", 
    	Date => "Jun 3, 2016",
    	Authors => {{Name => "Giovanni Staglianò", 
		     Email => "giovannistagliano@gmail.com" 
                    }
                   },
    	Headline => "Some computations for rational maps between projective varieties",
    	DebuggingMode => false,
	Reload => false
    	)

export{
   "composeRationalMaps",
   "degreeOfRationalMap",   
   "invertBirMap",
   "isBirational",
   "isInverseMap",
   "kernelComponent",
   "projectiveDegrees",
   "toMap",
   "MathMode", 
   "Dominant", 
   "OnlySublist",
   "approximateInverseMap",
   "Multidegree"
};

needsPackage "Parametrization"

verbose:=false;

composeRationalMaps=method(TypicalValue => RingMap);
degreeOfRationalMap=method(TypicalValue => ZZ, Options => {MathMode => false});
invertBirMap=method(TypicalValue => RingMap, Options => {MathMode => false});
isBirational=method(TypicalValue => Boolean);
isInverseMap=method(TypicalValue => Boolean);
kernelComponent=method(TypicalValue => Ideal);
projectiveDegrees=method(TypicalValue => List, Options => {OnlySublist => infinity});
toMap=method(TypicalValue => RingMap, Options => {Dominant => null});
approximateInverseMap=method(TypicalValue => RingMap, Options => {Multidegree => null});

composeRationalMaps(RingMap,RingMap) := (phi,psi) -> (
   -- input: phi:P^n-->P^m, psi:P^m-->P^r ; output: P^n-->P^r
   eta:=phi*psi;
   linSys:=flatten entries toMatrix eta;
   fixComp:=gcd linSys;
      if isUnit fixComp then return map(target phi,source psi,linSys);
   if verbose then <<"Fixed component of the linear system: "<<toString(fixComp)<<endl;
   map(target phi,source psi,lift(matrix{linSys/fixComp},target phi))
);

degreeOfRationalMap RingMap := o -> (phi) -> (
   -- computes the degree of a rational map as the degree of the generic fibre
   -- input: rational map P^n-->P^m 
   -- output: integer, the degree of the rational map   
   checkRationalMapFromPn phi;
      if o.MathMode then return degreeOfMapMath phi;
   ringPn:=target phi;
   ringPm:=source phi;
   n:=numgens ringPn -1;
   m:=numgens ringPm -1;
   if m<n then return 0;
   p:=preimage(phi,randomLinearSubspace(ringPn,0));   
   idealFibre:=saturate(phi(p),ideal toMatrix phi);
   hP:=hilbertPolynomial(idealFibre,Projective=>false);
   degMap:=if degree hP > {0} then 0 else sub(hP,ZZ);
   degMap 
);

degreeOfMapMath = (phi) -> (
   -- input: rational map P^n-->P^m 
   -- output: integer, the degree of the rational map   
   ringPn:=target phi;
   ringPm:=source phi;
   n:=numgens ringPn -1;
   m:=numgens ringPm -1;
   if m<n then return 0;
    K:=coefficientRing ringPn;
    a:=local a;
    K':=frac(K[a_0..a_n]);
    Pn':=K'[gens ringPn];
    Pm':=K'[gens ringPm];
    phi':=map(Pn',Pm',sub(toMatrix phi,Pn'));
    x:=gens Pn';
   p:=preimage(phi',ideal(for i from 1 to n list a_i*x_0-a_0*x_i));
   idealFibre:=saturate(phi'(p),ideal toMatrix phi');
   hP:=hilbertPolynomial(idealFibre,Projective=>false);
   degMap:=if degree hP > {0} then 0 else sub(hP,ZZ);
   degMap 
);

invertBirMap (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   a:=ideal source phi;
   F:=toMatrix phi;
   G:=if (isPolynomialRing target phi and (o.MathMode =!= null)) then invertBirationalMapRS(F,a) else invertBirationalMapViaParametrization(F,a);
   psi:=map(source phi,target phi,G);
         if class o.MathMode === Boolean then if o.MathMode then try isInv:=(isInverseMap(phi,psi) and isInverseMap(psi,phi)) then (if isInv then <<"Good news! :-)  'invertBirMap' has successfully worked!"<<endl else <<"Bad news! :-(  The output of 'invertBirMap' is incorrect!"<<endl) else (<<"'MathMode' option failed, try 'isInverseMap()'"<<endl);
   psi
);

isBirational (RingMap) := (phi) -> (
   checkRationalMap phi;
   X:=target phi; Y:=source phi;
   if dim X != dim Y then return false;
   if isPolynomialRing X then return degreeOfRationalMap phi == 1;
   first projectiveDegrees(phi,OnlySublist=>0) == degree Y 
);

isInverseMap(RingMap,RingMap) := (mapF,mapG) -> (
   checkRationalMap mapF;
   checkRationalMap mapG;
   ringOfSource:=target mapF;
   try Composition:=toMatrix(mapF*mapG) else return false;
   fixedComponent:=sub(Composition_(0,0)/(vars ringOfSource)_(0,0),ringOfSource);
   identityOfSource:=fixedComponent*(vars ringOfSource);
   identityOfSource - Composition == 0
);

kernelComponent(RingMap,ZZ) := (phi,d) -> (
   checkRationalMap phi;
        if not isPolynomialRing target phi then (
           if (numgens ideal target phi == 1) and ( degree ideal target phi == d * (max flatten degrees ideal toMatrix phi) ) then (
               phi0:=map(ambient target phi,source phi,lift(toMatrix phi,ambient target phi)) * map(source phi,ambient source phi);
               K0:=ideal join variationOFhomogPartOfImage(phi0,ideal target phi);
               return trim sub(K0,source phi);
           );
           if verbose then <<"Running 'ideal image basis("<<d<<",kernel phi)'..."<<endl;
           return ideal image basis(d,trim kernel phi); 
        );
   phi':=phi * map(source phi,ambient source phi);
   K:=ideal homogPartOfImage(phi',d); 
   trim sub(K,source phi)
); 

projectiveDegrees (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   if o.OnlySublist < 0 then return {};
   k:=dim ideal target phi -1;
   L:={projDegree(phi,0,k)};
   for i from 1 to min(k,o.OnlySublist) do (
      phi=genericRestriction phi;
      L={projDegree(phi,0,k-i)}|L
   );
   L
);

toMap Matrix := o -> (F)  -> ( 
   checkLinearSystem F;
   K:=coefficientRing ring F; 
   N:=numgens source F-1; 
   if N == -1 then return map(ring F,K[]);   
   x:=local x;   -- (x,y,z,t,u,v)
   PN:=K[x_0..x_N]; 
   if numgens ambient ring F -1 == N then PN=ambient ring F;
   phi:=map(ring F,PN,F);
      if class o.Dominant === ZZ then (
      return map(ring F,PN/(kernelComponent(phi,o.Dominant)),F);
      ); 
      if (o.Dominant === infinity or o.Dominant === true) then (
      return map(ring F,PN/(saturate kernel phi),F);
      ); 
   return phi;
);
toMap List := o -> (F)  -> toMap(matrix{F},Dominant=>o.Dominant);
toMap Ideal := o -> (F)  -> toMap(gens F,Dominant=>o.Dominant);
toMap RingMap := o -> (phi)  -> (
      phi=phi * map(source phi,ambient source phi);
      if class o.Dominant === ZZ then (
      return map(target phi,(source phi)/(kernelComponent(phi,o.Dominant)),toMatrix phi);
      ); 
      if (o.Dominant === infinity or o.Dominant === true) then (
      return map(target phi,(source phi)/(saturate kernel phi),toMatrix phi);
      ); 
      if class o.Dominant === Ideal then if ring o.Dominant === source phi then if (phi o.Dominant == 0 and isHomogeneous o.Dominant) then (
      return map(target phi,(source phi)/(o.Dominant),toMatrix phi);
      ); 
   return phi;
);

toMap (Ideal,ZZ) := o -> (I,v) -> (
   if not isHomogeneous I then error("the ideal must be homogeneous");
   linSys:=gens image basis(v,I);
   toMap(linSys,Dominant=>o.Dominant)
);

toMap (Ideal,ZZ,ZZ) := o -> (I,v,b) -> (
   if not isHomogeneous I then error("the ideal must be homogeneous");
   if not isPolynomialRing ring I then error("expected ideal in a polynomial ring");
   if b!=1 and b!=2 then error("expected 1 or 2 as third argument");
   if b==1 then return toMap(I,v,Dominant=>o.Dominant);
   linSys:=linearSystemOfHypersurfacesOfGivenDegreeThatAreSingularAlongAGivenSubscheme(I,v);
   toMap(linSys,Dominant=>o.Dominant)
);

approximateInverseMap (RingMap) := o -> (phi) -> (
    if verbose then  <<"Running 'approximateInverseMap'..."<<endl;
    -- input: a birational map phi:X --->Y 
    -- output: a map Y--->X in some sense related to the inverse of phi
    checkRationalMap phi;
    n:=numgens ambient target phi -1;
    if o.Multidegree =!= null then (
        (d,c,b):=if class o.Multidegree === List then aboutBaseLocusOfInverseMap0(phi,o.Multidegree) else error("invalid multidegree provided"); 
    );
    phiRes:=local phiRes; B:=local B;
    if o.Multidegree =!= null then (
        B=trim sum for i from 1 to ceiling((n+1)/(c-1)) list (
             phiRes=phi;
             for i0 from 1 to c-1 do phiRes=genericRestriction phiRes;
             if verbose then <<"Computing kernel(..,SubringLimit=>"<<(c-1)<<")... ("<<i<<" of "<<ceiling((n+1)/(c-1))<<")"<<endl;
             kernel(phiRes,SubringLimit=>(c-1))  -- kernelComponent(phiRes,d)
          );
    ) else (
        B=trim sum for i from 1 to n+1 list (
             phiRes=genericRestriction phi;
             if verbose then <<"Computing kernel(..,SubringLimit=>1)... ("<<i<<" of "<<(n+1)<<")"<<endl;
             kernel(phiRes,SubringLimit=>1)
        );
    );
   if not isPolynomialRing source phi then (
         B=trim(sub(B,ambient source phi) + ideal image basis(min flatten degrees B,ideal source phi));
         B=sub(ideal for i to n list sum for j to numgens B -1 list (random coefficientRing target phi)*B_j,source phi)   
   );
   if not(numgens B == n+1 and min flatten degrees B == max flatten degrees B) then return approximateInverseMap(phi,Multidegree=>o.Multidegree);
   if o.Multidegree =!= null then if not (codim B == c and degree B == b and min flatten degrees B == d) then return approximateInverseMap(phi,Multidegree=>o.Multidegree);
   psi:=map(source phi,target phi,gens B);
   psi
);

checkRationalMap = (phi) -> ( -- phi RingMap
   if not isField coefficientRing target phi then error("the coefficient ring needs to be a field");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and (isPolynomialRing target phi or isQuotientRing target phi) and isHomogeneous ideal source phi and isHomogeneous ideal target phi) then error("source and target of the ring map need to be quotients of polynomial rings by homogeneous ideals");
   if not (isHomogeneous ideal toMatrix phi and max degrees ideal compress toMatrix phi == min degrees ideal compress toMatrix phi) then error("the map needs to be defined by homogeneous polynomials of the same degree");
);

checkRationalMapFromPn = (phi) -> ( -- phi RingMap
   if not isField coefficientRing target phi then error("the coefficient ring needs to be a field");
   if not isPolynomialRing target phi then error("the target of the ring map needs to be a polynomial ring");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and isHomogeneous ideal source phi) then error("the source of the ring map needs to be a quotient of a polynomial ring by a homogeneous ideal");
   if not (isHomogeneous ideal toMatrix phi and max degrees ideal compress toMatrix phi == min degrees ideal compress toMatrix phi) then error("the map needs to be defined by homogeneous polynomials of the same degree");
);

checkLinearSystem = (F) -> ( -- F row matrix
   if not isField coefficientRing ring F then error("the coefficient ring needs to be a field");
   if not ((isPolynomialRing ring F or isQuotientRing ring F) and isHomogeneous ideal ring F) then error("the base ring must be a quotient of a polynomial ring by a homogeneous ideal");
   if not (numgens target F == 1) then error("expected a row matrix");
   if numgens source F == 0 then return true;
   if not (isHomogeneous ideal F and max degrees ideal F == min degrees ideal F) then error("expected homogeneous elements of the same degree");
);

linearSystemOfHypersurfacesOfGivenDegreeThatAreSingularAlongAGivenSubscheme = (I,v) -> (
   -- returns the linear system of hypersurfaces of degree v that are singular along V(I) subset PP^d
   parametrizeLinearSubspace := (L) -> (
      K:=coefficientRing ring L;
      A:=transpose sub(last coefficients(gens L,Monomials=>toList(gens ring L)),K);
      N:=mingens kernel A;
      t:=local t;
      T:=K[t_0..t_(numgens source N-1)];
      psi:=map(T,ring L,(vars T)*transpose(N));
      -- <<"test for parametrizeLinearSubspace: "<<(if dim target psi > 0 then kernel psi == L else dim L <= 0)<<endl;
      return psi;
   );
   K:=coefficientRing ring I; 
   d:=numgens ring I -1;
   x:=local x;
   PP:=K[x_0..x_d];
   I=saturate I;
   C:=ideal image basis(v-1,sub(I,vars PP));
   n:=numgens C -1;
   if n==-1 then return sub(matrix{{}},ring I);
   Basis:=gens image basis(v,sub(I,vars PP));
   N:=numgens source Basis -1;
   if N==-1 then return sub(matrix{{}},ring I);
   a:=local a; b:=local b;
   R:=K[b_(0,0)..b_(n,d), a_0..a_N, MonomialOrder=>Eliminate ((d+1)*(n+1))];
   R':=R[x_0..x_d];
   M:=sub(jacobian Basis,R')*sub(transpose matrix{{a_0..a_N}},R') - transpose((gens sub(C,R'))*sub(matrix for i to n list for j to d list b_(i,j),R'));
   f:=parametrizeLinearSubspace sub(ideal selectInSubring(1,gens gb sub(trim ideal last coefficients M,R)),K[a_0..a_N]);
   PP':=PP[gens target f];
   linSys:=transpose sub(sub((coefficients (sub(matrix f,PP') * transpose sub(Basis,PP'))_(0,0))_1,PP),vars ring I);
   -- <<"test for linearSys... : "<<last(Test:=true, eLinSys:=flatten entries linSys, for i to #eLinSys-1 do Test and isSubset(ideal jacobian ideal eLinSys_i,I),Test)<<endl;
   linSys
); 
 
homogPartOfImage = (phi,d) -> (
   if verbose then <<"Running 'homogPartOfImage'..."<<endl;  
   -- helps to determine the image of rational maps 
   -- input: 1) ring map, representing a rational map F:P^n-->Z subset P^m 
   --        2) positive integer i
   -- output: list, a basis of H^0(P^m,I_Z(i))
   R:=target phi; 
   kk:=coefficientRing R;
   m:=numgens R-1;
   n:=numgens source phi-1;
   N:=binomial(n+d,n)-1;
   a:=local a; A:=kk[a_0..a_N];
   t:=local t; S:=A[t_0..t_m];
   F:=sub(toMatrix phi,vars(S));
   y:=local y; T:=A[y_0..y_n];
   ST:=A[t_0..t_m,y_0..y_n];
   allMons:=gens(ideal(y_0..y_n))^d;
   GenPol:=(gens(ideal(a_0..a_N))*transpose(allMons))_(0,0);
   subGenPol:=GenPol;
   for i to n list subGenPol=sub(subGenPol,y_i=>sub(F_(0,i),ST)); 
   subGenPol=sub(subGenPol,S);
   Eqs:=sub((coefficients subGenPol)#1,A);
   Eqs=transpose gens trim ideal Eqs; 
      if verbose then <<"Size lin. sys. matrix: "<<numgens target Eqs<<"x"<<N+1<<endl;
   coeffMatrix:=matrix(for i to numgens target Eqs-1 list 
                       for j to N list coefficient(a_j,Eqs_(i,0)));
   solutions:=mingens kernel coeffMatrix;
   dimension:=numgens source solutions;
   Basis:={};
   pol:=local pol;
   for j to dimension-1 list (
   pol_j=sub(GenPol,T);
   for i to N list pol_j=sub(pol_j,a_i=>solutions_(i,j));
   Basis=Basis|{sub(pol_j,vars(source phi))});
   Basis
);

variationOFhomogPartOfImage = (phi,Z) -> ( 
    if verbose then <<"Running 'variationOFhomogPartOfImage'..."<<endl;
    -- input: phi, a ring map, representing a rational map phi=(q_0,...q_n):P^m--->P^n, Z ideal of a hypersurface in P^m 
    -- output: basis (divided into two lists) {F_0,...,F_s},{F_(s+1),...,F_r} of H^0(P^n,I_(phi(Z))(d)), d=(deg Z)/(deg lin sys phi), with F_i(q_0,...,q_n) === 0 iff i <= s   
       if not ring matrix phi === ring Z then error("invalid input data");
       if not numgens Z == 1 then error("expected ideal of a hypersurface");
    R:=target phi; 
    kk:=coefficientRing R;
    m:=numgens R-1;
    n:=numgens source phi-1;
    G:=Z_0;
    d:=(degree ideal G)/(max flatten degrees ideal toMatrix phi); 
       if floor d == ceiling d then d=floor d else error("the degree of the linear system has to divide the degree of the hypersurface");
    N:=binomial(n+d,n)-1;
    a:=local a; A:=kk[a_0..a_(N+1)];
    t:=local t; S:=A[t_0..t_m];
    F:=sub(matrix phi,vars(S));
    y:=local y; T:=A[y_0..y_n];
    ST:=A[t_0..t_m,y_0..y_n];
    allMons:=gens(ideal(y_0..y_n))^d;
    GenPol:=(gens(ideal(a_0..a_N))*transpose(allMons))_(0,0);
    subGenPol:=GenPol;
    for i to n list subGenPol=sub(subGenPol,y_i=>sub(F_(0,i),ST)); 
    subGenPol=sub(subGenPol,S);
    subGenPol=subGenPol-a_(N+1)*sub(G,vars S);
    Eqs:=sub((coefficients subGenPol)#1,A);
    Eqs=transpose gens trim ideal Eqs; 
       if verbose then  <<"Size lin. sys. matrix: "<<numgens target Eqs<<"x"<<N+2<<endl;
    coeffMatrix:=matrix(for i to numgens target Eqs-1 list 
                        for j to N+1 list coefficient(a_j,Eqs_(i,0)));
    solutions:=mingens kernel coeffMatrix;
    dimension:=numgens source solutions;
    Basis:={};
    pol:=local pol;
    for j to dimension-1 list (
    pol_j=sub(GenPol,T);
    for i to N+1 list pol_j=sub(pol_j,a_i=>solutions_(i,j));
    Basis=Basis|{sub(pol_j,vars(source phi))});
    Basis=flatten entries gens trim ideal Basis;
    firstList:={};
    secondList:={};
    for i to #Basis -1 do if phi Basis_i == 0 then firstList=firstList|{Basis_i} else secondList=secondList|{Basis_i};
       if verbose then <<"Output 'variationOFhomogPartOfImage': (#list1,#list2)="<<(# firstList,# secondList)<<endl;
    (firstList,secondList)
);

invertBirationalMapRS = (F,a)  -> ( 
   if verbose then  <<"Running 'invertBirationalMapRS'..."<<endl;
   --
   -- Notation as in the paper [Russo, Simis - On Birational Maps and Jacobian Matrices] 
   -- Computes the inverse map of a birational map via the Russo and Simis's algorithm
   -- input: 1) row matrix, representing a birational map P^n-->P^m 
   --        2) ideal, representing the image of the map 
   -- output: row matrix, representing the inverse map   
   n:=numgens ring F-1;
   x:=local x;
   R:=coefficientRing(ring F)[x_0..x_n];
   I:=sub(F,vars R);
   S:=ring a;
   phi:=syz I;
   local q;
   for j to numgens source phi-1 list 
      if max degrees ideal matrix phi_j == {1} then q=j+1;
   phi1:=submatrix(phi,{0..q-1});
   RtensorS:=tensor(R,S);
   phi1=sub(phi1,RtensorS);
   Y:=sub(vars S,RtensorS);
   theta:=transpose submatrix(jacobian ideal(Y*phi1),{0..n},);
   theta=sub(theta,S);
   S':=S/a; theta':=sub(theta,S');
   Z:=kernel theta';
   basisZ:=mingens Z; 
      if numgens source basisZ == 0 then error("it has not been possible to determine the inverse rational map");
   g:=sub(basisZ_0,S);
   Inv:=transpose matrix(g);
   Inv 
);

invertBirationalMapViaParametrization = (F,a)  -> (
   if verbose then  <<"Running 'invertBirationalMapViaParametrization'..."<<endl;
   --
   -- F matrix over K[x_0,...,x_n]/I,
   -- a is an ideal in K[y_0,...,y_m]
   -- phi: V(I) ---> P^m
   Pn:=ambient ring F;
   I:=ideal ring F;
   ImInv:=invertBirationalMap(I,lift(F,Pn));
   transpose sub(ImInv#0,vars ring a)
);

projDegree = (phi,i,dimSubVar) -> (
   -- Notation as in [Harris J., Algebraic Geometry, A First Course], p. 240.
   -- phi:X \subset P^n ---> Y \subset P^m, 
   -- i integer, 0 <= i <= k, k=dim X=dimSubVar.
   ringX:=target phi;
   ringY:=source phi;
   m:=numgens ambient ringY -1;
   n:=numgens ambient ringX -1;
   k:=if dimSubVar >=0 then dimSubVar else dim ideal ringX -1;
   L:=sub(randomLinearSubspace(ambient ringY,m-k+i),ringY);
   Z:=saturate(phi L,ideal toMatrix phi, MinimalGenerators=>true);
   if dim Z == i+1 then degree Z else 0
);

aboutBaseLocusOfInverseMap = (phi) -> ( -- this is never used --
   if verbose then <<"Running 'aboutBaseLocusOfInverseMap'..."<<endl;
   -- this is a probabilistic method, an easy application of projDegree
   -- input: a birational transformation of type (d_1,d_2)
   -- output: (d_2,codim B,degree B), where B is the base locus of the inverse map
   k:=dim ideal target phi -1;
   z:=degree source phi;
   phi=genericRestriction phi; 
   d:=floor(projDegree(phi,0,k-1) / z); 
   b:=local b;
   for i from 2 to k do (
      phi=genericRestriction phi;
      b=z*d^i - projDegree(phi,0,k-i);
      if b!=0 then return (d,i,b);
   );
);

aboutBaseLocusOfInverseMap0 = (phi,m) -> (
   if verbose then <<"Running 'aboutBaseLocusOfInverseMap0'..."<<endl;
   -- phi rational map, m the list of projective degrees of phi
   -- output as aboutBaseLocusOfInverseMap(phi)
   k:=#m -1; 
   z:=m_k;   
   d:=floor(m_(k-1)/z);
   b:=local b;
   for i from 2 to k do (
      b=z*d^i - m_(k-i); 
      if b!=0 then return (d,i,b);
   );
);

toMatrix = (phi) -> ( -- phi RingMap
   submatrix(matrix phi,{0..(numgens source phi -1)})
);

random1 = (R) -> (
   K:=coefficientRing R;
   if class K =!= FractionField then random(1,R) else sum for s to numgens R -1 list (sum for b to abs random(ZZ) list random(b,ring numerator 1_K)) * (gens R)_s
);

randomLinearSubspace = (R,i) -> (
   -- input: polynomial ring R, integer i
   -- output: ideal of a random i-dimensional linear subspace of Proj(R)
   n:=numgens R -1;
   if i == n then return ideal R;
   if i <=-1 then return sub(ideal 1,R);
   L:=trim ideal for j to n-1-i list random1(R);
   -- return if dim L - 1 == i then L else randomLinearSubspace(R,i);
   L
);

genericRestriction = (phi) -> (
   -- restriction of a rational map X \subset P^n ---> Y \subset P^m to a general hyperplane section of X 
   Pn:=ambient target phi;
   n:=numgens Pn -1;
   K:=coefficientRing Pn;
   x:=local x;
   H:=K[x_0..x_(n-1)];
   j:=map(H,Pn,random(toList(x_0..x_(n-1))|{(trim ideal random1 H)_0}));
   j=map(H/j(ideal target phi),target phi,toMatrix j);
   phi':=j*phi;
   phi'
);

beginDocumentation() 
   document { 
    Key => Cremona, 
    Headline => "package for some computations on rational maps between projective varieties", 
          EM "Cremona", " is a package to perform some basic computations on rational and birational maps between absolutely irreducible projective varieties over a field ",TEX///$K$///,", with particular emphasis when the source variety is a projective space. ",TEX///$K$///," is allowed to be ",TEX///$\mathbb{Q}$///,", a finite field, or also a ", TO FractionField,".", 
          PARA{}, 
          "Let ",TEX///$\Phi:X ---> Y$///,"  be a rational map from a subvariety ",TEX///$X=V(I)\subseteq\mathbb{P}^n=Proj(K[x_0,\ldots,x_n])$///," to a subvariety ",TEX///$Y=V(J)\subseteq\mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". The map ",TEX///$\Phi $///," (in a non-pathological case) can be represented, although not uniquely, by a homogeneous ring map ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," of quotients of polynomial rings by homogeneous ideals. These kinds of ring maps are the typical inputs for the methods in this package. The method ", TO toMap," constructs such a map from a list of ",TEX///$m+1$///," homogeneous elements of the same degree in ",TEX///$K[x_0,...,x_n]/I$///,".", 
         PARA{},
         "Below is an example using the methods provided by this package, dealing with a birational transformation ",TEX///$\Phi:\mathbb{P}^6 ---> \mathbb{G}(2,4)\subset\mathbb{P}^9$///," of bidegree ",TEX///$(3,3)$///,".",
    Subnodes => { 
          "main routines:", 
          TO invertBirMap, 
          TO degreeOfRationalMap, 
          TO projectiveDegrees, 
          TO kernelComponent
             },
    EXAMPLE { 
          "ZZ/33331[t_0..t_6];", 
          "time phi=toMap minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}})", 
          "time J=kernelComponent(phi,2)", 
          "time degreeOfRationalMap phi", 
          "time projectiveDegrees phi", 
          "time projectiveDegrees(phi,OnlySublist=>1)", 
          "time phi=toMap(phi,Dominant=>J)", 
          "time psi=invertBirMap phi", 
          "time isInverseMap(phi,psi)", 
          "time projectiveDegrees psi" 
          }, 
          PARA{},
          "This package contains the main tools applied in the paper ",
          HREF{"http://dx.doi.org/10.1016/j.jsc.2015.11.004","doi:10.1016/j.jsc.2015.11.004"},".", 
          } 
   document { 
    Key => {invertBirMap, (invertBirMap,RingMap)}, 
    Headline => "inverse of a birational map with source a projective space", 
     Usage => "invertBirMap phi", 
     Inputs => { "phi" => RingMap => {"representing a birational map ",TEX///$\Phi$///," from a projective space to a projective variety"} 
          }, 
     Outputs => { 
          {"a ring map representing the inverse of ",TEX///$\Phi$///,""} 
          }, 
          PARA{}, 
         "The method computes the inverse rational map of a birational map ",TEX///$\mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) ---> V(J) \subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///, " represented by a ring map ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]$///,". The algorithm used is that described in the paper by Russo and Simis - On birational maps and Jacobian matrices - Compos. Math. 126 (3), 335-358, 2001. This algorithm requires (besides, of course, the birationality of the map) that a certain condition on ranks of appropriate matrices be satisfied. For efficiency purpose, no check is done on the input, but the output can be checked through the option ", TO MathMode, TT "=>true",".", 
    EXAMPLE { 
          "-- A Cremona transformation of P^20 
ringP20=QQ[t_0..t_20];", 
          "phi=map(ringP20,ringP20,{t_10*t_15-t_9*t_16+t_6*t_20,t_10*t_14-t_8*t_16+t_5*t_20,t_9*t_14-t_8*t_15+t_4*t_20,t_6*t_14-t_5*t_15+t_4*t_16,t_11*t_13-t_16*t_17+t_15*t_18-t_14*t_19+t_12*t_20,t_3*t_13-t_10*t_17+t_9*t_18-t_8*t_19+t_7*t_20,t_10*t_12-t_2*t_13-t_7*t_16-t_6*t_18+t_5*t_19,t_9*t_12-t_1*t_13-t_7*t_15-t_6*t_17+t_4*t_19,t_8*t_12-t_0*t_13-t_7*t_14-t_5*t_17+t_4*t_18,t_10*t_11-t_3*t_16+t_2*t_20,t_9*t_11-t_3*t_15+t_1*t_20,t_8*t_11-t_3*t_14+t_0*t_20,t_7*t_11-t_3*t_12+t_2*t_17-t_1*t_18+t_0*t_19,t_6*t_11-t_2*t_15+t_1*t_16,t_5*t_11-t_2*t_14+t_0*t_16,t_4*t_11-t_1*t_14+t_0*t_15,t_6*t_8-t_5*t_9+t_4*t_10,t_3*t_6-t_2*t_9+t_1*t_10,t_3*t_5-t_2*t_8+t_0*t_10,t_3*t_4-t_1*t_8+t_0*t_9,t_2*t_4-t_1*t_5+t_0*t_6})", 
          "time psi=invertBirMap phi", 
          "time isInverseMap(phi,psi)"
          },
   EXAMPLE { 
          "-- A Cremona transformation of P^26 
ringP26=QQ[t_0..t_26];", 
          "phi=map(ringP26,ringP26,{t_21*t_22-t_20*t_23-t_15*t_24-t_10*t_25-t_0*t_26,t_19*t_22-t_18*t_23-t_16*t_24-t_11*t_25-t_1*t_26,t_19*t_20-t_18*t_21-t_17*t_24-t_12*t_25-t_2*t_26,t_15*t_19-t_16*t_21+t_17*t_23-t_13*t_25-t_3*t_26,t_10*t_19-t_11*t_21+t_12*t_23+t_13*t_24-t_4*t_26,t_0*t_19-t_1*t_21+t_2*t_23+t_3*t_24+t_4*t_25,t_15*t_18-t_16*t_20+t_17*t_22-t_14*t_25-t_5*t_26,t_10*t_18-t_11*t_20+t_12*t_22+t_14*t_24-t_6*t_26,t_0*t_18-t_1*t_20+t_2*t_22+t_5*t_24+t_6*t_25,t_12*t_16-t_11*t_17-t_13*t_18+t_14*t_19-t_7*t_26,t_2*t_16-t_1*t_17-t_3*t_18+t_5*t_19+t_7*t_25,t_12*t_15-t_10*t_17-t_13*t_20+t_14*t_21-t_8*t_26,t_11*t_15-t_10*t_16-t_13*t_22+t_14*t_23-t_9*t_26,t_2*t_15-t_0*t_17-t_3*t_20+t_5*t_21+t_8*t_25,t_1*t_15-t_0*t_16-t_3*t_22+t_5*t_23+t_9*t_25,t_5*t_13-t_3*t_14+t_7*t_15-t_8*t_16+t_9*t_17,t_5*t_12-t_2*t_14-t_6*t_17-t_8*t_18+t_7*t_20,t_3*t_12-t_2*t_13-t_4*t_17-t_8*t_19+t_7*t_21,t_5*t_11-t_1*t_14-t_6*t_16-t_9*t_18+t_7*t_22,t_3*t_11-t_1*t_13-t_4*t_16-t_9*t_19+t_7*t_23,t_2*t_11-t_1*t_12-t_4*t_18+t_6*t_19-t_7*t_24,t_7*t_10-t_8*t_11+t_9*t_12+t_6*t_13-t_4*t_14,t_5*t_10-t_0*t_14-t_6*t_15-t_9*t_20+t_8*t_22,t_3*t_10-t_0*t_13-t_4*t_15-t_9*t_21+t_8*t_23,t_2*t_10-t_0*t_12-t_4*t_20+t_6*t_21-t_8*t_24,t_1*t_10-t_0*t_11-t_4*t_22+t_6*t_23-t_9*t_24,t_4*t_5-t_3*t_6-t_0*t_7+t_1*t_8-t_2*t_9})", 
          "time psi=invertBirMap phi", 
          "time isInverseMap(phi,psi)"
          },
          "The method works even in the case when the source of the rational map ",TEX///$\Phi$///," is not a projective space; however, in this case, the computation is transferred to the method ", TO "invertBirationalMap", " provided by the package ", TO "Parametrization", "."
            }
   document { 
    Key => {degreeOfRationalMap, (degreeOfRationalMap,RingMap)}, 
    Headline => "degree of a rational map with source a projective space", 
     Usage => "degreeOfRationalMap phi", 
     Inputs => { "phi" => RingMap => {"which represents a rational map ",TEX///$\Phi$///," from a projective space to a projective variety"} 
          }, 
     Outputs => { {"the degree of ",TEX///$\Phi$///} 
          }, 
     Consequences => { 
          "The method returns 1 if and only if the map is birational onto its image" 
          }, 
          "Let ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to  K[x_0,\ldots,x_n]$///," be a ring map representing a rational map ",TEX///$\Phi:\mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) --->V(J)\subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". If ",TEX///$p$///," is a general point of ",TEX///$\mathbb{P}^n$///,", denote by ",TEX///$F_p(\Phi)$///," the closure of ",TEX///$\Phi^{-1}(\Phi(p))\subseteq \mathbb{P}^n$///,". The degree of ",TEX///$\Phi$///," is defined as the degree of ",TEX///$F_p(\Phi)$///,"  if ",TEX///$dim F_p(\Phi) = 0$///," and ",TEX///$0$///," otherwise. If ",TEX///$\Phi$///," is defined by forms ",TEX///$F_0(x_0,\ldots,x_n),\ldots,F_m(x_0,\ldots,x_n)$///," and ",TEX///$I_p$///," is the ideal of the point ",TEX///$p$///,", then the ideal of ",TEX///$F_p(\Phi)$///," is nothing but the saturation ",TEX///${(\phi(\phi^{-1}(I_p))):(F_0,....,F_m)}^{\infty}$///,".", 
   EXAMPLE { 
          "-- Take a birational map phi:P^8--->G(1,5) subset P^14 defined by the maximal minors 
-- of a generic 2 x 6 matrix of linear forms on P^8 (thus phi is birational onto its image)
K=ZZ/331; ringP8=K[x_0..x_8]; ringP14=K[t_0..t_14];",
          "phi=map(ringP8,ringP14,gens minors(2,matrix pack(6,for i to 11 list random(1,ringP8))))",
          "time degreeOfRationalMap phi",
          "-- Compose phi:P^8--->P^14 with a linear projection P^14--->P^8 from a general subspace of P^14 
-- of dimension 5 (so that the composition phi':P^8--->P^8 must have degree equal to deg(G(1,5))=14)
phi'=phi*map(ringP14,ringP8,for i to 8 list random(1,ringP14))",
          "time degreeOfRationalMap phi'"
          }, 
          "This method can be generalized for the case when the source of the map ",TEX///$\Phi$///," is just parameterized by a projective space.",
    Caveat => {"With the option ",TO MathMode, TT "=>true"," this method is too slow to be useful."}, 
    SeeAlso => {projectiveDegrees} 
          } 
   document { 
    Key => {projectiveDegrees,(projectiveDegrees,RingMap)}, 
    Headline => "projective degrees of a rational map between projective varieties", 
     Usage => "projectiveDegrees phi", 
     Inputs => { "phi" => RingMap => {"which represents a rational map ",TEX///$\Phi$///," between projective varieties"} 
          }, 
     Outputs => { {"the list of the projective degrees of ",TEX///$\Phi$///} 
          }, 
          "Let ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," be a ring map representing a rational map ",TEX///$\Phi: V(I) \subseteq \mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) ---> V(J) \subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". The ",TEX///$i$///,"-th projective degree of ",TEX///$\Phi$///," is defined in terms of dimension and degree of the closure of ",TEX///$\Phi^{-1}(L)$///,", where ",TEX///$L$///," is a general linear subspace of ",TEX///$\mathbb{P}^m$///," of a certain dimension; for the precise definition, see Harris's book (Algebraic geometry: A first course - Vol. 133 of Grad. Texts in Math., p. 240). If ",TEX///$\Phi$///," is defined by elements ",TEX///$F_0(x_0,\ldots,x_n),\ldots,F_m(x_0,\ldots,x_n)$///," and ",TEX///$I_L$///," denotes the ideal of the subspace ",TEX///$L\subseteq \mathbb{P}^m$///,", then the ideal of the closure of ",TEX///$\Phi^{-1}(L) $///," is nothing but the saturation of the ideal ",TEX///$(\phi(I_L))$///," by ",TEX///$(F_0,....,F_m)$///," in the ring ",TEX///$K[x_0,\ldots,x_n]/I$///,".", 
    EXAMPLE { 
          "-- map from P^4 to G(1,3) given by the quadrics through a rational normal curve of degree 4
GF(331^2)[t_0..t_4]; phi=toMap minors(2,matrix{{t_0..t_3},{t_1..t_4}})", 
          "time projectiveDegrees phi"          
           }, 
    EXAMPLE { 
          "-- map P^8--->P^8 defined by the quadrics through P^2 x P^2 
phi=toMap minors(2,genericMatrix(ZZ/3331[x_0..x_8],3,3))", 
          "time projectiveDegrees phi", 
          "time projectiveDegrees(phi,OnlySublist=>1)" 
          }, 
    Caveat => {"This is a probabilistic method."}, 
    SeeAlso => {degreeOfRationalMap} 
          } 
   document { 
    Key => {MathMode, [invertBirMap,MathMode], [degreeOfRationalMap,MathMode]}, 
    Headline => "whether or not to ensure correctness of output", 
    "This option accepts a ", TO Boolean, " value, default value ",TT "false",".",
     PARA{},
     "When passed to the method ", TO invertBirMap,", it means whether or not a test must be performed to confirm the correctness of the output (and of the input); however, in this version, no error message will be generated, but a message will be displayed. When passed to the method ", TO degreeOfRationalMap,", it means whether or not to use a non-probabilistic algorithm."
          } 
   document { 
    Key => {Dominant, [toMap,Dominant]}, 
--    Headline => "makes a dominant rational map" , 
           "When a sufficiently large integer (allowed ", TO infinity,") is passed to this option, the kernel of the returned ring map will be zero.",
            } 
   document { 
    Key => {OnlySublist, [projectiveDegrees,OnlySublist]}, 
--    Headline => "to get partial outputs",
    "This is an optional argument of ", TO projectiveDegrees, " and accepts a non-negative integer, the number ", TEX///$-1$///," of projective degrees to be computed.",
          } 
   document {
    Key => {Multidegree, [approximateInverseMap,Multidegree]}, 
    "This is an optional argument of ", TO approximateInverseMap, ". It allows to inform the method about the list of projective degrees of the map in input.",
          } 
   document { 
    Key => {kernelComponent,(kernelComponent, RingMap,ZZ)}, 
    Headline => "about the image of a rational map", 
     Usage => "kernelComponent(phi,d)", 
     Inputs => { 
          "phi" => RingMap => {"a homogeneous map ",TEX///$K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]$///,", where ",TEX///$J$///," is a homogeneous ideal"}, 
          "d" => ZZ 
          }, 
     Outputs => { {"the ideal generated by all homogeneous elements of degree ", TT "d"," belonging to the kernel of ",TT "phi"} 
          }, 
     Consequences => { 
          {TT "kernelComponent(phi,d)"," is contained in ", TT "kernel phi"} 
          }, 
          " Assume, for simplicity, that ", TEX///$J=0$///,", and let ",TEX///$\phi:K[y_0,\ldots,y_m] \to K[x_0,\ldots,x_n]$///," be a ring map representing a rational map ",TEX///$\Phi: \mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) ---> \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". Then ", TT "kernelComponent(phi,d)"," returns the ideal generated by a basis of the vector space ",TEX///$H^0(\mathbb{P}^m,\mathcal{I}(d))$///,", where ",TEX///$\mathcal{I}$///," denotes the ideal sheaf of the image of ",TEX///$\Phi$///,".", 
          PARA{}, 
"This is equivalent to ",TT "ideal image basis(d,kernel phi)",", but a more direct algorithm is used. Indeed, taking a generic homogeneous degree ",TEX///$d$///," polynomial ", TEX///$G(y_0,\ldots,y_m)$///," and substituting the ",TEX///$y_i$///,"'s with the ",TEX///$F_i$///,"'s, where ",TEX///$F_i=F_i(x_0,\ldots,x_n)=\phi(y_i)$///,", we obtain a homogeneous polynomial that vanishes identically if and only if ",TEX///$G$///," lies in the kernel of ",TEX///$phi$///,"; thus, the problem is reduced to resolving a homogeneous linear system in the coefficients of ",TEX///$G$///,".", 
    EXAMPLE { 
          "-- A special birational transformation of P^8 into a complete intersection of three quadrics in P^11
K=QQ; ringP8=K[x_0..x_8]; ringP11=K[y_0..y_11];",
          "phi=map(ringP8,ringP11,{-5*x_0*x_3+x_2*x_4+x_3*x_4+35*x_0*x_5-7*x_2*x_5+x_3*x_5-x_4*x_5-49*x_5^2-5*x_0*x_6+2*x_2*x_6-x_4*x_6+27*x_5*x_6-4*x_6^2+x_4*x_7-7*x_5*x_7+2*x_6*x_7-2*x_4*x_8+14*x_5*x_8-4*x_6*x_8,-x_1*x_2-6*x_1*x_5-5*x_0*x_6+2*x_1*x_6+x_4*x_6+x_5*x_6-5*x_0*x_7-x_1*x_7+2*x_2*x_7+7*x_5*x_7-2*x_6*x_7+2*x_1*x_8-3*x_7*x_8,-25*x_0^2+9*x_0*x_2+10*x_0*x_4-2*x_2*x_4-x_4^2+29*x_0*x_5-x_2*x_5-7*x_4*x_5-13*x_0*x_6+3*x_4*x_6+x_5*x_6-x_0*x_7+2*x_2*x_7-x_4*x_7+7*x_5*x_7-2*x_6*x_7-8*x_0*x_8+2*x_4*x_8-3*x_7*x_8,x_2*x_4+x_3*x_4+x_4^2+7*x_2*x_5-9*x_4*x_5+12*x_5*x_6-4*x_6^2+2*x_3*x_7+2*x_4*x_7-14*x_5*x_7+4*x_6*x_7+x_3*x_8-x_4*x_8-14*x_5*x_8+x_6*x_8,-5*x_0*x_4+x_2*x_4-7*x_2*x_5+8*x_4*x_5-5*x_0*x_6+2*x_2*x_6-x_4*x_6+x_5*x_6-x_4*x_7+7*x_5*x_7-2*x_6*x_7-x_4*x_8+7*x_5*x_8-2*x_6*x_8,x_0*x_4+x_4^2-7*x_1*x_5-8*x_4*x_5+x_0*x_6+x_1*x_6+2*x_4*x_6-x_5*x_6+x_4*x_7-7*x_5*x_7+2*x_6*x_7+x_4*x_8-7*x_5*x_8+2*x_6*x_8,x_2*x_3+x_4^2-8*x_4*x_5+x_4*x_6+6*x_5*x_6-2*x_6^2+x_3*x_7+x_4*x_7-7*x_5*x_7+2*x_6*x_7+x_4*x_8-7*x_5*x_8+2*x_6*x_8,x_1*x_3-7*x_1*x_5+x_1*x_6+x_4*x_6-7*x_5*x_6+2*x_6^2-x_3*x_7,-4*x_0*x_3+x_3*x_4-x_4^2-7*x_0*x_5+8*x_4*x_5+x_0*x_6-x_4*x_6-6*x_5*x_6+2*x_6^2-x_3*x_7-x_4*x_7+7*x_5*x_7-2*x_6*x_7-x_4*x_8+7*x_5*x_8-2*x_6*x_8,-5*x_0*x_2+2*x_2^2+x_2*x_4-x_4^2-x_2*x_5+8*x_4*x_5-10*x_0*x_6+2*x_5*x_6+2*x_2*x_7-2*x_4*x_7+14*x_5*x_7-4*x_6*x_7+5*x_0*x_8-3*x_2*x_8-2*x_4*x_8+7*x_5*x_8-2*x_6*x_8-3*x_7*x_8,-5*x_0*x_1+x_1*x_2+x_1*x_4-4*x_0*x_6-x_1*x_6+x_4*x_6+x_0*x_7,x_0*x_2-x_1*x_2+5*x_0*x_4+x_1*x_4-14*x_1*x_5-x_2*x_5-8*x_4*x_5-8*x_0*x_6+2*x_1*x_6+4*x_4*x_6+2*x_2*x_7+4*x_0*x_8+3*x_1*x_8-7*x_5*x_8+2*x_6*x_8-3*x_7*x_8})",
          "time kernelComponent(phi,1)",
          "time kernelComponent(phi,2)"
          },
          PARA{},
          "An obvious change to the idea of the algorithm allows to perform this computation even when the source of the rational map ", TEX///$\Phi$///, " is a hypersurface of degree ", TEX///$d$///, " times the degree of the forms defining the map.",
    EXAMPLE { 
          "-- phi':phi^(-1)(P^10)--->P^11, restriction of phi:P^8--->P^11 
-- to the inverse image of a general hyperplane H in P^11
H=trim ideal random(1,ringP11)",
          "ringHypersurface=ringP8/phi(H); phi'=map(ringHypersurface,ringP8) * phi;",
          "time kernelComponent(phi',1)",
          },
    SeeAlso => {(kernel,RingMap)} 
          } 
   document { 
    Key => {isBirational,(isBirational,RingMap)}, 
    Headline => "whether a rational map is birational", 
     Usage => "isBirational phi", 
     Inputs => { 
          "phi" => RingMap => {"representing a rational map ",TEX///$\Phi:X--->Y$///," (with ",TEX///$X$///," and ",TEX///$Y$///," absolutely irreducible)"}         
               }, 
     Outputs => { 
          Boolean => {"whether ",TEX///$\Phi$///," is birational"  } 
                },
          "The testing passes through the methods ", TO degreeOfRationalMap, " and ", TO projectiveDegrees, ", so that there is a positive (but very small) probability of obtaining a wrong answer.
",
          PARA{},
          "In the example below we check the birationality of a map between two quadric hypersurfaces in ",TEX///$\mathbb{P}^8$///,".",
    EXAMPLE { 
          "K=ZZ/331",
          "X=K[x_0..x_8]/ideal(x_0^2+60*x_0*x_1-155*x_1^2-108*x_0*x_2-87*x_1*x_2-23*x_2^2-153*x_0*x_3-44*x_1*x_3-101*x_2*x_3-23*x_3^2-108*x_0*x_4+108*x_1*x_4-74*x_2*x_4+157*x_3*x_4+4*x_4^2+92*x_0*x_5-23*x_2*x_5+46*x_3*x_5+85*x_4*x_5+99*x_0*x_6-77*x_1*x_6-131*x_2*x_6+85*x_3*x_6+104*x_4*x_6-97*x_6^2-141*x_0*x_7-147*x_1*x_7+127*x_2*x_7-104*x_3*x_7-113*x_5*x_7-151*x_0*x_8+142*x_1*x_8+104*x_2*x_8+3*x_3*x_8-113*x_4*x_8+111*x_5*x_8+81*x_6*x_8+120*x_7*x_8)",
          "Y=K[t_0..t_8]/ideal(t_1*t_2-t_2*t_3+104*t_0*t_5+71*t_0*t_7-142*t_1*t_7+154*t_2*t_7-124*t_3*t_7-70*t_4*t_7-43*t_5*t_7+155*t_6*t_7+11*t_7^2-10*t_7*t_8)",
          "phi=map(X,Y,{10*x_2*x_3+165*x_1*x_4+153*x_2*x_5-102*x_4*x_5-121*x_1*x_6+28*x_2*x_6-152*x_3*x_6-27*x_4*x_6-141*x_6^2+131*x_1*x_7+27*x_3*x_7-38*x_5*x_7-144*x_1*x_8+157*x_2*x_8+83*x_5*x_8-100*x_6*x_8+126*x_7*x_8, -71*x_1*x_3+2*x_0*x_4-98*x_1*x_4-85*x_2*x_4-95*x_3*x_4+59*x_4^2-51*x_0*x_5-86*x_2*x_5-126*x_3*x_5+71*x_4*x_5-68*x_0*x_6-144*x_1*x_6+150*x_2*x_6-155*x_3*x_6-81*x_4*x_6-80*x_6^2-11*x_0*x_7-28*x_1*x_7-80*x_2*x_7+81*x_3*x_7+16*x_5*x_7-32*x_0*x_8-11*x_1*x_8-19*x_4*x_8+155*x_5*x_8-95*x_6*x_8-7*x_7*x_8, -131*x_0*x_3+81*x_0*x_5+142*x_4*x_5-102*x_0*x_6-48*x_3*x_6+159*x_4*x_6+80*x_6^2-159*x_3*x_7-16*x_5*x_7+161*x_0*x_8-148*x_5*x_8-110*x_6*x_8-80*x_7*x_8, 94*x_2^2+2*x_0*x_4-98*x_1*x_4-85*x_2*x_4-95*x_3*x_4+59*x_4^2-51*x_0*x_5+142*x_2*x_5+125*x_3*x_5+71*x_4*x_5-68*x_0*x_6+130*x_1*x_6+90*x_2*x_6+38*x_3*x_6-81*x_4*x_6-145*x_6^2-11*x_0*x_7-17*x_1*x_7-91*x_2*x_7+81*x_3*x_7+29*x_5*x_7-32*x_0*x_8-11*x_1*x_8-19*x_4*x_8+155*x_5*x_8-95*x_6*x_8-7*x_7*x_8, -49*x_1*x_2+107*x_0*x_4-18*x_1*x_4+131*x_2*x_4-134*x_3*x_4+62*x_4^2-66*x_0*x_5-56*x_2*x_5-37*x_3*x_5-29*x_4*x_5-36*x_0*x_6+14*x_1*x_6-109*x_2*x_6-120*x_3*x_6+130*x_4*x_6+8*x_6^2-34*x_0*x_7+125*x_1*x_7+106*x_2*x_7-130*x_3*x_7-134*x_5*x_7+139*x_0*x_8-38*x_1*x_8+142*x_4*x_8-158*x_5*x_8+58*x_6*x_8-32*x_7*x_8, 69*x_0*x_2-58*x_0*x_5+48*x_3*x_5+85*x_0*x_6-96*x_2*x_6+133*x_3*x_6-102*x_6^2+13*x_2*x_7-112*x_5*x_7, -147*x_1^2+87*x_0*x_4-27*x_1*x_4-59*x_2*x_4-121*x_3*x_4+140*x_4^2+123*x_0*x_5-98*x_2*x_5+84*x_3*x_5-106*x_4*x_5+2*x_0*x_6-141*x_1*x_6-156*x_2*x_6-140*x_3*x_6-34*x_4*x_6+117*x_6^2+30*x_0*x_7-133*x_1*x_7-63*x_2*x_7+34*x_3*x_7+109*x_5*x_7-153*x_0*x_8+137*x_1*x_8+148*x_4*x_8-164*x_5*x_8-129*x_6*x_8+109*x_7*x_8, -131*x_0*x_1+87*x_0*x_5-139*x_2*x_5-123*x_0*x_6-48*x_1*x_6-130*x_2*x_6+46*x_6^2-159*x_1*x_7+57*x_5*x_7, -111*x_0*x_1-127*x_1^2-65*x_0*x_2+12*x_1*x_2+26*x_2^2+101*x_0*x_3-51*x_1*x_3+71*x_2*x_3+26*x_3^2+116*x_0*x_4-89*x_1*x_4+55*x_2*x_4+100*x_3*x_4-32*x_4^2+154*x_0*x_5-5*x_2*x_5+138*x_3*x_5+99*x_4*x_5-46*x_0*x_6+84*x_1*x_6+68*x_2*x_6+74*x_3*x_6+129*x_4*x_6+80*x_6^2+135*x_0*x_7+49*x_1*x_7-29*x_2*x_7-129*x_3*x_7-16*x_5*x_7-47*x_0*x_8+104*x_1*x_8-60*x_2*x_8+11*x_3*x_8+18*x_4*x_8-90*x_5*x_8+80*x_6*x_8+47*x_7*x_8})",
          "time isBirational phi"
            }
          } 
   document { 
    Key => {isInverseMap,(isInverseMap,RingMap,RingMap)}, 
    Headline => "checks whether a rational map is the inverse of another", 
     Usage => "isInverseMap(phi,psi)", 
     Inputs => { 
          "phi" => RingMap => {"representing a rational map ",TEX///$\Phi:X--->Y$///,""}, 
          "psi" => RingMap => {"representing a rational map ",TEX///$\Psi:Y--->X$///,""} 
          }, 
     Outputs => { 
          Boolean => {"according to the condition that the composition ",TEX///$\Psi\,\Phi:X--->X$///," coincides with the identity of ",TEX///$X$///," (as a rational map)" 
          	 } 
          } 
          } 
   document { 
    Key => {composeRationalMaps,(composeRationalMaps,RingMap,RingMap)}, 
    Headline => "composition of rational maps", 
     Usage => "composeRationalMaps(phi,psi)", 
     Inputs => { 
          RingMap => "phi" => { TEX///$R <--- S$/// },
          RingMap => "psi" => { TEX///$S <--- T$/// }
          }, 
     Outputs => { 
          RingMap => { TEX///$R <--- T$/// }
          }, 
          "We illustrate this with a simple example.",
    EXAMPLE { 
          "R=QQ[x_0..x_3]; S=QQ[y_0..y_4]; T=QQ[z_0..z_4];", 
          "phi=map(R,S,{x_0*x_2,x_0*x_3,x_1*x_2,x_1*x_3,x_2*x_3})",
          "psi=map(S,T,{y_0*y_3,-y_2*y_3,y_1*y_2,y_2*y_4,-y_3*y_4})",
          "phi*psi",
          "composeRationalMaps(phi,psi)"
          } 
         }
   document { 
    Key => {toMap,(toMap,Matrix),(toMap,Ideal),(toMap,Ideal,ZZ),(toMap,Ideal,ZZ,ZZ),(toMap,List),(toMap,RingMap)}, 
    Headline => "rational map defined by a linear system", 
     Usage => "toMap(\"linear system\")", 
     Inputs => { 
          Matrix => { "or a ", TO List, ", etc."},
          }, 
     Outputs => { RingMap 
          }, 
     Consequences => { 
          }, 
          "When the input represents a list of homogeneous elements ",TEX///$F_0,\ldots,F_m\in R=K[t_0,\ldots,t_n]/I$///," of the same degree, then the method returns the ring map ",TEX///$\phi:K[x_0,\ldots,x_m] \to R$///," that sends ",TEX///$x_i$///," into ",TEX///$F_i$///,".", 
    EXAMPLE { 
          "QQ[t_0,t_1];", 
          "linSys=gens (ideal(t_0,t_1))^5",
          "phi=toMap linSys", 
          }, 
          "If a positive integer ",TEX///$d$///," is passed to the option ", TO Dominant, ", then the method returns the induced map on ",TEX///$K[x_0,\ldots,x_m]/J_d$///,", where ",TEX///$J_d$///," is the ideal generated by all homogeneous elements of degree ",TEX///$d$///," of the kernel of ",TEX///$\phi$///," (in this case ", TO kernelComponent, " is called).",
    EXAMPLE { 
          "phi'=toMap(linSys,Dominant=>2)", 
          }, 
 "If the input is a pair consisting of a homogeneous ideal ",TEX///$I$///, " and an integer ",TEX///$v$///,", then the output will be the map defined by the linear system of hypersurfaces of degree ",TEX///$v$///, " which contain the projective subscheme defined by ",TEX///$I$///,".",
    EXAMPLE { 
          "I=kernel phi",
          "toMap(I,2)" 
          }, 
"This is equivalent to ", TT "toMap(I,v,1)", ", while the output of ", TT "toMap(I,v,2)", " will be the map defined by the linear system of hypersurfaces of degree ",TEX///$v$///, " which are singular along the projective subscheme defined by ",TEX///$I$///,".",
    EXAMPLE { 
          "toMap(I,2,1)", 
          "toMap(I,2,2)", 
          "toMap(I,3,2)" 
          }, 
         } 
   document { 
    Key => {approximateInverseMap,(approximateInverseMap,RingMap)}, 
    Headline => "random map related to the inverse of a birational map", 
     Usage => "approximateInverseMap phi", 
     Inputs => { 
          RingMap => "phi" => {"representing a birational map ",TEX///$\Phi:X--->Y$///}
          }, 
     Outputs => { 
          RingMap => {"a ring map representing a random rational map ",TEX///$Y--->X$///,", which in some sense is related to the inverse of ",TEX///$\Phi$///," (e.g., the base locus of this map has the same dimension and degree of the base locus of the inverse of ",TEX///$\Phi$///,")"}
          }, 
          "The algorithm is to try to construct the ideal of the base locus of the inverse by looking for the images via ", TEX///$\Phi$///," of random linear sections of the source variety. Generally, one can speed up the process by passing the list of projective degrees of ",TEX///$\Phi$///," to the option ", TO Multidegree," (this is useful because from the multidegree one obtains easily the dimension (and other numerical invariants) of the base locus of the inverse).",
     PARA{},
    EXAMPLE { 
          "P8=ZZ/97[t_0..t_8];", 
          "phi=invertBirMap toMap(trim(minors(2,genericMatrix(P8,3,3))+random(2,P8)),Dominant=>infinity)",
          "time psi=approximateInverseMap phi",
          "isInverseMap(phi,psi) and isInverseMap(psi,phi)",
          "time m=projectiveDegrees phi",
          "time psi'=approximateInverseMap(phi,Multidegree=>m)",
          "psi===psi'"
          }, 
     EXAMPLE { 
          "P11=ZZ/12347[x_0..x_11];", 
          "phi=map(P11,P11,{-4719*x_0^2+2105*x_0*x_1-6092*x_1^2+5204*x_0*x_2-608*x_1*x_2-4519*x_2^2+503*x_0*x_3-3510*x_1*x_3-5403*x_2*x_3-1337*x_3^2-2597*x_0*x_4+3189*x_1*x_4-641*x_2*x_4+1831*x_3*x_4-2697*x_4^2-1427*x_0*x_5-2809*x_1*x_5-3553*x_2*x_5-750*x_3*x_5+3929*x_4*x_5+4899*x_5^2+1093*x_0*x_6+3370*x_1*x_6-2424*x_2*x_6-2035*x_3*x_6+5349*x_4*x_6-2945*x_5*x_6-4062*x_6^2+3494*x_0*x_7-1729*x_1*x_7+5713*x_2*x_7-2147*x_3*x_7+3890*x_4*x_7-4871*x_5*x_7+2259*x_6*x_7-195*x_7^2-3965*x_0*x_8-455*x_1*x_8+4372*x_2*x_8-4920*x_3*x_8+3196*x_4*x_8+3382*x_5*x_8-1648*x_6*x_8+2242*x_7*x_8-1091*x_8^2+4541*x_0*x_9-5407*x_1*x_9+2430*x_2*x_9+1910*x_3*x_9-2021*x_4*x_9+5330*x_5*x_9-893*x_6*x_9+1788*x_7*x_9-3663*x_8*x_9+5099*x_9^2+121*x_0*x_10+3574*x_1*x_10+817*x_2*x_10+4623*x_3*x_10-3623*x_4*x_10+5091*x_5*x_10-2318*x_6*x_10+716*x_7*x_10+886*x_8*x_10-4323*x_9*x_10+3302*x_10^2-3559*x_0*x_11-3407*x_1*x_11-3854*x_2*x_11-1691*x_3*x_11+373*x_4*x_11+5572*x_5*x_11+5258*x_6*x_11+2448*x_7*x_11+2841*x_8*x_11-2030*x_9*x_11-94*x_10*x_11-5324*x_11^2,-3871*x_0^2+3402*x_0*x_1-3663*x_1^2+4268*x_0*x_2+3917*x_1*x_2-4082*x_2^2+2258*x_0*x_3-4325*x_1*x_3+2118*x_2*x_3+3700*x_3^2-2620*x_0*x_4-2806*x_1*x_4+2876*x_2*x_4-5123*x_3*x_4-1092*x_4^2+1672*x_0*x_5+1700*x_1*x_5-5567*x_2*x_5-3035*x_3*x_5+5273*x_4*x_5+5879*x_5^2-1910*x_0*x_6-3984*x_1*x_6-3249*x_2*x_6-4570*x_3*x_6+154*x_4*x_6+657*x_5*x_6-5319*x_6^2+3720*x_0*x_7+4983*x_1*x_7+1212*x_2*x_7-5748*x_3*x_7+760*x_4*x_7-854*x_5*x_7-5874*x_6*x_7+2710*x_7^2-4604*x_0*x_8-4528*x_1*x_8+4459*x_2*x_8+5444*x_3*x_8-1647*x_4*x_8-3084*x_5*x_8+781*x_6*x_8-402*x_7*x_8-5672*x_8^2-1421*x_0*x_9+1067*x_1*x_9-3204*x_2*x_9-4684*x_3*x_9-1279*x_4*x_9+3672*x_5*x_9-1897*x_6*x_9-666*x_7*x_9+2364*x_8*x_9+6117*x_9^2-2901*x_0*x_10+1805*x_1*x_10+4141*x_2*x_10-1896*x_3*x_10+4544*x_4*x_10+1263*x_5*x_10-2731*x_6*x_10+4829*x_7*x_10-2044*x_8*x_10-632*x_9*x_10-1052*x_10^2-5720*x_0*x_11+3986*x_1*x_11-5806*x_2*x_11+3622*x_3*x_11-357*x_4*x_11+4298*x_5*x_11+5254*x_6*x_11+5844*x_7*x_11-3525*x_8*x_11+5658*x_9*x_11+3484*x_10*x_11+2453*x_11^2,-2399*x_0^2+1059*x_0*x_1+283*x_1^2+3684*x_0*x_2+3953*x_1*x_2+4212*x_2^2+2506*x_0*x_3-2474*x_1*x_3+5559*x_2*x_3+728*x_3^2+1325*x_0*x_4+3898*x_1*x_4+5764*x_2*x_4-5631*x_3*x_4-2443*x_4^2-2184*x_0*x_5-5609*x_1*x_5+2935*x_2*x_5+5402*x_3*x_5-3847*x_4*x_5+3945*x_5^2+3570*x_0*x_6+2120*x_1*x_6+4982*x_2*x_6-3837*x_3*x_6+1677*x_4*x_6-5304*x_5*x_6-5439*x_6^2-4924*x_0*x_7-5475*x_1*x_7-3849*x_2*x_7+490*x_3*x_7+5954*x_4*x_7-6157*x_5*x_7+991*x_6*x_7+4365*x_7^2-4907*x_0*x_8+5953*x_1*x_8-2309*x_2*x_8+2268*x_3*x_8-3417*x_4*x_8-1380*x_5*x_8+2969*x_6*x_8-4120*x_7*x_8-5831*x_8^2+1158*x_0*x_9+1899*x_1*x_9+741*x_2*x_9-1629*x_3*x_9-5085*x_4*x_9+5805*x_5*x_9+4990*x_6*x_9-3625*x_7*x_9-5229*x_8*x_9-5913*x_9^2+1322*x_0*x_10+1698*x_1*x_10+3242*x_2*x_10-2786*x_3*x_10-5671*x_4*x_10+2423*x_5*x_10+5336*x_6*x_10+563*x_7*x_10+1896*x_8*x_10-5432*x_9*x_10+4953*x_10^2-4921*x_0*x_11+4350*x_1*x_11-4701*x_2*x_11+225*x_3*x_11-3079*x_4*x_11-5183*x_5*x_11-475*x_6*x_11-349*x_7*x_11+1858*x_8*x_11-1224*x_9*x_11+1135*x_10*x_11+3341*x_11^2,2561*x_0^2+4738*x_0*x_1-2568*x_1^2-2279*x_0*x_2+5882*x_1*x_2-6008*x_2^2+1227*x_0*x_3+2942*x_1*x_3-4899*x_2*x_3-2383*x_3^2+4551*x_0*x_4+584*x_1*x_4-1592*x_2*x_4+1625*x_3*x_4-3963*x_4^2-2081*x_0*x_5-5354*x_1*x_5+2261*x_2*x_5-4855*x_3*x_5+4577*x_4*x_5+6003*x_5^2-4729*x_0*x_6-3542*x_1*x_6-3398*x_2*x_6+4782*x_3*x_6-3162*x_4*x_6-3285*x_5*x_6-1441*x_6^2-5565*x_0*x_7-4432*x_1*x_7+4619*x_2*x_7+1664*x_3*x_7-5976*x_4*x_7-2172*x_5*x_7-5784*x_6*x_7-5727*x_7^2-3709*x_0*x_8+2778*x_1*x_8+5053*x_2*x_8-3324*x_3*x_8+5133*x_4*x_8-2305*x_5*x_8+607*x_6*x_8-1419*x_7*x_8-3327*x_8^2-4622*x_0*x_9-2122*x_1*x_9+5224*x_2*x_9-4912*x_3*x_9-3420*x_4*x_9+16*x_5*x_9+953*x_6*x_9-1040*x_7*x_9-4515*x_8*x_9+1028*x_9^2-3761*x_0*x_10+1711*x_1*x_10-1042*x_2*x_10-2025*x_3*x_10-3103*x_4*x_10+1809*x_5*x_10+625*x_6*x_10+5293*x_7*x_10+5175*x_8*x_10-3391*x_9*x_10+1622*x_10^2-4259*x_0*x_11+3450*x_1*x_11+329*x_2*x_11+3890*x_3*x_11-5200*x_4*x_11-1957*x_5*x_11-622*x_6*x_11-448*x_7*x_11-381*x_8*x_11+2488*x_9*x_11-39*x_10*x_11+1874*x_11^2,-520*x_0^2+5241*x_0*x_1-1282*x_1^2-2173*x_0*x_2+4309*x_1*x_2+3370*x_2^2-5171*x_0*x_3-2253*x_1*x_3-5937*x_2*x_3-1738*x_3^2-78*x_0*x_4+5109*x_1*x_4-2714*x_2*x_4-5863*x_3*x_4-3724*x_4^2+3809*x_0*x_5+3104*x_1*x_5-2993*x_2*x_5-1916*x_3*x_5+4073*x_4*x_5-4291*x_5^2-2149*x_0*x_6+471*x_1*x_6+4166*x_2*x_6-352*x_3*x_6-2086*x_4*x_6+4986*x_5*x_6+5462*x_6^2+6116*x_0*x_7-5797*x_1*x_7-5876*x_2*x_7-4057*x_3*x_7+5112*x_4*x_7-2536*x_5*x_7-4708*x_6*x_7+5266*x_7^2-5759*x_0*x_8-2513*x_1*x_8+5651*x_2*x_8+2035*x_3*x_8-1521*x_4*x_8+1209*x_5*x_8-1625*x_6*x_8-2418*x_7*x_8-6063*x_8^2+6128*x_0*x_9-5895*x_1*x_9+2141*x_2*x_9+318*x_3*x_9+1436*x_4*x_9+4134*x_5*x_9+990*x_6*x_9-2090*x_7*x_9-2413*x_8*x_9+4320*x_9^2-2705*x_0*x_10-4493*x_1*x_10+3526*x_2*x_10-4612*x_3*x_10-4878*x_4*x_10-392*x_5*x_10-922*x_6*x_10+994*x_7*x_10+4286*x_8*x_10-1565*x_9*x_10+2567*x_10^2+5696*x_0*x_11-867*x_1*x_11-3197*x_2*x_11+1303*x_3*x_11+3354*x_4*x_11-1193*x_5*x_11-1320*x_6*x_11+4864*x_7*x_11-3025*x_8*x_11-3261*x_9*x_11+6143*x_10*x_11-492*x_11^2,6156*x_0^2+5975*x_0*x_1+309*x_1^2-663*x_0*x_2-3330*x_1*x_2-5275*x_2^2-220*x_0*x_3-2766*x_1*x_3-3518*x_2*x_3+4592*x_3^2+2571*x_0*x_4+5199*x_1*x_4+5128*x_2*x_4-4182*x_3*x_4-5576*x_4^2-2158*x_0*x_5-3305*x_1*x_5-3255*x_2*x_5+2392*x_3*x_5-2955*x_4*x_5-5673*x_5^2+5021*x_0*x_6-5200*x_1*x_6-5709*x_2*x_6-4335*x_3*x_6+2426*x_4*x_6-1629*x_5*x_6-1976*x_6^2-3420*x_0*x_7+2569*x_1*x_7-5109*x_2*x_7+3287*x_3*x_7-6166*x_4*x_7+3002*x_5*x_7-2564*x_6*x_7-3032*x_7^2+3392*x_0*x_8+1856*x_1*x_8-129*x_2*x_8+3637*x_3*x_8+3719*x_4*x_8-5778*x_5*x_8+2933*x_6*x_8+1468*x_7*x_8+4041*x_8^2+2907*x_0*x_9+937*x_1*x_9-2353*x_2*x_9-3264*x_3*x_9+6023*x_4*x_9+3036*x_5*x_9+3225*x_6*x_9-5316*x_7*x_9+1197*x_8*x_9-5845*x_9^2+1272*x_0*x_10+4484*x_1*x_10-1241*x_2*x_10+189*x_3*x_10+2402*x_4*x_10-3525*x_5*x_10-4024*x_6*x_10+3391*x_7*x_10-1051*x_8*x_10+4694*x_9*x_10+978*x_10^2+2638*x_0*x_11-3850*x_1*x_11+5139*x_2*x_11+2247*x_3*x_11+1916*x_4*x_11+760*x_5*x_11+5190*x_6*x_11+4865*x_7*x_11+2645*x_8*x_11+3698*x_9*x_11+83*x_10*x_11-5890*x_11^2,-959*x_0^2+1246*x_0*x_1+1866*x_1^2-844*x_0*x_2-4673*x_1*x_2+5683*x_2^2-5505*x_0*x_3-1295*x_1*x_3+5049*x_2*x_3+912*x_3^2+2362*x_0*x_4+3439*x_1*x_4+2944*x_2*x_4+1884*x_3*x_4+3771*x_4^2+4896*x_0*x_5-4531*x_1*x_5+248*x_2*x_5+3625*x_3*x_5+3423*x_4*x_5-572*x_5^2+3654*x_0*x_6-643*x_1*x_6+4025*x_2*x_6+3616*x_3*x_6+3086*x_4*x_6-3805*x_5*x_6-159*x_6^2-4524*x_0*x_7-542*x_1*x_7-5479*x_2*x_7-1513*x_3*x_7-5116*x_4*x_7-5862*x_5*x_7-1429*x_6*x_7-2442*x_7^2-2988*x_0*x_8+5719*x_1*x_8-4403*x_2*x_8+2118*x_3*x_8+1321*x_4*x_8-547*x_5*x_8+5989*x_6*x_8-2491*x_7*x_8+4493*x_8^2+31*x_0*x_9-621*x_1*x_9+5989*x_2*x_9+3164*x_3*x_9+4243*x_4*x_9+972*x_5*x_9-4166*x_6*x_9-5072*x_7*x_9-3363*x_8*x_9-4584*x_9^2-5595*x_0*x_10-2132*x_1*x_10-2217*x_2*x_10+5923*x_3*x_10-5169*x_4*x_10-4091*x_5*x_10-1057*x_6*x_10-5807*x_7*x_10+287*x_8*x_10+34*x_9*x_10+2774*x_10^2-3005*x_0*x_11+442*x_1*x_11+5537*x_2*x_11-1382*x_3*x_11-2592*x_4*x_11-1304*x_5*x_11-1297*x_6*x_11-2521*x_7*x_11-3785*x_8*x_11+1959*x_9*x_11+3479*x_10*x_11+2742*x_11^2,2181*x_0^2-4540*x_0*x_1+318*x_1^2-63*x_0*x_2+5210*x_1*x_2+425*x_2^2-2285*x_0*x_3-5883*x_1*x_3-4022*x_2*x_3+4580*x_3^2-5204*x_0*x_4-1804*x_1*x_4-3014*x_2*x_4+4917*x_3*x_4-4059*x_4^2-3114*x_0*x_5-1514*x_1*x_5-2819*x_2*x_5+4097*x_3*x_5+5980*x_4*x_5-3415*x_5^2+4352*x_0*x_6+5481*x_1*x_6-648*x_2*x_6-2857*x_3*x_6-2115*x_4*x_6+918*x_5*x_6-2486*x_6^2+3762*x_0*x_7-2570*x_1*x_7-5569*x_2*x_7-1974*x_3*x_7+4749*x_4*x_7-2786*x_5*x_7-1919*x_6*x_7-4219*x_7^2-1371*x_0*x_8+5162*x_1*x_8-948*x_2*x_8-2661*x_3*x_8+4864*x_4*x_8+3793*x_5*x_8+2964*x_6*x_8+2644*x_7*x_8-4847*x_8^2-2870*x_0*x_9-332*x_1*x_9+4038*x_2*x_9+6138*x_3*x_9+4069*x_4*x_9-2878*x_5*x_9+5950*x_6*x_9+4863*x_7*x_9-2141*x_8*x_9+2706*x_9^2-3750*x_0*x_10+4112*x_1*x_10-47*x_2*x_10+5371*x_3*x_10+3442*x_4*x_10-4770*x_5*x_10+5930*x_6*x_10-260*x_7*x_10-3013*x_8*x_10+5846*x_9*x_10-4774*x_10^2+4269*x_0*x_11+1671*x_1*x_11-339*x_2*x_11+2883*x_3*x_11+5006*x_4*x_11-4509*x_5*x_11+4406*x_6*x_11-1184*x_7*x_11+2189*x_8*x_11-2698*x_9*x_11-2087*x_10*x_11+1638*x_11^2,-4633*x_0^2-1300*x_0*x_1-1687*x_1^2-451*x_0*x_2-5049*x_1*x_2-1797*x_2^2+1208*x_0*x_3+2233*x_1*x_3-4246*x_2*x_3-4893*x_3^2+238*x_0*x_4-743*x_1*x_4-3347*x_2*x_4-5579*x_3*x_4-5167*x_4^2+2914*x_0*x_5-5655*x_1*x_5+415*x_2*x_5-5583*x_3*x_5-1166*x_4*x_5-881*x_5^2+4868*x_0*x_6-2607*x_1*x_6+545*x_2*x_6-3747*x_3*x_6+3772*x_4*x_6-5150*x_5*x_6+2193*x_6^2-614*x_0*x_7+3831*x_1*x_7+4723*x_2*x_7-4360*x_3*x_7+2436*x_4*x_7+12*x_5*x_7+3441*x_6*x_7-4218*x_7^2+2478*x_0*x_8+4190*x_1*x_8+2768*x_2*x_8+2859*x_3*x_8-1226*x_4*x_8+1312*x_5*x_8-4763*x_6*x_8+3168*x_7*x_8+4625*x_8^2+4526*x_0*x_9+4862*x_1*x_9-24*x_2*x_9-675*x_3*x_9-3572*x_4*x_9+963*x_5*x_9-6073*x_6*x_9-4131*x_7*x_9-88*x_8*x_9+3512*x_9^2+5860*x_0*x_10-1462*x_1*x_10+2406*x_2*x_10-728*x_3*x_10+1319*x_4*x_10+2121*x_5*x_10-2628*x_6*x_10+5744*x_7*x_10+3866*x_8*x_10-6054*x_9*x_10-4407*x_10^2-4639*x_0*x_11-1002*x_1*x_11+1878*x_2*x_11+5471*x_3*x_11+3518*x_4*x_11-2501*x_5*x_11+3106*x_6*x_11-2283*x_7*x_11+4926*x_8*x_11+469*x_9*x_11+602*x_10*x_11-3791*x_11^2,3023*x_0^2+323*x_0*x_1-4356*x_1^2-1478*x_0*x_2+4965*x_1*x_2+159*x_2^2+674*x_0*x_3+6134*x_1*x_3-4980*x_2*x_3-2025*x_3^2-4700*x_0*x_4+1082*x_1*x_4-2387*x_2*x_4-3050*x_3*x_4-6128*x_4^2-4665*x_0*x_5+895*x_1*x_5+5877*x_2*x_5-2594*x_3*x_5+2155*x_4*x_5-4995*x_5^2+3254*x_0*x_6-2605*x_1*x_6-6088*x_2*x_6+4388*x_3*x_6-5432*x_4*x_6-1271*x_5*x_6-3076*x_6^2-3145*x_0*x_7-3502*x_1*x_7+1871*x_2*x_7+680*x_3*x_7+964*x_4*x_7+5428*x_5*x_7+969*x_6*x_7+3840*x_7^2-392*x_0*x_8+4987*x_1*x_8-919*x_2*x_8-4702*x_3*x_8-4325*x_4*x_8-4590*x_5*x_8+5582*x_6*x_8-5421*x_7*x_8+1004*x_8^2+3147*x_0*x_9+5676*x_1*x_9+4711*x_2*x_9-91*x_3*x_9-3969*x_4*x_9-844*x_5*x_9+1942*x_6*x_9-4347*x_7*x_9+2242*x_8*x_9-3037*x_9^2-5913*x_0*x_10+5062*x_1*x_10-1616*x_2*x_10-4640*x_3*x_10-1296*x_4*x_10-43*x_5*x_10-3846*x_6*x_10-1614*x_7*x_10-1731*x_8*x_10-2574*x_9*x_10+1396*x_10^2+3970*x_0*x_11+3393*x_1*x_11+3009*x_2*x_11+6029*x_3*x_11+4943*x_4*x_11-2159*x_5*x_11-2039*x_6*x_11+3132*x_7*x_11-4070*x_8*x_11-2913*x_9*x_11-4*x_10*x_11+4004*x_11^2,427*x_0^2+964*x_0*x_1-1894*x_1^2+1891*x_0*x_2+2027*x_1*x_2-3196*x_2^2-1235*x_0*x_3-3554*x_1*x_3-3211*x_2*x_3-1318*x_3^2-4491*x_0*x_4-130*x_1*x_4+380*x_2*x_4-4344*x_3*x_4-1649*x_4^2-937*x_0*x_5+204*x_1*x_5+5548*x_2*x_5+5642*x_3*x_5-1337*x_4*x_5-5180*x_5^2-1839*x_0*x_6-2261*x_1*x_6-173*x_2*x_6+5865*x_3*x_6-3207*x_4*x_6+3195*x_5*x_6-362*x_6^2+1171*x_0*x_7+1369*x_1*x_7+3898*x_2*x_7+3586*x_3*x_7+5014*x_4*x_7+5538*x_5*x_7-4969*x_6*x_7+614*x_7^2-226*x_0*x_8-4641*x_1*x_8+4506*x_2*x_8-4177*x_3*x_8-427*x_4*x_8-4297*x_5*x_8+3007*x_6*x_8+1167*x_7*x_8+4375*x_8^2+2432*x_0*x_9-5751*x_1*x_9+237*x_2*x_9+2014*x_3*x_9+2960*x_4*x_9-1360*x_5*x_9-5453*x_6*x_9-5908*x_7*x_9-3830*x_8*x_9-392*x_9^2-193*x_0*x_10-247*x_1*x_10+2198*x_2*x_10-4158*x_3*x_10+268*x_4*x_10+4390*x_5*x_10+2730*x_6*x_10+576*x_7*x_10-458*x_8*x_10+1120*x_9*x_10+5180*x_10^2+3232*x_0*x_11+5887*x_1*x_11+1020*x_2*x_11+2012*x_3*x_11+5606*x_4*x_11-1003*x_5*x_11+3474*x_6*x_11-1025*x_7*x_11-3246*x_8*x_11+3356*x_9*x_11+4286*x_10*x_11+1870*x_11^2,-5682*x_0^2-5592*x_0*x_1+1611*x_1^2-3557*x_0*x_2+5651*x_1*x_2+5075*x_2^2-1686*x_0*x_3-1285*x_1*x_3+1229*x_2*x_3-1080*x_3^2+2951*x_0*x_4-3250*x_1*x_4-129*x_2*x_4+845*x_3*x_4-881*x_4^2+1885*x_0*x_5-349*x_1*x_5-2678*x_2*x_5-5012*x_3*x_5+3716*x_4*x_5+3132*x_5^2-4951*x_0*x_6-831*x_1*x_6+4012*x_2*x_6+3925*x_3*x_6-4275*x_4*x_6-2941*x_5*x_6+461*x_6^2-4256*x_0*x_7-x_1*x_7+1829*x_2*x_7-2580*x_3*x_7+5292*x_4*x_7-3160*x_5*x_7-3742*x_6*x_7-5990*x_7^2-1687*x_0*x_8+1320*x_1*x_8-4094*x_2*x_8+3557*x_3*x_8-2371*x_4*x_8-3385*x_5*x_8-864*x_6*x_8-5402*x_7*x_8-2275*x_8^2+1011*x_0*x_9-4102*x_1*x_9+5734*x_2*x_9-2889*x_3*x_9-1757*x_4*x_9-1314*x_5*x_9-4279*x_6*x_9+3998*x_7*x_9+2507*x_8*x_9+1604*x_9^2-4999*x_0*x_10+2885*x_1*x_10+5387*x_2*x_10-2762*x_3*x_10-4905*x_4*x_10-2743*x_5*x_10-6085*x_6*x_10+873*x_7*x_10+5658*x_8*x_10-4589*x_9*x_10-442*x_10^2+4187*x_0*x_11-877*x_1*x_11+81*x_2*x_11-3960*x_3*x_11-457*x_4*x_11-1155*x_5*x_11+832*x_6*x_11+5971*x_7*x_11+6153*x_8*x_11+1748*x_9*x_11-5172*x_10*x_11-2049*x_11^2})",
          "time psi=approximateInverseMap(phi,Multidegree=>{1,2,4,8,16,23,23,16,8,4,2,1})",
          "-- but...
isInverseMap(psi,phi)",
          "-- now we fix the error of the approximation 
-- (note that the composition of phi with psi is a projective transformation)
time psi'=composeRationalMaps(psi,toMap((vars P11)*(last coefficients matrix composeRationalMaps(phi,psi))^(-1)))",
"isInverseMap(psi',phi)",
          } 
         }


TEST ///  --- quadro-quadric Cremona transformations 
    K=ZZ/3331; ringP5=K[x_0..x_5]; ringP8=K[x_0..x_8]; ringP14=K[x_0..x_14]; ringP20=K[t_0..t_20]; 
    phi1=toMap minors(2,genericSymmetricMatrix(ringP5,3)) 
    phi2=toMap minors(2,genericMatrix(ringP8,3,3)) 
    phi3=toMap pfaffians(4,genericSkewMatrix(ringP14,6)) 
    phi4=map(ringP20,ringP20,{t_10*t_15-t_9*t_16+t_6*t_20,t_10*t_14-t_8*t_16+t_5*t_20,t_9*t_14-t_8*t_15+t_4*t_20,t_6*t_14-t_5*t_15+t_4*t_16,t_11*t_13-t_16*t_17+t_15*t_18-t_14*t_19+t_12*t_20,t_3*t_13-t_10*t_17+t_9*t_18-t_8*t_19+t_7*t_20,t_10*t_12-t_2*t_13-t_7*t_16-t_6*t_18+t_5*t_19,t_9*t_12-t_1*t_13-t_7*t_15-t_6*t_17+t_4*t_19,t_8*t_12-t_0*t_13-t_7*t_14-t_5*t_17+t_4*t_18,t_10*t_11-t_3*t_16+t_2*t_20,t_9*t_11-t_3*t_15+t_1*t_20,t_8*t_11-t_3*t_14+t_0*t_20,t_7*t_11-t_3*t_12+t_2*t_17-t_1*t_18+t_0*t_19,t_6*t_11-t_2*t_15+t_1*t_16,t_5*t_11-t_2*t_14+t_0*t_16,t_4*t_11-t_1*t_14+t_0*t_15,t_6*t_8-t_5*t_9+t_4*t_10,t_3*t_6-t_2*t_9+t_1*t_10,t_3*t_5-t_2*t_8+t_0*t_10,t_3*t_4-t_1*t_8+t_0*t_9,t_2*t_4-t_1*t_5+t_0*t_6})
    time psi1=invertBirMap(phi1,MathMode=>null)
    time psi2=invertBirMap(phi2,MathMode=>true)
    time psi3=invertBirMap(phi3,MathMode=>false)
    time psi4=invertBirMap phi4
    time assert (isInverseMap(phi1,psi1) and isInverseMap(phi2,psi2) and isInverseMap(phi3,psi3) and isInverseMap(phi4,psi4))
    time assert (degreeOfRationalMap phi1 == 1 and degreeOfRationalMap phi2 == 1 and degreeOfRationalMap phi3 == 1 and degreeOfRationalMap phi4 == 1)
///
    
TEST /// -- Hankel matrices
    phi = (r,K) -> (x:=local x; R:=K[x_0..x_(2*r)]; M:=matrix(for i to r list for j to r list x_(i+j)); toMap ideal jacobian ideal det M);
    psi = (r,K) -> (x:=local x; R:=K[x_0..x_(2*r)]; M:=matrix(for i to r-1 list for j to r+1 list x_(i+j)); toMap minors(r,M));
    psi'= (r,K) -> toMap(psi(r,K),Dominant=>2);
    psi0= (r,K) -> (f:=psi'(r,K); map((target f)/(ideal f(sub(random(1,ambient source f),source f))),target f) * f);
    assert(projectiveDegrees phi(2,frac(ZZ/331[i]/(i^2+1)))  == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees psi'(2,ZZ/331) == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees psi0(2,ZZ/331) ==    {2, 4, 4, 2})
    assert(degreeOfRationalMap phi(2,QQ) == 2)
    assert(projectiveDegrees invertBirMap psi'(2,ZZ/101) == reverse {1, 2, 4, 4, 2})
    assert(projectiveDegrees phi(3,ZZ/3331)  == {1, 3, 9, 17, 21, 15, 5})   
    assert(projectiveDegrees psi'(3,ZZ/3331) == {1, 3, 9, 17, 21, 15, 5})   
    assert(projectiveDegrees psi0(3,ZZ/3331) ==    {3, 9, 17, 21, 15, 5})
    assert(degreeOfRationalMap phi(3,ZZ/3331) == 5)
    assert(projectiveDegrees invertBirMap psi'(3,ZZ/3331) == reverse {1, 3, 9, 17, 21, 15, 5})
    assert(degreeOfRationalMap phi(4,ZZ/431) == 14)
///    

TEST ///  -- special map P^8 ---> P^11
    K=ZZ/331; ringP8=K[x_0..x_8]; ringP11=K[y_0..y_11];
    phi=map(ringP8,ringP11,{-5*x_0*x_3+x_2*x_4+x_3*x_4+35*x_0*x_5-7*x_2*x_5+x_3*x_5-x_4*x_5-49*x_5^2-5*x_0*x_6+2*x_2*x_6-x_4*x_6+27*x_5*x_6-4*x_6^2+x_4*x_7-7*x_5*x_7+2*x_6*x_7-2*x_4*x_8+14*x_5*x_8-4*x_6*x_8,-x_1*x_2-6*x_1*x_5-5*x_0*x_6+2*x_1*x_6+x_4*x_6+x_5*x_6-5*x_0*x_7-x_1*x_7+2*x_2*x_7+7*x_5*x_7-2*x_6*x_7+2*x_1*x_8-3*x_7*x_8,-25*x_0^2+9*x_0*x_2+10*x_0*x_4-2*x_2*x_4-x_4^2+29*x_0*x_5-x_2*x_5-7*x_4*x_5-13*x_0*x_6+3*x_4*x_6+x_5*x_6-x_0*x_7+2*x_2*x_7-x_4*x_7+7*x_5*x_7-2*x_6*x_7-8*x_0*x_8+2*x_4*x_8-3*x_7*x_8,x_2*x_4+x_3*x_4+x_4^2+7*x_2*x_5-9*x_4*x_5+12*x_5*x_6-4*x_6^2+2*x_3*x_7+2*x_4*x_7-14*x_5*x_7+4*x_6*x_7+x_3*x_8-x_4*x_8-14*x_5*x_8+x_6*x_8,-5*x_0*x_4+x_2*x_4-7*x_2*x_5+8*x_4*x_5-5*x_0*x_6+2*x_2*x_6-x_4*x_6+x_5*x_6-x_4*x_7+7*x_5*x_7-2*x_6*x_7-x_4*x_8+7*x_5*x_8-2*x_6*x_8,x_0*x_4+x_4^2-7*x_1*x_5-8*x_4*x_5+x_0*x_6+x_1*x_6+2*x_4*x_6-x_5*x_6+x_4*x_7-7*x_5*x_7+2*x_6*x_7+x_4*x_8-7*x_5*x_8+2*x_6*x_8,x_2*x_3+x_4^2-8*x_4*x_5+x_4*x_6+6*x_5*x_6-2*x_6^2+x_3*x_7+x_4*x_7-7*x_5*x_7+2*x_6*x_7+x_4*x_8-7*x_5*x_8+2*x_6*x_8,x_1*x_3-7*x_1*x_5+x_1*x_6+x_4*x_6-7*x_5*x_6+2*x_6^2-x_3*x_7,-4*x_0*x_3+x_3*x_4-x_4^2-7*x_0*x_5+8*x_4*x_5+x_0*x_6-x_4*x_6-6*x_5*x_6+2*x_6^2-x_3*x_7-x_4*x_7+7*x_5*x_7-2*x_6*x_7-x_4*x_8+7*x_5*x_8-2*x_6*x_8,-5*x_0*x_2+2*x_2^2+x_2*x_4-x_4^2-x_2*x_5+8*x_4*x_5-10*x_0*x_6+2*x_5*x_6+2*x_2*x_7-2*x_4*x_7+14*x_5*x_7-4*x_6*x_7+5*x_0*x_8-3*x_2*x_8-2*x_4*x_8+7*x_5*x_8-2*x_6*x_8-3*x_7*x_8,-5*x_0*x_1+x_1*x_2+x_1*x_4-4*x_0*x_6-x_1*x_6+x_4*x_6+x_0*x_7,x_0*x_2-x_1*x_2+5*x_0*x_4+x_1*x_4-14*x_1*x_5-x_2*x_5-8*x_4*x_5-8*x_0*x_6+2*x_1*x_6+4*x_4*x_6+2*x_2*x_7+4*x_0*x_8+3*x_1*x_8-7*x_5*x_8+2*x_6*x_8-3*x_7*x_8});
    Z=ideal(y_2*y_4+y_3*y_4+y_4^2+5*y_2*y_5+y_3*y_5+5*y_4*y_5-y_1*y_6-4*y_2*y_6-5*y_5*y_6-4*y_2*y_7-2*y_4*y_7-y_1*y_8+4*y_4*y_8-5*y_5*y_8-4*y_5*y_9+3*y_7*y_9-4*y_8*y_9-y_3*y_10-3*y_6*y_10-5*y_8*y_10-y_4*y_11+4*y_6*y_11+5*y_8*y_11,3*y_1*y_3-y_2*y_3-3*y_3*y_4-y_4^2+2*y_0*y_5-y_3*y_5+y_1*y_6+2*y_2*y_6+3*y_5*y_6-7*y_2*y_7-4*y_4*y_7+7*y_1*y_8-2*y_4*y_8+y_0*y_9-y_4*y_9+2*y_5*y_9+2*y_7*y_9+y_8*y_9-7*y_0*y_10+5*y_3*y_10-3*y_6*y_10-y_0*y_11-2*y_3*y_11-2*y_4*y_11,7*y_0*y_1+y_0*y_4+7*y_1*y_4-y_3*y_4+8*y_0*y_5-y_3*y_5-y_1*y_6+7*y_2*y_6+8*y_5*y_6+y_2*y_7+8*y_4*y_7-y_1*y_8-8*y_4*y_8+7*y_5*y_9-8*y_7*y_9+7*y_8*y_9+y_0*y_10-y_3*y_10+8*y_6*y_10-7*y_0*y_11-7*y_4*y_11-7*y_6*y_11);
    time assert(kernelComponent(phi,2) == Z)
    time assert(projectiveDegrees phi == {1, 2, 4, 8, 16, 23, 23, 16, 8})
    time assert(degreeOfRationalMap phi == 1)
    H=ideal random(1,ringP11)
    phi'=map(ringP8/phi(H),ringP8) * phi;
    time assert ( kernelComponent(phi',1) == H )
    Q=ideal random(2,ringP11)
    phi'=map(ringP8/phi(Q),ringP8) * phi;
    time assert ( kernelComponent(phi',2) == Q+Z )
/// 
    
TEST ///
    K=ZZ/761; P4=K[x_0..x_4];
    S4=P4/ideal(x_0^3+x_1^3+x_2^3); u=gens S4;
    time phi=toMap(minors(2,matrix{{u_0,u_1,u_2,u_3},{u_1,u_2,u_3,u_4}}),Dominant=>infinity)
    time assert (projectiveDegrees phi == {3, 6, 12, 12})
///

TEST ///
    K=ZZ/101; R=K[t_0..t_7];
    R=R/ideal(t_3^2*t_5^2-4*t_1*t_3*t_4*t_6+2*t_2*t_3*t_5*t_6+t_2^2*t_6^2-4*t_0*t_4*t_6^2-4*t_1*t_3^2*t_7-4*t_0*t_3*t_6*t_7);
    t=gens R;
    S=K[x_0..x_8];
    phi=map(R,S,{t_4*t_6+t_3*t_7,t_5^2-t_1*t_7,t_4*t_5-t_2*t_7,t_3*t_5+t_2*t_6,t_2*t_5-t_0*t_7,t_1*t_4-t_0*t_7,t_1*t_3+t_0*t_6,t_2^2-t_0*t_4,t_1*t_2-t_0*t_5});
    Out=ideal(x_3*x_5-x_2*x_6-x_0*x_8,x_4^2-x_4*x_5-x_1*x_7+x_2*x_8,x_3^2-4*x_0*x_6);
    assert( kernelComponent(phi,2) == Out )
    assert( ideal image basis(2,kernel phi) == Out )
    phi'=toMap(phi,Dominant=>ideal(Out_0,Out_1))
    assert( ideal image basis(2,kernel phi') == kernelComponent(phi',2) )
///

end 

 

