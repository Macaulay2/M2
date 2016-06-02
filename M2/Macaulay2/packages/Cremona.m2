
newPackage(
       "Cremona",
	Version => "1.1", 
    	Date => "Jun 1, 2016",
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
    (d,c,b):=if o.Multidegree =!= null then (
                   if class o.Multidegree =!= List then error("invalid multidegree provided") else aboutBaseLocusOfInverseMap0(phi,o.Multidegree)
             ) else aboutBaseLocusOfInverseMap phi;
    phiRes:=local phiRes;
    B:=trim sum for i from 1 to ceiling((n+1)/(c-1)) list (
         phiRes=phi;
         for i from 1 to c-1 do phiRes=genericRestriction phiRes;
         kernelComponent(phiRes,d)
       );
   if not isPolynomialRing source phi then (
       B=trim(sub(B,ambient source phi) + ideal image basis(d,ideal source phi));
       B=sub(ideal for i to numgens target phi -1 list sum for j to numgens B -1 list (random coefficientRing target phi)*B_j,source phi)   
   );
   B=gens B;
   if numgens source B != numgens target phi then return approximateInverseMap(phi);
   psi:=map(source phi,target phi,B);
   Bs:=ideal toMatrix psi; if not (codim Bs == c and degree Bs == b and first flatten degrees Bs == d) then return approximateInverseMap(phi);
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

aboutBaseLocusOfInverseMap = (phi) -> (
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
   j:=map(H,Pn,{x_0..x_(n-1),(trim ideal random1 H)_0});
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
          "The algorithm proceeds into two stages. Firstly, numerical invariants of the inverse map are computed using ", TO projectiveDegrees," (one can speed up the process by passing the list of projective degrees of ",TEX///$\Phi$///," to the option ", TO Multidegree,"); secondly, we basically construct the ideal of the base locus of the inverse by looking for the images via ", TEX///$\Phi$///," of random linear subspaces (here is used ", TO kernelComponent,"). Although at first glance this method may seem crazy, it could be useful in some cases.",
     PARA{},
    EXAMPLE { 
          "P8=ZZ/97[t_0..t_8];", 
          "phi=invertBirMap toMap(trim(minors(2,genericMatrix(P8,3,3))+random(2,P8)),Dominant=>infinity)",
          "time psi=approximateInverseMap phi",
          "isInverseMap(phi,psi) and isInverseMap(psi,phi)",
          "time m=projectiveDegrees phi",
          "time psi'=approximateInverseMap(phi,Multidegree=>m)",
          "psi===psi'"
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

 

