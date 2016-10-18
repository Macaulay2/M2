
newPackage(
       "Cremona",
	Version => "2.0", 
    	Date => "Jun 8, 2016",
    	Authors => {{Name => "Giovanni Staglianò", 
		     Email => "giovannistagliano@gmail.com" 
                    }
                   },
    	Headline => "Some computations for rational maps between projective varieties",
    	DebuggingMode => false,
	Reload => true
    	)

export{
   "composeRationalMaps",
   "degreeOfRationalMap",   
   "invertBirMap",
   "isBirational",
   "isDominant",
   "isInverseMap",
   "kernelComponent",
   "projectiveDegrees",
   "toMap",
   "MathMode", 
   "Dominant", 
   "OnlySublist",
   "approximateInverseMap",
   "CodimBsInv"
};

-- needsPackage "Parametrization"

verbose:=false;

MathVerb:=true;
certificate:="MathMode: output certified!\n";

composeRationalMaps=method(TypicalValue => RingMap);
degreeOfRationalMap=method(TypicalValue => ZZ, Options => {MathMode => false});
invertBirMap=method(TypicalValue => RingMap, Options => {MathMode => false});
isBirational=method(TypicalValue => Boolean, Options => {MathMode => false});
isDominant=method(TypicalValue => Boolean, Options => {MathMode => false});
isInverseMap=method(TypicalValue => Boolean);
kernelComponent=method(TypicalValue => Ideal);
projectiveDegrees=method(TypicalValue => List, Options => {MathMode => false, OnlySublist => infinity});
toMap=method(TypicalValue => RingMap, Options => {Dominant => null});
approximateInverseMap=method(TypicalValue => RingMap, Options => {MathMode => false, CodimBsInv => null});

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
   if MathVerb then <<certificate;
   degMap 
);

invertBirMap (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   a:=ideal source phi;
   F:=toMatrix phi;
   G:=matrix{{(numgens target phi):0_(source phi)}}; G':=G;
   if o.MathMode === null then return map(source phi,target phi,invertBirationalMapViaParametrization phi);
   if isPolynomialRing target phi then try G=invertBirationalMapRS(F,a);
   if not ((isPolynomialRing target phi) and (max flatten degrees ideal G > 0)) then (
       verb:=MathVerb; MathVerb=false; isDom:=isDominant(phi,MathMode=>true); MathVerb=verb;
       if not isDom then error("trying to invert non-dominant map");
       try G=invertBirationalMapViaParametrization(phi);
   );
   if G === G' then error("do not able to obtain an inverse rational map");
   psi:=map(source phi,target phi,G);
   if o.MathMode then (
        if (isInverseMap(phi,psi) and isInverseMap(psi,phi)) then (if MathVerb then <<certificate) else error("do not able to obtain an inverse rational map");
   ); 
   psi
);

isBirational (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   X:=target phi; Y:=source phi;
   if dim X != dim Y then (if MathVerb and o.MathMode then <<certificate; return false);
   if o.MathMode then (
         verb:=MathVerb; MathVerb=false; isDom:=isDominant(phi,MathMode=>true); MathVerb=verb;
         if not isDom then (if MathVerb then <<certificate; return false);      
   );
-- if isPolynomialRing X then return degreeOfRationalMap(phi,MathMode=>o.MathMode) == 1;
   first projectiveDegrees(phi,OnlySublist=>0,MathMode=>o.MathMode) == degree Y 
);

isDominant (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
      if o.MathMode then return isDominantMath phi;
   X:=target phi; Y:=source phi;
   n:=dim X -1; m:=dim Y -1; 
   if n < m then return false;
   for i from 1 to n-m do phi=genericRestriction phi;
   first projectiveDegrees(phi,OnlySublist=>0) != 0
);

isDominantMath = (phi) -> (
   -- phi:X--->Y
   X:=target phi; Y:=source phi;
   n:=dim X -1; m:=dim Y -1; 
   PN:=ambient X; PM:=ambient Y;
   N:=numgens PN -1; M:=numgens PM -1;
   if n < m then (if MathVerb then <<certificate; return false);
   -- if there exists Z subset Y (with dim Z = 0) s.t dim phi^(-1)(Z) = n-m, then phi is dominant
   Z:=ideal(Y) + randomLinearSubspace(PM,M-m);
   while (-1 + dim(Z) != 0) do Z=ideal(Y) + randomLinearSubspace(PM,M-m);
   Z=sub(Z,Y);
   if dim saturate(phi Z,ideal toMatrix phi) -1 == n-m then (if MathVerb then <<certificate; return true);
   if kernel(phi,SubringLimit=>1) != 0 then (if MathVerb then <<certificate; return false);
   return isDominantMath(phi);
);

isInverseMap(RingMap,RingMap) := (phi,psi) -> (
   checkRationalMap phi;
   checkRationalMap psi;
   if (source phi =!= target psi or target phi =!= source psi) then return false; 
   try phipsi:=toMatrix(phi*psi) else return false;
   x:=gens target phi; 
   i:=0; while x_i == 0 do i=i+1;
   (q,r):=quotientRemainder((flatten entries phipsi)_i,x_i);
   if r != 0 then return false; 
   if q == 0 then return false;
   phipsi - q*(vars target phi) == 0
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
   if o.MathMode then return projectiveDegreesMath(phi,min(k,o.OnlySublist));
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
      return map(ring F,PN/(trim kernel phi),F);
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
      return map(target phi,(source phi)/(trim kernel phi),toMatrix phi);
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
 
approximateInverseMap (RingMap) := o -> (phi) -> (
    if verbose then  <<"Running 'approximateInverseMap'..."<<endl;
    -- input: a birational map phi:X --->Y 
    -- output: a map Y--->X in some sense related to the inverse of phi
    checkRationalMap phi;
    n:=numgens ambient target phi -1;
    c:=2;
    if o.CodimBsInv =!= null then (if (try (class o.CodimBsInv === ZZ and o.CodimBsInv >= 2 and o.CodimBsInv <= n+1) else false) then c=o.CodimBsInv else (<<"--warning: option CodimBsInv ignored"<<endl));
    phiRes:=local phiRes;
    B:=trim sum for i from 1 to ceiling((n+1)/(c-1)) list (
         phiRes=phi;
         for i0 from 1 to c-1 do phiRes=genericRestriction phiRes;
         if verbose then <<"Computing kernel(..,SubringLimit=>"<<(c-1)<<")... ("<<i<<" of "<<ceiling((n+1)/(c-1))<<")"<<endl;
         kernel(phiRes,SubringLimit=>(c-1))  
       );
   if not(numgens B <= n+1 and min flatten degrees B == max flatten degrees B) then return approximateInverseMap(phi,CodimBsInv=>o.CodimBsInv,MathMode=>o.MathMode);
   if numgens B < n+1 then B=B+ideal((n+1-numgens(B)) : 0_(ring B));
   psi:=if isPolynomialRing target phi then map(source phi,target phi,gens B) else toMap(map(source phi,ambient target phi,gens B),Dominant=>ideal(target phi));
   if o.MathMode then (
          if isPolynomialRing target phi then (
                 try psi=composeRationalMaps(psi,toMap((vars target phi)*(last coefficients matrix composeRationalMaps(phi,psi))^(-1)));
                 if isInverseMap(phi,psi) and isInverseMap(psi,phi) then (if MathVerb then <<certificate; return psi) else error("MathMode: approximateInverseMap returned "|toExternalString(psi)|" but this is not the inverse map");
          ) else (
                 if source psi =!= target phi then error("MathMode: approximateInverseMap returned "|toExternalString(psi)|" but this map has an incorrect target variety");
                 if source psi === target phi then if isInverseMap(phi,psi) and isInverseMap(psi,phi) then (if MathVerb then <<certificate; return psi) else error("MathMode: approximateInverseMap returned "|toExternalString(psi)|" but this is not the inverse map");
          );
   );
   return psi;
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

-- invertBirationalMapViaParametrization = (F,a)  -> (
--    -- F matrix over K[x_0,...,x_n]/I,
--    -- a is an ideal in K[y_0,...,y_m]
--    -- phi: V(I) ---> P^m
--    Pn:=ambient ring F;
--    I:=ideal ring F;
--    ImInv:=invertBirationalMap(I,lift(F,Pn));
--    transpose sub(ImInv#0,vars ring a)
-- );

invertBirationalMapViaParametrization = (phi)  -> (
   if verbose then  <<"Running 'invertBirationalMapViaParametrization'..."<<endl;
   Bl := ideal target first graph phi;
   n := numgens ambient target phi -1;
   Sub := map(source phi,ring Bl,matrix{{(n+1):0_(source phi)}}|(vars source phi));
   T:=transpose gens kernel transpose Sub submatrix(jacobian Bl,{0..n},);
   psi:=for i to numgens target T -1 list map(source phi,target phi,submatrix(T,{i},));
   if verbose then (
            <<"...obtained "<<(#psi)<<" maps"<<endl;
            for i to #psi-1 do <<i<<": "<<flatten degrees ideal toMatrix psi_i<<" isInverse? "<<isInverseMap(phi,psi_i)<<endl;
   );
   toMatrix first psi
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

toMatrix = (phi) -> ( -- phi RingMap
   submatrix(matrix phi,{0..(numgens source phi -1)})
);

random1 = (R) -> (
   K:=coefficientRing R;
   if class K =!= FractionField then random(1,R) else sum for s to numgens R -1 list (sum for b to abs random(ZZ) list random(b,ring numerator 1_K)) * (gens R)_s
);

random1A = (R) -> (
   K:=coefficientRing R;
   if class K === FractionField then return random1 R;
   sum for i to numgens R -1 list random(0,1) * (gens R)_i
);

randomLinearSubspace = (R,i) -> (
   if verbose then <<"taking linear subspace of P^"<<numgens(R)-1<<" of dimension "<<i<<endl;
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

graph=method();
graph (RingMap) := (phi) -> (
   -- phi: X subset P^n ---> Y subset P^m
   if verbose then <<"computing blowing-up..."<<endl;
   Pn:=ambient target phi;
   K:=coefficientRing Pn;
   n:=numgens Pn -1;
   X:=ideal target phi;
   B:=ideal lift(toMatrix phi,Pn);
   Pm:=ambient source phi;
   m:=numgens Pm - 1;
   Y:=ideal source phi;
   x:=local x; y:=local y;
   R:=K[x_0..x_n,y_0..y_m];
   p1:=map(R,Pn,{x_0..x_n});
   p2:=map(R,Pm,{y_0..y_m});
   bl:=saturate(p1(X) + minors(2,(p1 gens B)||matrix{{y_0..y_m}}),p1(B));
   Z:=R/bl;
   p1=map(Z,target phi,{x_0..x_n});
   -- p2=map(Z,Pm,{y_0..y_m});
   -- if not(p2 Y == 0 and dim Z == dim X +1) then error("error in graph");
   p2=map(Z,source phi,{y_0..y_m});
   (p1,p2)
);

projectiveDegreesMath = (phi,oSublist) -> ( 
   Bl := ideal target first graph phi;
   n := numgens ambient target phi -1;
   r := dim target phi -1;
   m := numgens ring Bl - n - 2;
   p1 := map(ring Bl,target phi,(gens ring Bl)_{0..n});
   p2 := map(ring Bl,ambient source phi,(gens ring Bl)_{(n+1)..(n+1+m)});
   J := local J;  d := local d;
   J_0 = Bl; d_0 = dim J_0;
    for i from 1 to r do (
       while true do (
          if verbose then <<"computing list of subschemes of blowing-up: "<<i<<" of "<<r<<endl;
          J_i = saturate(p2(ideal random1A source p2) + J_(i-1),ideal toMatrix p2);
          d_i = dim J_i;
          if ( d_i < d_(i-1) or d_(i-1) < 1 ) then break;
       );
    );
   if verbose then <<"dimensions obtained: "<<toString(for i to r list (d_i - 2))<<endl;
   degs:=for i from r-oSublist to r list degree preimage(p1,J_i);
   if MathVerb then <<certificate;
   degs
);

-- projectiveDegreesAluffi = (phi) -> (
--    needs "CSM.m2"; 
--    if not isPolynomialRing target phi then error("the target of the ring map needs to be a polynomial ring");
--    last presegre(coefficientRing target phi,target phi,numgens target phi -1,ideal matrix phi)
-- );

checkRationalMap = (phi) -> ( -- phi RingMap
   if not isField coefficientRing target phi then error("the coefficient ring needs to be a field");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and (isPolynomialRing target phi or isQuotientRing target phi) and isHomogeneous ideal source phi and isHomogeneous ideal target phi) then error("source and target of the ring map need to be quotients of polynomial rings by homogeneous ideals");
   if not (isHomogeneous ideal toMatrix phi) then error("the map needs to be defined by homogeneous polynomials of the same degree");
   D:=degrees ideal compress toMatrix phi; if #D != 0 then if not (min D == max D) then error("the map needs to be defined by homogeneous polynomials of the same degree");
);

checkRationalMapFromPn = (phi) -> ( -- phi RingMap
   if not isField coefficientRing target phi then error("the coefficient ring needs to be a field");
   if not isPolynomialRing target phi then error("the target of the ring map needs to be a polynomial ring");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and isHomogeneous ideal source phi) then error("the source of the ring map needs to be a quotient of a polynomial ring by a homogeneous ideal");
   if not (isHomogeneous ideal toMatrix phi) then error("the map needs to be defined by homogeneous polynomials of the same degree");
   D:=degrees ideal compress toMatrix phi; if #D != 0 then if not (min D == max D) then error("the map needs to be defined by homogeneous polynomials of the same degree");
);

checkLinearSystem = (F) -> ( -- F row matrix
   if not isField coefficientRing ring F then error("the coefficient ring needs to be a field");
   if not ((isPolynomialRing ring F or isQuotientRing ring F) and isHomogeneous ideal ring F) then error("the base ring must be a quotient of a polynomial ring by a homogeneous ideal");
   if not (numgens target F == 1) then error("expected a row matrix");
   if numgens source F == 0 then return;
   if not (isHomogeneous ideal F) then error("expected homogeneous elements of the same degree");
   D:=degrees ideal compress F; if #D != 0 then if not (min D == max D) then error("expected homogeneous elements of the same degree");
);

beginDocumentation() 
   document { 
    Key => Cremona, 
    Headline => "package for some computations on rational maps between projective varieties", 
          EM "Cremona", " is a package to perform some basic computations on rational and birational maps between absolutely irreducible projective varieties over a field ",TEX///$K$///,", with particular emphasis when the source variety is a projective space. ",
          PARA{}, 
          "Let ",TEX///$\Phi:X ---> Y$///,"  be a rational map from a subvariety ",TEX///$X=V(I)\subseteq\mathbb{P}^n=Proj(K[x_0,\ldots,x_n])$///," to a subvariety ",TEX///$Y=V(J)\subseteq\mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". The map ",TEX///$\Phi $///," (in a non-pathological case) can be represented, although not uniquely, by a homogeneous ring map ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," of quotients of polynomial rings by homogeneous ideals. These kinds of ring maps are the typical inputs for the methods in this package. The method ", TO toMap," constructs such a map from a list of ",TEX///$m+1$///," homogeneous elements of the same degree in ",TEX///$K[x_0,...,x_n]/I$///,".", 
         PARA{},
         "Below is an example using the methods provided by this package, dealing with a birational transformation ",TEX///$\Phi:\mathbb{P}^6 ---> \mathbb{G}(2,4)\subset\mathbb{P}^9$///," of bidegree ",TEX///$(3,3)$///,".",
    EXAMPLE { 
          "ZZ/33331[t_0..t_6];", 
          "time phi=toMap minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}})", 
          "time J=kernelComponent(phi,2)", 
          "time degreeOfRationalMap phi", 
          "time projectiveDegrees phi", 
          "time projectiveDegrees(phi,OnlySublist=>0)", 
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
    Headline => "inverse of a birational map", 
     Usage => "invertBirMap phi", 
     Inputs => { "phi" => RingMap => {"representing a birational map ",TEX///$\Phi$///," between projective varieties"} 
          }, 
     Outputs => { 
          {"a ring map representing the inverse of ",TEX///$\Phi$///,""} 
          }, 
          PARA{}, 
         "The method computes the inverse rational map of a birational map ",TEX///$V(I)\subseteq\mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) ---> V(J) \subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///, " represented by a ring map ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///,". If the source variety is a projective space and if a further technical condition is satisfied, then the algorithm used is that described in the paper by Russo and Simis - On birational maps and Jacobian matrices - Compos. Math. 126 (3), 335-358, 2001. For the general case, the algorithm used is the same as for ", TO "invertBirationalMap", " in the package ", TO "Parametrization", ". Note that if the passed map is not birational and the option ", TO MathMode, " is set to ", TT "false", ", you might not get any error message.",
    PARA{},
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
          }
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
    Caveat => {"This method can be generalized for the case when the source of the rational map is just parameterized by a projective space."}, 
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
          "Let ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," be a ring map representing a rational map ",TEX///$\Phi: V(I) \subseteq \mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) ---> V(J) \subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". The ",TEX///$i$///,"-th projective degree of ",TEX///$\Phi$///," is defined in terms of dimension and degree of the closure of ",TEX///$\Phi^{-1}(L)$///,", where ",TEX///$L$///," is a general linear subspace of ",TEX///$\mathbb{P}^m$///," of a certain dimension; for the precise definition, see Harris's book (Algebraic geometry: A first course - Vol. 133 of Grad. Texts in Math., p. 240). If ",TEX///$\Phi$///," is defined by elements ",TEX///$F_0(x_0,\ldots,x_n),\ldots,F_m(x_0,\ldots,x_n)$///," and ",TEX///$I_L$///," denotes the ideal of the subspace ",TEX///$L\subseteq \mathbb{P}^m$///,", then the ideal of the closure of ",TEX///$\Phi^{-1}(L) $///," is nothing but the saturation of the ideal ",TEX///$(\phi(I_L))$///," by ",TEX///$(F_0,....,F_m)$///," in the ring ",TEX///$K[x_0,\ldots,x_n]/I$///,". So, replacing in the definition, ", EM "general linear subspace", " by ", EM "random linear subspace", ", we get a probabilistic algorithm to compute all projective degrees. This is what the method uses if ", TO MathMode, " is set to ", TT "false",". If instead ", TO MathMode, " is set to ", TT "true", ", then the method uses a deterministic algorithm which is described by P. Aluffi in the paper ", HREF{"http://dx.doi.org/10.1016/S0747-7171(02)00089-5","doi:10.1016/S0747-7171(02)00089-5"},".",
     PARA{},
    EXAMPLE { 
          "-- map from P^4 to G(1,3) given by the quadrics through a rational normal curve of degree 4
GF(331^2)[t_0..t_4]; phi=toMap minors(2,matrix{{t_0..t_3},{t_1..t_4}})", 
          "time projectiveDegrees phi",
          "time projectiveDegrees(phi,MathMode=>true)",
          "psi=invertBirMap(toMap(phi,Dominant=>infinity))", 
          "time projectiveDegrees psi",
          "time projectiveDegrees(psi,MathMode=>true)",     
           }, 
    EXAMPLE { 
          "-- map P^8--->P^8 defined by the quadrics through P^2 x P^2 
phi=toMap minors(2,genericMatrix(ZZ/3331[x_0..x_8],3,3))", 
          "time projectiveDegrees phi", 
          "time projectiveDegrees(phi,OnlySublist=>1)" 
          }, 
Caveat => {"In the package ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", P. Aluffi implemented the same deterministic algorithm to compute projective degrees in the case when the source of the rational map is a projective space (this is done in an internal routine called ",TT "presegre","). It seems that his implementation is faster."},
    SeeAlso => {degreeOfRationalMap} 
          } 
   document { 
    Key => {MathMode, [invertBirMap,MathMode], [projectiveDegrees,MathMode],[degreeOfRationalMap,MathMode],[approximateInverseMap,MathMode],[isDominant,MathMode],[isBirational,MathMode]}, 
    Headline => "whether or not to ensure correctness of output", 
    "This option accepts a ", TO Boolean, " value, default value ",TT "false",".",
     PARA{},
     "If turned on in the methods ", TO invertBirMap," and ", TO approximateInverseMap, ", then it will be checked whether the maps in input and output are one the inverse of the other, throwing an error if they are not. Actually, ", TO approximateInverseMap, " will first try to fix the error of the approximation. 
 When turned on in the methods ", TO projectiveDegrees,", ", TO degreeOfRationalMap, ", ", TO isBirational," and ", TO isDominant, ", it means whether or not to use a non-probabilistic algorithm."
          } 
   document { 
    Key => {Dominant, [toMap,Dominant]}, 
--    Headline => "makes a dominant rational map" , 
           "This is an optional argument for ", TO toMap,". When a sufficiently large integer (allowed ", TO infinity,") is passed to this option, the kernel of the returned ring map will be zero.",
            } 
   document { 
    Key => {OnlySublist, [projectiveDegrees,OnlySublist]}, 
    "This is an optional argument for ", TO projectiveDegrees, " and accepts a non-negative integer, the number ", TEX///$-1$///," of projective degrees to be computed.",
          } 
   document {
    Key => {CodimBsInv, [approximateInverseMap,CodimBsInv]}, 
    "This is a technical option for ", TO approximateInverseMap, ". It accepts an integer which should be a lower bound for the codimension of the base locus of the inverse map. In most cases, one can obtain the optimal value to be passed as in the following example.",
    EXAMPLE { 
          "codimBsInv = (m) -> (
   -- input: m, the list of projective degrees of a birational map
   -- output: the codimension of the base locus of the inverse map
   k:=#m -1; z:=m_k; d:=floor(m_(k-1)/z);
   for i from 2 to k do if z*d^i - m_(k-i) > 0 then return i;
);",
"phi=toMap trim minors(2,genericSymmetricMatrix(QQ[x_0..x_5],3))",
"codimBsInv projectiveDegrees phi"
          },
       "However, sometimes larger values may be preferable."
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
     Consequences => { 
          TT "isBirational phi => isDominant phi" 
          }, 
          "The testing passes through the methods ", TO degreeOfRationalMap, " and ", TO projectiveDegrees,".",
          PARA{},
    EXAMPLE { 
          "GF(331^2)[t_0..t_4]",
          "phi=toMap(minors(2,matrix{{t_0..t_3},{t_1..t_4}}),Dominant=>infinity)",
          "time isBirational phi",
          "time isBirational(phi,MathMode=>true)"
            }
          }
   document { 
    Key => {isDominant,(isDominant,RingMap)}, 
    Headline => "whether a rational map is dominant", 
     Usage => "isDominant phi", 
     Inputs => { 
          "phi" => RingMap => {"representing a rational map ",TEX///$\Phi:X--->Y$///," (with ",TEX///$X$///," and ",TEX///$Y$///," absolutely irreducible)"}         
               }, 
     Outputs => { 
          Boolean => {"whether ",TEX///$\Phi$///," is dominant"  } 
                },
          "This method is based on the fibre dimension theorem. A more general way is to perform the command ", TT "kernel phi == 0",".",
          PARA{},
    EXAMPLE { 
          "P8=ZZ/101[x_0..x_8];",
          "phi=toMap ideal jacobian ideal det matrix{{x_0..x_4},{x_1..x_5},{x_2..x_6},{x_3..x_7},{x_4..x_8}}",
          "time isDominant(phi,MathMode=>true)",
          "P7=ZZ/101[x_0..x_7];",   
          "-- hyperelliptic curve of genus 3
C=ideal(x_4*x_5+23*x_5^2-23*x_0*x_6-18*x_1*x_6+6*x_2*x_6+37*x_3*x_6+23*x_4*x_6-26*x_5*x_6+2*x_6^2-25*x_0*x_7+45*x_1*x_7+30*x_2*x_7-49*x_3*x_7-49*x_4*x_7+50*x_5*x_7,x_3*x_5-24*x_5^2+21*x_0*x_6+x_1*x_6+46*x_3*x_6+27*x_4*x_6+5*x_5*x_6+35*x_6^2+20*x_0*x_7-23*x_1*x_7+8*x_2*x_7-22*x_3*x_7+20*x_4*x_7-15*x_5*x_7,x_2*x_5+47*x_5^2-40*x_0*x_6+37*x_1*x_6-25*x_2*x_6-22*x_3*x_6-8*x_4*x_6+27*x_5*x_6+15*x_6^2-23*x_0*x_7-42*x_1*x_7+27*x_2*x_7+35*x_3*x_7+39*x_4*x_7+24*x_5*x_7,x_1*x_5+15*x_5^2+49*x_0*x_6+8*x_1*x_6-31*x_2*x_6+9*x_3*x_6+38*x_4*x_6-36*x_5*x_6-30*x_6^2-33*x_0*x_7+26*x_1*x_7+32*x_2*x_7+27*x_3*x_7+6*x_4*x_7+36*x_5*x_7,x_0*x_5+30*x_5^2-11*x_0*x_6-38*x_1*x_6+13*x_2*x_6-32*x_3*x_6-30*x_4*x_6+4*x_5*x_6-28*x_6^2-30*x_0*x_7-6*x_1*x_7-45*x_2*x_7+34*x_3*x_7+20*x_4*x_7+48*x_5*x_7,x_3*x_4+46*x_5^2-37*x_0*x_6+27*x_1*x_6+33*x_2*x_6+8*x_3*x_6-32*x_4*x_6+42*x_5*x_6-34*x_6^2-37*x_0*x_7-28*x_1*x_7+10*x_2*x_7-27*x_3*x_7-42*x_4*x_7-8*x_5*x_7,x_2*x_4-25*x_5^2-4*x_0*x_6+2*x_1*x_6-31*x_2*x_6-5*x_3*x_6+16*x_4*x_6-24*x_5*x_6+31*x_6^2-30*x_0*x_7+32*x_1*x_7+12*x_2*x_7-40*x_3*x_7+3*x_4*x_7-28*x_5*x_7,x_0*x_4+15*x_5^2+48*x_0*x_6-50*x_1*x_6+46*x_2*x_6-48*x_3*x_6-23*x_4*x_6-28*x_5*x_6+39*x_6^2+38*x_1*x_7-5*x_3*x_7+5*x_4*x_7-34*x_5*x_7,x_3^2-31*x_5^2+41*x_0*x_6-30*x_1*x_6-4*x_2*x_6+43*x_3*x_6+23*x_4*x_6+7*x_5*x_6+31*x_6^2-19*x_0*x_7+25*x_1*x_7-49*x_2*x_7-16*x_3*x_7-45*x_4*x_7+25*x_5*x_7,x_2*x_3+13*x_5^2-45*x_0*x_6-22*x_1*x_6+33*x_2*x_6-26*x_3*x_6-21*x_4*x_6+34*x_5*x_6-21*x_6^2-47*x_0*x_7-10*x_1*x_7+29*x_2*x_7-46*x_3*x_7-x_4*x_7+20*x_5*x_7,x_1*x_3+22*x_5^2+4*x_0*x_6+3*x_1*x_6+45*x_2*x_6+37*x_3*x_6+17*x_4*x_6+36*x_5*x_6-2*x_6^2-31*x_0*x_7+3*x_1*x_7-12*x_2*x_7+19*x_3*x_7+28*x_4*x_7+30*x_5*x_7,x_0*x_3-47*x_5^2-43*x_0*x_6+6*x_1*x_6-40*x_2*x_6+21*x_3*x_6+26*x_4*x_6-5*x_5*x_6-5*x_6^2+4*x_0*x_7-15*x_1*x_7+18*x_2*x_7-31*x_3*x_7+50*x_4*x_7-46*x_5*x_7,x_2^2+4*x_5^2+31*x_0*x_6+41*x_1*x_6+31*x_2*x_6+28*x_3*x_6+42*x_4*x_6-28*x_5*x_6-4*x_6^2-7*x_0*x_7+15*x_1*x_7-9*x_2*x_7+31*x_3*x_7+3*x_4*x_7+7*x_5*x_7,x_1*x_2-46*x_5^2-6*x_0*x_6-50*x_1*x_6+32*x_2*x_6-10*x_3*x_6+42*x_4*x_6+33*x_5*x_6+18*x_6^2-9*x_0*x_7-20*x_1*x_7+45*x_2*x_7-9*x_3*x_7+10*x_4*x_7-8*x_5*x_7,x_0*x_2-9*x_5^2+34*x_0*x_6-45*x_1*x_6+19*x_2*x_6+24*x_3*x_6+23*x_4*x_6-37*x_5*x_6-44*x_6^2+24*x_0*x_7-33*x_2*x_7+41*x_3*x_7-40*x_4*x_7+4*x_5*x_7,x_1^2+x_1*x_4+x_4^2-28*x_5^2-33*x_0*x_6-17*x_1*x_6+11*x_3*x_6+20*x_4*x_6+25*x_5*x_6-21*x_6^2-22*x_0*x_7+24*x_1*x_7-14*x_2*x_7+5*x_3*x_7-39*x_4*x_7-18*x_5*x_7,x_0*x_1-47*x_5^2-5*x_0*x_6-9*x_1*x_6-45*x_2*x_6+48*x_3*x_6+45*x_4*x_6-29*x_5*x_6+3*x_6^2+29*x_0*x_7+40*x_1*x_7+46*x_2*x_7+27*x_3*x_7-36*x_4*x_7-39*x_5*x_7,x_0^2-31*x_5^2+36*x_0*x_6-30*x_1*x_6-10*x_2*x_6+42*x_3*x_6+9*x_4*x_6+34*x_5*x_6-6*x_6^2+48*x_0*x_7-47*x_1*x_7-19*x_2*x_7+25*x_3*x_7+28*x_4*x_7+34*x_5*x_7);",
          "phi=toMap(C,3,2)",
          "time isDominant(phi,MathMode=>true)"
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
"This is identical to ", TT "toMap(I,v,1)", ", while the output of ", TT "toMap(I,v,2)", " will be the map defined by the linear system of hypersurfaces of degree ",TEX///$v$///, " which are singular along the projective subscheme defined by ",TEX///$I$///,".",
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
          RingMap => "phi" => {"representing a birational map ",TEX///$\Phi:X\subseteq\mathbb{P}^n--->Y$///}
          }, 
     Outputs => { 
          RingMap => {"a ring map representing a random rational map ",TEX///$Y--->\mathbb{P}^n$///," (or ",TEX///$Y--->X$///,"), which in some sense is related to the inverse of ",TEX///$\Phi$///," (e.g., they should have the same base locus)"}
          }, 
          "The algorithm is to try to construct the ideal of the base locus of the inverse by looking for the images via ", TEX///$\Phi$///," of random linear sections of the source variety. Generally, one can speed up the process by passing through the option ", TO CodimBsInv," a good lower bound for the codimension of the base locus of ",TEX///$\Phi^{-1}$///," (note that, from the multi-degree of ",TEX///$\Phi$///,", one obtains easily the codimension and other numerical invariants of the base locus of the inverse).",
     PARA{},
    EXAMPLE { 
          "P8=ZZ/97[t_0..t_8];", 
          "phi=invertBirMap toMap(trim(minors(2,genericMatrix(P8,3,3))+random(2,P8)),Dominant=>infinity)",
          "time psi=approximateInverseMap phi",
          "isInverseMap(phi,psi) and isInverseMap(psi,phi)",
          "time psi'=approximateInverseMap(phi,CodimBsInv=>5)",
          "psi===psi'"
          }, 
       PARA{},
       "A more complicated example is the following (here ", TO invertBirMap," takes a lot of time!).",
     EXAMPLE { 
          "phi=map(P8,ZZ/97[x_0..x_11]/ideal(x_1*x_3-8*x_2*x_3+25*x_3^2-25*x_2*x_4-22*x_3*x_4+x_0*x_5+13*x_2*x_5+41*x_3*x_5-x_0*x_6+12*x_2*x_6+25*x_1*x_7+25*x_3*x_7+23*x_5*x_7-3*x_6*x_7+2*x_0*x_8+11*x_1*x_8-37*x_3*x_8-23*x_4*x_8-33*x_6*x_8+8*x_0*x_9+10*x_1*x_9-25*x_2*x_9-9*x_3*x_9+3*x_4*x_9+24*x_5*x_9-27*x_6*x_9-5*x_0*x_10+28*x_1*x_10+37*x_2*x_10+9*x_4*x_10+27*x_6*x_10-25*x_0*x_11+9*x_2*x_11+27*x_4*x_11-27*x_5*x_11,x_2^2+17*x_2*x_3-14*x_3^2-13*x_2*x_4+34*x_3*x_4+44*x_0*x_5-30*x_2*x_5+27*x_3*x_5+31*x_2*x_6-36*x_3*x_6-x_0*x_7+13*x_1*x_7+8*x_3*x_7+9*x_5*x_7+46*x_6*x_7+41*x_0*x_8-7*x_1*x_8-34*x_3*x_8-9*x_4*x_8-46*x_6*x_8-17*x_0*x_9+32*x_1*x_9-8*x_2*x_9-35*x_3*x_9-46*x_4*x_9+26*x_5*x_9+17*x_6*x_9+15*x_0*x_10+35*x_1*x_10+34*x_2*x_10+20*x_4*x_10+14*x_0*x_11+36*x_1*x_11+35*x_2*x_11-17*x_4*x_11,x_1*x_2-40*x_2*x_3+28*x_3^2-x_0*x_4+5*x_2*x_4-16*x_3*x_4+5*x_0*x_5-36*x_2*x_5+37*x_3*x_5+48*x_2*x_6-5*x_1*x_7-5*x_3*x_7+x_5*x_7+20*x_6*x_7+10*x_0*x_8+34*x_1*x_8+41*x_3*x_8-x_4*x_8+x_6*x_8+40*x_0*x_9-32*x_1*x_9+5*x_2*x_9-11*x_3*x_9-20*x_4*x_9+45*x_5*x_9-14*x_6*x_9-25*x_0*x_10+45*x_1*x_10-41*x_2*x_10-46*x_4*x_10+8*x_6*x_10-28*x_0*x_11+11*x_2*x_11+14*x_4*x_11-8*x_5*x_11),{t_4^2+t_0*t_5+t_1*t_5+35*t_2*t_5+10*t_3*t_5+25*t_4*t_5-5*t_5^2-14*t_0*t_6-14*t_1*t_6-5*t_2*t_6-13*t_4*t_6+37*t_5*t_6+22*t_6^2-31*t_3*t_7+26*t_4*t_7+12*t_5*t_7-45*t_6*t_7-46*t_3*t_8+37*t_4*t_8+28*t_5*t_8+33*t_6*t_8,t_3*t_4+4*t_0*t_5+39*t_1*t_5-40*t_2*t_5+40*t_3*t_5+26*t_4*t_5-20*t_5^2+41*t_0*t_6+36*t_1*t_6-22*t_2*t_6+36*t_4*t_6-30*t_5*t_6-13*t_6^2-25*t_3*t_7+5*t_4*t_7-35*t_5*t_7+10*t_6*t_7+11*t_3*t_8+46*t_4*t_8+29*t_5*t_8+28*t_6*t_8,t_2*t_4-5*t_0*t_5-40*t_1*t_5+12*t_2*t_5+47*t_3*t_5+37*t_4*t_5+25*t_5^2-27*t_0*t_6-22*t_1*t_6+27*t_2*t_6-23*t_4*t_6+5*t_5*t_6-13*t_6^2-39*t_3*t_7-29*t_4*t_7+9*t_5*t_7+39*t_6*t_7+36*t_3*t_8+13*t_4*t_8+26*t_5*t_8+37*t_6*t_8,t_0*t_4-t_0*t_5-8*t_1*t_5-35*t_2*t_5-10*t_3*t_5-33*t_4*t_5+5*t_5^2+15*t_0*t_6+15*t_1*t_6+5*t_2*t_6+15*t_4*t_6-38*t_5*t_6-22*t_6^2+31*t_3*t_7-25*t_4*t_7-19*t_5*t_7+47*t_6*t_7+46*t_3*t_8-36*t_4*t_8-35*t_5*t_8-31*t_6*t_8,t_2*t_3-t_0*t_5-t_1*t_5-35*t_2*t_5-10*t_3*t_5-33*t_4*t_5+5*t_5^2+14*t_0*t_6+14*t_1*t_6+5*t_2*t_6+14*t_4*t_6-31*t_5*t_6-24*t_6^2+32*t_3*t_7-25*t_4*t_7-19*t_5*t_7+47*t_6*t_7+46*t_3*t_8-36*t_4*t_8-35*t_5*t_8-31*t_6*t_8,t_1*t_3-7*t_1*t_5+t_1*t_6+t_4*t_6-7*t_5*t_6+2*t_6^2-t_3*t_7,t_0*t_3-46*t_0*t_5-39*t_1*t_5-43*t_2*t_5-41*t_3*t_5-26*t_4*t_5-28*t_5^2-35*t_0*t_6-36*t_1*t_6+20*t_2*t_6-36*t_4*t_6+9*t_5*t_6+15*t_6^2+26*t_3*t_7-5*t_4*t_7+35*t_5*t_7-10*t_6*t_7-10*t_3*t_8-46*t_4*t_8+47*t_5*t_8-25*t_6*t_8,t_2^2-46*t_1*t_4-33*t_0*t_5-45*t_1*t_5-39*t_2*t_5-39*t_3*t_5-46*t_4*t_5-29*t_5^2-48*t_0*t_6-38*t_1*t_6-30*t_2*t_6+19*t_4*t_6-44*t_5*t_6-47*t_6^2-36*t_0*t_7-46*t_1*t_7+t_2*t_7-44*t_3*t_7+48*t_4*t_7-14*t_5*t_7+4*t_6*t_7-36*t_0*t_8-46*t_1*t_8+47*t_2*t_8-34*t_3*t_8-24*t_4*t_8-12*t_5*t_8-47*t_6*t_8+47*t_7*t_8,t_1*t_2+6*t_1*t_5+5*t_0*t_6-2*t_1*t_6-t_4*t_6-t_5*t_6+5*t_0*t_7+t_1*t_7-2*t_2*t_7-7*t_5*t_7+2*t_6*t_7-2*t_1*t_8+3*t_7*t_8,t_0*t_2+t_1*t_4+5*t_0*t_5+32*t_1*t_5-20*t_2*t_5-47*t_3*t_5-37*t_4*t_5-25*t_5^2+19*t_0*t_6+22*t_1*t_6-25*t_2*t_6+25*t_4*t_6-5*t_5*t_6+13*t_6^2+5*t_0*t_7+t_1*t_7+39*t_3*t_7+28*t_4*t_7-9*t_5*t_7-39*t_6*t_7+4*t_0*t_8+t_1*t_8-36*t_3*t_8-14*t_4*t_8-26*t_5*t_8-37*t_6*t_8,t_0*t_1-39*t_1*t_4+40*t_1*t_5-37*t_0*t_6-39*t_1*t_6+19*t_4*t_6-39*t_5*t_6-38*t_0*t_7+39*t_1*t_7+19*t_2*t_7+18*t_5*t_7-19*t_6*t_7+19*t_1*t_8+20*t_7*t_8,t_0^2+12*t_1*t_4+20*t_0*t_5+27*t_1*t_5-8*t_2*t_5+37*t_3*t_5+28*t_4*t_5+30*t_5^2-46*t_0*t_6+24*t_1*t_6-40*t_2*t_6+25*t_4*t_6+16*t_5*t_6-35*t_6^2+29*t_0*t_7+12*t_1*t_7-35*t_2*t_7-8*t_3*t_7-18*t_4*t_7+42*t_5*t_7-12*t_6*t_7-6*t_0*t_8+12*t_1*t_8-15*t_3*t_8+9*t_4*t_8+20*t_5*t_8-30*t_6*t_8+4*t_7*t_8})", 
          "-- without 'CodimBsInv=>4' takes about triple time 
time psi=approximateInverseMap(phi,CodimBsInv=>4)",
          "-- but...
isInverseMap(phi,psi)",
          "-- in this case we can remedy enabling the option MathMode
time psi'=approximateInverseMap(phi,CodimBsInv=>4,MathMode=>true)",
"isInverseMap(phi,psi')",
          },
    Caveat => {"For the purpose of this method, the option ", TO MathMode, TT"=>true"," is too rigid, especially when the source of the passed map is not a projective space."}
         }


TEST ///  --- quadro-quadric Cremona transformations 
    K=ZZ/3331; ringP5=ZZ/33331[x_0..x_5]; ringP8=GF(331^2)[x_0..x_8]; ringP14=QQ[x_0..x_14]; ringP20=(ZZ/3331)[t_0..t_20]; 
    phi1=toMap trim minors(2,genericSymmetricMatrix(ringP5,3)) 
    phi2=toMap minors(2,genericMatrix(ringP8,3,3)) 
    phi3=toMap pfaffians(4,genericSkewMatrix(ringP14,6)) 
    phi4=map(ringP20,ringP20,{t_10*t_15-t_9*t_16+t_6*t_20,t_10*t_14-t_8*t_16+t_5*t_20,t_9*t_14-t_8*t_15+t_4*t_20,t_6*t_14-t_5*t_15+t_4*t_16,t_11*t_13-t_16*t_17+t_15*t_18-t_14*t_19+t_12*t_20,t_3*t_13-t_10*t_17+t_9*t_18-t_8*t_19+t_7*t_20,t_10*t_12-t_2*t_13-t_7*t_16-t_6*t_18+t_5*t_19,t_9*t_12-t_1*t_13-t_7*t_15-t_6*t_17+t_4*t_19,t_8*t_12-t_0*t_13-t_7*t_14-t_5*t_17+t_4*t_18,t_10*t_11-t_3*t_16+t_2*t_20,t_9*t_11-t_3*t_15+t_1*t_20,t_8*t_11-t_3*t_14+t_0*t_20,t_7*t_11-t_3*t_12+t_2*t_17-t_1*t_18+t_0*t_19,t_6*t_11-t_2*t_15+t_1*t_16,t_5*t_11-t_2*t_14+t_0*t_16,t_4*t_11-t_1*t_14+t_0*t_15,t_6*t_8-t_5*t_9+t_4*t_10,t_3*t_6-t_2*t_9+t_1*t_10,t_3*t_5-t_2*t_8+t_0*t_10,t_3*t_4-t_1*t_8+t_0*t_9,t_2*t_4-t_1*t_5+t_0*t_6})
    time psi1=invertBirMap(phi1)
    time psi2=invertBirMap(phi2,MathMode=>true)
    time psi3=invertBirMap(phi3,MathMode=>false)
    time psi4=invertBirMap phi4
    time assert (isInverseMap(phi1,psi1) and isInverseMap(phi2,psi2) and isInverseMap(phi3,psi3) and isInverseMap(phi4,psi4))
    time assert (degreeOfRationalMap(phi1,MathMode=>true) == 1 and degreeOfRationalMap phi2 == 1 and degreeOfRationalMap phi3 == 1 and degreeOfRationalMap phi4 == 1)
///

TEST /// 
    ringP5=(ZZ/7)[x_0..x_5];
    phi=toMap(minors(2,matrix {{x_0, x_1, x_2, x_3, x_4}, {x_1, x_2, x_3, x_4, x_5}}),Dominant=>infinity);
    time psi=invertBirMap(phi)
    assert(isInverseMap(phi,psi))
    phi'=invertBirMap(psi);
    m={1, 2, 4, 8, 11, 10};
    time assert(isInverseMap(phi',psi) and isInverseMap(psi,phi'))
    time assert(projectiveDegrees(psi,MathMode=>true) == reverse {1, 2, 4, 8, 11, 10})
///
    
TEST /// -- Hankel matrices
    phi = (r,K) -> (x:=local x; R:=K[x_0..x_(2*r)]; M:=matrix(for i to r list for j to r list x_(i+j)); toMap ideal jacobian ideal det M);
    psi = (r,K) -> (x:=local x; R:=K[x_0..x_(2*r)]; M:=matrix(for i to r-1 list for j to r+1 list x_(i+j)); toMap minors(r,M));
    psi'= (r,K) -> toMap(psi(r,K),Dominant=>2);
    psi0= (r,K) -> (f:=psi'(r,K); map((target f)/(ideal f(sub(random(1,ambient source f),source f))),target f) * f);
    assert(projectiveDegrees phi(2,frac(ZZ/331[i]/(i^2+1)))  == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees psi'(2,ZZ/331) == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees(psi'(2,ZZ/331),MathMode=>true) == {1, 2, 4, 4, 2})   
    assert(projectiveDegrees psi0(2,ZZ/331) ==    {2, 4, 4, 2})
    assert(projectiveDegrees(psi0(2,ZZ/331),MathMode=>true) ==    {2, 4, 4, 2})
    assert(degreeOfRationalMap phi(2,QQ) == 2)
    assert(projectiveDegrees invertBirMap psi'(2,ZZ/101) == reverse {1, 2, 4, 4, 2})
    assert(projectiveDegrees(invertBirMap psi'(2,ZZ/5),MathMode=>true) == reverse {1, 2, 4, 4, 2})
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
    phi=toMap(phi,Dominant=>Z)
    ideal matrix approximateInverseMap(approximateInverseMap(phi,MathMode=>true)) == ideal matrix phi
/// 
    
TEST ///
    K=ZZ/761; P4=K[x_0..x_4];
    S4=P4/ideal(x_0^3+x_1^3+x_2^3); u=gens S4;
    time phi=toMap(minors(2,matrix{{u_0,u_1,u_2,u_3},{u_1,u_2,u_3,u_4}}),Dominant=>infinity)
    time assert (projectiveDegrees phi == {3, 6, 12, 12})
    time assert (projectiveDegrees(phi,MathMode=>true) == {3, 6, 12, 12})
    time assert(isDominant(phi,MathMode=>true) and isBirational phi)
    time psi=invertBirMap(phi,MathMode=>true)
    time assert (projectiveDegrees psi == reverse {3, 6, 12, 12})
    time assert (projectiveDegrees(psi,MathMode=>true) == reverse {3, 6, 12, 12})
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

TEST ///
    R=ZZ/331[t_0,t_1];
    phi=toMap(kernel toMap((ideal vars R)^4),Dominant=>2)
    psi=invertBirMap phi
    assert( projectiveDegrees(phi,MathMode=>true) == {1, 2, 4, 4, 2})
    assert( projectiveDegrees(psi,MathMode=>true) == reverse({1, 2, 4, 4, 2}))
///

end 

 

