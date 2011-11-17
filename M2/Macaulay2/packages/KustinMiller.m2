-- -*- coding: utf-8-unix -*-

--needsPackage "SimplicialComplexes"

newPackage(
	"KustinMiller",
    	Version => "1.3",
    	Date => "Nov 11, 2011",
    	Authors => {{Name => "Janko Boehm", 
		  Email => "boehm@math.uni-sb.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/jb/"},
                  {Name => "Stavros Papadakis", 
		  Email => "papadak@math.ist.utl.pt", 
		  HomePage => "http://www.math.ist.utl.pt/~papadak/"}
                   },
    	Headline => "Unprojection and the Kustin-Miller complex construction",
	PackageExports => {"SimplicialComplexes"},
    	DebuggingMode => true
        )


{*

      Installation:

      Put the file KustinMiller.m2 somewhere into the path of Macaulay2      
      (usually into the directory .Macaulay2/code inside your home directory, type
      path in M2 to see the path) and do inside M2

      installPackage "KustinMiller"
      
      This package requires the package SimplicialComplexes.m2 Version 1.2 or higher,
      so install this first.

*}




--------------------------------------------------------------------

-- the commands available to the user:



export({"kustinMillerComplex","unprojectionHomomorphism","verbose"})

export({"resBE","dualComplex"})

export({"stellarSubdivision","delta","isExactRes"})


needsPackage "SimplicialComplexes"

if version#"VERSION" < "1.4" then error "This package was written for Macaulay2 Version 1.4 or higher.";
if (options SimplicialComplexes).Version < "1.2" then error "This package requires the SimplicialComplexes package Version 1.2 or higher."

------------------------------------------------------------------------
-- Buchsbaum-Eisenbud resolution of the ideal of submaximal Pfaffians of a 
-- skew symmetric matrix, keeping the syzygy matrix skew-symmetric

resBE=method()
resBE(Matrix) := (A) -> (
R:=ring A;
p:=gens pfaffians(-1+rank source A,A);
n:=rank source p;
g:=matrix {apply(n,j-> (-1)^(j)*p_(n-j-1)_0)};
chainComplex {g,A,transpose g,map(R^1,R^0,0)});
--cc=resBE b2


-------------------------------------------------------------------------
-- Boundary complex of a cyclic polytope

-- find out the index of a variable
positionRing=method()
positionRing(RingElement):=(m)->(
position(gens ring m,j->j==m));

--R=QQ[x_1..x_10]
--positionRing(x_1)

isContigous=method()
isContigous(List):=(X)->(
if X=={} then return(false);
X1:=sort(X);
p2:=X1#(#X1-1);
p1:=X1#0;
abs(positionRing(p2)-positionRing(p1))==#X1-1);

{*
isContigous({x_2,x_3,x_6,x_4,x_5,x_8,x_7})
isContigous({x_2,x_3,x_6,x_4,x_5,x_8,x_7})
isContigous({x_1,x_2,x_5,x_3})
*}


contigousSubsets=method()
contigousSubsets(List):=(W)->(
select(subsets(W),isContigous))

{*
contigousSubsets({x_4,x_5})
contigousSubsets({x_3,x_4})
contigousSubsets({x_1,x_2,x_3})
contigousSubsets({x_2,x_3,x_4})
contigousSubsets({x_1,x_2,x_4})
*}


maximalElements=method()
maximalElements(List):=(L)->(
L2:=L;
L1:=maxmon(L2);
while #L1<#L2 do (
  L2=L1;
  L1=maxmon(L2);
);
L1)

--maximalElements({{x_1,x_2,x_3},{x_1,x_2},{x_3,x_4},{x_2,x_3,x_4,x_7,x_8}})

maxmon=method()
maxmon(List):=(L)->(
rm:=-1;
j:=0;
jj:=0;
while j<#L and rm==-1 do (
  jj=0;
  while jj<#L and rm==-1 do (
    if j!=jj and isSubset(set L#j,set L#jj)==true then rm=j;
  jj=jj+1);
j=j+1);
L1:={};
j=0;
while j<#L do (
  if j!=rm then L1=append(L1,L#j);
j=j+1);
L1);

--maxmon({{x_1,x_2,x_3},{x_1,x_2},{x_3,x_4},{x_2,x_3,x_4,x_7,x_8}})

maximalContigousSubsets=method()
maximalContigousSubsets(List):=(L)->(
maximalElements(contigousSubsets(L)))

{*
maximalContigousSubsets({x_1,x_2,x_4})
maximalContigousSubsets({x_1,x_2,x_4,x_5,x_7,x_8,x_9})
*}


isEndset=method()
isEndset(List):=(L)->(
v:=(entries vars ring L#0)#0;
if isSubset({v#0},L)==true or isSubset({v#(#v-1)},L)==true then return(true);
false)

{*
isEndset({x_1,x_2})
isEndset({x_1,x_3})
isEndset({x_2,x_3})
*}


removeEndsets=method()
removeEndsets(List):=(L)->(
select(L,j->not isEndset(j)))


{*
removeEndsets({{x_1,x_2},{x_3,x_4}})
removeEndsets({{x_1,x_3},{x_7,x_8}})

removeEndsets(maximalContigousSubsets({x_1,x_2,x_4}))
removeEndsets(maximalContigousSubsets({x_1,x_2,x_4,x_5,x_7,x_8}))
*}

oddContigousNonEndsets=method()
oddContigousNonEndsets(List):=(L)->(
L1:=removeEndsets(maximalContigousSubsets(L));
select(L1,j->odd(#j)))

{*
maximalContigousSubsets({x_1,x_2,x_4,x_5,x_7,x_8,x_9})
removeEndsets(oo)
oddContigousNonEndsets({x_1,x_2,x_4,x_5,x_7,x_8,x_9})
*}

isFaceOfCyclicPolytope=method()
isFaceOfCyclicPolytope(List,ZZ):=(W,d)->(
W=={} or #oddContigousNonEndsets(W)<=d-#W);

{*
isFaceOfCyclicPolytope({x_2,x_3},3)
isFaceOfCyclicPolytope({x_3,x_4},3)
isFaceOfCyclicPolytope({x_4,x_9},3)
*}

-- boundary complex of a cyclic polytope
delta=method()
delta(ZZ,PolynomialRing):=(d,R)->(
S1:=apply(select(subsets(gens R,d),j->isFaceOfCyclicPolytope(j,d)),face);
simplicialComplex S1);


-----------------------------------------------------------------------------
-- Constructing the Kustin-Miller complex

kustinMillerComplex=method(Options=>{verbose=>0})

kustinMillerComplex(Ideal,Ideal,PolynomialRing):=opt->(I,J,T0)->(
if ring I =!= ring J then error("expected ideals in the same ring");
if I+J!=J then error("expected first ideal contained in second");
if codim(I) != -1+codim(J) then error("expected codim 1 unprojection locus");
kustinMillerComplex(res I,res J,T0,opt));


kustinMillerComplex(ChainComplex,ChainComplex,PolynomialRing):=opt->(cI0,cJ0,T0)->(
     if opt.verbose>1 then (
       print("------------------------------------------------------------------------------------------------------------------------");
       print("");
       print("res(I): ");
       print("");
       for j from 1 to length(cI0) do (
         print("a_"|j|" = "|net(cI0.dd_j)|" : "|net(degrees source cI0.dd_j)|" -> "|net(degrees target cI0.dd_j));print("");
       );
       print("");
        print("------------------------------------------------------------------------------------------------------------------------");
       print("");
       print("res(J): ");
       print("");
       for j from 1 to length(cJ0) do (
         print("b_"|j|" = "|net(cJ0.dd_j)|" : "|net(degrees source cJ0.dd_j)|" -> "|net(degrees target cJ0.dd_j));print("");
       );
        print("------------------------------------------------------------------------------------------------------------------------");
       print("");
     );
I:=ideal(cI0.dd_1);
J:=ideal(cJ0.dd_1);
phi:=unprojectionHomomorphism(I,J);
R:=ring I;
if R =!= ring J then error("expected complexes over the same ring");
K:=coefficientRing R;
degT:=degree phi;
S:=K[toSequence(prepend(T0_0,gens R)),Degrees=>prepend(degT,degrees R)];
cI:=substitute(cI0,S);
cJ:=substitute(cJ0,S);
g:=length(cJ);
Is:=substitute(I,S);
Js:=substitute(J,S);
dualcI:= shiftComplex ( dualComplex cI, -codim I);
dualcJ:= shiftComplex ( dualComplex cJ, -codim I);
gJ:=gens source phi;
degshift:=degree phi;
if degshift#0<=0 then print("Warning: Unprojection variable does not have positive degree.");
     if opt.verbose>1 then (
        print("phi: "|toString((entries gJ)#0)|" -> "|toString((entries phi)#0));
        print "";
        print("degree phi = "|toString(degshift));
        print("");
        print("------------------------------------------------------------------------------------------------------------------------");
        print("");
     );
IJmap:=sub(matrix entries phi,S)*((gens ideal (dualcJ.dd_0))//(gens Js));
alphaDual:=extend(dualcI,dualcJ, map(dualcI#0,dualcJ#0,IJmap));
w:=(alphaDual_(length cI))_0_0;
alpha:=toList apply(g-1,j->sub(w^(-1),S)*(transpose alphaDual_(g-2-j)));
     if opt.verbose>1 then (
        print("");
        for j from 1 to #alpha do (
         print("alpha_"|j|" = "|net(alpha_(j-1)));print("");
        );
        print("");
        print("------------------------------------------------------------------------------------------------------------------------");
        print("");
     );
cJ1:=shiftComplex(cJ ,1);
betaMap:=sub(matrix entries phi,S)*((gens ideal (cJ1.dd_0))// (gens Js));
beta1:=extend(cI,cJ1,map((cI#0,cJ1#0,betaMap)));
beta:=toList apply(g, j-> -(beta1_j));
     if opt.verbose>1 then (
        for j from 1 to #beta-1 do (
         print("beta_"|j|" = "|net(beta_(j-1)));print("");
        );
        print("");
        print("------------------------------------------------------------------------------------------------------------------------");
        print("");
     );
u:=-(beta1_(length cI))_0_0;
     if opt.verbose>1 then (
        print("u = "|toString(u));
        print("");
        print("------------------------------------------------------------------------------------------------------------------------");
        print("");
     );
-- construct the homotopy h
h:={0_S};
for j from 1 to g-1 do (
  tC1:= chainComplex { id_(S^(rank (cI#j)))};
  tC2:= chainComplex { cI.dd_j };
  hi:= (extend ( tC2, tC1, map (tC2#0, tC1#0,  beta#(j-1)*alpha#(j-1) - h#(j-1)*cI.dd_j  )));
  h=append(h,hi_1);
);
     if opt.verbose>1 then (
        for j from 1 to #h-1 do (
         print("h_"|j|" = "|net(h_(j-1)));print("");
        );
        print("");
        print("------------------------------------------------------------------------------------------------------------------------");
        print("");
     );
-- form the differentials of the Kustin-Miller complex
L:={};
for j from 1 to g do (
  if g==2 then (
   if j==1 then (
     beta1:=beta#0;
     inL:={cI.dd_1,beta1,cJ.dd_1,S_0};
   );
   if j==2 then (
     ai:=cJ.dd_j;
     bim1:=cI.dd_(j-1);
     alphaim1:=alpha#(j-2);
     inL={alphaim1,ai,bim1,u,S_0};
   );
  );
  if g>=3 then (
   if j==1 then (
     beta1=beta#0;
     inL={cI.dd_1,beta1,cJ.dd_1,S_0};
   );
   if j==2 then (
     b2:=cI.dd_2;
     a2:=cJ.dd_2;
     beta2:=beta#1;
     h1:=h#1;
     inL={b2,beta2,h1,a2,alpha#0,S_0};
   );
   if j>=3 and j<=g-2 then (
     bi:=cI.dd_j;
     ai=cJ.dd_j;
     bim1=cI.dd_(j-1);
     alphaim1=alpha#(j-2);
     betai:=beta#(j-1);
     him1:=h#(j-1);
     inL={bi,betai,him1,ai,alphaim1,bim1,S_0};
   );
   if j>=3 and j==g-1 then (
     ai=cJ.dd_j;
     bim1=cI.dd_(j-1);
     alphaim1=alpha#(j-2);
     him1=h#(j-1);
     betai=beta#(j-1);
     inL={betai,him1,ai,alphaim1,bim1,S_0};
   );
   if j>=3 and j==g then (
     ai=cJ.dd_j;
     bim1=cI.dd_(j-1);
     alphaim1=alpha#(j-2);
     inL={alphaim1,ai,bim1,u,S_0};
   );
  );
  L=append(L,differentials(inL,j,g,degshift));
);
     if opt.verbose>1 then (
        for j from 1 to #L do (
         print("f_"|j|" = "|net(L_(j-1))|" : "|net(degrees source L_(j-1))|" -> "|net(degrees target L_(j-1)));print("");
        );
        print("");
        print("------------------------------------------------------------------------------------------------------------------------");
        print("");
     );
chainComplex(L))

--kustinMillerComplex(I,J,QQ[t])


{*
restart
installPackage("KustinMiller")
R = QQ[x_1..x_4,z_1..z_4, T]
I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
betti res I
J = ideal (z_1..z_4)
betti res J
cc=kustinMillerComplex(I,J,QQ[t]);
S=ring cc
cc
betti cc
isExactRes cc
cc.dd_1
cc.dd_2
cc.dd_3
cc.dd_4

*}


--kustinMillerComplex(L#0,L#1,QQ[t])



-- some auxiliary procedures

checkSameRing=method()
checkSameRing(List):=(L)->(
for j from 0 to #L-2 do (
  if (ring L#j)=!=(ring L#(j+1)) then return(false);
);
true)



differentials=method()
differentials(List,ZZ,ZZ,List):=(L,i,g,degshift)->(
       if i<0 or i>g then error("wrong index");
       if checkSameRing(L)==false then error("expected input over the same ring");
if g==2 then (
 if i==1 then (
   b1:=L#0;beta1:=L#1;a1:=L#2;T:=L#3;
       if rank target b1 != rank target beta1 or rank target b1 != rank target a1 then error("wrong size step "|(toString(i)));
   return(makeGrading((beta1+T*a1),L,{i,g},degshift));
 );
 if i==2 then (
  alphaim1:=L#0;ai:=L#1;bim1:=L#2;u:=L#3;T=L#4;
       if rank target alphaim1 != rank target ai then error("wrong size step "|(toString(i)));
       if rank source alphaim1 != rank source ai then error("wrong size step "|(toString(i)));
  return(makeGrading((-alphaim1+(-1)^i*u^(-1)*T*ai),L,{i,g},degshift))
 );
);
if g==3 then (
 if i==1 then (
   b1=L#0;beta1=L#1;a1=L#2;T=L#3;
       if rank target b1 != rank target beta1 or rank target b1 != rank target a1 then error("wrong size step "|(toString(i)));
   return(makeGrading(b1|(beta1+T*a1),L,{i,g},degshift));
 );
 if i==2 then (
  b2:=L#0;beta2:=L#1;h1:=L#2;a2:=L#3;alpha1:=L#4;T=L#5;
       if rank target b2 != rank target beta2 or rank target b2 != rank target h1 then error("wrong size step "|(toString(i)));
       if rank source a2 != rank source beta2 then error("wrong size step "|(toString(i)));
       if rank target a2 != rank target alpha1 then error("wrong size step "|(toString(i)));  
       if rank source h1 != rank source alpha1 then error("wrong size step "|(toString(i)));
  n:=rank source b2;
  m:=rank target a2;
  R:=ring b2;
  zero1:=map(R^m,R^n,0);
  return(makeGrading((beta2|(h1+T*id_(R^(rank target b2))))||((-a2)|(-alpha1)),L,{i,g},degshift));
 );
 if i==3 then (
  alphaim1=L#0;ai=L#1;bim1=L#2;u=L#3;T=L#4;
       if rank target alphaim1 != rank target ai then error("wrong size step "|(toString(i)));
       if rank source alphaim1 != rank source ai then error("wrong size step "|(toString(i)));
       if rank source ai != rank source bim1 then error("wrong size step "|(toString(i)));  
  return(makeGrading((-alphaim1+(-1)^i*u^(-1)*T*ai)||(bim1),L,{i,g},degshift))
 );
);
if i==1 then (
   b1=L#0;beta1=L#1;a1=L#2;T=L#3;
        if rank target b1 != rank target beta1 or rank target b1 != rank target a1 then error("wrong size step "|(toString(i)));
   return(makeGrading(b1|(beta1+T*a1),L,{i,g},degshift));
);
if i==2 then (
  b2=L#0;beta2=L#1;h1=L#2;a2=L#3;alpha1=L#4;T=L#5;
       if rank target b2 != rank target beta2 or rank target b2 != rank target h1 then error("wrong size step "|(toString(i)));
       if rank source a2 != rank source beta2 then error("wrong size step "|(toString(i)));
       if rank target a2 != rank target alpha1 then error("wrong size step "|(toString(i)));  
       if rank source h1 != rank source alpha1 then error("wrong size step "|(toString(i)));
  n=rank source b2;
  m=rank target a2;
  R=ring b2;
  zero1=map(R^m,R^n,0);
  return(makeGrading((b2|beta2|(h1+T*id_(R^(rank target b2))))||(zero1|(-a2)|(-alpha1)),L,{i,g},degshift));
);
if i>=3 and i<=g-2 then (
  bi:=L#0;betai:=L#1;him1:=L#2;ai=L#3;alphaim1=L#4;bim1=L#5;T=L#6;
       if rank target bi != rank target betai or rank target bi != rank target him1 then error("wrong size step "|(toString(i)));
       if rank source ai != rank source betai then error("wrong size step "|(toString(i)));
       if rank target ai != rank target alphaim1 then error("wrong size step "|(toString(i)));  
       if rank source him1 != rank source alphaim1 then error("wrong size step "|(toString(i)));
       if rank source bim1 != rank source alphaim1 then error("wrong size step "|(toString(i)));
  n=rank source bi;
  m=rank target ai;
  l:=rank source ai;
  k:=rank target bim1;
  R=ring bi;
  zero1=map(R^m,R^n,0);
  zero2:=map(R^k,R^n,0);
  zero3:=map(R^k,R^l,0);
  return(makeGrading((bi|betai|(him1+(-1)^i*T*id_(R^(rank target bi))))||(zero1|(-ai)|(-alphaim1))||(zero2|zero3|bim1),L,{i,g},degshift))
);
if i>=3 and i==g-1 then (
  betai=L#0;him1=L#1;ai=L#2;alphaim1=L#3;bim1=L#4;T=L#5;
       if rank target betai != rank target him1 then error("wrong size step "|(toString(i)));
       if rank source ai != rank source betai then error("wrong size step "|(toString(i)));
       if rank target ai != rank target alphaim1 then error("wrong size step "|(toString(i)));  
       if rank source him1 != rank source alphaim1 then error("wrong size step "|(toString(i)));
       if rank source bim1 != rank source alphaim1 then error("wrong size step "|(toString(i)));
  l=rank source ai;
  k=rank target bim1;
  R=ring ai;
  zero3=map(R^k,R^l,0);
  return(makeGrading((betai|(him1+(-1)^i*T*id_(R^(rank source bim1))))||((-ai)|(-alphaim1))||(zero3|bim1),L,{i,g},degshift))
);
if i>=3 and i==g then (
  alphaim1=L#0;ai=L#1;bim1=L#2;u=L#3;T=L#4;
       if rank target alphaim1 != rank target ai then error("wrong size step "|(toString(i)));
       if rank source alphaim1 != rank source ai then error("wrong size step "|(toString(i)));
       if rank source ai != rank source bim1 then error("wrong size step "|(toString(i)));  
  return(makeGrading((-alphaim1+(-1)^i*u^(-1)*T*ai)||(bim1),L,{i,g},degshift))
);
error("wrong index"))

-- give the Kustin-Miller complex the correct grading
makeGrading=method()
makeGrading(Matrix,List,List,List):=(M,L,ig,degshift)->(
i:=ig#0;
g:=ig#1;
R:=ring M;
if g==2 then (
   if i==1 then (
     b1:=L#0;beta1:=L#1;a1:=L#2;T:=L#3;
     return(map(target b1,(source a1)**R^{-degshift},M));
   );
   if i==2 then (
     alphaim1:=L#0;ai:=L#1;bim1:=L#2;u:=L#3;T=L#4;
     return(map((target ai)**R^{-degshift},(source bim1)**R^{-degshift},M));
   );
);
if g==3 then (
 if i==1 then (
   b1=L#0;beta1=L#1;a1=L#2;T=L#3;
   return(map(target b1,(source b1)++(source a1)**R^{-degshift},M));
 );
 if i==2 then (
   b2:=L#0;beta2:=L#1;h1:=L#2;a2:=L#3;alpha1:=L#4;T=L#5;
   return(map((target b2)++(target a2)**R^{-degshift},((source a2)**R^{-degshift})++(target b2)**R^{-degshift},M));
 );
 if i==3 then (
   alphaim1=L#0;ai=L#1;bim1=L#2;u=L#3;T=L#4;
   return(map(((target ai)**R^{-degshift})++(target bim1)**R^{-degshift},(source bim1)**R^{-degshift},M));
 );
);
if g>3 then (
  if i==1 then (
    b1=L#0;beta1=L#1;a1=L#2;T=L#3;
    return(map(target b1,(source b1)++(source a1)**R^{-degshift},M));
  );
  if i==2 then (
    b2=L#0;beta2=L#1;h1=L#2;a2=L#3;alpha1=L#4;T=L#5;
    return(map((target b2)++(target a2)**R^{-degshift},(source b2)++((source a2)**R^{-degshift})++(target b2)**R^{-degshift},M));
  );
  if i>=3 and i<=g-2 then (
    bi:=L#0;betai:=L#1;him1:=L#2;ai=L#3;alphaim1=L#4;bim1=L#5;T=L#6;
    return(map((target bi)++((target ai)**R^{-degshift})++(target bim1)**R^{-degshift},(source bi)++((source ai)**R^{-degshift})++(target bi)**R^{-degshift},M));
  );
  if i>=3 and i==g-1 then (
    betai=L#0;him1=L#1;ai=L#2;alphaim1=L#3;bim1=L#4;T=L#5;
    return(map((source bim1)++((target ai)**R^{-degshift})++(target bim1)**R^{-degshift},((source ai)**R^{-degshift})++(source bim1)**R^{-degshift},M));
  );
  if i>=3 and i==g then (
    alphaim1=L#0;ai=L#1;bim1=L#2;u=L#3;T=L#4;
    return(map(((target ai)**R^{-degshift})++(target bim1)**R^{-degshift},(source bim1)**R^{-degshift},M));
  );
);
error("wrong index"))


------------------------------------------------------------------------------
-- Compute the unprojection homomorphism phi

unprojectionHomomorphism=method()
unprojectionHomomorphism(Ideal,Ideal):=(I,J)->(
R:=ring I;
     if ring I =!= ring J then error("expected ideals in the same ring");
     if I+J!=J then error("expected first ideal contained in second");
M:=Hom(J,R^1/I);
-- give some feedback on wrong input
     if rank source gens M != 2 and rank source gens M != 1 then (
        for j from 0 to -1+rank source gens M do (
           phi:=homomorphism M_{j};
           gJ:=gens source phi;
           print("phi: "|toString((entries gJ)#0)|" -> "|toString((entries phi)#0));
        );
        error("wrong number of generators");
     );
     if codim(I) != -1+codim(J) then error("expected codim 1 unprojection locus");
if rank source gens M == 1 then (
  print "Warning: Ideal J is principle in R/I.";
  return homomorphism M_{0};
) else (
  f1:=homomorphism M_{0};
  f2:=homomorphism M_{1};
  if J+I==I+ideal (entries f1)#0 then (
   return f2
  ) else (
   return f1
  )
))


---------------------------------------------------------------------
-- some usefull stuff for chain complexes

-- check whether a chain complex is a resolution
-- note that this is not the same as isExact in the
-- chain complex extras
isExactRes=method()
isExactRes(ChainComplex):=(cc)->(
tst:=true;
for j from min(cc)+1 to max(cc) do (
    if cc.dd_(j)*cc.dd_(j+1) !=0 then return false;
    if (HH^j cc) !=0 then return false;
);
true)

-- with this method also the substituted complexes
-- recognize when printed, if a name is assigned to
-- the ring of the complex
substitute(ChainComplex,Ring):=(cc,S)->(
    dual cc;
    cn:= new ChainComplex;
    cn.ring = S;
    for i from min(cc) to max(cc) do cn#i = S^(degrees (cc#i));
    for i from min(cc)+1 to max(cc) do cn.dd_i = sub(cc.dd_i,S);
    cn)


-- this behaves different to the grading than [ ]
shiftComplex= method()
shiftComplex(ChainComplex,ZZ) := (CJ,p) ->  (
    CJShifted := new ChainComplex;
    CJShifted.ring = ring CJ;
    for i from min(CJ) -p  to max (CJ)-p   do  CJShifted#i = (CJ#(i+p));
    for i from min(CJ) -p+1  to max (CJ)-p   do  CJShifted.dd_i = CJ.dd_(i+p);
    CJShifted   )

-- this is not introducing the alternating sign as the built in M2 command would do
dualComplex= method()
dualComplex(ChainComplex)  := (CJ) -> (
  dual CJ;
  CJdual := new ChainComplex; 
  CJdual.ring = ring CJ;
  for i from -max(CJ) to -min(CJ) do  CJdual#i = CJ#(-i);
  for i from -max(CJ)+1  to -min(CJ) do  CJdual.dd#i = transpose  CJ.dd_(-i+1); 
  CJdual )




--------------------------------------------------------------------------
-- Stellar subdivision code


stellarSubdivisionSimplex=method()
stellarSubdivisionSimplex(Face,Face,PolynomialRing,PolynomialRing):=(D,s,n,R)->(
   if  isSubface(s,D)==false then ( 
      {substitute(D,R)}
   ) else ( 
      facets(subdivideFace (D,s,n,R),useFaceClass=>true)
   )
)

-- stellar subdivision of a simplicial complex with respect to the face
-- introducing a new variable
stellarSubdivision=method()
stellarSubdivision(SimplicialComplex,Face,PolynomialRing):= (D,s0,n)  ->  (
   R1:=ring D;
   s:=substitute(s0,R1);
   if isFaceOf(s,D)==false then (
      use ring D;
      error("not a face");
   );
   L :={};
   fc:=facets(D,useFaceClass=>true);
   i:=0;
   R1=ring D;
   K:=coefficientRing R1;
   v:=join(gens R1,gens n);
   R:=K[v];
   for i from 0 to #fc-1  do (
     L = join(L,stellarSubdivisionSimplex (fc#i,s,n,R));
   );
simplicialComplex L)

joinFaces=method()
joinFaces(Face,Face):=(F,G)->(
v1:=vertices F;
v2:=vertices G;
face(v1|v2))

listMinus=method()
listMinus(List,List):=(L1,L2)->(
L3:={};
q:=0;
while q<#L1 do (
  if member(L1#q,L2)==false then L3=append(L3,L1#q);
q=q+1);
L3)



coFace=method()
coFace(Face,Face):=(F,G)->(
v1:=vertices F;
v2:=vertices G;
R:=ring G;
face(listMinus(v2,v1),R))



subdivideFace=method()
subdivideFace(Face,Face,PolynomialRing,PolynomialRing):= (D,s,n,R) -> (
   comp := substitute(coFace(s,D),R);
   nface:= substitute(face {n_0},R);
   nc:=joinFaces(comp,nface);
   L := {};
   i:=0;
   nfc:={};
   vs:=vertices s;
   vn:=n_{0};
   for i from 0 to  #vs-1 do (
      nfc=joinFaces(nc,substitute(coFace(face {vs#i},s),R));
      L= append(L,nfc);
   );
simplicialComplex L)



{*
installPackage "KustinMiller"
R=QQ[x_1..x_6]
I=monomialIdeal(product((entries vars R)#0))
D=simplicialComplex I
Dsigma=stellarSubdivision(D,face {x_1,x_2,x_3},QQ[t])


*}

-------------------------------------------------------------------------------

{*
Copyright (C) [2011] [Janko Boehm, Stavros Papadakis]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*}


------------------------------------------------------------------------------------------------------------------
-- documentation

beginDocumentation()

doc ///
  Key
    KustinMiller
  Headline
    Unprojection and the Kustin-Miller complex construction
  Description
    Text
      This package implements the construction of the Kustin-Miller complex [1]. This is the
      fundamental construction of resolutions in unprojection theory [2]. For details on the
      computation of the Kustin-Miller complex see [3].

      The main goal of unprojection theory is to provide a substitute for structure theorems
      not available for Gorenstein rings of codimension >3.

      It has been applied in various cases to construct new varieties, e.g., in [4] for Campedelli surfaces and [5] for Calabi-Yau varieties.
      
      We provide a general command @TO kustinMillerComplex@ for the Kustin-Miller complex construction and demonstrate it at several examples connecting unprojection theory
      and combinatorics like stellar subdivisions of simplicial complexes [6],
      minimal resolutions of Stanley-Reisner rings of boundary complexes {\Delta}(d,m) 
      of cyclic polytopes of dimension d on m vertices [7], and the classical 
      (non-monomial) Tom example of unprojection [2].
      

      {\bf References:}

      For the Kustin-Miller complex see:

      [1] {\it A. Kustin and M. Miller, Constructing big Gorenstein ideals from small ones, J. Algebra 85 (1983), 303-322}.

      [2] {\it S. Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268}, @HREF"http://arxiv.org/abs/math/0111195"@

      [3] {\it J. Boehm, S. Papadakis: Implementing the Kustin-Miller complex construction}, @HREF"http://arxiv.org/abs/1103.2314"@

      For constructing new varieties see for example:

      [4] {\it J. Neves and S. Papadakis, A construction of numerical Campedelli surfaces with ZZ/6 torsion, Trans. Amer. Math. Soc. 361 (2009), 4999-5021}.

      [5] {\it J. Neves and S. Papadakis, Parallel Kustin-Miller unprojection with an application to Calabi-Yau geometry, preprint, 2009, 23 pp}, @HREF"http://arxiv.org/abs/0903.1335"@

      For the stellar subdivision case see:

      [6] {\it J. Boehm, S. Papadakis: Stellar subdivisions and Stanley-Reisner rings of Gorenstein complexes}, @HREF"http://arxiv.org/abs/0912.2151"@

      For the case of cyclic polytopes see:

      [7] {\it J. Boehm, S. Papadakis: On the structure of Stanley-Reisner rings associated to cyclic polytopes}, @HREF"http://arxiv.org/abs/0912.2152"@, to appear in Osaka J. Math.


      {\bf Examples:}

      @TO "Cyclic Polytopes"@  -- Minimal resolutions of Stanley-Reisner rings of boundary complexes of cyclic polytopes

      @TO "Stellar Subdivisions"@  -- Stellar subdivisions and unprojection

      @TO "Tom"@  -- The Tom example of unprojection

      
      {\bf Key user functions:}

      {\it The central function of the package is:}

      @TO kustinMillerComplex@  -- The Kustin-Miller complex construction


      {\it Also important is the function to represent the unprojection data as a homomorphism:}

      @TO unprojectionHomomorphism@ -- Compute the homomorphism associated to an unprojection pair


      {\it Functions used in the examples to compare with the combinatorics:}

      @TO delta@  --  The boundary complex of a cyclic polytope

      @TO stellarSubdivision@  -- Compute the stellar subdivision of a simplicial complex


///



doc ///
  Key
    kustinMillerComplex
    (kustinMillerComplex,Ideal,Ideal,PolynomialRing)
    (kustinMillerComplex,ChainComplex,ChainComplex,PolynomialRing)
  Headline
    Compute Kustin-Miller resolution of the unprojection of I in J
  Usage
    kustinMillerComplex(I,J,W)
    kustinMillerComplex(cI,cJ,W)
  Inputs
    J:Ideal
        in a positively graded polynomial ring R
    I:Ideal
        contained in J
    cI:ChainComplex
        resolution of I
    cJ:ChainComplex
        resolution of J
    W:PolynomialRing
        over the the same @TO coefficientRing@ as R
        with one variable T.
  Outputs
    :ChainComplex
  Description
   Text
    Compute Kustin-Miller resolution of the unprojection of I in J (or
    equivalently of J \subset R/I) with unprojection variable T.

    We have the following setup:

    Assume R is a @TO PolynomialRing@ over a field, the degrees of all
    variables positive and I inside J inside R two homogeneous ideals of R
    such that R/I and R/J are Gorenstein and dim(R/J)=dim(R/I)-1.

    For a description of this resolution and how it is computed see
    
    J. Boehm, S. Papadakis: Implementing the Kustin-Miller complex construction, @HREF"http://arxiv.org/abs/1103.2314"@


    It is also possible to specify minimal resolutions of I and J.

    The function @TO kustinMillerComplex@ returns a chain complex over a new polynomial ring S
    with the same @TO coefficientRing@ as R and the variables of R and W, where
    degree(T) = @TO degree@ @TO unprojectionHomomorphism@(I,J). 
    
    To avoid printing the variables of this ring when printing the chain complex
    just give a name to the ring (e.g., do S = @TO ring@ cc  to call it S).

    We illustrate the Kustin-Miller complex construction at the example described in Section 5.5 of 

    Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268, @HREF"http://arxiv.org/abs/math/0111195"@


   Example
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     betti res I
     J = ideal (z_1..z_4)
     betti res J
     cc=kustinMillerComplex(I,J,QQ[T]);
     S=ring cc
     cc
     betti cc
     isExactRes cc
     print cc.dd_1
     print cc.dd_2
     print cc.dd_3
     print cc.dd_4
  SeeAlso
    unprojectionHomomorphism
///



doc ///
  Key
    unprojectionHomomorphism
    (unprojectionHomomorphism,Ideal,Ideal)
  Headline
    Compute the homomorphism associated to an unprojection pair
  Usage
    unprojectionHomomorphism(I,J)
  Inputs
    J:Ideal
    I:Ideal
        contained in J
  Outputs
    f:Matrix
  Description
   Text
    Compute the deformation associated to the unprojection of $I \subset J$ (or
    equivalently of $J \subset R/I$ where $R$ = @TO ring@ $I$ ), i.e., a homomorphism 
    
    $\phi:J \to R/I$
    
    such that the unprojected ideal is given by the ideal

    $( T*u - \phi(u)  |  u \in J ) \subset R[T]$.


    The result is represented by a matrix $f$ with @TO source@ $f$ = @TO image@ @TO gens@ $I$
    and @TO target@ $f$ = @TO cokernel@ @TO gens@ $I$.

   Example
     R = QQ[x_1..x_4,z_1..z_4, T]
     I = ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     J = ideal (z_1..z_4)
     unprojectionHomomorphism(I,J)
  SeeAlso
    kustinMillerComplex
///


doc ///
  Key
    delta
    (delta,ZZ,PolynomialRing)
  Headline
    Boundary complex of cyclic polytope.
  Usage
    delta(d,R)
  Inputs
    d:ZZ
       positive
    R:PolynomialRing
  Outputs
    :SimplicialComplex
  Description
   Text
      Boundary complex of a cyclic polytope of dimension d on the variables of R as vertices, i.e., {\Delta}(d,m) if m is the number of variables of R.

   Example
     K=QQ;
     R=K[x_0..x_6];
     C=delta(4,R)
     fVector C
     I=ideal C
     betti res I
///



doc ///
  Key
    verbose
    [kustinMillerComplex,verbose]
  Headline
    Option to print intermediate data
  Description
   Text
    @TO Option@ to print the intermediate results.

    It takes @TO ZZ@ values (standard is 0), increasing the amount of output with the value.
///


{*
doc ///
  Key
    differentials
    (differentials,List,ZZ,ZZ,List)
  Headline
    Generate the differentials of the Kustin-Miller resolution
  Usage
    differentials(L,j,g,s)
  Inputs
    L:List
       with entries of type @TO Matrix@ and the last entry of type @TO RingElement@,
       all of them defined over the same ring.
    g:ZZ
       positive
    j:ZZ
       from 1 to g
    s:ZZ
  Outputs
    :Matrix
  Description
   Text
    Generate the j-th differential of a Kustin-Miller resolution
    of length g. So, e.g., for j=1 we obtain the relations of the 
    ring resolved and for j=2 the first syzygies of those.

    We use the notation of

    J. Boehm, S. Papadakis: Implementing the Kustin-Miller complex construction, @HREF"http://arxiv.org/abs/1103.2314"@

    For any j the @TO last@ entry of L should be the variable T.

    For j=1 we assume L = \{ b_1, beta_1, a_1, T \}.

    For j=2 we assume L = \{ b_2, beta_2, h_1, a_2, alpha_1, T \}.

    For j=3,...,g-1 we assume L = \{ b_j, beta_j, h_{j-1}, a_j, alpha_{j-1}, b_{j-1}, T \}.

    For j=g-1 we assume L = \{ beta_{g-1}, h_{g-1}, a_{g-1}, alpha_{g-2}, b_{g-2}, T \}.

    For j=g we assume L = \{ alpha_{g-1}, a_g, b_{g-1}, u, T \}.
    
    Finally s equals k_1-k_2.

  SeeAlso
    kustinMillerComplex
  Caveat
    This is not really a user level function, however it is exported as occasionally it can be useful.
    The export may be removed at some point.
///
*}


doc ///
  Key
    resBE
    (resBE,Matrix)
  Headline
    Buchsbaum-Eisenbud resolution
  Usage
    resBE(A)
  Inputs
    A:Matrix
        skew-symmetric
  Outputs
    :ChainComplex
  Description
   Text
      Returns the Buchsbaum-Eisenbud resolution of the ideal of submaximal @TO pfaffians@ 
      of a skew-symmetric matrix A. The syzygy matrix will be A.
      
   Example
      R=QQ[x_1..x_4,z_1..z_4];
      A=matrix {{0,x_1,x_2,x_3,x_4},{-x_1,0,0,z_1,z_2},{-x_2,0,0,z_3,z_4},{-x_3,-z_1,-z_3,0,0},{-x_4,-z_2,-z_4,0,0}}
      resBE A
  SeeAlso
     res
///



doc ///
  Key
    isExactRes
    (isExactRes,ChainComplex)
  Headline
    Test whether a chain complex is an exact resolution.
  Usage
    isExactRes(cc)
  Inputs
    cc:ChainComplex
  Outputs
    :Boolean
  Description
   Text
    Test whether a chain complex is an exact resolution (except at position 0)

   Example
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cc= res I
     isExactRes cc
  SeeAlso
    res
///


doc ///
  Key
    (substitute,ChainComplex,Ring)
  Headline
    Substitute a chain complex to a new ring.
  Usage
    substitute(cc,R)
  Inputs
    cc:ChainComplex
    R:Ring
  Outputs
    :ChainComplex
  Description
   Text
    Substitute a chain complex cc to a new ring R.

   Example
     R=QQ[x_1..x_4,z_1];
     cc=res ideal(x_4*x_3, -x_1*x_2+x_4*z_1);
     cs=substitute(cc,QQ[x_1..x_4])
     cs.dd_1
  SeeAlso
    substitute
///

{*
doc ///
  Key
    shiftComplex
    (shiftComplex,ChainComplex,ZZ)
  Headline
    Shift the indexing of a chain complex
  Usage
    shiftComplex(cc,p)
  Inputs
    cc:ChainComplex
    p:ZZ
  Outputs
    cs:ChainComplex
  Description
   Text
     Shifts the chain complex cc by p, i.e., returns a new chain complex CS
     with cs_i =  cc_{i+p} and the same differentials as cc.     

   Example
     R = QQ[x_1..x_4,z_1..z_4, T]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cc = res I
     betti cc
     cs=shiftComplex(cc,-3)
     betti cs
  SeeAlso
     res
     betti
///
*}

doc ///
  Key
    dualComplex
    (dualComplex,ChainComplex)
  Headline
    Dualize a chain complex
  Usage
    dualComplex(cc)
  Inputs
    cc:ChainComplex
  Outputs
    dc:ChainComplex
  Description
   Text
     Dualizes the chain complex cc, i.e., dc is the chain complex with modules
     dc_p = (dual cc_(-p)) and  differentials dc_p \to dc_{p-1}   the  transpose of cc_{-p+1} \to cc_p 

     Different to the M2 method @TO (dual,ChainComplex)@ we do
     not introduce an alternating sign.

   Example
     R = QQ[x_1..x_4,z_1..z_4, T]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cc = res I
     betti cc
     dc=dualComplex(cc)
     betti dc
  SeeAlso
     (dual,ChainComplex)
///



doc ///
  Key
    stellarSubdivision
    (stellarSubdivision,SimplicialComplex,Face,PolynomialRing)
  Headline
    Compute the stellar subdivision of a simplicial complex.
  Usage
    stellarSubdivision(D,F,S)
  Inputs
    D:SimplicialComplex 
        a simplicial complex on the variables of the polynomial ring R.
    F:Face
        a face of D
    S:PolynomialRing
        a polynomial ring in one variable corresponding to the new vertex
  Outputs
    :SimplicialComplex
        the stellar subdivision of D with respect to F and S
  Description
   Text
        Computes the stellar subdivision of a simplicial complex D by subdividing the face F with a new vertex
        corresponding to the variable of S.
        The result is a complex on the variables of R**S. It is a subcomplex of the simplex on the variables of R**S.
        
   Example
     R=QQ[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     betti res I
     D=simplicialComplex(I)
     fc=facets(D,useFaceClass=>true)
     S=QQ[x_5]
     D5=stellarSubdivision(D,fc#0,S)
     I5=ideal D5
     betti res I5
   Text

   Example
     R=QQ[x_1..x_6]
     I=monomialIdeal(product((entries vars R)#0))
     D=simplicialComplex I
     S=QQ[x_7]
     Dsigma=stellarSubdivision(D,face {x_1,x_2,x_3},S)
     betti res ideal Dsigma
  SeeAlso
     simplicialComplex
     facets
     ideal
///




-----------------------------------------------------------------
-- Tests

-- test unprojectionHomomorphism
///TEST
     R = QQ[x_1..x_4,z_1..z_4, T];
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3);
     J = ideal (z_1..z_4);
     phi=unprojectionHomomorphism(I,J);
assert(phi==map(coker gens I,image gens J,matrix {{x_1*x_3, x_1*x_4, x_2*x_3, x_2*x_4}}));
///


-- test cyclic polytope command
///TEST
     K=QQ;
     R=K[x_0..x_6];
     C=delta(4,R);
     fVector C;
assert(ideal C==ideal(x_0*x_2*x_4,x_0*x_2*x_5,x_0*x_3*x_5,x_1*x_3*x_5,x_1*x_3*x_6,x_1*x_4*x_6,x_2*x_4*x_6))
///

-- test Buchsbaum-Eisenbud resolution command
///TEST
      R=QQ[x_1..x_4,z_1..z_4];
      A=matrix {{0,x_1,x_2,x_3,x_4},{-x_1,0,0,z_1,z_2},{-x_2,0,0,z_3,z_4},{-x_3,-z_1,-z_3,0,0},{-x_4,-z_2,-z_4,0,0}};
      cc=resBE A;
assert(matrix entries cc.dd_2==A);
assert(pfaffians(4,A)==ideal cc.dd_1);
///


-- test isExactRes
///TEST
     R = QQ[x_1..x_4,z_1..z_4];
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3);
     cc= res I;
assert(isExactRes cc);
///

-- test stellar subdivision code
TEST ///
     K=QQ;
     R=K[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     D=simplicialComplex(I)
     S=K[x_5]
     D5=stellarSubdivision(D,face {x_0,x_2},S)
     I5=ideal D5
     use ring I5
     assert(I5==ideal(x_4*x_5,x_3*x_5,x_1*x_5,x_3*x_4,x_0*x_4,x_2*x_3,x_1*x_2,x_0*x_2,x_0*x_1));
///


-- test Kustin-Miller complex command using C47 example
TEST ///
     K=QQ;
     C26=delta(2,K[z,x_2..x_6])
     R=K[z,x_1..x_7]
     J=sub(ideal C26,R)
     c26=res J;
     C47=delta(4,K[x_1..x_7])
     I=sub(ideal C47,R)
     c47=res I;
     cc=kustinMillerComplex(c47,c26,K[x_8]);
assert(rank(cc#1)==16);
assert(rank(cc#2)==30);
assert(isExactRes(cc));
///

-- test whether the result of unprojection is C48
TEST ///
     K=QQ;
     C26=delta(2,K[z,x_2..x_6])
     R=K[z,x_1..x_7]
     J=sub(ideal C26,R)
     c26=res J;
     C47=delta(4,K[x_1..x_7])
     I=sub(ideal C47,R)
     c47=res I;
     cc=kustinMillerComplex(c47,c26,K[x_8]);
     R'=K[x_1..x_8];
     C48=delta(4,R');
     I48=ideal C48;
assert(I48==sub(ideal cc.dd_1,R'))
///


-- test Kustin-Miller complex command using Tom example
TEST ///
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cI=res I
     betti cI
     J = ideal (z_1..z_4)
     cJ=res J
     betti cJ
     cc=kustinMillerComplex(cI,cJ,QQ[T]);
assert(rank(cc#1)==9);
assert(rank(cc#2)==16);
assert(isExactRes(cc));
///


-----------------------------------------------------------------
-- Examples


doc ///
  Key
    "Stellar Subdivisions"
  Headline
    The Kustin-Miller complex for stellar subdivisions
  Description
   Text
    We consider a Gorenstein* simplicial complex C and the complex C' obtained by
    stellar subdivision (see @TO stellarSubdivision@) of a face F of C,
    and the corresponding Stanley-Reisner ideals I and I'.

    We construct a resolution of I' from a resolution of I and from a resolution of the
    Stanley-Reisner ideal of the link of F using the Kustin-Miller complex construction 
    implemented in @TO kustinMillerComplex@. Note that this resolution
    is not necessarily minimal (for facets it is).

    For details see

    {\it J. Boehm, S. Papadakis: Stellar subdivisions and Stanley-Reisner rings of Gorenstein complexes}, @HREF"http://arxiv.org/abs/0912.2151"@


    (1) The simplest example:

    The stellar of the edge \{x_1,x_2\}\  of the triangle with vertices x_1,x_2,x_3.
    The new vertex is x_4 and z_1 is the base of the unprojection deformation.

   Example
    K=QQ;
    R=K[x_1..x_3,z_1];
    I=ideal(x_1*x_2*x_3)
    Ilink=I:ideal(x_1*x_2)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_4]);
    S=ring cc
    cc
    betti cc
    isExactRes cc
    cc.dd_1
    cc.dd_2
   Text

    Obviously the ideal resolved by the Kustin-Miller complex at the special fiber z_1=0
    is the Stanley-Reisner ideal of the stellar subdivision (i.e., of a 4-gon).


    (2) Stellar of the facet \{x_1,x_2,x_4,x_6\}\  of the simplicial complex associated to the complete intersection (x_1*x_2*x_3, x_4*x_5*x_6).
    The result is a Pfaffian:

   Example
    R=K[x_1..x_6,z_1..z_3];
    I=ideal(x_1*x_2*x_3,x_4*x_5*x_6)
    Ilink=I:ideal(x_1*x_2*x_4*x_6)
    J=Ilink+ideal(z_1*z_2*z_3)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_7]);
    S=ring cc
    cc
    betti cc
    isExactRes cc
    cc.dd_1
    cc.dd_2
    cc.dd_3
   Text

    We compare with the combinatorics, i.e., check that the ideal
    resolved by the Kustin Miller complex at the special fiber is the
    Stanley-Reisner ideal of the stellar subdivision:

   Example
    R=K[x_1..x_6];
    C=simplicialComplex monomialIdeal(x_1*x_2*x_3,x_4*x_5*x_6)
    fVector C
    F=face {x_1,x_2,x_4,x_6}
    R'=K[x_1..x_7];
    C'=substitute(stellarSubdivision(C,F,K[x_7]),R')
    fVector C'
    I'=monomialIdeal(sub(cc.dd_1,R'))
    C'==simplicialComplex I'
   Text

    One observes that in this case the resulting complex is minimal
    This is always true for stellars of facets.


    (3) Stellar of an edge:

   Example
    R=K[x_1..x_5,z_1];
    I=monomialIdeal(x_1*x_2*x_3,x_4*x_5)
    C=simplicialComplex I
    fVector C
    F=face {x_1,x_2}
    Ilink=I:ideal(product vertices F)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_6]);
    S=ring cc
    cc
    betti cc
    isExactRes cc
    cc.dd_1
    cc.dd_2
    cc.dd_3
    cc.dd_4
   Text

    (4) Starting out with the Pfaffian elliptic curve:

   Example
    R=K[x_1..x_5,z_1];
    I=ideal(x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1)
    Ilink=I:ideal(x_1*x_3)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_10]);
    betti cc
   Text

    (5) One more example of a stellar of an edge starting with a codimension 4 complete intersection:
 
   Example
    R=K[x_1..x_9,z_1];
    I=monomialIdeal(x_1*x_2,x_3*x_4,x_5*x_6,x_7*x_8*x_9)
    Ilink=I:ideal(x_1*x_3)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_10]);
    S=ring cc;
    cc
    betti cc
   Text

    We compare again with the combinatorics:

   Example
    R=K[x_1..x_9];
    C=simplicialComplex monomialIdeal(sub(I,R))
    fVector C
    F=face {x_1,x_3}
    R'=K[x_1..x_10];
    C'=substitute(stellarSubdivision(C,F,K[x_10]),R')
    fVector C'
    I'=monomialIdeal(sub(cc.dd_1,R'))
    C'==simplicialComplex I'
  SeeAlso
    kustinMillerComplex
    res
    betti
///




doc ///
  Key
    "Cyclic Polytopes"
  Headline
    Constructing minimal resolutions for Stanley-Reisner rings of boundary complexes of cyclic polytopes
  Description
   Text
    In the following example we construct the minimal resolution of the Stanley-Reisner ring of
    the codimension 4 cyclic polytope {\Delta}(4,8) from those of the
    cyclic polytopes {\Delta}(2,6) and {\Delta}(4,7) (the last one being Pfaffian).

    Of course this process can be iterated to give a recursive construction of the
    resolutions of all cyclic polytopes, for details see

    {\it J. Boehm, S. Papadakis: On the structure of Stanley-Reisner rings associated to cyclic polytopes}, @HREF"http://arxiv.org/abs/0912.2152"@, to appear in Osaka J. Math.

   Example
     K=QQ;
     C26=delta(2,K[z,x_2..x_6])
     R=K[z,x_1..x_7]
     J=sub(ideal C26,R)
     c26=res J;
     betti c26
     C47=delta(4,K[x_1..x_7])
     I=sub(ideal C47,R)
     c47=res I;
     betti c47
     cc=kustinMillerComplex(c47,c26,K[x_8]);
     betti cc
   Text

     We compare with the combinatorics, i.e., check that
     the Kustin-Miller complex at the special fiber z=0 indeed resolves 
     the Stanley-Reisner ring of {\Delta}(4,8).

   Example
     R'=K[x_1..x_8];
     C48=delta(4,R')
     I48=ideal C48
     betti res I48
     I48==sub(ideal cc.dd_1,R')
   Text

     We finish the example by printing the differentials of the Kustin-Miller complex:

   Example
     print cc.dd_1
     print cc.dd_2
     print cc.dd_3
  SeeAlso
    kustinMillerComplex
    res
    betti
///

-- remark: print command avoids matrices to be broken to the next line in the html


doc ///
  Key
    "Tom"
  Headline
    The Kustin-Miller complex for Tom
  Description
   Text
    The Kustin-Miller complex construction for the Tom example which can be found in Section 5.5 of 

    Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268, @HREF"http://arxiv.org/abs/math/0111195"@

    Here we pass from a Pfaffian to a codimension 4 variety.

   Example
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cI=res I
     betti cI
     J = ideal (z_1..z_4)
     cJ=res J
     betti cJ
     cc=kustinMillerComplex(cI,cJ,QQ[T]);
     S=ring cc
     cc
     betti cc
     isExactRes cc
     print cc.dd_1
     print cc.dd_2
     print cc.dd_3
     print cc.dd_4
  SeeAlso
    kustinMillerComplex
    res
    betti
///




{*
check "KustinMiller"
uninstallPackage("KustinMiller")
installPackage("KustinMiller")
installPackage("KustinMiller",RerunExamples=>true)
viewHelp("KustinMiller")
*}