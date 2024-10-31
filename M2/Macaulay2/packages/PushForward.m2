-- TODO:
--  finish doc
--  how to interact with pushForward?
--   issues: pushForward seems somewhat faster, in the homogeneous case...
--           also, are these stashed in that case?  (They are not here, yet).

newPackage(
        "PushForward",
        Version => "0.6",
        Date => "May 14, 2021",
        Authors => {
            {Name => "Claudiu Raicu", 
                Email => "craicu@nd.edu", 
                HomePage => "http://www3.nd.edu/~craicu"},
            {Name => "David Eisenbud", 
                Email => "de@msri.org", 
                HomePage => "http://www.msri.org/~de"},
            {Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://pi.math.cornell.edu/~mike"}
            },
        Headline => "push forwards of finite ring maps",
        Keywords => {"Commutative Algebra"}
        )
        
-- note, this version has a slight change added by Karl Schwede.  It has an option to turn off the prune calls.
-- Recently, David Eisenbud and Mike Stillman have extended it, fixing some bugs too.

export {
    "isModuleFinite",
    "pushFwd", 
    "NoPrune"
    }

isInclusionOfCoefficientRing = method()
isInclusionOfCoefficientRing RingMap := Boolean => inc -> (
    --checks whether the map is the inclusion of the coefficientRing
    if source inc =!= coefficientRing target inc then return false;
    inc vars source inc == promote (vars source inc, target inc)
    )

isFinite1 = (f) -> (
    A := source f;
    B := target f;
    matB := null;
    mapf := null;
    pols := f.matrix;
    (FA, phiA) := flattenRing A;
    iFA := ideal FA;
    varsA := flatten entries phiA^-1 vars FA;
    RA := try(ring source presentation FA) else FA;
    (FB, phiB) := flattenRing B;
    iFB := ideal FB;
    varsB := flatten entries phiB^-1 vars FB;
    RB := try(ring source presentation FB) else FB;
    m := numgens FA;
    n := numgens FB;
    pols = pols_{0..(m-1)};
    R := try(tensor(RB, RA, Join => false)) else tensor(RB, RA, Join => true);
    xvars := (gens R)_{n..n+m-1};
    yvars := (gens R)_{0..n-1};
    iA := sub(ideal FA,matrix{xvars});
    iB := sub(ideal FB,matrix{yvars});
    iGraph := ideal(matrix{xvars}-sub(pols,matrix{yvars}));
    I := iA+iB+iGraph;
    inI := leadTerm I;
    r := ideal(sub(inI,matrix{yvars | splice{m:0}}));     
    for i from 1 to n do
        if ideal(sub(gens r,matrix{{(i-1):0,1_R,(m+n-i):0}}))!=ideal(1_R) then
            return false;
    true
    )

isModuleFinite = method()
isModuleFinite Ring := Boolean => R -> (
    I := ideal leadTerm ideal R;
    ge := flatten select(I_*/support, ell -> #ell == 1);
    set ge === set gens ring I
    )
isModuleFinite RingMap := Boolean => f -> (
    if isInclusionOfCoefficientRing f then
        isModuleFinite target f
    else
        isFinite1 f
    )

pushFwd=method(Options => {NoPrune => false})
pushFwd RingMap := Sequence => o -> (f) ->
--pfB is B^1 as an A-module
--matB is the set of monomials in B that form a set of generators as an A-module
--mapf takes as arg an element of B, and returns ??
(
     A:=source f;
     B:=target f;
     deglenA:=degreeLength A;
     deglenB:=degreeLength B;
     (matB, mapfaux) := pushAuxHgs f;

     pfB := makeModule(B^1,f,matB);
     g := map(pfB,,gens pfB);
     mapf := (b) -> g*(mapfaux b); 
     (pfB,matB,mapf)
     )

pushFwd Ring := Sequence => o -> B -> pushFwd(map(B, coefficientRing B), o)
pushFwd Module := Module => o -> M -> pushFwd(map(ring M, coefficientRing ring M), M, o)
pushFwd Matrix := Matrix => o -> d -> pushFwd(map(ring d, coefficientRing ring d), d, o)

pushFwd(RingMap,Module):=Module=>o->(f,N)->
(
     B:=target f;
     aN:=ann N;
     C:=B/aN;
     bc:=map(C,B);
     g:=bc*f;
     
     matB:=(pushAuxHgs g)_0;
     if (o.NoPrune == false) then prune makeModule(N**C,g,matB) else makeModule(N**C,g,matB)
     )

pushFwd(RingMap,Matrix):=Matrix=>o->(f,d)->
(
     A:=source f;
     B:=target f;
     pols:=f.matrix;
     pM:=source d;
     pN:=target d;
     
     amn:=intersect(ann pM,ann pN);
     C:=B/amn;
     bc:=map(C,B);
     g:=bc*f;     
     M:=pM**C;
     N:=pN**C;
   
     psh:=pushAuxHgs g;
     matB:=psh_0;
     mapf:=psh_1;     
          
     pushM:=makeModule(M,g,matB);
     pushN:=makeModule(N,g,matB);
     
     matMap:=symbol matMap;
     gR:=matB**matrix d;
     c:=numgens source gR;
     l:=numgens target gR;
     k := numcols matB;
     matMap=mutableMatrix(A,k*l,c);
     
     for i1 from 0 to c-1 do
     	  for i2 from 0 to l-1 do
	  (
       	       e:=mapf(gR_i1_i2);
	       for i3 from 0 to k-1 do matMap_(i2+l*i3,i1)=e_0_i3;	       
	   );

          if (o.NoPrune == false) then prune map(pushN,pushM,matrix matMap) else map(pushN,pushM,matrix matMap)
     )


-- TODO: stash the matB, pf?  Make accessor functions to go to/from gens of R over A, or M to M_A.
-- TODO: given: M = pushFwd N, get the maps from N --> M (i.e. stash it somewhere).
--   also, we want the map going backwards too: given an element of M, lift it to N.


-- makeModule
-- internal function which implements the push forward of a module.
-- input: 
--   N     : Module, a module over B
--   f     : RingMap, A --> B
--   matB  : matrix over B, with one row, whose entries form a basis for B over A.
--           in fact, it can be any desired subset of A-generators of B, as well.
-- output:
--   the module N as an A-module.
-- notes:
--   if A is a field, this should be easier?
--   the map mp is basically
--     A^k --> auxN (over B)
--   and its kernel are the A-relations of the elements auxN

makeModule=method()
makeModule(Module,RingMap,Matrix):=(N,f,matB)->
(
     N = trim N;
     auxN:=ambient N/image relations N;
     A:=source f;
     k:=(numgens ambient N) * (numgens source matB);
     --mp:=try(map(auxN,,f,matB**gens N)) else map(auxN,A^k,f,matB**gens N);
     mp := if isHomogeneous f then 
               try(map(auxN,,f,matB**gens N)) else map(auxN,A^k,f,matB**gens N)
           else
               map(auxN,A^k,f,matB**gens N);
     ke:=kernel mp;
     (super ke)/ke
     )

-- what if B is an algebra over A (i.e. A is the coefficient ring of B)
-*
    TODO.
    g = gens gb ideal L
    m = lift(matB, ring g)
    coker last coefficients(g, Monomials => m)
*-

pushAuxHgs=method()
pushAuxHgs(RingMap):=(f)-> (

    A:=source f;
    B:=target f;

    matB := null;
    mapf := null;
    
     if isInclusionOfCoefficientRing f then (
     --case when the source of f is the coefficient ring of the target:
	 if not isModuleFinite target f then error "expected a finite map";
	 matB = basis(B, Variables => 0 .. numgens B - 1);
         mapf = if isHomogeneous f 
           then (b) -> (
             (mons,cfs) := coefficients(b,Monomials=>matB);
             lift(cfs, A)
	     )
           else (b) -> (
             (mons,cfs) := coefficients(b,Monomials=>matB);
             -- strip degrees on the target, as otherwise, with differing degrees in A and B,
             -- the degree cannot always be lifted:
             cfs = map(B^(numrows cfs),B^(numcols cfs),cfs); 
	     lift(cfs, A)
	     );
         return(matB,mapf)
	 );

     pols:=f.matrix;
          
     FlatA:=flattenRing A;
     FA:=FlatA_0;
     iFA:=ideal FA;
     varsA:=flatten entries FlatA_1^-1 vars FA;
     RA:=try(ring source presentation FA) else FA;
     FlatB:=flattenRing B;
     FB:=FlatB_0;
     iFB:= ideal FB;
     varsB:=flatten entries FlatB_1^-1 vars FB;
     RB:=try(ring source presentation FB) else FB;
     m:=numgens FA;
     n:=numgens FB;
     
     pols=pols_{0..(m-1)};
          
     R := try(tensor(RB, RA, Join => false)) else tensor(RB, RA, Join => true);
     xvars := (gens R)_{n..n+m-1};
     yvars := (gens R)_{0..n-1};
     iA:=sub(ideal FA,matrix{xvars});
     iB:=sub(ideal FB,matrix{yvars});
     iGraph:=ideal(matrix{xvars}-sub(pols,matrix{yvars}));
     I:=iA+iB+iGraph;
     inI:=leadTerm I;
     
     r:=ideal(sub(inI,matrix{yvars | splice{m:0}}));     
     for i from 1 to n do
	if ideal(sub(gens r,matrix{{(i-1):0,1_R,(m+n-i):0}}))!=ideal(1_R) then
     	  error "map is not finite";

     mat:=lift(basis(R/(r+ideal(xvars))),R);
     k:=numgens source mat;
     matB = sub(mat,matrix{varsB|toList(m:0_B)});
     assert(k == numcols matB);
     phi:=map(R,B,matrix{yvars});
     toA:=map(A,R,flatten{n:0_A,varsA});
     mapf = (b)->(
	  (mons,cfs):=coefficients((phi b)%I,Monomials=>mat,Variables=>yvars);
	  toA cfs	  
	  );
     matB,mapf     
     )

beginDocumentation()

doc ///
    Key
        PushForward
    Headline
        methods to compute the pushforward of a module along a ring map
    Description
        Text
            Given a ring map $f \colon A \to B$, and a $B$-module $M$,
            $M$ has the structure of an $A$-module, and if this module is
            finitely generated over $A$, the routine @TO pushFwd@ in this package
            will compute such an $A$-module.  This is also functorial, in that if a 
            map of $B$-modules (both of which are finitely generated over $A$), then
            @TO (pushFwd, RingMap, Matrix)@ will return the induced map
            on $A$-modules.
            
            In an algebraic sense, this is really a pull back, but thinking geometrically,
            the functions here implement the push forward of a module (or sheaf).
            
            This package was originally implemented by Claudiu Raicu, some changes were
            introduced by Karl Schwede, and later by David Eisenbud and Mike Stillman.
    Subnodes
        (pushFwd, RingMap)
        (pushFwd, RingMap, Module)
        (pushFwd, RingMap, Matrix)
///

doc ///
    Key
        (pushFwd, RingMap)
	(pushFwd, Ring)
    Headline
        push forward of a finite ring map
    Usage
        pushFwd f
        pushFwd B	
    Inputs
        f:RingMap
	    or a ring B, and the map is taken to be the natural map from coefficientRing B
    Outputs
        :Sequence 
    Description
        Text
            If $f: A \to B$ is a ring map, and $B$ is finitely generated as an $A$-module,
            then the function returns a sequence $(M, g, pf)$ containing
            (1) $M \cong B^1$ as $A$-modules,
            (2) a 1-row matrix $g$ of elements of B whose entries generate B as A-module,
            (3) a function $pf$ that
            assigns to each element $b \in B$, a matrix $A^1 \to M$,
            where the image of 1 is the element $b \in M$.
        Example
            kk = QQ;
            S = kk[a..d];
            I = monomialCurveIdeal(S, {1,3,4})
            B = S/I
            A = kk[a,d];
            f = map(B,A)
            (M,g,pf) = pushFwd f;
            M
            g
            use B
	    pf(a*b - c^2)
    Caveat
        This function is meant to be internally used.
    SeeAlso
        (pushFwd, RingMap, Module)
        (pushFwd, RingMap, Matrix)
///

doc ///
    Key
        (pushFwd, RingMap, Module)
        (pushFwd, Module)
    Headline
        push forward of a module via a finite ring map
    Usage
        N = pushFwd(f, M)
	N = pushFwd M
    Inputs
        f:RingMap
            from a ring $A$ to a ring $B$
	 	    or the natural map from coefficientRing B if f not specified
        M:Module
            a $B$-module, which via $f$ is a finite $A$-module
    Outputs
        N:Module
    Description
        Text
            Given a (not necessarily finite) ring map $f : A \to B$,
            the $B$-module $M$ can be considered as a module over $A$.
            If $M$ is finite, this method returns the corresponding
            $A$-module.
        Example
            kk = QQ;
            A = kk[t];
            B = kk[x,y]/(x*y);
            use B;
            i = ideal(x)
            f = map(B,A,{x})
            pushFwd(f,module i)
    SeeAlso
        (pushFwd, Matrix)
///

doc ///
    Key
        (pushFwd, RingMap, Matrix)
        (pushFwd, Matrix)	
    Headline
        push forward of a module map via a finite ring map
    Usage
        gA = pushFwd(f, g)
        gA = pushFwd g
    Inputs
        f:RingMap
            from a ring $A$ to a ring $B$
    	 	 or (if not specified) the natural map from A = coefficientRing ring g
        g:Matrix
            (a matrix), $g : M_1 \to M_2$ of modules over $B$
    Outputs
        gA:Matrix
    Description
        Text
            If $M_1$ and $M_2$ are both finite generated as $A$-modules, via $f$, this returns the induced map
            on $A$-modules.
        Example
            kk = QQ
            A = kk[a,b]
            B = kk[z,t]
            f = map(B,A,{z^2,t^2})
            M = B^1/ideal(z^3,t^3)
            g = map(M,M,matrix{{z*t}})
            p = pushFwd(f,g)
            source p == pushFwd(f, source g)
            target p == pushFwd(f, target g)
            kerg = pushFwd(f,ker g)
            kerp = prune ker p
	    
	    k = ZZ/32003
	    A = k[x,y]/(y^4-2*x^3*y^2-4*x^5*y+x^6-y^7)
	    A = k[x,y]/(y^3-x^7)	    
	    B = integralClosure(A, Keep =>{})
    	    describe B
	    f = map(B^1, B^1, matrix{{w_(3,0)}})
	    g = pushFwd(icMap A, f)
	    pushFwd(icMap A, f^2) == g*g
	    
	    A = kk[x]
	    B = A[y, Join => false]/(y^3-x^7)	    
	    pushFwd B^1
	    pushFwd matrix{{y}}
        Text
	    Pushforward is linear and respects composition:
    	Example
	    B = A[y,z,Join => false]/(y^3 - x*z, z^3-y^7);
            pushFwd B^1
	    fy = pushFwd matrix{{y}}
	    fz = pushFwd matrix{{z}};	    
	    fx = pushFwd matrix{{x_B}};
    	    g =  pushFwd matrix{{y*z -x_B*z^2}}
	    g == fy*fz-fx*fz^2
	    fz^3-fy^7 == 0
    SeeAlso   
        (pushFwd, Module)
///

document{
  Key => pushFwd,
  Headline => "push forward",
  "The push forward functor",
  UL {
       {TO (pushFwd,RingMap)," - for a finite ring map"},
       {TO (pushFwd,RingMap,Module), " - for a module"},
       {TO (pushFwd,RingMap,Matrix), " - for a map of modules"}
     }
  }   

-- document{
--   Key => (pushFwd,RingMap),
--   Headline => "push forward of a finite ring map",
--   Usage => "pushFwd f",
--   Inputs => { "f" },
--   Outputs => {{"a presentation of the target of ",TT "f", " as a module over the source"},{"the matrix of generators of the target of ",TT "f"," as a module over the source"},{"a map that assigns to each element of the target of ", TT "f"," its representation as an element of the pushed forward module"}},
--   EXAMPLE lines ///
--   kk = QQ
--   S = kk[a..d]
--   I = monomialCurveIdeal(S, {1,3,4})
--   R = S/I
--   A = kk[a,d]
--   use R
--   F = map(R,A)
--   (M,g,pf) = pushFwd F;
--   M
--   g
--   pf(a*b - c^2)
--   ///,
--   Caveat => {TEX "In a previous version of this package, the third output was a function which assigned to each element of the target of ", TT "f", " its representation as an element of a free module 
--       which surjected onto the pushed forward module."}
--   }

-- document{
--   Key => (pushFwd,RingMap,Module),
--   Headline => "push forward of a module",
--   Usage => "pushFwd(f,N)",
--   Inputs => { "f", "N" },
--   Outputs => {{"a presentation of ",TT "N", " as a module over the source of ",TT "f"}},
--   TEX "Given a (not necessarily finite) ring map $f:A \\rightarrow{} B$ and a $B$-module $N$ which is finite over $A$, the function returns a presentation of $N$ as an $A$-module.",
--   PARA{},
--   EXAMPLE lines ///
--   kk = QQ
--   A = kk[t]
--   B = kk[x,y]/(x*y)
--   use B
--   i = ideal(x)
--   f = map(B,A,{x})
--   pushFwd(f,module i)
--   ///
--   }

-- document{
--   Key => (pushFwd,RingMap,Matrix),
--   Headline => "push forward of a map of modules",
--   Usage => "pushFwd(f,d)",
--   Inputs => { "f", "d" },
--   Outputs => {{"the push forward of the map d"}},
--   EXAMPLE lines ///
--   kk = QQ
--   R = kk[a,b]
--   S = kk[z,t]
--   f = map(S,R,{z^2,t^2})
--   M = S^1/ideal(z^3,t^3)
--   g = map(M,M,matrix{{z*t}})
--   p = pushFwd(f,g)
--   kerg = pushFwd(f,ker g)
--   kerp = prune ker p
--   ///
--   }

doc ///
    Key
        (isModuleFinite, RingMap)
        (isModuleFinite, Ring)
        isModuleFinite
    Headline
        whether the target of a ring map is finitely generated over source
    Usage
        isModuleFinite f
        isModuleFinite R
    Inputs
        f:RingMap
            or $R$ @ofClass Ring@
    Outputs
        :Boolean
    Description
        Text
            A ring map $f \colon A \to B$ makes $B$ into a module over $A$.
            This method returns true if and only if this module is a finitely generated
            $A$-module.
        Example
            kk = QQ;
            A = kk[t];
            C = kk[x,y];
            B = C/(y^2-x^3);
            f = map(A, B, {t^2, t^3})
            isWellDefined f
            isModuleFinite f
        Example
            f = map(kk[x,y], A, {x+y})
            assert not isModuleFinite f
        Text
            If a ring $R$ is given, this method returns true if and only if $R$
            is a finitely generated module over its coefficient ring.
        Example
            A = kk[x]
            B = A[y]/(y^3+x*y+3)
            isModuleFinite B
    SeeAlso
        pushFwd
///

doc ///
Key
  NoPrune
  [pushFwd,NoPrune]
Headline
  NoPrune option for pushFwd
Description
 Text
  This is an optional argument for the @TO pushFwd@ function. Its default value is {\tt false},
  which means that the presentation of a pushed forward module is pruned by default. If NoPrune 
  is set to {\tt true}, then the prune calls in pushFwd are turned off.
 Example
  R5=QQ[a..e]
  R6=QQ[a..f]
  M=coker genericMatrix(R6,a,2,3)
  G=map(R6,R5,{a+b+c+d+e+f,b,c,d,e})
  notpruned = pushFwd(G,M,NoPrune => true)
  pruned = pushFwd(G,M)
///

--test 0
TEST ///

kk=ZZ/32003
R4=kk[a..d]
R5=kk[a..e]
R6=kk[a..f]
M=coker genericMatrix(R6,a,2,3)
pdim M

G=map(R6,R5,{a+b+c+d+e+f,b,c,d,e})
F=map(R5,R4,random(R5^1,R5^{4:-1}))

P=pushFwd(G,M)
assert (pdim P==1)

Q=pushFwd(F,P)
assert (pdim Q==0)
///

-- test 1
TEST ///
P3=QQ[a..d]
M=comodule monomialCurveIdeal(P3,{1,2,3})

P2=QQ[a,b,c]
F=map(P3,P2,random(P3^1,P3^{-1,-1,-1}))
N=pushFwd(F,M)

assert(hilbertPolynomial M==hilbertPolynomial N)
///

-- test 2
TEST ///
kk = QQ
R = kk[x,y]/(x^2-y^3-y^5)
R' = integralClosure R
pr = pushFwd map(R',R)
q = pr_0 / (pr_0)_0
use R
assert(ann q==ideal(x,y))
assert isModuleFinite map(R', R)
///

-- test 3
TEST ///
kkk=ZZ/23
kk=frac(kkk[u])
T=kk[t]
x=symbol x
PR=kk[x_0,x_1]
R=PR/kernel map(T,PR,{t^3-1,t^4-t})
PS=kk[x_0,x_1,x_2]
S=PS/kernel map(T,PS,{t^3-1,t^4-t,t^5-t^2})

rs=map(S,R,{x_0,x_1})
st=map(T,S,{t^3-1,t^4-t,t^5-t^2})
assert isModuleFinite rs
assert isModuleFinite st
pst=pushFwd st

MT=pst_0
k=numgens MT

un=transpose matrix{{1_S,(k-1):0}}
MT2=MT**MT

mtt2=map(MT2,MT,un**id_MT-id_MT**un)
MMS=kernel mtt2

r1=trim minimalPresentation kernel pushFwd(rs,mtt2)
r2=trim minimalPresentation pushFwd(rs,MMS)
r3=trim (pushFwd rs)_0

assert(r1==r2)
assert(flatten entries relations r2 == flatten entries relations r3)
///

-- test 4
TEST ///
kk=ZZ/3
T=frac(kk[t])
A=T[x,y]/(x^2-t*y)

R=A[p]/(p^3-t^2*x^2)
S=A[q]/(t^3*(q-1)^6-t^2*x^2)
f=map(S,R,{t*(q-1)^2})
assert isModuleFinite f
pushFwd f

p=symbol p
R=A[p_1,p_2]/(p_1^3-t*p_2^2)
S=A[q]
f=map(S,R,{t*q^2,t*q^3})
assert isModuleFinite f
pushFwd f

i=ideal(q^2-t*x,q*x*y-t)
p=pushFwd(f,i/i^3)
assert(numgens p==2)
///

-- test 5
TEST ///
kk=QQ
A=kk[x]
B=kk[y]/(y^2)
f=map(B,A,{y})
assert isModuleFinite f
pushFwd f
use B
d=map(B^1,B^1,matrix{{y^2}})
assert(pushFwd(f,d)==0)
///

-- test 6
TEST ///
kk=QQ
A=kk[t]
B=kk[x,y]/(x*y)
use B
i=ideal(x)
f=map(B,A,{x})
assert not isModuleFinite f
assert(isFreeModule pushFwd(f,module i))
///

-- test 7
TEST ///
kk=ZZ/101
n=2

PA=kk[x_1..x_(2*n)]
iA=ideal apply(toList(1..n),i->(x_(2*i-1)^i-x_(2*i)^(i+1)))
A=PA/iA

PB=kk[y_1..y_(2*n-1)]
l=apply(toList(1..(2*n-1)),i->(x_i+x_(i+1)))
g=map(A,PB,l)
time iB=kernel g;
B=PB/iB

f=map(A,B,l)
assert isModuleFinite f
assert isModuleFinite g
time h1=pushFwd g;
ph1=cokernel promote(relations h1_0,B);
time h2=pushFwd f;

assert(ph1==h2_0)
///

--test 8
TEST ///
A = QQ
B = QQ[x]/(x^2)
N = B^1 ++ (B^1/(x))
f = map(B,A)
assert isModuleFinite f
pN = pushFwd(f,N)
assert(isFreeModule pN)
assert(numgens pN == 3) 
///

TEST///
-*
  restart
*-
  debug needsPackage "PushForward"
  kk = ZZ/101
  A = kk[s]
  B = A[t]
  C = B[u]
  f = map(C,B)
  g = map(C,B,{t})
  assert(isInclusionOfCoefficientRing f)
  assert(isInclusionOfCoefficientRing g)

  kk = ZZ/101
  A = frac (kk[s])
  B = A[t]
  C = B[u]
  f = map(C,B)
  g = map(C,B,{t})
  assert(isInclusionOfCoefficientRing f)
  assert(isInclusionOfCoefficientRing g)
///

TEST///
-*
  restart
*-
  debug  needsPackage "PushForward"
  s = symbol s; t = symbol t  
  kk = ZZ/101
  A = kk[s,t]
  -- note: this ideal is NOT the rational quartic, and in fact has an annihilator over A.
  L = A[symbol b, symbol c, Join => false]/(b*c-s*t, t*b^2-s*c^2, b^3-s*c^2, c^3 - t*b^2)
  isHomogeneous L
  describe L
  basis(L, Variables => L_*)
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  assert isModuleFinite inc
  (M,B,pf) = pushFwd inc
  assert( B*presentation M  == 0)
  assert(numcols B == 5)
///

TEST///
-*
  restart
*-
  debug  needsPackage "PushForward"
  s = symbol s; t = symbol t  
  kk = ZZ/101
  A = kk[s,t]
  L = A[symbol b, symbol c, Join => false]/(b*c-s*t,c^3-b*t^2,s*c^2-b^2*t,b^3-s^2*c)
  isHomogeneous L
  describe L
  basis(L, Variables => L_*)
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  assert isModuleFinite inc
  (M,B,pf) = pushFwd inc -- ok.  this works, but isn't awesome, as it uses a graph ideal.
  assert( B*presentation M  == 0)
  assert(numcols B == 5)
///

TEST///
-*
  restart
*-
  debug  needsPackage "PushForward"
  s = symbol s; t = symbol t  
  kk = ZZ/101
  L = kk[s, symbol b, symbol c, t]/(b*c-s*t, t*b^2-s*c^2, b^3-s*c^2, c^3 - t*b^2)
  A = kk[s,t]
  isHomogeneous L
  inc = map(L, A)
  (M,B,pf) = pushFwd inc
  assert( B * inc presentation M  == 0)
  assert(numcols B == 5)
  pushForward(inc, L^1)
///

TEST///
-*
  restart
  needsPackage "PushForward"
*-
  kk = QQ
  A = kk[x]
  R = A[y, Join=> false]/(y^7-x^3-x^2)
  (M,B,pf) = pushFwd map(R,A)
  (M1,B1,pf1) = pushFwd R
  assert(pushFwd(R^3) == pushFwd(map(R,A), R^3))
  assert((M1,B1) == (M,B))
  assert(pushFwd matrix{{y}} == pushFwd(map(R,A),matrix{{y}}))
  assert(isFreeModule M and rank M == 7)
  assert(B == basis(R, Variables => R_*))
  assert( pf(y+x)- matrix {{x}, {1}, {0}, {0}, {0}, {0}, {0}} == 0)
  R' = integralClosure R
  (M,B,pf) = pushFwd map(R',R)
  use R
  assert(M == cokernel(map(R^2,R^{{-6}, {-4}},{{-x^2-x,y^4}, {y^3,-x}})))
  assert(pf w_(2,0) - matrix {{0}, {1}} == 0)
///

TEST ///
-*
  restart
  needsPackage "PushForward"
*-
  kk = QQ
  A = kk[x, DegreeRank => 0]
  R = A[y,z, Join => false]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{1}
  fy = pushFwd matrix{{y}}
  fz = pushFwd matrix{{z}}
  assert(fy*fz == pushFwd matrix{{y*z}})
  inc = map(B,A)
  pushFwd(inc, B^1)
  pushFwd(inc, B^{1})
  fy = pushFwd(inc, matrix{{y}})
  fz = pushFwd(inc, matrix{{z}})
  assert(fy*fz == pushFwd(inc, matrix{{y*z}}))

  kk = QQ
  A = kk[x]
  R = A[y,z, Join => false]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{1}
  fy = pushFwd matrix{{y}} 
  fz = pushFwd matrix{{z}}
  assert(fy*fz == pushFwd matrix{{y*z}}) -- false
  assert(fy*fz - pushFwd matrix{{y*z}} == 0)
  inc = map(B,A)
  pushFwd(inc, B^1)
  pushFwd(inc, B^{1})
  fy = pushFwd(inc, matrix{{y}})
  fz = pushFwd(inc, matrix{{z}})
  assert(fy*fz == pushFwd(inc, matrix{{y*z}}))

  kk = QQ
  A = kk[x, DegreeRank => 0]
  R = A[y,z]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{1}
  fy = pushFwd matrix{{y}}
  fz = pushFwd matrix{{z}}
  fy*fz == pushFwd matrix{{y*z}}

  kk = QQ
  A = kk[x]
  R = A[y,z]
  I = ideal(y^4-x*y-(x^2+1)*z^2, z^4 - (x-1)*y-z^2 - z - y^3)
  B = R/I
  assert isModuleFinite map(B,A)
  (M,g,pf) = pushFwd B
  pushFwd B^1
  pushFwd B^{{0,1}}
  fy = pushFwd matrix{{y}} -- good
  fz = pushFwd matrix{{z}}
  assert(fy*fz == pushFwd matrix{{y*z}}) -- good
  assert(fy*fz - pushFwd matrix{{y*z}} == 0)
///

TEST ///
-*
restart
needsPackage "PushForward"
*-
  n = 4
  d = 4
  c = 2
  kk = ZZ/32003;
  S = kk[x_1..x_n];
  I = ideal random(S^1, S^{c:-d});
  R = S/I;
  A = kk[t_1..t_(n-c)];
  phi = map(R, A, random(R^1, R^{n-c:-1}));
  elapsedTime assert isModuleFinite phi
  elapsedTime M1 = pushFwd(phi, R^1)
  elapsedTime M2 = pushForward(phi, R^1);
  assert(M1 == M2)
///

end--

restart
uninstallPackage"PushForward"
restart
installPackage"PushForward"
x = symbol x;y= symbol y;
check PushForward
viewHelp PushForward



target oo == pr_0
pushFwd(map(R',R), R'^1)
---
A = QQ
B = QQ[x]/(x^2)
N = B^1 ++ (B^1/(x))
f = map(B,A)
pushFwd(f,N)
pushFwd f

-- example bug -----------------------------------
-- DE + MES

///
  restart
  needsPackage "PushForward"
  

  -- This one works
  kk = ZZ/101
  A = kk[s,t]
  C = A[x,y,z]/(x^2, y^2, z^2)
  phi = map(C,A)
  f = map(C^1, A^4, phi, {{x,s*y,t*y, z}})
  ker f

  -- This one fails, degrees are screwed up.
  kk = ZZ/101
  A = kk[s,t]
  B = frac A
  C = B[x,y,z]/(x^2, y^2, z^2)
  phi = map(C,B)
  f = map(C^1, B^3, phi, {{x,s*y,z}})
  ker f
///

TEST ///      
-*
  restart
 
  needsPackage "NoetherNormalForm"
*-
  needsPackage "PushForward"
  s = symbol s; t = symbol t  
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol a.. symbol d]/(d-t, a-s, b*c-s*t, b^2-(s/t)*c^2)
  describe L
  ML = pushFwd(map(L,frac A), L^1) -- dim 4, free -- FAILS

  -- simpler example which fails
  -- FIX THIS: should not create a graph ring.
  restart
  debug needsPackage "PushForward"
  s = symbol s; t = symbol t  
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol b, symbol c]/(b*c-s*t, b^2-(s/t)*c^2)
  basis L
  describe L
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  pushFwd inc
  ML = pushFwd(map(L,frac A), L^1)

  -- FIX THIS: should not create a graph ring.
  -- FIX ME?
  restart
  debug needsPackage "PushForward"
  s = symbol s; t = symbol t  
  A = QQ
  L = A[symbol b, symbol c]/(b*c-13, b^3-c^2)
  describe L
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  (LA, bas, pf) = pushFwd inc -- this works
  pf(b^2+c^2) -- maybe a better way?


  restart
  debug needsPackage "PushForward"
  s = symbol s; t = symbol t  
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol b, symbol c]/(b^2-(s/t)*c^2 - c, c^3)
  basis L
  describe L
  inc = map(L, A)
  pushForward(inc, A^1) -- now fails...
  pushFwd inc
///


///
-- Case 1. 
-- ring map is f : A --> B = A[xs]/I, A is a polynomial ring, quotient field, basic field.

///

///
    Key
    Headline
    Usage
    Inputs
    Outputs
    Description
        Text
        Example
    Caveat
    SeeAlso
///
