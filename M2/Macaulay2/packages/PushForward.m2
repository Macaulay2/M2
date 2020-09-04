newPackage(
        "PushForward",
        Version => "0.1",
        Date => "November 1, 2015",
        Authors => {{Name => "Claudiu Raicu", 
                  Email => "craicu@nd.edu", 
                  HomePage => "http://www3.nd.edu/~craicu"}},
        Headline => "push forwards of finite ring maps",
        DebuggingMode => false
        )
        
--note, this version has a slight change added by Karl Schwede.  It has an option to turn off the prune calls.

export {"pushFwd", "NoPrune"}

pushFwd=method(Options => {NoPrune => false})
pushFwd(RingMap):=o->(f)->
(
     A:=source f;
     B:=target f;
     deglenA:=degreeLength A;
     deglenB:=degreeLength B;
     psh:= pushAuxHgs f;
     matB:=psh_0;
     mapfaux:=psh_7;
     local ke;
     local freeA;

     pfB := makeModule(B^1,f,matB);
     g := map(pfB,freeA,gens pfB);
     mapf := (b) -> g*(mapfaux b); 
     pfB,matB,mapf
     )


pushFwd(RingMap,Module):=o->(f,N)->
(
     B:=target f;
     aN:=ann N;
     C:=B/aN;
     bc:=map(C,B);
     g:=bc*f;
     
     matB:=(pushAuxHgs g)_0;
     if (o.NoPrune == false) then prune makeModule(N**C,g,matB) else makeModule(N**C,g,matB)
     )

pushFwd(RingMap,ModuleMap):=o->(f,d)->
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
     k:=psh_1;
     R:=psh_2;
     I:=psh_3;
     mat:=psh_4;
     n:=psh_5;
     varsA:=psh_6;
     mapf:=psh_7;
          
     pushM:=makeModule(M,g,matB);
     pushN:=makeModule(N,g,matB);
     
     matMap:=symbol matMap;
     gR:=matB**matrix d;
     c:=numgens source gR;
     l:=numgens target gR;
     matMap=mutableMatrix(A,k*l,c);
     
     for i1 from 0 to c-1 do
     	  for i2 from 0 to l-1 do
	  (
       	       e:=mapf(gR_i1_i2);
	       for i3 from 0 to k-1 do matMap_(i2+l*i3,i1)=e_0_i3;	       
	   );

          if (o.NoPrune == false) then prune map(pushN,pushM,matrix matMap) else map(pushN,pushM,matrix matMap)
     )

makeModule=method()
makeModule(Module,RingMap,Matrix):=(N,f,matB)->
(
     auxN:=ambient N/image relations N;
     A:=source f;
     k:=(numgens ambient N) * (numgens source matB);
     mp:=try(map(auxN,,f,matB**gens N)) else map(auxN,A^k,f,matB**gens N);
     ke:=kernel mp;
     (super ke)/ke
     )

pushAuxHgs=method()
pushAuxHgs(RingMap):=(f)->
(
     A:=source f;
     B:=target f;
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
     matB:=sub(mat,matrix{varsB|toList(m:0_B)});

     phi:=map(R,B,matrix{yvars});
     toA:=map(A,R,flatten{n:0_A,varsA});
     mapf:=(b)->(
	  (mons,cfs):=coefficients((phi b)%I,Monomials=>mat,Variables=>yvars);
	  toA cfs	  
	  );
     
     matB,k,R,I,mat,n,varsA,mapf
     )

beginDocumentation()

document{
  Key => PushForward,
  Headline => "pushforward functor for finite ring maps",
  EM "PushForward", " is a package that implements the pushforward functor for finite ring maps",
  Caveat => "Works only for maps of rings finitely generated over a base field "
  }

document{
  Key => pushFwd,
  Headline => "push forward",
  "The push forward functor",
  UL {
       {TO (pushFwd,RingMap)," - for a finite ring map"},
       {TO (pushFwd,RingMap,Module), " - for a module"},
       {TO (pushFwd,RingMap,ModuleMap), " - for a map of modules"}
     }
  }   

document{
  Key => (pushFwd,RingMap),
  Headline => "push forward of a finite ring map",
  Usage => "pushFwd f",
  Inputs => { "f" },
  Outputs => {{"a presentation of the target of ",TT "f", " as a module over the source"},{"the matrix of generators of the target of ",TT "f"," as a module over the source"},{"a map that assigns to each element of the target of ", TT "f"," its representation as an element of the pushed forward module"}},
  EXAMPLE lines ///
  kk = QQ
  S = kk[a..d]
  I = monomialCurveIdeal(S, {1,3,4})
  R = S/I
  A = kk[a,d]
  use R
  F = map(R,A)
  (M,g,pf) = pushFwd F;
  M
  g
  pf(a*b - c^2)
  ///,
  TEX "In a previous version of this package, the third output was a function which assigned to each element of the target of ", TT "f", " its representation as an element of a free module which surjected onto the pushed forward module."
  }

document{
  Key => (pushFwd,RingMap,Module),
  Headline => "push forward of a module",
  Usage => "pushFwd(f,N)",
  Inputs => { "f", "N" },
  Outputs => {{"a presentation of ",TT "N", " as a module over the source of ",TT "f"}},
  TEX "Given a (not necessarily finite) ring map $f:A \\rightarrow{} B$ and a $B$-module $N$ which is finite over $A$, the function returns a presentation of $N$ as an $A$-module.",
  PARA{},
  EXAMPLE lines ///
  kk = QQ
  A = kk[t]
  B = kk[x,y]/(x*y)
  use B
  i = ideal(x)
  f = map(B,A,{x})
  pushFwd(f,module i)
  ///
  }

document{
  Key => (pushFwd,RingMap,ModuleMap),
  Headline => "push forward of a map of modules",
  Usage => "pushFwd(f,d)",
  Inputs => { "f", "d" },
  Outputs => {{"the push forward of the map d"}},
  EXAMPLE lines ///
  kk = QQ
  R = kk[a,b]
  S = kk[z,t]
  f = map(S,R,{z^2,t^2})
  M = S^1/ideal(z^3,t^3)
  g = map(M,M,matrix{{z*t}})
  p = pushFwd(f,g)
  kerg = pushFwd(f,ker g)
  kerp = prune ker p
  ///
  }

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
pushFwd f

p=symbol p
R=A[p_1,p_2]/(p_1^3-t*p_2^2)
S=A[q]
f=map(S,R,{t*q^2,t*q^3})
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
assert(isFreeModule pushFwd(f,module i))
///

-- test 7
TEST ///
kk=ZZ/101
n=3

PA=kk[x_1..x_(2*n)]
iA=ideal
iA=ideal apply(toList(1..n),i->(x_(2*i-1)^i-x_(2*i)^(i+1)))
A=PA/iA

PB=kk[y_1..y_(2*n-1)]
l=apply(toList(1..(2*n-1)),i->(x_i+x_(i+1)))
g=map(A,PB,l)
time iB=kernel g;
B=PB/iB

f=map(A,B,l)

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
pN = pushFwd(f,N)
assert(isFreeModule pN)
assert(numgens pN == 3) 
///
end

restart
uninstallPackage"PushForward"
installPackage"PushForward"
check PushForward
viewHelp PushForward
kk = QQ
R = kk[x,y]/(x^2-y^3-y^5)
R' = integralClosure R
pr = pushFwd map(R',R)
pf = last pr;
pf w_(0,0)
target oo == pr_0
---
A = QQ
B = QQ[x]/(x^2)
N = B^1 ++ (B^1/(x))
f = map(B,A)
pushFwd(f,N)
pushFwd f
