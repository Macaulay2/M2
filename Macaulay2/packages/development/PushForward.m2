newPackage(
        "PushForward",
        Version => "0.1", 
        Date => "December 15, 2009",
        Authors => {{Name => "Claudiu Raicu", 
                  Email => "claudiu@math.berkeley.edu", 
                  HomePage => ""}},
        Headline => "push forwards of finite ring maps",
        DebuggingMode => true
        )

export {pushFwd}

pushFwd=method()
pushFwd(RingMap):=(f)->
(
     A:=source f;
     B:=target f;     
     psh:=pushAux f;
     matB:=psh_0;
     k:=psh_1;
     mapf:=psh_7;
          
     ke:=kernel map(B^1,A^k,f,matB);
     A^k/ke,matB,mapf --the output should remember the generators as elts of the ring 
     )

pushFwd(Module,RingMap):=(N,f)->
(
     B:=target f;
     aN:=ann N;
     C:=B/aN;
     bc:=map(C,B);
     g:=bc*f;
     
     matB:=(pushAux g)_0;
     makeModule(N**C,g,matB)
     )

pushFwd(ModuleMap,RingMap):=(d,f)->
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
   
     psh:=pushAux g;
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

     map(pushN,pushM,matrix matMap)
     )

makeModule=method()
makeModule(Module,RingMap,Matrix):=(N,f,matB)->
(
     auxN:=ambient N/image relations N;
     ke:=kernel map(auxN,,f,matB**gens N);
     (super ke)/ke          
     )

pushAux=method()
pushAux(RingMap):=(f)->
(
     A:=source f;
     B:=target f;
     pols:=f.matrix;
          
     FlatA:=flattenRing A;
     FA:=FlatA_0;
     varsA:=flatten entries FlatA_1^-1 vars FA;
     FlatB:=flattenRing B;
     FB:=FlatB_0;
     varsB:=flatten entries FlatB_1^-1 vars FB;
     m:=numgens FA;
     n:=numgens FB;
     kk:=coefficientRing FA;
     x:=symbol x;
     y:=symbol y;
     
     pols=pols_{0..(m-1)};
          
     R:=kk[y_1..y_n,x_1..x_m,MonomialOrder=>{n,m}];
     iA:=sub(ideal FA,matrix{{x_1..x_m}});
     iB:=sub(ideal FB,matrix{{y_1..y_n}});
     iGraph:=ideal(matrix{{x_1..x_m}}-sub(pols,matrix{{y_1..y_n}}));
     I:=iA+iB+iGraph;
     inI:=leadTerm I;
     
     r:=ideal(sub(inI,matrix{{y_1..y_n,m:0}}));     
     for i from 1 to n do
	if ideal(sub(gens r,matrix{{(i-1):0,1_R,(m+n-i):0}}))!=ideal(1_R) then
     	  error "map is not finite";

     mat:=lift(basis(R/(r+ideal(x_1..x_m))),R);
     k:=numgens source mat;
     matB:=sub(mat,matrix{varsB|toList(m:0_B)});

     phi:=map(R,B,matrix{{y_1..y_n}});
     toA:=map(A,R,flatten{n:0_A,varsA});
     mapf:=(b)->(
	  (mons,cfs)=coefficients((phi b)%I,Monomials=>mat,Variables=>{y_1..y_n});
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
  Subnodes => {
       TO {(pushFwd,RingMap), " - for a ring map"},
       TO {(pushFwd,Module,RingMap), " - for a module"}
       }
  }   

document{
  Key => {(pushFwd,RingMap),pushFwd},
  Headline => "pushes forward a finite ring map",
  Usage => "pushFwd f",
  Inputs => { "f" },
  Outputs => {{"a presentation of the target of ",TT "f", " as a module over the base"},{"the matrix of generators of the target of ",TT "f"," as a module over the base"},{"a map that assigns to each element of the target its representation as an element of the pushed forward module"}},
  EXAMPLE lines ///
  kk = QQ
  S = kk[a..d]
  I = monomialCurveIdeal(S, {1,3,4})
  R = S/I
  A = kk[a,d]
  use R
  F = map(R,A)
  pushFwd F
  ///
  }

document{
  Key => {(pushFwd,Module,RingMap),pushFwd},
  Headline => "pushes forward a module over the target which is finite over the source (the ring map does not have to be finite)",
  Usage => "pushFwd(N,f)",
  Inputs => { "N", "f" },
  Outputs => {{"a presentation of ",TT "N", " as a module over the source of ",TT "f"}},
  EXAMPLE lines ///
  kk = QQ
  S = kk[a..d]
  I = monomialCurveIdeal(S, {1,3,4})
  R = S/I
  A = kk[a,d]
  use R
  F = map(R,A)
  pushFwd F
  ///
  }

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end

restart
installPackage "PushForward"
viewHelp PushForward

restart

loadPackage "PushForward"

-- example -- rational quartic
kk = QQ
S = kk[a..d]
I = monomialCurveIdeal(S, {1,3,4})
R = S/I
A = kk[a,d]
use R
F = map(R,A)
pushFwd F

-- example
R = kk[x,y]/(x^2-y^3-y^5)
R' = integralClosure R
pushFwd map(R',R)
oo_0 / (oo_0)_0
trim ann oo
---

--triple node
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

trim minimalPresentation kernel pushFwd(mtt2,rs)
trim minimalPresentation pushFwd(MMS,rs)
trim (pushFwd rs)_0

matB=pst_1
gnB=st(gens MMS)
matB*gnB
--last three outputs should be all the same

--
kk=ZZ/2
T1=kk[x]/(x^2)
T2=T1[y]/(y^3)

A1=T2[z,t]/(z^2-t^3)
B1=T2[u]

f=map(B1,A1,{u^3,u^2})
pushFwd f

use B1
g=map(B1^2,B1^2,promote(matrix{{x,x*y^2},{y,u}},B1))
pushFwd(g,f)

--example
kk=QQ
R=kk[i1,i2]
S=kk[z,t]
f=map(S,R,{z^2,t^2})

M=ideal(z,t)/ideal(z^2,t^2)
g=map(M,M,matrix{{z*t,1},{0,1}})
pushFwd(g,f)
--

--
kk=ZZ/3
T=frac(kk[t])
A=T[x,y]/(x^2-t*y)

--
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
pushFwd(i/i^3,f)

--zero map
kk=QQ
A=kk[x]
B=kk[y]/(y^2)
f=map(B,A,{y})
pushFwd f
use B
d=map(B^1,B^1,matrix{{y^2}})
pushFwd(d,f)

--not finite map of rings
i=symbol i;x=symbol x;
kk=QQ
A=kk[t]
B=kk[x,y]/(x*y)
use B
i=ideal(x)
f=map(B,A,{x})
pushFwd(module i,f)


--over ZZ
ZZ[x,y]/(x^3,y^5,4*x^2*y,3*x*y^3)
basis oo
ZZ[x,y,z]/(4*x*y,6*x*z,10*y*z,x^2,y^2,z^2)
basis oo
--
kk=ZZ
A=kk[x,y,z]
B=kk[a,b,c]/(4*a*b,7*a*c,10*b*c)
f=map(B,A,{a^2,b^2,c^2})
pushFwd f