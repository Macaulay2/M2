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
     pols:=f.matrix;
          
     kA:=A;
     varsA:={};
     kB:=B;
     varsB:={};
               
     while (not isField kA) do
     (
	  varsA=varsA|gens kA;
	  kA=coefficientRing kA;
	  );
     
     while (kA=!=kB) do 
     (
	  varsB=varsB|gens kB;
      	  kB=coefficientRing kB;
	  );

     x:=symbol x;
     y:=symbol y;
     kk:=kA;
     m:=length varsA;
     PA:=kk[x_1..x_m];
     idealA:=kernel map(A,PA,varsA);
     n:=length varsB;
     PB:=kk[y_1..y_n];
     idealB:=kernel map(B,PB,varsB);
          
     R:=kk[y_1..y_n,x_1..x_m,MonomialOrder=>{n,m}];
     iA:=sub(idealA,matrix{{x_1..x_m}});
     iB:=sub(idealB,matrix{{y_1..y_n}});
     iGraph:=ideal(matrix{{x_1..x_m}}-sub(pols,matrix{{y_1..y_n}}));
     I:=iA+iB+iGraph;
     inI:=leadTerm I;
     
     r:=ideal(sub(inI,matrix{{y_1..y_n,m:0}}));     
     if radical r != ideal(y_1..y_n) then error "map is not finite";
     mat:=lift(basis(R/(r+ideal(x_1..x_m))),R);
     k:=numgens source mat;
     matB:=sub(mat,matrix{varsB|toList(m:0_B)});
     ke:=kernel map(B^1,A^k,f,matB);
     
     -- Added, MES:
     phi := map(R,B,matrix{{y_1..y_n}});
     toA := map(A,R,flatten{n:0_A, varsA});
     mapf := (b) -> (
	  (mons,cfs) := coefficients((phi b) % I, Monomials => mat, Variables => {y_1..y_n});
	  toA cfs
	  );
     --
     
     A^k/ke,matB,mapf --the output should remember the generators as elts of the ring 
     )

pushFwd(Module,RingMap):=(N,f)->
(
     mat:=(pushFwd f)_1;
     auxN:=ambient N/image relations N;
     ke:=kernel map(auxN,,f,mat**gens N);
     (super ke)/ke     
     )

pushFwd(ModuleMap,RingMap):=(d,f)->
(
     A:=source f;
     B:=target f;
     pols:=f.matrix;
     M:=source d;
     N:=target d;
          
     kA:=A;
     varsA:={};
     kB:=B;
     varsB:={};
               
     while (not isField kA) do
     (
	  varsA=varsA|gens kA;
	  kA=coefficientRing kA;
	  );
     
     while (kA=!=kB) do 
     (
	  varsB=varsB|gens kB;
      	  kB=coefficientRing kB;
	  );

     x:=symbol x;
     y:=symbol y;
     kk:=kA;
     m:=length varsA;
     PA:=kk[x_1..x_m];
     idealA:=kernel map(A,PA,varsA);
     n:=length varsB;
     PB:=kk[y_1..y_n];
     idealB:=kernel map(B,PB,varsB);
          
     R:=kk[y_1..y_n,x_1..x_m,MonomialOrder=>{n,m}];
     iA:=sub(idealA,matrix{{x_1..x_m}});
     iB:=sub(idealB,matrix{{y_1..y_n}});
     iGraph:=ideal(matrix{{x_1..x_m}}-sub(pols,matrix{{y_1..y_n}}));
     I:=iA+iB+iGraph;
     inI:=leadTerm I;
     
     r:=ideal(sub(inI,matrix{{y_1..y_n,m:0}}));     
     if radical r != ideal(y_1..y_n) then error "map is not finite";
     mat:=lift(basis(R/(r+ideal(x_1..x_m))),R);
     k:=numgens source mat;
     matB:=sub(mat,matrix{varsB|toList(m:0_B)});

     auxM:=ambient M/image relations M;
     keM:=kernel map(auxM,,f,matB**gens M);
     pushM:=(super keM)/keM;

     auxN:=ambient N/image relations N;
     keN:=kernel map(auxN,,f,matB**gens N);
     pushN:=(super keN)/keN;
     
     matMap:=symbol matMap;
     
     if d!=0 then
     (
            dR:=sub(matrix d,matrix{{y_1..y_n}});
     	    gR:=mat**dR;
     
     c:=degree source gR;
     l:=degree target gR;
     matMap=mutableMatrix(A,k*l,c);
     
     --i1:=symbol i1;...?
     for i1 from 0 to c-1 do
     	  for i2 from 0 to l-1 do
	  (
       	   e:=gR_i1_i2%I;
	   for i3 from 0 to k-1 do
    	     matMap_(i2+l*i3,i1)=sub(e//mat_i3_0,matrix{{n:0}|varsA});
	   );
     )
     else
     (
     matMap=mutableMatrix(A,numgens pushM,numgens pushN);	  
     );
     indMap:=map(pushN,pushM,matrix matMap);
     indMap     
     )

end
beginDocumentation()

multidoc ///
Node
  Key
    PushForward
  Headline
    pushforward functor for finite ring maps
  Description
    Text
  Caveat
  SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end
-----------  
Node
  Key
    pushFwd
    (pushFwd,RingMap)
  Headline
    push forward functor for finite ring maps
  Usage
  Inputs
  Outputs
  Consequences
  Description
    Text
    Example
    Code
    Pre
  Caveat
  SeeAlso
///

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
kk=QQ
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
--last three outputs should be all the same

--
kk=ZZ/23
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
R=kk[x,y]
S=kk[z,t]
f=map(S,R,{z^2,t^2})

M=ideal(z,t)/ideal(z^2,t^2)
g=map(M,M,matrix{{z*t,1},{0,1}})
pushFwd(g,f)
--