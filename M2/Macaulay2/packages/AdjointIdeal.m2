-- -*- coding: utf-8 -*- this has to be on the first line
newPackage(
	"AdjointIdeal",
    	Version => "0.6", 
    	Date => "August 25, 2010",
    	Authors => {{Name => "Janko Boehm", 
		  Email => "boehm@mathematik.uni-kl.de", 
		  HomePage => "https://agag-jboehm.math.rptu.de/~boehm/"}},
    	Headline => "adjoint ideals of plane curves and related computations",
	Keywords => {"Commutative Algebra"},
    	DebuggingMode => false,
	CacheExampleOutput => true,
	PackageImports => {"IntegralClosure","MapleInterface"},
	AuxiliaryFiles => true
    	)

-- For information see documentation key "AdjointIdeal" below.


export {"adjointIdeal"}
export {"geometricGenus"}
export {"LRdecomposition"}
export {"extractRightUpper"}
export {"extractLeftLower"}
export {"forwardSubstitution"}
export {"backwardSubstitution"}
export {"traceMatrix"}

-- numerator of a row matrix
matnum=(ibmm)->(
q:=0;
ibl:={};
while q<rank(source(ibmm)) do (
   ibl=append(ibl,numerator ibmm_(0,q));
q=q+1);
matrix {ibl});

-- denominator of a row matrix
matden=(ibmm)->(
q:=0;
ibl:={};
while q<rank(source(ibmm)) do (
   ibl=append(ibl,denominator ibmm_(0,q));
q=q+1);
matrix {ibl});

-- reduce row matrix in ring A and substitute to R
rmodA=(ibm,R,A)->(
num:=matnum(ibm);
den:=matden(ibm);
rsnum:=substitute(substitute(num,A),ring den);
--print(ring rsnum,ring den);
--print(toString rsnum,toString den);
q:=0;
ibl:={};
while q<rank(source(ibm)) do (
   ibl=append(ibl,rsnum_(0,q)/den_(0,q));
q=q+1);
--print(ibm,ibl);
matrix {ibl});

-- the quotient of two one-row matrices
matdiv=(ibm)->(
q:=0;
ibl:={};
while q<rank(source(ibm#0)) do (
   --print(ring (ibm#0)_(0,q), ring (ibm#1)_(0,q));
   ibl=append(ibl,(ibm#0)_(0,q)/(ibm#1)_(0,q));
q=q+1);
matrix {ibl});


-----------------------------------------------------------------------------
-- we use the P*A = L*R -decomposition to compute the inverse



-- find pivot element in matrix trmw
-- in column spa starting with row k
-- we look for the minimal degree entry


-- we use as pivotfunction the vanishing order at 0 (not the degree)
-- i.e. we look for the entry with the minimal vanishing
-- so 0 get assigned infinity (not -infinity)

vanishingOrder=(p)->(
if p==0 then return(infinity);
--print(p,degree p);
if p!=0 then return((degree p)#0);
)

-- vanishingOrder=(p)->(
-- -(degree numerator p)#0)
-*
vanishingOrder=(p)->(
if p==0 then return(infinity);
--print(p,degree p);
if p!=0 then return((degree numerator p)#0);
)
*-

pivot=method()
pivot(ZZ,ZZ,Matrix,Function):=(spa,k,trmw,pivotfunction)->(
qu:=k;
n:=rank source trmw;
del:={};
while qu<n do (
   del=append(del,pivotfunction(trmw_(qu,spa)));
qu=qu+1);
--print(del,k,k+maxPosition((-1)*del));
--print trmw;
--print(del,k,k+minPosition(del));
k+minPosition(del))
--J=adjointIdeal(I,ib,ver)


-- permute the columns of the matrix trmw by the permutation in the 2-list lip
permute=method()
permute(List,Matrix):=(lip,trmw)->(
n:=rank source trmw;
kl:=lip#0;
rl:=lip#1;
perm:=join(0..(min({kl,rl})-1),(max {kl,rl})..(max {kl,rl}),(min({kl,rl})+1)..(max({kl,rl})-1),(min {kl,rl})..(min {kl,rl}),(max({kl,rl})+1)..(n-1));
transpose (transpose trmw)_(deepSplice({perm})))


-----------------------------------------------------------------------------------

-- extractLeftLower and extractRightUpper extract from the storage matrix trmw
-- the left lower triangular matrix (1 on the diag) 
-- and the right upper triangular matrix

-- returns the entry of trmw with coordinates in the list lijj
-- if lijj#0>lijj#1 and 0 otherwise
-- 
funkL=(lijj,trmw)->(
otp:=0;
if lijj#0>lijj#1 then otp=trmw_(lijj#0,lijj#1);
otp)

-- returns the entry of trmw with coordinates in the list lijj
-- if lijj#0<=lijj#1 and 0 otherwise
-- 
funkR=(lijj,trmw)->(
otp:=0;
if lijj#0<=lijj#1 then otp=trmw_(lijj#0,lijj#1);
otp)


extractRightUpper=method()
extractRightUpper(Matrix):=(trmw)->(
n:=rank source trmw;
matrix(table(n,n,(i,j)->funkR({i,j},trmw))))

extractLeftLower=method()
extractLeftLower(Matrix):=(trmw)->(
n:=rank source trmw;
QRx:=ring trmw;
id_(QRx^n)+matrix(table(n,n,(i,j)->funkL({i,j},trmw)))
)

-----------------------------------------------------------------------------------


-- trace of the product of the lis#0 and lis#1 entry of ib
-- uses precomputed mRI the inverse of the coefficient matrix of the integral basis
fieldTrace=(lis,ib,mRI,tracerings)->(
A:=tracerings#0;
R:=tracerings#1;
RE:=tracerings#2;
QRx:=tracerings#3;
ver:=0;
if ver>0 then print("mRI",mRI,ring mRI);
if ver>0 then print("lis",lis);
if ver>0 then print("ib",ib, ring (ib_(0,0)),vars ring (ib_(0,0)));
if ver>0 then print("A",A);
if ver>0 then print("R",R);
if ver>0 then print("RE",RE);
if ver>0 then print("QRx",QRx);
n:=rank source mRI;
perm:=(entries(matrix({deepSplice({-(n-1)..0})})*-1))#0;
-- compute product of the lis#0 and lis#1 entry of ib and the matrix ib
mB:=substitute(
 (
  transpose(
     (
         coefficients(
              substitute(
                  matdiv({
                    substitute(
                        substitute(
                             numerator(ib_(0,lis#0))*numerator(ib_(0,lis#1))*matnum(ib),
                        A)
                    ,R)
                    ,
                    substitute(
                             denominator(ib_(0,lis#0))*denominator(ib_(0,lis#1))*matden(ib)
                    ,R)
                  })
              ,RE)
         )
     )#1
  )
 )_perm
,QRx);
if ver>0 then print("mB",mB);
tra:=mRI_(0,0)*mB_(0,0);
qj:=1;
qz:=0;
while qj<n do (
  qz=0;
  while qz<qj+1 do (
    tra=tra+mRI_(qj,qz)*mB_(qz,qj);
  qz=qz+1);
qj=qj+1);
if ver>0 then print("tra",tra);
if ver>0 then print("------------------------------------");
tra)


-- function doing Gauss step in k-th column on the matrix trmw
-- used by the LR decomposition
gaussStep=method()
gaussStep(ZZ,Matrix):=(k,trmw)->(
n:=rank source trmw;
matrix(table(n,n,(i,j)->gaussStepEntry({i,j},k,trmw))))



-- function doing Gauss step on the matrix trmw
-- give the lij#0,lij#1 entry of the matrix obtained by Gauss step in the k-th column
-- used by the LR decomposition
gaussStepEntry=method()
gaussStepEntry(List,ZZ,Matrix):=(lij,k,trmw)->(
otp:=trmw_(lij#0,lij#1);
if ((lij#0>k) and (lij#1==k)) then otp=(trmw_(lij#0,lij#1))/trmw_(k,k);
if ((lij#0>k) and (lij#1>k)) then otp=(trmw_(lij#0,lij#1))-(trmw_(k,lij#1))*(trmw_(lij#0,k))/trmw_(k,k);
otp)




-- geometric genus
geometricGenus=method()

-*
--old code without the coordinate change

geometricGenus(Ideal):=(I1)->(
if dim I1 != 2 then error("Not a curve");
if codim I1 != 1 or rank source vars ring I1 !=3 then error("Not a plane curve");
R0:=ring I1;
z:=(vars ring I1)_(0,-1+rank source vars ring I1);
K:=coefficientRing R0;
R:=K[R0_0,R0_1];
I:=substitute(substitute(I1,R0_2=>1),R);
Iinf:=saturate substitute(substitute(I1+ideal jacobian I1,R0_2=>0),R);
if degree Iinf > 0 then error("The curve has singularities at infinity");
d:=degree I;
if contract((R_0)^d,I_(0))!=1 then error("Curve contains (1:0:0)");
QR:=frac R;
ib:=sub(integralBasis(I),QR);
geometricGenus(I1,ib))
*-

geometricGenus(Ideal):=(I1)->(
if dim I1 != 2 then error("Not a curve");
if codim I1 != 1 or rank source vars ring I1 !=3 then error("Not a plane curve");
R0:=ring I1;
x:=R0_0;
y:=R0_1;
z:=R0_2;
d:=degree I1;
tstmonic:=true;
switchvars:=false;
f:=I1_(0);
if contract(x^d,f)==0 then (
   if contract(y^d,f)==0 then (
     tstmonic=false;
     I:=sub(I1,{y=>x+y});     
   ) else (
     switchvars=true;
     I=sub(I1,{y=>x,x=>y});
   ); 
) else (
   I=I1;
);
K:=coefficientRing R0;
R1:=K[x,y];
Ia:=substitute(substitute(I,z=>1),R1);
Iinf:=saturate substitute(substitute(I+ideal jacobian I,z=>0),R1);
tstinf:=false;
if degree Iinf > 0 then (
  use R0;
  --error("The curve has singularities at infinity");
  cs:=findCoordinateChange(I);
  I=sub(I,cs#0);
  Ia=substitute(substitute(I,z=>1),R1);
  tstinf=true;
);
QR:=frac R1;
--print Ia;
ib:=sub(integralBasis(Ia),QR);
geometricGenus(I1,ib))



geometricGenus(Ideal,Matrix):=(I1,ib)->(
n:=(degree (gens I1)_(0,0))#0;
gen:=(n-1)*(n-2)/2-sum(apply((entries matden(ib))#0,j->(degree j)#0));
if denominator(gen)!=1 then print("ERROR");
use ring I1;
numerator(gen));
--geomgenus(I,ib)



-- compute the trace matrix
traceMatrix=method()

traceMatrix(Ideal,Matrix):=(I1,ib)->(
R:=ring numerator 1_(ring ib);
R0:=ring I1;
I:=substitute(substitute(I1,R0_2=>1),R);
A:=R/I;
K:=coefficientRing R;
Ru:=K[R_1];
QRx:=frac(Ru);
RE:=QRx[R_0];
gl:=substitute(ib,RE);
n:=rank source ib;
-- the coefficient matrix mR of the integral basis
perm:=(entries(matrix({deepSplice({-(n-1)..0})})*-1))#0;
mR:=(transpose(coefficients(gl,Variables=>{R_0}))#1)_perm;
mRI:=(substitute(mR,QRx))^-1;
traceMatrix(ib,mRI,{A,R,RE,QRx}));
--traceMatrix(I,ib)

-- uses precomputed inverse of the triangular coefficient matrix of the integral basis
traceMatrix(Matrix,Matrix,List):=(ib,mRI,tracerings)->(
sy:=0;
lix:={};
liy:={};
sx:=0;
trv:=0;
ver:=0;
n:=rank source ib;
while sy<n do (
  lix={};
  sx=0;
  while sx<n do (
     if sx<sy then trv=0;
     if sx==sy then trv=1/2*fieldTrace({sx,sy},ib,mRI,tracerings);
     if sx>sy then trv=fieldTrace({sx,sy},ib,mRI,tracerings);
     lix=append(lix,trv);
     if ver>1 then print(sx,sy,trv);
  sx=sx+1);
  liy=append(liy,lix);
sy=sy+1);
-- symmetrize
trmw:=matrix(liy);
trmw=trmw+transpose(trmw);
trmw);


-- do PA=LR
-- pivoting is done using the minimal argument of pivotfunction
-- returns the matrix trmw containing L and R and the permutation matrix prmt
LRdecomposition=method()
LRdecomposition(Matrix,Function):=(trmw,pivotfunction)->(
n:=rank source trmw;
prmt:=matrix {deepSplice({0..(n-1)})};
k:=0;
r:=0;
ver:=0;
while k<(n-1) do (
   r=pivot(k,k,trmw,pivotfunction);
   if ver>1 then print k;
   if r!=k then (
        trmw=permute({k,r},trmw);
        kl:=k;
        rl:=r;
        perm1:=join(0..(min({kl,rl})-1),(max {kl,rl})..(max {kl,rl}),(min({kl,rl})+1)..(max({kl,rl})-1),(min {kl,rl})..(min {kl,rl}),(max({kl,rl})+1)..(n-1));
        prmt=prmt_(deepSplice({perm1}))
    );
   trmw=gaussStep(k,trmw);
k=k+1);
prmt=(entries prmt)#0;
(prmt,trmw))


-- forward substitution
forwardSubstitution=method()
forwardSubstitution(Matrix,Matrix):=(trmw,ibr)->(
n:=rank source trmw;
rst:={ibr_(0,0)};
i:=1;
j:=0;
rstt:=0;
while i<n do (
  rstt=ibr_(i,0);
  j=0;
  while j<i do (
     --if ver>0 then print i;
     rstt=rstt-trmw_(i,j)*rst#j;
  j=j+1);
  rst=append(rst,rstt);
i=i+1);
ze:=matrix {rst};
transpose(ze));

-- backward substitution
backwardSubstitution=method()
backwardSubstitution(Matrix,Matrix):=(trmw,rhs)->(
n:=rank source trmw;
ze:=transpose rhs;
rst:={ze_(0,n-1)/trmw_(n-1,n-1)};
i:=n-2;
rstt:=0;
while i>-1 do (
  rstt=ze_(0,i);
  j:=i+1;
  while j<n do (
     rstt=rstt-trmw_(i,j)*rst#(n-1-j);
  j=j+1);
  rstt=rstt/trmw_(i,i);
  rst=append(rst,rstt);
i=i-1);
transpose((matrix {rst})_(deepSplice {apply(0..(n-1),i->n-1-i)})));


-- key function: adjointIdeal(Ideal,Matrix)
--
-- Computing the adjoint ideal of an algebraic curve
-- 1st argument: The homogeneous ideal I of an irreducible plane algebraic curve C
--               with the following properties:
--               Denote the variables of R=ring(I) by v,u,z.
--               All singularities of C have to lie in the chart z<>0
--               and the generator of I has to be monic in v
-- 2nd argument: An integral basis of the closure of K[C] in K(C)
--               given as a row matrix of length n=degree(f) of elements in 
--               K(u)[v] inside frac(R) where the i-th entry is of degree
--               i in v for i=0..n-1
--
-- can be used with optional
-- 3rd argument: 0 for no additional output (default)
--               1 to print additional data like geometric genus of C, trace matrix,...

adjointIdeal=method()

adjointIdeal(Ideal):=(I1)->(
if dim I1 != 2 then error("Not a curve");
if codim I1 != 1 or rank source vars ring I1 !=3 then error("Not a plane curve");
R0:=ring I1;
x:=R0_0;
y:=R0_1;
z:=R0_2;
d:=degree I1;
tstmonic:=true;
switchvars:=false;
f:=I1_(0);
if contract(x^d,f)==0 then (
   if contract(y^d,f)==0 then (
     tstmonic=false;
     I:=sub(I1,{y=>x+y});     
   ) else (
     switchvars=true;
     I=sub(I1,{y=>x,x=>y});
   ); 
) else (
   I=I1;
);
K:=coefficientRing R0;
R1:=K[x,y];
Ia:=substitute(substitute(I,z=>1),R1);
--print integralBasis(Ia);
Iinf:=saturate substitute(substitute(I+ideal jacobian I,z=>0),R1);
tstinf:=false;
if degree Iinf > 0 then (
  use R0;
  --error("The curve has singularities at infinity");
  cs:=findCoordinateChange(I);
  I=sub(I,cs#0);
  Ia=substitute(substitute(I,z=>1),R1);
  tstinf=true;
);
QR:=frac R1;
--cf:=contract(x^d,I_(0));
--I=ideal(sub(1/cf,ring I)*I_(0));
--Ia=ideal(sub(1/cf,ring Ia)*Ia_(0));
ib:=sub(integralBasis(Ia),QR);
adj:=adjointIdeal(I,ib,0);
if tstinf==true then (
   adj=sub(adj,cs#1);
);
if tstmonic==false then (
   adj=sub(adj,{y=>-x+y});
);
if switchvars==true then (
   adj=sub(adj,{y=>x,x=>y});
);
use R0;
adj);

findCoordinateChange=method()
findCoordinateChange(Ideal):=(I)->(
R:=ring I;
x:=R_0;
y:=R_1;
z:=R_2;
h:=1;
d:=degree I;
tst:=false;
J:=ideal jacobian I;
while tst==false do (
   c:=1;
   while c<=h and tst==false do (
     b:=0;
     while b<=h-c and tst==false do (
       a:=h-b-c;
       l:=a*x+b*y+c*z;
       if saturate(ideal(l)+J)==ideal(1_R) then (
         slist:={z=>z-a/c*x-b/c*y};
         slistback:={z=>z+a/c*x+b/c*y};
         f:=sub(I_(0),slist);--print f;
         if contract(x^d,f)!=0 then tst=true;
       );
       --print l;
     b=b+1);
   c=c+1);
   h=h+1;
   if d>1000 then error("Did not find appropriate coordinate change, please take care that the curve does not have singularities at infinity");
);
{slist,slistback})
--findCoordinateChange I

-*

     installPackage "AdjointIdeal"
     K=QQ
     R=K[v,u,z]
     I=ideal(v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2)
     J=adjointIdeal(I)


     K=QQ
     R=K[v,u,z]
     I=ideal(z^3-v^2*u)
     J=adjointIdeal(I)
*-

adjointIdeal(Ideal,Matrix):=(I1,ib)->(
adjointIdeal(I1,ib,0));

adjointIdeal(Ideal,Matrix,ZZ):=(I1,ib,ver)->(
QR:=ring ib;
R0:=ring I1;
z:=(vars ring I1)_(0,-1+rank source vars ring I1);
K:=coefficientRing R0;
R:=K[R0_0,R0_1];
I:=substitute(substitute(I1,R0_2=>1),R);
n:=(degree I_0)#0;
A:=R/I;
-- compute the genus
gen:=(n-1)*(n-2)/2-sum(apply((entries matden(ib))#0,j->(degree j)#0));
if ver>0 then (print("genus = "|toString gen);print(" "));
prmt:=0;
--
trmw:=traceMatrix(I1,ib);
--return trmw;
if ver>0 then (print("Trace matrix = ",trmw);print(" "););
--
-- differentiation of f by the first variable of R
dfdv:=substitute((diff(vars R,gens I))_(0,0),QR);
if ver>1 then (print("df/dv = ",dfdv);print(" "););
--
-- compute P * (-) = L * R decomposition of trmw
-- the permutation will be stored in prmt, the matrix in Pm
(prmt,trmw)=LRdecomposition(trmw,vanishingOrder);
if ver>1 then print(extractLeftLower(trmw),extractRightUpper(trmw),prmt);
Pm:=transpose (id_(QR^n))_prmt;
--
ibr:=Pm*(transpose rmodA(dfdv*ib,R,A));
if ver>0 then (print("ib*df/dv= ",transpose(ibr));print(" "););
trmw=substitute(trmw,QR);
--
ze:=forwardSubstitution(trmw,ibr);
if ver>0 then (print("forward  ",ze);print(" "););
--
rst:=backwardSubstitution(trmw,ze);
if ver>0 then (print("backward ",rst);print(" "));
--
-- minimal generators adjoint ideal
adjb:=substitute(rst,R);
adj:=mingens ideal adjb;
-- homogenize again
Rx:=ring I1;
interadj:=homogenize(substitute(adj,Rx),z);
ideal mingens saturate ideal interadj)


-- print docExample
-- geometricGenus
-- adjointIdeal

-------------------------------------------------------------------------------

-*
Copyright (C) [2009] [Janko Boehm]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-




beginDocumentation()



doc ///
  Key
    AdjointIdeal
  Headline
    Adjoint ideal of a plane curve and related computations
  Description
   Text

     {\bf Overview:}

     {\it AdjointIdeal} is a package to compute the adjoint ideal and the geometric genus of projective plane curves.
     
     This is used in particular in the case of genus 0 in the package {\it Parametrization}
     to map any singular plane rational curve to a (smooth) rational normal curve.

     Suppose (note, that the implementation in @TO adjointIdeal@ does not require this) the curve C is given 
     by {f(x,y,z) = 0} and C does not have singularities
     at infinity {z = 0} and the point (1:0:0) is not on C.
     Note that these conditions can always be met by a projective automorphism.

     Considering the affine curve in z!=0 we take y as transcendental and x as algebraic
     and compute an integral basis in CC(y)[x] of the integral closure of CC[y] in CC(x,y) using 
     the algorithm from
    
     Mark van Hoeij: An algorithm for computing an integral basis in an algebraic function field,
     @HREF"http://www.math.fsu.edu/~hoeij/papers/comments/jsc1994.html"@,
     Journal of Symbolic Computation,
     Volume 18 ,  Issue 4  (October 1994),
     Pages: 353 - 363.

     So far we call Maple using the MapleInterface package, but once the functionality will be
     available natively in Macaulay2 this will no longer be necessary.

     From this data the adjoint ideal can be obtained by linear algebra.

     The package is work in progress, so there will be future improvements and more testing is necessary.


     For more theoretical details see 
     J. Boehm: Rational parametrization of rational curves,
     @HREF"https://agag-jboehm.math.rptu.de/~boehm/diplom%20janko%20boehm.pdf"@.


     {\bf Key user functions:}

     @TO adjointIdeal@  --  compute the adjoint ideal

     @TO geometricGenus@  -- compute the geometric genus of a plane curve

    {\bf Setup:}

    This package uses the package {\it MapleInterface}, so install this first.

    Place the file AdjointIdeal.m2 somewhere into the M2 search path (type path to see it) and install the package by doing

    installPackage("AdjointIdeal")
///


doc ///
  Key
    adjointIdeal
    (adjointIdeal,Ideal)
    (adjointIdeal,Ideal,Matrix)
    (adjointIdeal,Ideal,Matrix,ZZ)
  Headline
    Compute the adjoint ideal of a plane curve
  Usage
    adjointIdeal(I)
    adjointIdeal(I,ib)
  Inputs
    I:Ideal
        the homogeneous ideal I of an irreducible plane algebraic curve C over K
    ib:Matrix
        containing an integral basis of the closure of L[C] in L(C) where L is the algebraic closure of K
        given as a row matrix of length n=degree(I).
  Outputs
    :Ideal
        the adjoint ideal in the ring I
  Description
   Text
     Computes the adjoint ideal of an irreducible plane curve.


     If ib is specified (and only then) we assume that I has the following properties:

     Denote the variables of R=ring(I) by v,u,z. 
     All singularities of C have to lie in the chart z!=0 and the curve should not contain the point (1:0:0).

     Furthermore we assume that ib has the following properties:
     The entries are in K(u)[v] inside frac(R) where the i-th entry is of degree $i$ in v for i=0..n-1.
     Note that this always can be achieved.

     If ib is not specified the function takes care of these conditions
     by applying an appropriate projective automorphism before doing the
     computation and afterwards applying its inverse. The algorithm will try to choose
     an automorphism as simple as possible, however note that this process may
     destroy sparseness and harm the performance.

     A rational curve with three double points:

   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2)
     J=adjointIdeal(I)
   Text
     
     Same example but specifying the integral basis:

   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2)
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix {{1, v, (-1+v^2)/(-1+u), 1/(-1+u)/u*v^3+(-2+u)/(-1+u)/u*v}};
     J=adjointIdeal(I,ib)
   Text

     The Cusp:
   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^3-u^2*z)
     J=adjointIdeal(I)
   Text
     
     Same example but specifying the integral basis:

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^3-u^2*z)
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix({{1,v,v^2/u}});
     J=adjointIdeal(I,ib)
   Text

     A curve of genus 4:
   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^6+(7/5)*v^2*u^4+(6/5)*u^6+(21/5)*v^2*u^3*z+(12/5)*u^5*z+(21/5)*v^2*u^2*z^2+(6/5)*u^4*z^2+(7/5)*v^2*u*z^3)
     J=adjointIdeal(I)
   Text
     
     Same example but specifying the integral basis:

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^6+(7/5)*v^2*u^4+(6/5)*u^6+(21/5)*v^2*u^3*z+(12/5)*u^5*z+(21/5)*v^2*u^2*z^2+(6/5)*u^4*z^2+(7/5)*v^2*u*z^3);
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix({{1,v,v^2,v^3/(u+1),1/u/(u+1)*v^4,1/u^2/(u+1)*v^5-7/5*(u-1)/u*v}});
     J=adjointIdeal(I,ib)
   Text
   
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^8-u^3*(z+u)^5);
     Ruv=K[v,u];
     QR=frac(Ruv);
     ib=matrix({{1,v,v^2/(1+u),v^3/u/(1+u),v^4/u/(1+u)^2,v^5/u/(1+u)^3,v^6/u^2/(1+u)^3,v^7/u^2/(1+u)^4}});
     J=adjointIdeal(I,ib)
     apply((entries gens J)#0,factor)
  Caveat
     The function so far does not cache the integral basis computation.
  SeeAlso
     geometricGenus
     integralClosure
///



doc ///
  Key
    geometricGenus
    (geometricGenus,Ideal)
    (geometricGenus,Ideal,Matrix)
  Headline
    Geometric genus of a plane curve
  Usage
    geometricGenus(I)
    geometricGenus(I,ib)
  Inputs
    I:Ideal
        the homogeneous ideal I of an irreducible plane algebraic curve C over K
    ib:Matrix
        containing an integral basis of the closure of L[C] in L(C) where L is the algebraic closure of K
        given as a row matrix of length n=degree(I).
  Outputs
    :ZZ
        the geometric genus of C
  Description
   Text
     Computes the geometric genus of a plane curve.


     If ib is specified (and only then) we assume that I has the following properties:

     Denote the variables of R=ring(I) by v,u,z. 
     All singularities of C have to lie in the chart z!=0 and the curve should not contain the point (1:0:0).

     Furthermore we assume that ib has the following properties:
     The entries are in K(u)[v] inside frac(R) where the i-th entry is of degree $i$ in v for i=0..n-1.
     Note that this always can be achieved.

     If ib is not specified the function takes care of these conditions
     by applying an appropriate projective automorphism before doing the
     computation and afterwards applying its inverse. The algorithm will try to choose
     an automorphism as simple as possible, however note that this process may
     destroy sparseness and harm the performance.

     A rational curve with three double points:

   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2)
     geometricGenus(I)
   Text
     
     Same example but specifying the integral basis:

   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2)
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix {{1, v, (-1+v^2)/(-1+u), 1/(-1+u)/u*v^3+(-2+u)/(-1+u)/u*v}};
     geometricGenus(I,ib)
   Text

     The Cusp:
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^3-u^2*z)
     geometricGenus(I)
   Text
     
     Same example but specifying the integral basis:

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^3-u^2*z)
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix({{1,v,v^2/u}});
     geometricGenus(I,ib)
   Text

     A curve of genus 4:
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^6+(7/5)*v^2*u^4+(6/5)*u^6+(21/5)*v^2*u^3*z+(12/5)*u^5*z+(21/5)*v^2*u^2*z^2+(6/5)*u^4*z^2+(7/5)*v^2*u*z^3);
     geometricGenus(I)
   Text
     
     Same example but specifying the integral basis:

   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^6+(7/5)*v^2*u^4+(6/5)*u^6+(21/5)*v^2*u^3*z+(12/5)*u^5*z+(21/5)*v^2*u^2*z^2+(6/5)*u^4*z^2+(7/5)*v^2*u*z^3);
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix({{1,v,v^2,v^3/(u+1),1/u/(u+1)*v^4,1/u^2/(u+1)*v^5-7/5*(u-1)/u*v}});
     geometricGenus(I,ib)
  Caveat
     The function so far does not cache the integral basis computation.
  SeeAlso
     adjointIdeal
     integralClosure
///



doc ///
  Key
    LRdecomposition
    (LRdecomposition,Matrix,Function)
  Headline
    LR decomposition
  Usage
    LRdecomposition(A,pivotfunction)
  Inputs
    A:Matrix
        square and invertible.
    pivotfunction:Function
        used for pivoting.
  Outputs
    :Sequence
        of a list with a permutation of the rows of A corresponding to P,
        and a matrix containing L and R.
  Description
   Text
     Does the PA=LR decomposition on the matrix A, pivoting by the minimal value of pivotfunction.
     L can be extracted by the function extractLeftLower,
     R by extractRightUpper.
     This has been implemented with fields of rational functions in mind.
   Example
     A=random(QQ^3,QQ^3)
     (perm,LR)=LRdecomposition(A,j->-j);
     perm
     P=transpose (id_(QQ^3))_perm
     R=extractRightUpper(LR)
     L=extractLeftLower(LR)
     L*R==P*A
  SeeAlso
     extractRightUpper
     extractLeftLower
///


doc ///
  Key
    extractRightUpper
    (extractRightUpper,Matrix)
  Headline
    Extract R from the LR decomposition result.
  Usage
    extractRightUpper(A)
  Inputs
    A:Matrix
        square.
  Outputs
    :Matrix
        right upper triangular.
  Description
   Text
     Returns a right upper triangular matrix formed by the corresponding entries of A.
   Example
     A=random(QQ^3,QQ^3)
     (perm,LR)=LRdecomposition(A,j->-j);
     LR
     P=transpose (id_(QQ^3))_perm
     R=extractRightUpper(LR)
     L=extractLeftLower(LR)
     L*R==P*A
  SeeAlso
     LRdecomposition
     extractLeftLower
///


doc ///
  Key
    extractLeftLower
    (extractLeftLower,Matrix)
  Headline
    Extract L from the LR decomposition result.
  Usage
    extractLeftLower(A)
  Inputs
    A:Matrix
        square.
  Outputs
    :Matrix
        left lower triangular with 1 on the diagonal.
  Description
   Text
     Returns a left lower triangular matrix formed by the corresponding entries of A and with 1 on the diagonal.
   Example
     A=random(QQ^3,QQ^3)
     (perm,LR)=LRdecomposition(A,j->-j);
     LR
     P=transpose (id_(QQ^3))_perm
     R=extractRightUpper(LR)
     L=extractLeftLower(LR)
     L*R==P*A
  SeeAlso
     LRdecomposition
     extractRightUpper
///



doc ///
  Key
    forwardSubstitution
    (forwardSubstitution,Matrix,Matrix)
  Headline
    Forward substitution.
  Usage
    forwardSubstitution(L,b)
  Inputs
    L:Matrix
        left lower triangular with 1 on the diagonal.
    b:Matrix
        one column, same number of rows as L.
  Outputs
    :Matrix
        Solution of the system L*x=b.
  Description
   Text
     Solves the system L*x=b via forward substitution.
   Example
     A=random(QQ^3,QQ^3)
     (perm,LR)=LRdecomposition(A,j->-j);
     LR
     P=transpose (id_(QQ^3))_perm
     R=extractRightUpper(LR)
     L=extractLeftLower(LR)
     L*R==P*A
     b=random(QQ^3,QQ^1);
     y=forwardSubstitution(LR,P*b)
     x=backwardSubstitution(LR,y)
     inverse(A)*b==x
  SeeAlso
     LRdecomposition
     extractRightUpper
     extractLeftLower
     backwardSubstitution
///





doc ///
  Key
    backwardSubstitution
    (backwardSubstitution,Matrix,Matrix)
  Headline
    Backward substitution.
  Usage
    backwardSubstitution(R,b)
  Inputs
    R:Matrix
        invertible, right upper triangular.
    b:Matrix
        one column, same number of rows as R.
  Outputs
    :Matrix
        Solution of the system R*x=b.
  Description
   Text
     Solves the system R*x=b via backward substitution.
   Example
     A=random(QQ^3,QQ^3)
     (perm,LR)=LRdecomposition(A,j->-j);
     LR
     P=transpose (id_(QQ^3))_perm
     R=extractRightUpper(LR)
     L=extractLeftLower(LR)
     L*R==P*A
     b=random(QQ^3,QQ^1);
     y=forwardSubstitution(LR,P*b)
     x=backwardSubstitution(LR,y)
     inverse(A)*b==x
  SeeAlso
     LRdecomposition
     extractRightUpper
     extractLeftLower
     forwardSubstitution
///



doc ///
  Key
    traceMatrix
    (traceMatrix,Ideal,Matrix)
    (traceMatrix,Matrix,Matrix,List)
  Headline
    Compute trace matrix.
  Usage
    traceMatrix(I,ib)
  Inputs
    I:Ideal
        the homogeneous ideal I of an irreducible plane algebraic curve C over K
    ib:Matrix
        containing an integral basis of the closure of L[C] in L(C) where L is the algebraic closure of K
        given as a row matrix of length n=degree(I).
  Outputs
    :Matrix
        the trace matrix of the integral basis ib.
  Description
   Text
     Computes the trace matrix for the integral basis ib, i.e., the matrix containing the traces of the elements ib_(0,i)*ib_(0,j).

     We assume that I has the following properties:

     Denote the variables of R=ring(I) by v,u,z. 
     All singularities of C have to lie in the chart z!=0 and the curve should not contain (1:0:0).


     We assume that ib has the following properties:
     The entries are in K(u)[v] inside frac(R) where the i-th entry is of degree $i$ in v for i=0..n-1.
     Note that this always can be achieved.

     A rational curve with three double points:
   Example
     K=QQ
     R=K[v,u,z]
     I=ideal(v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2)
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix {{1, v, (-1+v^2)/(-1+u), 1/(-1+u)/u*v^3+(-2+u)/(-1+u)/u*v}};
     traceMatrix(I,ib)
   Text

     The Cusp:
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^3-u^2*z)
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix({{1,v,v^2/u}});
     traceMatrix(I,ib)
   Text

     A curve of genus 4:
   Example
     K=QQ;
     R=K[v,u,z];
     I=ideal(v^6+(7/5)*v^2*u^4+(6/5)*u^6+(21/5)*v^2*u^3*z+(12/5)*u^5*z+(21/5)*v^2*u^2*z^2+(6/5)*u^4*z^2+(7/5)*v^2*u*z^3);
     Rvu=K[v,u];
     QR=frac(Rvu);
     ib=matrix({{1,v,v^2,v^3/(u+1),1/u/(u+1)*v^4,1/u^2/(u+1)*v^5-7/5*(u-1)/u*v}});
     traceMatrix(I,ib)
  SeeAlso
     geometricGenus
     adjointIdeal
///


------------------------------------------------------------------------


TEST ///
A=random(QQ^3,QQ^3);
(perm,LR)=LRdecomposition(A,j->-j);
P=transpose (id_(QQ^3))_perm;
b=random(QQ^3,QQ^1);
y=forwardSubstitution(LR,P*b)
x=backwardSubstitution(LR,y);
assert(inverse(A)*b==x);
///

TEST ///
A=random(QQ^3,QQ^3);
(perm,LR)=LRdecomposition(A,j->-j);
P=transpose (id_(QQ^3))_perm;
R=extractRightUpper(LR);
L=extractLeftLower(LR);
assert(L*R==P*A);
///

TEST ///
K=QQ
R=K[v,u,z]
I=ideal matrix {{v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2}}
Ruv=K[v,u]
QR=frac(Ruv)
ib=matrix {{1, v, (-1+v^2)/(-1+u), 1/(-1+u)/u*v^3+(-2+u)/(-1+u)/u*v}}
ver=1;
J=adjointIdeal(I,ib,ver);
use R;
assert(J==ideal(u^2-u*z, v*u-v*z, v^2-u*z))
///


TEST ///
K=QQ;
R=K[v,u,z];
I=ideal(v^3-u^2*z);
Ruv=K[v,u];
QR=frac(Ruv);
ib=matrix({{1,v,v^2/u}});
ver=1;
J=adjointIdeal(I,ib,ver)
use R;
assert(J==ideal(u,v))
///

TEST ///
K=QQ;
R=K[v,u,z];
I=ideal(5*v^6+7*v^2*u^4+6*u^6+21*v^2*u^3+12*u^5+21*v^2*u^2+6*u^4+7*v^2*u);
Rvu=K[v,u];
QR=frac(Rvu);
ib=matrix({{1,v,v^2,v^3/(u+1),1/u/(u+1)*v^4,1/u^2/(u+1)*v^5-7/5*(u-1)/u*v}});
assert(4==geometricGenus(I,ib))
///

TEST ///
K=QQ;
R=K[v,u,z];
I=ideal(v^8-u^3*(z+u)^5);
Ruv=K[v,u];
QR=frac(Ruv);
ib=matrix({{1,v,v^2/(1+u),v^3/u/(1+u),v^4/u/(1+u)^2,v^5/u/(1+u)^3,v^6/u^2/(1+u)^3,v^7/u^2/(1+u)^4}});
J=adjointIdeal(I,ib)
use R;
assert(J==ideal(u^6+4*u^5*z+6*u^4*z^2+4*u^3*z^3+u^2*z^4,v*u^5+3*v*u^4*z+3*v*u^3*z^2+v*u^2*z^3,v^2*u^4+3*v^2*u^3*z+3*v^2*u^2*z^2+v^2*u*z^3,v^3*u^3+2*v^3*u^2*z+v^3*u*z^2,v^4*u^2+v^4*u*z,v^5*u+v^5*z,v^6))
///

end
