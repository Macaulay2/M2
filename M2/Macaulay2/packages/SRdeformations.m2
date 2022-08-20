-- -*- coding: utf-8 -*-
newPackage(
	"SRdeformations",
    	Version => "0.51", 
    	Date => "July 12, 2010",
    	Authors => {{Name => "Janko Boehm", 
		  Email => "boehm@mathematik.uni-kl.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/jb/"}
                  },
    	Headline => "deformations of Stanley-Reisner rings and related computations",
	Keywords => {"Combinatorial Commutative Algebra"},
    	DebuggingMode => false,
     	PackageImports => { "ConvexInterface", "OldPolyhedra" },
        Configuration => {"UseConvex"=>false}
        )

-- For information see documentation key "SRdeformations" below.

-*
Before using "ConvexInterface" via giving the option 

loadPackage("SRdeformations",Configuration=>{"UseConvex"=>true})

or editing the file

init-SRdeformations.m2

please set up "ConvexInterface" first.

"ConvexInterface" relies on "MapleInterface".

If you don't have Maple/Convex or haven't set up "ConvexInterface"
stay with the standard option UseConvex=>false.

Then automatically the package "OldPolyhedra" is used,
but that is several magnitudes slower compared to maple/convex.

"OldPolyhedra" relies on the package "FourierMotzkin".

These packages are only used for computing convex hulls and lattice points thereof.
Methods of "SRdeformations" relying on this package are so far:

convHull
globalSections

*-



-- the commands available to the user

export {"cokerElement","iszero","vectorToMonomial","preImage","FirstOrderDeformation",
"simplexRing","bigTorusDegree","numeratorMonomial","denominatorMonomial","grading","isNonzero","isTrivial",
"relationsCoefficients","gensSource","parameters","firstOrderDeformation",
"trivialDeformations",
"addCokerGrading","raysPPn","laurent","totalSpace","toHom","newEmptyComplex",
"Face","face","vert","simplexDim","ofComplex","dualFace","isSubface","faceToMonomial","Complex","isEquidimensional",
"noBoundary","complementComplex","dualComplex","polytopalFacets","embedded",
"eulerCharacteristic","complex","complexFromFacets","addFaceDataToComplex","addFacetDataToComplex",
"facets","coordinates","isPolytope","variables","edim","fvector","fc",
"intersectFaces","embeddingComplex",
"simplex","computeFaces","verticesDualPolytope","dualGrading",
"boundaryCyclicPolytope","fullCyclicPolytope",
"dualize",
"boundaryOfPolytope",
"idealToComplex","minimalNonFaces","complexToIdeal",
"CoComplex","coComplex","idealToCoComplex","coComplexToIdeal",
"link","closedStar",
"possibleDenominators","deformationsFace",
"globalSections","joinVectors",
"convHull","deform","PT1","tropDef","mirrorSphere","file","saveDeformations","loadDeformations",
"hull","isSimp"}

---------------------------------------------------------------------------------
-- deformations of Stanley-Reisner rings
---------------------------------------------------------------------------------
-- basic deformations stuff
-----------------------------------------------------------


--------------------------------------------
-- some functions to make life easier
-- >> export
cokerElement=method()
cokerElement(Vector,Matrix) := (v,A)->(
f:=inducedMap(coker A,target A);
f*v
)

cokerElement(Matrix,Matrix) := (v,A)->(
f:=inducedMap(coker A,target A);
f*v
)


iszero=method()
iszero(Thing):=v->(v==0_(class v))

-- why does isZero not work ?

--A= matrix {{-1, -1, -1}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
--a1=cokerElement(vector {1,1,0,0},A)
--a2=cokerElement(vector {1,-1,0,0},A)
--a2==(0_(class a2))
--iszero a1
--iszero a2

numer=method()
numer(Vector):=(m)->(
R:=ring class m;
n:=rank class m;
q:=0;
num:={};
while q<n do (
  if m_q>0_R then (
     num=append(num,m_q);
  ) else (
     num=append(num,0_R);
  );
q=q+1);
vector num)
--numer vector {1,0,-1}

denom=method()
denom(Vector):=(m)->(
R:=ring class m;
n:=rank class m;
q:=0;
den:={};
while q<n do (
  if m_q<0_R then (
     den=append(den,-m_q);
  ) else (
     den=append(den,0_R);
  );
q=q+1);
vector den)
--denom vector {1,0,-1}

tolist=method()
tolist(Vector):=(v)->(
(entries transpose (toList v)#0)#0)
--tolist vector {1,-1,0}

-- >> export
vectorToMonomial=method()
vectorToMonomial(Vector,PolynomialRing):=(L,R)->(
q:=0;
m:=1_R;
while q<rank source vars R do (
   m=m*(R_q)^(L_q);
q=q+1);
m)
-- R=QQ[x_0..x_4]
-- m=vector {1,2,1,0,0}
-- vectorToMonomial(m,R)

-- >> export
listToLaurentMonomial=method()
listToLaurentMonomial(List,PolynomialRing):=(L,R)->(
K:=frac R;
q:=0;
m:=1_R;
while q<rank source vars R do (
   m=m*(sub(R_q,K))^(L_q);
q=q+1);
m)
-- R=QQ[x_0..x_4]
-- m=vector {-1,2,-1,0,0}
-- listToLaurentMonomial(m,R)


-- big torus invariant first order deformation
-- >> export
FirstOrderDeformation = new Type of MutableHashTable
FirstOrderDeformation.synonym = "first order deformation"
FirstOrderDeformation#{Standard,AfterPrint} = m -> (
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << "first order deformation space of dimension "|dim(m)
      << endl;)
-- Laurent monomial representation of 

--net FirstOrderDeformation := (f) -> net(matrix({tolist(f.bigTorusDegree)}))
-- >> document
net FirstOrderDeformation := (f) -> net(listToLaurentMonomial(tolist(f.bigTorusDegree),simplexRing f))


newFirstOrderDeformation=method()
newFirstOrderDeformation(Ideal,Vector) := (I,m)-> (
newFirstOrderDeformation(gens I,m))

trivDef=method()
trivDef(Vector):=(v0)->(
v:=entries v0;
d:=0;
for j from 0 to #v-1 do 
   if v#j<0 then d=d-v#j;(
   if d>1 then return(false); 
);
if d==1 then return(true);
false)

extendParameters=method()
extendParameters(List,ZZ,Matrix):=(rmgidx,n,par)->(
zr:=matrix {toList apply(0..-1+rank source par,j->0)};
c:=0;
local parx;
for j from 0 to n-1 do (
  if member(j,rmgidx)==true then (
     if j==0 then (
       parx=par^{c};
       c=c+1;
     ) else (
       parx=parx||par^{c};
       c=c+1;
     );
  ) else (
     if j==0 then(
       parx=zr;
     ) else (
       parx=parx||zr;
     );
  );
);
parx)

--extendParameters({2,4},5,par)

protect relevantGens

newFirstOrderDeformation(Matrix,Vector) := (mg,m)-> (
  A:=grading ring mg;
  ck:=cokerElement(m,A);
--  if trivDef(m)==false then (
    L:=relevantMonomials(mg,m);
    rmg:=L#0;rmgidx:=L#1;
    rel:=relationsCoefficients(rmg,m);
    par:=mingens ker transpose rel;
    dimdef:=rank source par;
    par=extendParameters(rmgidx,rank source mg,par);
--  ) else (
--    dimdef=1;
--    rel="unknown";
--    par="unknown";
--    relevantGens="unknown";
--  );
  new FirstOrderDeformation from {
     symbol gens => mg,
     symbol relevantGens => rmg,
     symbol bigTorusDegree => m,
     symbol degree => ck,
     symbol isHomogeneous => iszero ck,
     symbol relationsCoefficients => rel,
     symbol parameters => par,
     symbol dim => dimdef,
     symbol isNonzero => (dimdef>0),
     symbol isTrivial => isTrivial m
  }
)

--     symbol simplexRing => ring mg,
--     symbol target => coker mg,
--     symbol source => image mg,
--     symbol numerator => numer m,
--     symbol denominator => denom m,
--     symbol numeratorMonomial => vectorToMonomial(numer m,ring mg),
--     symbol denominatorMonomial => vectorToMonomial(denom m,ring mg),
--     symbol grading => A,

--     symbol numeratorMonomial => vectorToMonomial(numer m,ring I),
--     symbol denominatorMonomial => vectorToMonomial(denom m,ring I),
--     symbol gensSource => mg,


----------------------------------------------------
-- elementary functions on deformations

FirstOrderDeformation == FirstOrderDeformation := (f1,f2)->(
if ((f1.bigTorusDegree)!=(f2.bigTorusDegree)) then return(false);
if f1.gens==f2.gens then return(true);
if ((ideal f1.gens) == (ideal f2.gens)) then return(true);
false)


-- >> export
simplexRing=method()
simplexRing FirstOrderDeformation := f -> ring f.gens
--simplexRing f
-- >> document
target FirstOrderDeformation := f -> coker(f.gens)
--target f
-- >> document
source FirstOrderDeformation := f -> image(f.gens)
--source f
-- >> export
bigTorusDegree=method()
bigTorusDegree FirstOrderDeformation := f -> f.bigTorusDegree
--bigTorusDegree f
-- >> document
numerator FirstOrderDeformation := f -> numer bigTorusDegree f
-- >> document
denominator FirstOrderDeformation := f -> denom bigTorusDegree f
-- >> export
numeratorMonomial=method()
numeratorMonomial FirstOrderDeformation := f -> vectorToMonomial(numerator f,simplexRing f)
-- >> export
denominatorMonomial=method()
denominatorMonomial FirstOrderDeformation := f -> vectorToMonomial(denominator f,simplexRing f)
-- >> document
degree FirstOrderDeformation := f -> f.degree
-- >> export
grading=method()
grading FirstOrderDeformation := f -> (simplexRing f).grading
-- >> document
isHomogeneous FirstOrderDeformation := f -> f.isHomogeneous
-- >> document
dim FirstOrderDeformation := f -> f.dim
-- >> export
isNonzero=method()
isNonzero(FirstOrderDeformation):=(f)->(
f.isNonzero)
-- check whether a deformation is trivial
-- >> export
isTrivial=method()
isTrivial(FirstOrderDeformation):=(f)->(
1==sum tolist denominator f)
isTrivial(Vector):=(m)->(1==sum tolist denom m)
--m=vector {-1,0,1,0,0}
--f=firstOrderDeformation(I,m)
--isTrivial f


------------------------------------------------------------------
-- relevant monomials

relevantMonomials=method()
relevantMonomials(Matrix,Vector):=(M,m)->(
I:=ideal M;
idx:={};
L:={};
q:=0;
r:=rank source M;
Lm:=laurent(m,ring M);
while q<r do (
  mtst:=M_(0,q)*Lm;
  --print(mtst,numerator mtst);
  if ideal(numerator mtst)+I!=I then (
      L=append(L,M_(0,q));
      idx=append(idx,q);
  );
q=q+1);
{matrix{L},idx});
--rmg=(relevantMonomials(mg,v))#0

-*
rmg=relevantMonomials(mg,v)
relationsCoefficientsSyzygies(rmg,v)
*-
-------------------------------------------------------------------
-- find the deformations in the m-graded part of deformation space
-- relations coming from denominator not dividing the monomial
relationsCoefficientsDivisible=method()
relationsCoefficientsDivisible(Matrix,Vector):=(M,m)->(
q:=0;
r:=rank source M;
R:=ring M;
E:=id_(ZZ^r);
rel:=E_{0}-E_{0};
while q<r do (
  if mapByLaurent(m,M_(0,q))==0_R then rel=rel|E_{q};
q=q+1);
rel);
-- relationsCoefficientsDivisible(mingens I,m)


-- relations coming from lifting syzygies
relationsCoefficientsSyzygies=method()
relationsCoefficientsSyzygies(Matrix,Vector):=(M,m)->(
r:=rank source M;
R:=ring M;
E:=id_(ZZ^r);
rel:=E_{0}-E_{0};
I:=ideal M;
mtst:=symbol mtst;
q:=0;
while q<r do (
  qq:=q+1;
  while qq<r do (
    mtst=lcm(M_(0,q),M_(0,qq))*laurent(m,R);
    if denominator(mtst)==1 then
       --print(q,qq,M_(0,q),M_(0,qq),mtst,ideal(numerator mtst)+I!=I);
       --print(numerator mtst);
       if ideal(numerator mtst)+I!=I then (
--          print(q,qq,M_(0,q),M_(0,qq),mtst);
          rel=rel|(E_{q}-E_{qq});
       );
  qq=qq+1);
q=q+1);
rel);
--relationsCoefficientsSyzygies(mg,v)

--m=vector {-1,-1,0,2,0}
--relationsCoefficientsDivisible(mingens I,m)
--relationsCoefficientsSyzygies(mingens I,m)
--relationsCoefficients(mingens I,m)
--f=firstOrderDeformation(I,m)

-- all relations
-- >> export
relationsCoefficients=method()
relationsCoefficients(FirstOrderDeformation):=(f)->f.relationsCoefficients
gensSource=method()
gensSource(FirstOrderDeformation):=(f)-> f.gens
relationsCoefficients(Matrix,Vector):=(M,m)->(
rel:=relationsCoefficientsDivisible(M,m)|relationsCoefficientsSyzygies(M,m);
mingens image rel)
--relationsCoefficients(mingens I,vector {-1,1,0,0,0})
--relationsCoefficientsSyzygies(mingens I,vector {-1,1,0,0,0})
--f=firstOrderDeformation(mingens I, vector {-1,1,0,0,0})

-- compute the coefficients of the deformations in the m-graded part
-- >> export
parameters=method()
parameters(FirstOrderDeformation):=(f)->f.parameters

parameters(Matrix,Vector):=(M,m)->(
mingens ker transpose relationsCoefficients(M,m));
--parameters(mingens I,m)

-- the dimension of the m-graded part, is 1 or 0 for manifolds
dimDef=method()
dimDef(Matrix,Vector):=(M,m)->(
rank source parameters(M,m))


------------------------------------------------------------------------
-- generate a first order deformation user functions
-- >> export
firstOrderDeformation=method()
firstOrderDeformation(Ideal,Vector):=(I,m)->(
newFirstOrderDeformation(I,m))
-- >> export
firstOrderDeformation(Matrix,Vector):=(M,m)->(
newFirstOrderDeformation(M,m))
firstOrderDeformation(MonomialIdeal,Vector):=(I,m)->(
newFirstOrderDeformation(ideal I,m))







-------------------------------------------------------------------------
-- add grading data by the cokernel of an integer matrix to a ring 
--
-- if A not specified use the standard rays of projective space fan
-- >> export
addCokerGrading=method()
addCokerGrading(PolynomialRing,Matrix):=(R,A)->(
v:=(entries vars R)#0;
if #v!=rank target A then error("number of rows does not match the number of variables");
R.grading=A)
-- firstOrderDeformation(I,m)
-- >> export
addCokerGrading(PolynomialRing):=(R)->(
n:=(rank source vars R)-1;
A:=raysPPn(n);
addCokerGrading(R,A))

addCokerGrading(PolynomialRing,List):=(R,wt)->(
addCokerGrading(R,raysPPn(wt)))


raysPPn=method()
-- standard rays of weighted projective space
raysPPn(List):=(wt)->(
if (wt#0)!=1 then error("expected first entry to be 1");
if #wt<2 then error("expected weight vector of length at least 2");
n:=-1+#wt;
E:=id_(ZZ^n);
(matrix({{apply(1..n,j->-wt#j)}}))||E)
--raysPPn({1,1,2,2,3})


-- standard rays of projective space
-- >> export
raysPPn(ZZ):=(n)->(
if n<1 then error("expected positive argument");
E:=id_(ZZ^n);
(matrix({{apply(1..n,j->-1)}}))||E)
--raysPPn(5)

-- >> export
grading PolynomialRing:=(R)->(R.grading)

--f=firstOrderDeformation(I,m)

---------------------------------------------------------------
-- total space

-- laurent monomial associated to a deformation
-- >> export
laurent=method()
laurent(FirstOrderDeformation):=(f)->(
listToLaurentMonomial(tolist(f.bigTorusDegree),simplexRing f))
--laurent f

laurent(Vector,PolynomialRing):=(m,R)->(
listToLaurentMonomial(tolist(m),R))
--laurent(m,R)

-- map a monomial by a laurent monomial associated to a deformation
-- >> export
mapByLaurent=method()
mapByLaurent(FirstOrderDeformation,RingElement):=(f,m0)->(
fm0:=m0*laurent(f);
if (degree(denominator(fm0)))#0>0 then return(0_(ring m0));
numerator(fm0))
--mapByLaurent(f,x_0*x_1)
mapByLaurent(Vector,RingElement):=(m,m0)->(
R:=ring m0;
fm0:=m0*laurent(m,R);
if (degree(denominator(fm0)))#0>0 then return(0_(ring m0));
numerator(fm0))
-- mapByLaurent(m,x_0*x_1)

mapByLaurent(FirstOrderDeformation):=(f)->(
apply(f.generators,j->mapByLaurent(f,j)))


-- general
-- >> export
totalSpace=method()
totalSpace(FirstOrderDeformation,PolynomialRing):=(f,T)->(
vT:=vars T;
nt:=rank source vT;
dimf:=dim f;
if nt!=dimf then error("Number of variables of the parameter ring should match the dimension of the deformation space "|dim f);
M:=gens source f;
R:=simplexRing f;
S:=T**R;
pars:=parameters f;
q:=0;
qq:=0;
L:={};
equ:=0_S;
tl:=0_S;
imm:=0_S;
while q<rank source M do (
  tl=0_S;
  imm=sub(mapByLaurent(f,M_(0,q)),S);
  qq=0;
  while qq<rank source pars do (
    tl=tl+sub(vT_(0,qq),S)*pars_(q,qq)*imm;
  qq=qq+1);
  equ=sub(M_(0,q),S)+tl;
  L=append(L,equ);
q=q+1);
ideal L);
--totalSpace({f},QQ[t,s])


-- for several first order deformations
-- >> export
totalSpace(List,PolynomialRing):=(L,T)->(
vT:=vars T;
nt:=rank source vT;
dimf:=sum(apply(L,j->dim j));
--print dimf;
if nt!=dimf then error("Number of variables of the parameter ring should match the dimension of the deformation space "|dimf);
if #(tally apply(L,j->gens source j))!=1 then error("expected all deformations having the same source");
M:=gens source L#0;
R:=simplexRing L#0;
S:=T**R;
pars0:=apply(L,parameters);
pars:=pars0#0;
f:=0;
q:=1;
while q<#pars0 do (pars=pars|pars0#q,q=q+1);
q=0;
qq:=0;
qf:=0;
L1:={};
equ:=0_S;
tl:=0_S;
imm:=0_S;
parcount:=0;
while q<rank source M do (
 qf=0;
 parcount=0;
 tl=0_S;
 equ=0_S;
 while qf<#L do (
  f=L#qf;
  imm=sub(mapByLaurent(f,M_(0,q)),S);
  tl=tl+sub(vT_(0,parcount),S)*sub(pars_(q,parcount),S)*imm;
  parcount=parcount+1;
 qf=qf+1);
 equ=sub(M_(0,q),S)+tl;
 L1=append(L1,equ);
q=q+1);
ideal L1);


--R=QQ[x_0..x_4]
--I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
--f=firstOrderDeformation(mingens I, vector {-1,1,0,0,0})
--f1=firstOrderDeformation(mingens I,vector {-1,-1,0,2,0})
--f2=firstOrderDeformation(mingens I,vector {0,-1,-1,0,2})
--L={f1,f2,f}
--totalSpace(L,QQ[t_0..t_2])



-------------------------------------------------------------
-- homomorphism representation of first order deformation
-- >> export
toHom=method()
toHom(FirstOrderDeformation):=(f)->(
if dim f >1 then error("Not implemented for higher-dimensional graded pieces");
sourcef:=source f;
M:=gens sourcef;
imM:=matrix {apply((entries M)#0,j->mapByLaurent(f,j))};
fhom:=map(coker M,sourcef,cokerElement(imM,M));
fhom)
--fhom=toHom f
--target fhom
--source fhom


---------------------------------------------------------------------------
-*
A=matrix {{-1, -1, -1, -1}, {1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0},{0,0,0,1}}
R=QQ[x_0..x_4]
addCokerGrading(R,A)
--addCokerGrading(R)
I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
m=vector {-1,-1,0,2,0}
--m=vector {-1,0,1,0,0}
f=firstOrderDeformation(I,m)
*-

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-- implementing the structure-type results
----------------------------------------------
-- open star structure of the deformation space



-*
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- deformations of SR pairs

liftDeformation=method()
liftDeformation(FirstOrderDeformation,ideal):=(f,J)->(
I:=ideal source f;
if I+J!=J then error("expected ideal containing the ideal associated to the deformation");
)

---------------------------------------------------------------------------------
-- extend a first order deformation

extendDeformation=method()
extendDeformation(FirstOrderDeformation,PolynomialRing,ZZ):=(f,T,m)->(

)



*-


-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- embedded complexes and corresponding Stanley-Reisner rings
--------------------------------------------------------------

---------------------------------------------------------
-- faces
---------------------

-- kept as simple as possible
-- if we have later a memory/performance issue then use as vertices elements in ZZ
-- use lists instead of sets of monomials to prevent M2 from reordering

-- >> export
Face = new Type of MutableHashTable
Face.synonym = "face"
--net Face := (f)-> net toList vert(f)
-- >> document
net Face := (f)-> (
v:=f.vert;
if #v==0 then return(net({}));
horizontalJoin(apply(v,net)))


Face#{Standard,AfterPrint} = m -> (
  vstr:=symbol vstr;
  if #(m.vert)==0 then vstr="empty face";
  if #(m.vert)==1 then vstr="face with "|#(m.vert)|" vertex";
  if #(m.vert)>1 then vstr="face with "|#(m.vert)|" vertices";
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << vstr
      << endl;)

-- >> export

face = method();
face(List):=(verticesfc)->(new Face from {symbol vert=>verticesfc});
face(Set):=(verticesfc)->(face toList verticesfc);



-- >> export
vert=method()
vert Face := F -> F.vert;

-- >> export
simplexDim=method()
simplexDim Face := F -> -1+#F
--vert F
--simplexDim F

-- >> document
Set == Set := (S1,S2)->(S1===S2)

-- >> document
Face == Face :=(F,G)->((set vert F) == (set vert G))

-*
F=face {x_0,x_1}
G=face {x_1,x_0}
F==G
*-

-- >> document
dim(Face,PolynomialRing):=(F,R)->(
A1:=grading R;
A:=entries A1;
verticesfc:=vert(F);
if #verticesfc==0 then return(-1);
vR:=(entries vars R)#0;
if rank target A1 != #vR then error("number of rows does not match the number of variables");
L:={};
q:=0;
qq:=0;
while q<#verticesfc do (
  qq=0;
  while qq<#vR do (
    if vR#qq==verticesfc#q then L=append(L,prepend(1,A#q));
  qq=qq+1);
q=q+1);
-1+rank matrix L
)
--F=face {x_0,x_2}
--dim(F,R)


dim(Face):=(F)->(
if member(symbol indices,keys F)==true then return(F.indices#0#0);
if member(symbol ofComplex,keys F)==true or #vert(F)>0 then return(dim(F,simplexRing(F)));
error("specify ring with coker grading"))



-- >> document
indices(Face):=(F)->(F.indices#0)

simplexRing(Face):=(F)->(
if member(symbol ofComplex,keys F)==true then return(((F.ofComplex)#0).simplexRing);
if #vert(F)>0 then return(ring((vert F)#0));
error("simplex ring of empty face not known"))

-- >> export
ofComplex=method()
ofComplex(Face):=(F)->((F.ofComplex)#0)

-- >> document
isSubface=method()
isSubface(Face,Face):=(F,G)->(
v1:=set vert F;
v2:=set vert G;
isSubset(v1,v2))

-- >> document
faceToMonomial=method()
faceToMonomial(Face,PolynomialRing):=(F,R)->(
v:=vert(F);
if v=={} then return(1_R);
product(v))
faceToMonomial(Face):=(F)->(
v:=vert(F);
if v!={} then return(product(v));
R:=(ofComplex F).simplexRing;
1_R)
-- we put the ring with possibly switching to ZZ vertices in mind
-- and to avoid getting 1_ZZ for the empty face

--------------------------------------------------------------------
-- functions dealing with lists of faces, used to create a complex


isSimp=method()
isSimp(List):=(L)->(
tst:=true;
q:=0;
qq:=0;
while q<#L and tst==true do (
  qq=0;
  while qq<#(L#q) and tst==true do (
    if #vert(L#q#qq)!=q then tst=false;
  qq=qq+1);
q=q+1);
tst)
--v=(entries vars R)#0
--L=toList apply(0..#v,j->subsets(v,j)) 
--isSimp L

isPolytope=method()
isPolytope(List):=(L)->(
numberOfFacets(L)==1
)
-- isPolytope L

dimComplex=method()
dimComplex(List):=(L)->(
q:=0;
d:=-1;
tst:=true;
while q<#L and tst==true do (
   if #(L#q)>0 then d=q-1;
q=q+1);
d)
-- dim(L)

dimCoComplex=method()
dimCoComplex(List):=(L)->(
q:=#L-1;
d:=#L-1;
tst:=true;
while q>=0 and tst==true do (
--   if #(L#q)>0 then d=#L-q-1;
   if #(L#q)>0 then d=q-1;
q=q-1);
d)
-- dim(L)


dim(List):=(L)->(
if #(L#(#L-1))==0 then return(dimComplex(L));
if #(L#0)==0 then return(dimCoComplex(L));
dimComplex(L))

fvector=method()
fvector(List):=(L)->(
apply(L,j->#j))


isEquidimensional=method()
isEquidimensional(List):=(L)->(
anzdim:=0;
q:=0;
while q<#L and anzdim<=1 do (
  if #(L#q)>0 then anzdim=anzdim+1;
q=q+1);
anzdim<=1);

applyC=method()
applyC(List,Function):=(L,f)->(
apply(L,L1->apply(L1,jj->f(jj))))



applyToComplex=method()
applyToComplex(List,Function):=(L,f)->(
L1:={};
L2:={};
q:=0;
qq:=0;
while q<#L do (
  qq=0;
  L2={};
  while qq<#(L#q) do (
    L2=append(L2,f(L#q#qq,q));
  qq=qq+1);
  L1=append(L1,L2);
q=q+1);
L1)
--applyToComplex(L,(j,q)->(#j)==q)


forAllFaces=method()
forAllFaces(List,Function):=(L,f)->(
tst:=true;
q:=0;
qq:=0;
while q<#L and tst==true do (
  qq=0;
  while qq<#(L#q) and tst==true do (
    if f(L#q#qq)!=true then tst=false;
  qq=qq+1);
q=q+1);
tst)


-----------------------------------

emptySpaces=method()
emptySpaces(String):=(S)->(
n:=#S;
S1:="";
for q from 1 to n do (
  S1=S1|" ";
);
S1)



-------------------------------------------------------------------
-- Complexes

-- if we face later a memory/performance issue then 
-- (in some case) we could try to store only the facets
-- but in the applications in mind all faces are obtained anyway

-- >> export
Complex = new Type of MutableHashTable
Complex.synonym = "embedded complex"


Complex#{Standard,AfterPrint} = m -> (
 sp:="non-simplicial";
 eq:="equidimensional";
 if m.isEquidimensional==false then eq="non-equidimensional";
 lineprint:= lineNumber | " : ";
 lineprint2:=emptySpaces(lineprint|" ");
 fvstr:="";
 if member(symbol fvector,keys m)==true then fvstr=", F-vector "|toString(m.fvector)|", Euler = "|toString(eulerCharacteristic(m.fvector));
 if m.isSimp==true then sp="simplicial";
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << "complex of dim "|m.dim|" embedded in dim "|m.edim|" (printing facets)"
      << endl
      << lineprint2
      << eq|", "|sp|fvstr
      << endl;)

-- >> export
net Complex := (C) -> (
--fct:=toList apply(-1..dim(C),j->C.facets_j);
if numberOfFacets(C)==0 then return(net "empty complex");
fct:=C.facets;
L:={};
nq:=symbol nq;
for q from 0 to #fct-1 do (
  if #(fct_q)>0 then (
    nq=(net(q-1))|(net ": ");
    for qq from 0 to #(fct_q)-1 do (
       nq=nq|net(fct_q_qq)|net(" ");
    );
    L=append(L,nq);
  );
);
stack(L))
-- product vert
--dC=boundaryOfPolytope(C)



-- >> export

addComplexToFace=method()
addComplexToFace(Face,Complex):=(F,C)->(
if member(symbol complex,keys F)==true then (
  F.ofComplex=join(F.ofComplex,{C});
) else (
  F.ofComplex={C};
);
"done");

addComplexToFace(Face,Complex,ZZ,ZZ):=(F,C,d,j)->(
if member(symbol complex,keys F)==true and member(symbol indices,keys F)==true then (
  F.ofComplex=join(F.ofComplex,{C});
  F.indices=join(F.indices,{d,j})
) else (
  F.ofComplex={C};
  F.indices={{d,j}}
);
"done");







-*
checkFaces=method()
checkFaces(List):=(L)->(
true)
*-



face(List,Complex):=(verticesfc,C)->(new Face from {symbol vert=>verticesfc, symbol ofComplex=>{C}});
face(List,Complex,ZZ,ZZ):=(verticesfc,C,d,j)->(new Face from {symbol vert=>verticesfc, symbol ofComplex=>{C}, symbol indices=>{{d,j}}});

 
eulerCharacteristic=method()
eulerCharacteristic(Complex):=(C)->(eulerCharacteristic(C.fvector))
eulerCharacteristic(List):=(L)->(
q:=0;
ec:=0;
while q<#L do (
  ec=ec+(-1)^(q+1)*L#q;
q=q+1);
ec)


newComplex=method()
newComplex(PolynomialRing,List,List,PolynomialRing) := (R,faceslist,facetlist,Rdual)-> (
 fc0:=new ScriptedFunctor from {subscript => i-> faceslist_(i+1)};
 L:={
     symbol simplexRing => R,
--     symbol dualSimplexRing => Rdual,
     symbol grading => R.grading,
--     symbol dualGrading => Rdual.grading,
     symbol isSimp => isSimp(faceslist),
     symbol edim => rank source R.grading,
     symbol dim => dim (faceslist),
     symbol fvector => fvector(faceslist),
     symbol facets => facetlist,
     symbol fc => fc0,
     symbol isEquidimensional => isEquidimensional(facetlist),
     symbol dualComplex=>newEmptyComplex(Rdual)
     };
 new Complex from L
)


newComplex(PolynomialRing,List,List) := (R,faceslist,facetlist)-> (
 fc0:=new ScriptedFunctor from {subscript => i-> faceslist_(i+1)};
 L:={
     symbol simplexRing => R,
--     symbol dualSimplexRing => false,
     symbol grading => R.grading,
--     symbol dualGrading => false,
     symbol isSimp => isSimp(facetlist),
     symbol edim => rank source R.grading,
     symbol dim => dim (facetlist),
     symbol fvector => fvector(faceslist),
     symbol facets => facetlist,
     symbol fc => fc0,
     symbol isEquidimensional => isEquidimensional(facetlist)
     };
 new Complex from L
)

newComplexFromFacets=method()
newComplexFromFacets(PolynomialRing,List,PolynomialRing):= (R,facetlist,Rdual)-> (
-- fc:=new ScriptedFunctor from {subscript => i-> {}};
 L:={
     symbol simplexRing => R,
     symbol grading => R.grading,
     symbol isSimp => isSimp(facetlist),
     symbol edim => rank source R.grading,
     symbol dim => dim (facetlist),
     symbol facets => facetlist,
--     symbol fc => fc,
     symbol isEquidimensional => isEquidimensional(facetlist),
     symbol dualComplex=>newEmptyComplex(Rdual)
     };
 new Complex from L
)


newComplexFromFacets(PolynomialRing,List):= (R,facetlist)-> (
-- fc:=new ScriptedFunctor from {subscript => i-> {}};
 L:={
     symbol simplexRing => R,
     symbol grading => R.grading,
     symbol isSimp => isSimp(facetlist),
     symbol edim => rank source R.grading,
     symbol dim => dim (facetlist),
     symbol facets => facetlist,
--     symbol fc => fc,
     symbol isEquidimensional => isEquidimensional(facetlist),
     };
 new Complex from L
)



-- user functions
complex=method()

-- >> export
complex(PolynomialRing,List,List,PolynomialRing) := (R,faceslist,facetlist,Rdual)-> (
newComplex(R,faceslist,facetlist,Rdual))

-- >> export
complex(PolynomialRing,List,PolynomialRing) := (R,faceslist,Rdual)-> (
complex(R,faceslist,facets(faceslist),Rdual))

-- >> export
complex(PolynomialRing,List,List) := (R,faceslist,facetlist)-> (
newComplex(R,faceslist,facetlist))

-- >> export
complex(PolynomialRing,List) := (R,faceslist)-> (
newComplex(R,faceslist,facets(faceslist)))

--C=complex(R,L)

-- >> export
complexFromFacets=method()
complexFromFacets(PolynomialRing,List,PolynomialRing) := (R,facetslist,Rdual)-> (
newComplexFromFacets(R,facetslist,Rdual))

-- >> export
complexFromFacets(PolynomialRing,List) := (R,facetslist)-> (
newComplexFromFacets(R,facetslist))


-- >> export
newEmptyComplex=method()
newEmptyComplex(PolynomialRing):=(R)->(
ed:=rank source R.grading;
faceslist:=toList apply(-1..ed,j->{});
complexFromFacets(R,faceslist))

-*
newEmptyComplex(PolynomialRing,PolynomialRing):=(R,Rdual)->(
ed:=rank source R.grading;
faceslist:=toList apply(-1..ed,j->{});
complexFromFacets(R,faceslist,Rdual))
*-

-- >> export
addFaceDataToComplex=method()
addFaceDataToComplex(Complex,List,List):=(C,faceslist,facetlist)->(
fc:=new ScriptedFunctor from {subscript => i-> faceslist_(i+1)};
C.isSimp=isSimp(facetlist);
C.dim=dim(facetlist);
C.fvector = fvector(faceslist);
C.facets = facetlist;
C.fc = fc;
C.isEquidimensional =isEquidimensional(facetlist);
C)

addFaceDataToComplex(Complex,List):=(C,faceslist)->(
addFaceDataToComplex(C,faceslist,facets(faceslist)))


addFacetDataToComplex=method()
addFacetDataToComplex(Complex,List):=(C,facetlist)->(
C.isSimp=isSimp(facetlist);
C.dim=dim(facetlist);
C.facets = facetlist;
C.isEquidimensional =isEquidimensional(facetlist);
"done")



-*
R=QQ[x_0..x_4]
addCokerGrading(R)
I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
C=idealToComplex(I)
Cl=newEmptyComplex(R)
addFaceDataToComplex(Cl,faces C)
Cl==C
*-

-*Note:
To save memory the goal is to store face of complexes and their 
subcomplexes only once. If (e.g. by boundaryOfPolytope, idealToCoComplex,...) 
a complex C1 is computed which is a subcomplex of C and is a face of C1
then then F.ofComplex returns still C.
This e.g. allows us to dualize F if C was a polytope
*-

--------------------------------------------------
-- computing the facets of a complex

isNonFacetComplex=method()

-- for faces and listlists of these
isNonFacetComplex(ZZ,Face,List):=(qF,F,L)->(
tst:=false;
q:=qF;
qq:=0;
L2:={};
--print(q,F,L#q);
while q<#L and tst==false do (
  L2={};
  qq=0;
  while qq<#(L#q) and tst==false do (
    if isSubset(vert F, vert L#q#qq) then tst=true;
  qq=qq+1); 
q=q+1);
tst)

-- for lists and listlist of these (not used any more)
isNonFacetComplex(ZZ,List,List):=(qF,F,L)->(
tst:=false;
q:=qF;
qq:=0;
L2:={};
--print(q,F,L#q);
while q<#L and tst==false do (
  L2={};
  qq=0;
  while qq<#(L#q) and tst==false do (
    if isSubset(F,L#q#qq) then tst=true;
  qq=qq+1); 
q=q+1);
tst)

-- L=faces C
--isNonFacetComplex(2,L#2#0,L)

facetsComplex=method()
facetsComplex(List):=(L)->(
q:=0;
qq:=0;
L1:={};
L2:={};
while q<#L do (
  L2={};
  qq=0;
  while qq<#(L#q) do (
    if isNonFacetComplex(q+1,L#q#qq,L)==false then L2=append(L2,L#q#qq);
  qq=qq+1);
  L1=append(L1,L2);
q=q+1);
L1)

--facets(faces C)
-- >> export
facets=method()
facets(List):=(L)->(
if #(L#0)==0 then return(facetsCoComplex(L));
if #(L#(#L-1))==0 then return(facetsComplex(L));
facetsComplex(L))


numberOfFacets=method()
numberOfFacets(List):=(L)->(
sum(apply(L,j->#j))
)
numberOfFacets(Complex):=(C)->(
numberOfFacets(C.facets))

-------------------------------------------------------
-- some simple functions on complexes
-- >> document
dim(Face,Complex):=(F,C)->(
R:=simplexRing C;
dim(F,R))

-- >> export
coordinates=method()
coordinates(Face,Complex):=(F,C)->(
v:=vert(F);
A:=grading(C);
L:={};
vr:=vars simplexRing C;
for q from 0 to #v-1 do (
 for qq from 0 to rank source vr-1 do (
   if vr_(0,qq)==v#q then L=append(L,(entries(A^{qq}))#0);
 );
);
L)
coordinates(Face):=(F)->(
if member(symbol ofComplex,keys F)==true or #vert(F)>0 then return(coordinates(F,ofComplex(F)));
error("specify complex"))
--coordinates(C.fc_1_0)


--coordinates(face {x_1,x_3,x_0},C)


-- >> export
isPolytope Complex:=(C)->(
if member(symbol isPolytope,keys C)==true then return(C.isPolytope);
numberOfFacets(C)==1)
-- >> export
isSimp Complex:=(C)->C.isSimp
-- >> document
dim Complex:= (C)->(C.dim)
-- >> export
-- to get vertices as faces do C.fc_0
vert Complex :=(C)->(
sort toList set join toSequence apply(join toSequence facets C,vert))

variables=method()
variables Complex :=(C)->(
(entries vars simplexRing C)#0)


edim=method()
edim Complex:= (C)->(C.edim)
-- >> document
fvector(Complex):=(C)->(C.fvector)
-- >> document
grading(Complex):=(C)->(C.grading)
-- >> export
isEquidimensional(Complex):=(C)->(C.isEquidimensional)
-- >> export
simplexRing(Complex):=(C)->(C.simplexRing)
-- >> export
facets(Complex):=(C)->(C.facets)
--faces=method()
--faces(Complex):=(C)->(C.fc)
-- >> export

fc=method()
fc(Complex):=(C)->(
if member(symbol fc,keys C)==true then return(toList apply(-1..edim(C),j->C.fc_j));
if member(symbol isSimp,keys C)==true then (
 if C.isSimp==true then (
--  faceslist:=computeFacesOfSimplicialComplex(C);
--  fc:=new ScriptedFunctor from {subscript => i-> faceslist_(i+1)};
  C.fc=computeFacesOfSimplicialComplex(C);
  C.fvector=fvector fc C;
  return(fc C);
 );
);
if member(symbol noBoundary,keys C)==true then (
 if C.noBoundary==true then (
  C.fc=computeFacesOfCompleteComplex(C);
  C.fvector=fvector fc C;
  return(fc C);
 );
);
error("faces unknown"))

fc(Complex,ZZ):=(C,d)->(
fc(C);
C.fc_d)

---------------------------------------------------------------------
-- compute faces of a compact complex

-- too slow, improve:

computeFacesOfCompleteComplex=method()
computeFacesOfCompleteComplex(Complex):=(C)->(
L:=new MutableList from toList apply(0..edim(C)+1,j->{});
--L#0={new Face from {symbol vert=>{},symbol ofComplex=>{C},symbol indices=>{{-1,0}}}};
fc:=join toSequence facets C;
tst:=false;
q:=1;
qq:=0;
anznew:=0;
while tst==false do (
  L1:=apply(subsets(fc,q),intersectFaces);
  anznew=0;
  qq=0;
  while qq<#L1 do (
    L#(1+dim(L1#qq))=setAppend(L#(1+dim(L1#qq)),L1#qq);
    anznew=anznew+1;
  qq=qq+1);
  if anznew==0 then tst=true;
q=q+1);
new ScriptedFunctor from {subscript => i-> L#(i+1)})

-*
L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}}
P=convHull L
dP=boundaryOfPolytope(P)
computeFacesOfCompleteComplex(dP)
*-


setAppend=method()
setAppend(List,Thing):=(L,F)->(
q:=0;
tst:=false;
while q<#L and tst==false do (
  if L#q==F then tst=true;
q=q+1);
if tst==false then return(append(L,F));
L)
--setAppend({1,2,4,5},3)
--setAppend({1,2,4,5},4)

--computeFacesOfCompleteComplex C

intersectFaces=method()
intersectFaces(List):=(S)->(
if #S==1 then return(S#0);
q:=2;
it:=intersectFaces(S#0,S#1);
while q<#S do (
  it=intersectFaces(it,S#q);
q=q+1);
it)

intersectLists=method()
intersectLists(List):=(S)->(
if #S==1 then return(S#0);
q:=2;
it:=intersectLists(S#0,S#1);
while q<#S do (
  it=intersectLists(it,S#q);
q=q+1);
it)

polytopalFacets=method()
polytopalFacets(Complex):=(C)->(C.polytopalFacets)

intersectFaces(Face,Face):=(F1,F2)->(
if member(symbol ofComplex,keys F1)==true and member(symbol ofComplex,keys F2)==true then (
  if not(F1.ofComplex===F2.ofComplex) then error("expected faces of the same complex");
--  if member(symbol dualFace,F1)==true then (
--    return(new Face from {symbol vert=>intersectLists(vert F1,vert F2), symbol ofComplex=>{F1.ofComplex}, symbol dualFace=>union(F1.dualFace,F2.dualFace)})
--  );
  return(new Face from {symbol vert=>intersectLists(vert F1,vert F2), symbol ofComplex=>F1.ofComplex});
);
new Face from {symbol vert=>intersectLists(vert F1,vert F2)})

intersectLists(List,List):=(L1,L2)->(
toList((set L1)*(set L2)))

--union(List,List):=(L1,L2)->(
--toList((set L1)+(set L2)))


-*intersectLists({x_1,x_2},{x_2,x_3})
union({x_1,x_2},{x_2,x_3})

intersectFaces(C.fc_2_0,C.fc_2_1)
intersectFaces({C.fc_2_0,C.fc_2_1,C.fc_2_2})

*-

---------------------------------------------------------------------
-- compute faces of a simplicial complex

computeFacesOfSimplicialComplex=method()
computeFacesOfSimplicialComplex(Complex):=(C)->(
aface:=findAface C;
C0:=(aface.ofComplex)#0;
d:=dim(C);
v:=variables(C);
fc:=facets C;
q:=0;
L:={};
while q<=d+1 do (
  L=append(L,apply(select(subsets(v,q),j->isSubfaceOfFacets(j,fc)),jj->
                    new Face from {symbol vert=>jj, symbol ofComplex=>{C0}, symbol indices=>{{d,q}}}
                  ));
q=q+1);
for q from d+2 to edim(C)+1 do L=append(L,{});
pf:=member(symbol polytopalFacets,keys C);
new ScriptedFunctor from {subscript => i->( 
            if pf==true and i==dim(C)-1 then return(C.polytopalFacets);
            L_(i+1)
)})

--C=simplex(R)
--computeFacesOfSimplicialComplex(C)

isSubfaceOfFacets=method()
isSubfaceOfFacets(List,List):=(F,L)->(
q:=0;
tst:=false;
d:=0;
while d<#L and tst==false do (
  q=0;
  while q<#(L#d) and tst==false do (
    if isSubset(set F,vert L#d#q)==true then tst=true;
  q=q+1);
d=d+1);
tst)

-*
fcC=facets C;
isSubfaceOfFacets({x_1},fcC)
*-

-*
C0=simplex R
C=boundaryOfPolytope C0
Cf=complexFromFacets(R,facets C)
peek Cf
fc Cf
peek Cf
dualize Cf

Cf=complexFromFacets(R,facets C)
Cf.noBoundary=true
fc Cf
dualize Cf
*-

---------------------------------------------------------------
applyToComplex(Complex,Function):=(C,f)->(
applyToComplex(fc(C),f))

--------------------------------------------------------------------
-*
compare two complexes:
note that in many real world examples of high codimension 
(given through their faces) the Stanley-Reisner ideal is not computeable 
as it is too big to write down, so we can't use it
*-
-- >> document
Complex == Complex :=(C1,C2)->(
if not(C1.simplexRing===C2.simplexRing) then return(false);
if C1.grading!=C2.grading then return(false);
if C1.dim!=C2.dim then return(false);
return(compareFaces(C1.facets,C2.facets))
)

compareFaces=method()
compareFaces(List,List):=(L1,L2)->(
tst:=true;
j:=0;
while j<#L1 and tst==true do (
   if compareFacesj(L1#j,L2#j)==false then tst=false;
j=j+1);
tst)
--compareFaces(C.facets,C.facets)

compareFacesj=method()
compareFacesj(List,List):=(L1,L2)->(
anz:=0;
tst:=true;
j:=0;
while j<#L1 and tst==true do (
  anz=0;
  for jj from 0 to #L2-1 do (
    if L2#jj == L1#j then anz=anz+1;
  );
  if anz!=1 then tst=false;
j=j+1);
tst)
--compareFaces((C.facets)#5,(C.facets)#5)


-----------------------------------------------------------------------
-- dual faces of a polytope


-*
-- obsolete

addDualFaces=method()
addDualFaces(Complex,PolynomialRing):=(C,Rdual)->(
if isPolytope(C)==false then error("expected polytope");
fc:=C.fc_(dim(C)-1);
C.dualSimplexRing=Rdual;
C.dualGrading=Rdual.grading;
d:=-1;
q:=0;
while d<=dim(C) do (
  print(d);
  q=0;
  while q<#(C.fc_d) do (
    --(C.fc_d_q).dualVertices=computeDualVertices(C.fc_d_q,fc,Rdual);
    computeDualVertices(C.fc_d_q,fc,Rdual);
  q=q+1);
d=d+1);
fc)

addDualFaces(Complex):=(C)->(
n:=#(C.fc_(dim(C)-1));
K:=coefficientRing simplexRing C;
v:=symbol v;
Rdual:=K[v_0..v_(n-1)];
Rdual.grading=0;
addDualFaces(C,Rdual))

*-

-*
R=QQ[x_0..x_10]
Rdual=QQ[v_0..v_10]
addCokerGrading(R)
addCokerGrading(Rdual)
C=simplex(R)
addDualFaces(C,Rdual)
peek C.fc_2_2
peek C.fc_-1_0
peek C.fc_dim(C)_0
*-

computeDualVertices=method()

computeDualVertices(Face,List,PolynomialRing):=(F,fcts,Rdual)->(
v:=(entries vars(Rdual))#0;
L:={};
q:=0;
while q<#fcts do (
  if isSubface(F,fcts#q)==true then L=append(L,v#q);
q=q+1);
L)


computeDualVertices(Face,List):=(F,fcts)->(
L:={};
q:=0;
while q<#fcts do (
  if isSubface(F,fcts#q)==true then L=append(L,q);
q=q+1);
L)


-*
-- obsolete
computeDualVertices(List,List,PolynomialRing):=(F,fcts,Rdual)->(
v:=(entries vars(Rdual))#0;
L:={};
q:=0;
while q<#fcts do (
  if isSubset(F,fcts#q)==true then L=append(L,v#q);
q=q+1);
L)
*-

-----------------------------------------------------------------------
-- make a simplex with dual faces
-- >> export

-*
-- obsolete

simplexWithDualFaces=method()

simplexWithDualFaces(PolynomialRing):=(R)->(
n:=rank source vars R;
K:=coefficientRing R;
v:=symbol v;
Rdual:=K[v_0..v_(n-1)];
Rdual.grading=R.grading;
------------------------------------------------------------------- fix this
simplex(R,Rdual))

simplexWithDualFaces(PolynomialRing,PolynomialRing):=(R,Rdual)->(
v:=(entries vars R)#0;
A:=grading R;
Cl:=newEmptyComplex(R,Rdual);
F:=symbol F;
S:=symbol S;
fc:=subsets(v,#v-1);
if (rank A)<(#v-1) then error("grading matrix does not have correct rank");
faceslist:=toList apply(0..#v,j->(
 if j!=#v-1 then (
     S=subsets(v,j);
 ) else (
     S=fc;
 );
 toList apply(0..#S-1,
                      jj->new Face from {symbol vert=>S#jj, symbol ofComplex=>{Cl}, symbol indices=>{{j-1,jj}}
                                            ,symbol dualVertices=> toList apply(select(0..#fc-1,jjj->isSubset(S#jj,fc#jjj)),jjjj->Rdual_jjjj)
                                        }
             )
));
q:=0;
facetlist:={};
while q<#v do (
  facetlist=append(facetlist,{});
q=q+1);
facetlist=append(facetlist,{
           new Face from {symbol vert=>v, symbol ofComplex=>{Cl}, symbol indices=>{{#v-1,0}}
                          ,symbol dualVertices=>{}
                         }
                           });
addFaceDataToComplex(Cl,faceslist,facetlist);
Cl)

*-



embeddingComplex=method()
embeddingComplex(Complex):=(C)->(
F:=findAface C;
(F.ofComplex)#0)


--------------------------------------------------------------------

--> export
simplex=method(Options=>{computeFaces=>null})

simplex(PolynomialRing):=opts->(R)->(
if member(symbol grading,keys R)==false then addCokerGrading(R);
A:=grading R;
n:=rank source vars R;
K:=coefficientRing R;
v:=symbol v;
Rdual:=K[v_0..v_(n-1)];
--Rdual.grading=R.grading;
simplex(R,Rdual,opts))

--> export
verticesDualPolytope=method()
verticesDualPolytope(Complex):=(C)->(
n:=#vert(C.polytopalFacets#0);
if isPolytope(C)==false then error("expected a polytope");
if rank(C.grading)!=rank source C.grading then error("expected a polytope of full dimension");
if member(symbol polytopalFacets,keys C)==false then (
   C.polytopalFacets=C.fc_(dim C-1);
);
T:=transpose(matrix{toList(apply(0..n-2,j->-1))})|((ZZ^(n-1))_{0..n-2});
rels:=apply(apply(C.polytopalFacets,coordinates),matrix);
L:=apply(apply(apply(apply(rels,j->T*j),ker),gens),transpose);
L1:={};
q:=0;
for q from 0 to #L-1 do (
  rv:=(L#q*(transpose(rels#q)))_(0,0);
  L1=append(L1,(entries((-1/rv)*sub(L#q,QQ)))#0)
);
matrix L1)
--verticesDualPolytope C

--> export
dualGrading=method()
dualGrading(Complex):=(C)->(C.dualComplex.grading)

simplex(PolynomialRing,PolynomialRing):=opts->(R,Rdual)->(
v:=(entries vars R)#0;
A:=grading R;
n:=rank source vars R;
if rank source A!=n-1 or rank target A!=n or rank A!=n-1 then error("vertices do not form a simplex of full dimension");
Cl:=newEmptyComplex(R);
q:=0;
facetlist:={};
while q<#v do (
  facetlist=append(facetlist,{});
q=q+1);
facetlist=append(facetlist,{face(v,Cl,#v-1,0)});
Cl.noBoundary=false;
--S:=apply(0..#v-1,j->remove(v,j));
S:=subsets(v,#v-1);
Cl.polytopalFacets=toList apply(0..#S-1,jj->face(S#jj,Cl,#v-2,jj));
Cl.isSimp=true;
Cl.isPolytope=true;
Rdual.grading=verticesDualPolytope(Cl);
dCl:=newEmptyComplex(Rdual);
dCl.dualComplex=Cl;
dCl.noBoundary=false;
dCl.isPolytope=true;
--
dv:=(entries vars Rdual)#0;
dS:=subsets(dv,#dv-1);
dCl.polytopalFacets=toList apply(0..#dS-1,jj->face(dS#jj,Cl,#dv-2,jj));
dfacetlist:={};
q=0;
while q<#dv do (
  dfacetlist=append(dfacetlist,{});
q=q+1);
dfacetlist=append(dfacetlist,{face(dv,dCl,#dv-1,0)});
addFacetDataToComplex(dCl,dfacetlist);
Cl.dualComplex=dCl;
if class(opts.computeFaces)===Boolean then (
 if opts.computeFaces==false then (
    addFacetDataToComplex(Cl,facetlist);
    --dCl.dim=Cl.dim;
    return(Cl);
 );
);
F:=symbol F;
if (rank A)<(#v-1) then error("grading matrix does not have correct rank");
faceslist:=toList apply(0..#v,j->(
 S=subsets(v,j);
 toList apply(0..#S-1,jj->face(S#jj,Cl,j-1,jj))
));
addFaceDataToComplex(Cl,faceslist,facetlist);
--dCl.dim=Cl.dim;
Cl)
--simplex(R,Rdual)

-- implement dual simplex in facet representation i.e. for computeFaces=>false

-*
simplex(R)
simplex(R,computeFaces=>true)
simplex(R,computeFaces=>false)
*-


-*
simplex(PolynomialRing):=(R)->(
n:=rank source vars R;
A:=raysPPn(n-1);
simplex(R,A))
*-

-*
R=QQ[x_0..x_5]
addCokerGrading(R)
C0=simplex(R)
dC0=boundaryOfPolytope(C0)
*-


--------------------------------------------------------
-- cyclic polytopes


positionRing=method()
positionRing(RingElement):=(m)->(
v:=(entries vars ring m)#0;
for q from 0 to #v-1 do (
if v#q==m then return(q);
);
);

--R=QQ[x_1..x_10]
--positionRing(x_1)

isContigous=method()
isContigous(List):=(X)->(
if X=={} then return(false);
X1:=sort(X);
p2:=X1#(#X1-1);
p1:=X1#0;
if abs(positionRing(p2)-positionRing(p1))==#X1-1 then return(true);
false);

-*
isContigous({x_2,x_3,x_6,x_4,x_5,x_8,x_7})
isContigous({x_2,x_3,x_6,x_4,x_5,x_8,x_7})
isContigous({x_1,x_2,x_5,x_3})
*-


contigousSubsets=method()
contigousSubsets(List):=(W)->(
select(subsets(W),isContigous))

-*
contigousSubsets({x_4,x_5})
contigousSubsets({x_3,x_4})
contigousSubsets({x_1,x_2,x_3})
contigousSubsets({x_2,x_3,x_4})
contigousSubsets({x_1,x_2,x_4})
*-


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
--print(rm);
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
-*
maximalContigousSubsets({x_1,x_2,x_4})
maximalContigousSubsets({x_1,x_2,x_4,x_5,x_7,x_8,x_9})
*-


isEndset=method()
isEndset(List):=(L)->(
v:=(entries vars ring L#0)#0;
if isSubset({v#0},L)==true or isSubset({v#(#v-1)},L)==true then return(true);
false)

-*
isEndset({x_1,x_2})
isEndset({x_1,x_3})
isEndset({x_2,x_3})
*-


removeEndsets=method()
removeEndsets(List):=(L)->(
L1:={};
j:=0;
for j from 0 to #L-1 do (
  if isEndset(L#j)==false then L1=append(L1,L#j);
);
L1);

-*
removeEndsets({{x_1,x_2},{x_3,x_4}})
removeEndsets({{x_1,x_3},{x_7,x_8}})

removeEndsets(maximalContigousSubsets({x_1,x_2,x_4}))
removeEndsets(maximalContigousSubsets({x_1,x_2,x_4,x_5,x_7,x_8}))
*-

oddContigousNonEndsets=method()
oddContigousNonEndsets(List):=(L)->(
L2:={};
j:=0;
L1:=removeEndsets(maximalContigousSubsets(L));
for j from 0 to #L1-1 do (
  if odd(#(L1#j))==true then L2=append(L2,L1#j);
);
L2)

-*
maximalContigousSubsets({x_1,x_2,x_4,x_5,x_7,x_8,x_9})
removeEndsets(oo)
oddContigousNonEndsets({x_1,x_2,x_4,x_5,x_7,x_8,x_9})
*-

isFaceOfCyclicPolytope=method()
isFaceOfCyclicPolytope(List,ZZ):=(W,d)->(
 if W=={} then return(true);
 if #oddContigousNonEndsets(W)<=d-#W then return(true);
false);

-*
isFaceOfCyclicPolytope({x_2,x_3},3)
isFaceOfCyclicPolytope({x_3,x_4},3)
isFaceOfCyclicPolytope({x_4,x_9},3)
*-


boundaryCyclicPolytope0=method()
boundaryCyclicPolytope0(ZZ,PolynomialRing):=(d,R)->(
C0:=simplex(R,computeFaces=>false);
Cl:=newEmptyComplex(R);
M:=(entries vars R)#0;
F:=face {};
addComplexToFace(F,C0);
L:={{F}};
S:={};
S1:={};
for j from 1 to #M do (
 if j<=d then (
  S=subsets(M,j);
  S1={};
  for jj from 0 to #S-1 do (
    if isFaceOfCyclicPolytope(S#jj,d)==true then ( 
       F=face S#jj;
       addComplexToFace(F,C0);
       S1=append(S1,F)
    );
  );
  L=append(L,S1);
 ) else (
  L=append(L,{});
 );
);
addFaceDataToComplex(Cl,L);
Cl);

-*
R=QQ[x_0..x_7]
--addCokerGrading(R)
--simplex(R)
C=boundaryCyclicPolytope0(4,R)
peek C.fc_2_0
dC=dualize C
dC.grading
*-

boundaryCyclicPolytope=method()
boundaryCyclicPolytope(ZZ,PolynomialRing):=(d,R)->(
if member(symbol grading,keys R)==false then addCokerGrading(R);
boundaryCyclicPolytope0(d,R))


fullCyclicPolytope=method()
fullCyclicPolytope(ZZ,PolynomialRing):=(d,R)->(
Cl:=cyclicPolytopeFaces(d,R);
K:=coefficientRing R;
v:=symbol v;
n:=#(Cl.polytopalFacets);
Rdual:=K[v_0..v_(n-1)];
Rdual.grading=verticesDualPolytope(Cl);
dCl:=newEmptyComplex(Rdual);
dCl.dualComplex=Cl;
dCl.isPolytope=true;
dCl.isSimp=false;
dCl.dim=Cl.dim;
Cl.dualComplex=dCl;
Cl)

fullCyclicPolytope(ZZ,PolynomialRing,PolynomialRing):=(d,R,Rdual)->(
Cl:=cyclicPolytopeFaces(d,R);
Rdual.grading=verticesDualPolytope(Cl);
dCl:=newEmptyComplex(Rdual);
dCl.dim=Cl.dim;
dCl.dualComplex=Cl;
Cl.dualComplex=dCl;
Cl)


cyclicPolytopeFaces=method()
cyclicPolytopeFaces(ZZ,PolynomialRing):=(d,R)->(
if member(symbol grading,keys R)==true then print("Warning: Coker grading has been overwritten");
-- or option overwriteGrading=>true;
M:=(entries vars R)#0;
n:=#M;
Lv:=toList apply(0..n-1,j->toList apply(1..d,jj->j^jj));
tv:=sum Lv;
Lv=apply(Lv,j->n*j-tv);
A:=matrix Lv;
addCokerGrading(R,A);
Cl:=newEmptyComplex(R);
Cl.noBoundary=false;
Cl.isSimp=false;
L:={{face({},Cl)}};
S:={};
S1:={};
for j from 1 to d+1 do (
 if j<=d then (
  S=subsets(M,j);
  S1={};
  for jj from 0 to #S-1 do (
    if isFaceOfCyclicPolytope(S#jj,d)==true then ( 
       S1=append(S1,face(S#jj,Cl))
    );
  );
  L=append(L,S1);
 ) else (
  L=append(L,{face(M,Cl)});
 );
);
addFaceDataToComplex(Cl,L);
Cl.polytopalFacets=L#(#L-2);
Cl);




-*
R=QQ[x_0..x_6]
C=fullCyclicPolytope(4,R)
*-

----------------------------------------------------

dualize=method()
dualize(Face):=(F)->(
idx:=0;
dF:=symbol dF;
if member(symbol dualFace,keys F)==true then return(F.dualFace);
if member(symbol ofComplex,keys F)==false then error("expected face of a complex or co-complex");
C:=(F.ofComplex)#0;
if isPolytope(C)==false then error("expected face of polytope");
if member(symbol dualComplex,keys C)==true then (
 if member(symbol indices, keys F)==true then idx=(F.indices)#0#1;
 dF=new Face from {symbol vert=>computeDualVertices(F,C.polytopalFacets,C.dualComplex.simplexRing),
                    symbol dualFace=>F,
                    symbol ofComplex=>{C.dualComplex},
                    symbol indices=>{{edim(C) - dim(F)-1,idx}}};
 F.dualFace=dF;
 return(dF);
);
dF=new Face from {symbol vert=>computeDualVertices(F,C.fc_(dim(C)-1)),
               symbol dualFace=>F};
F.dualFace=dF;
dF)


-*
R=QQ[x_0..x_4]
addCokerGrading R
R.grading
C=simplex R
F=C.fc_2_0
peek F
dF=dualize F
peek dF
dF.ofComplex
dF.ofComplex.simplexRing
dF.ofComplex.grading
F==dualize dF
F===dualize dF
*-

findAface=method()
findAface(Complex):=(C)->(
fc:=C.facets;
q:=0;
qq:=0;
while q<#fc do (
    if #(fc#q)>0 then return(fc#q#0);
q=q+1);
error("empty complex"))

-*
     n:=#(C.fc_(dim(C)-1));
     K:=coefficientRing simplexRing C;
     v:=symbol v;
     Rdual:=K[v_0..v_(n-1)];
     Rdual.grading=false;
*-

dualize(Complex):=(C)->(
if member(symbol dualComplex,keys C)==true then (
   if member(symbol fc,keys C.dualComplex)==true or numberOfFacets(C.dualComplex)>0 then return(C.dualComplex);
);
if member(symbol fc,keys C)==false then fc(C);
aface:=findAface C;
C0:=(aface.ofComplex)#0;
--print(C0);
if member(symbol dualComplex,keys C0)==false then error("expected face of a complex which has a dual complex");
L:={};
L1:={};
q:=0;
d:=dim(C0);
--print(d);
while d>=-1 do (
  q=0;
  L1={};
  while q<#(C.fc_d) do (
    L1=append(L1,dualize(C.fc_d_q));
  q=q+1);
  L=append(L,L1);
d=d-1);
--print(L);
dC:=symbol dC;
if member(symbol dualComplex,keys C)==true then (
  dC=C.dualComplex;
  dC.dualComplex=C;
  addFaceDataToComplex(dC,L);
  if #(L#0)==0 then dC=new CoComplex from dC;
  return(dC)
);
dC=complex(C0.dualComplex.simplexRing,L);
if #(L#0)==0 then dC=new CoComplex from dC;
dC.dualComplex=C;
C.dualComplex=dC;
dC)

-*
R=QQ[x_0..x_4]
addCokerGrading R
C=simplex(R)
dC=dualize C
peek dC.fc_2_0

R=QQ[x_0..x_4]
addCokerGrading R
C=simplex(R)
bC=boundaryOfPolytope C
dbC=dualize bC
bC.dualComplex==dbC
dbC.dualComplex==bC
dF=dbC.fc_2_0
peek dF
dualize C
peek dF
*-


---------------------------------------
-- boundary of a polytope

replaceDim=method()
replaceDim(List,ZZ,Thing):=(L,d,L2)->(
L1:={};
for j from 0 to #L-1 do (
  if j==d then ( 
     L1=append(L1,L2);
  ) else ( 
     --if class L2 === List then apply(L#j,jj->addComplexToFace(jj,C,0,0));
     L1=append(L1,L#j);
  );
);
L1)
--replaceDim(C.facets,3,{})

-- >> export


boundaryOfPolytope=method()
boundaryOfPolytope(Complex):=(C)->(
R:=C.simplexRing;
if isPolytope(C)==false then error("expected polytope");
 newfacets:=replaceDim(replaceDim(facets(C),dim(C)+1,{}),dim(C),C.polytopalFacets);
-- newfacets:=replaceDim(replaceDim(facets(C),dim(C)+1,{}),dim(C),C.fc_(dim(C)-1));
L:={};
if member(symbol fc,keys C)==false then (
 L={
     symbol simplexRing => C.simplexRing,
     symbol grading => R.grading,
     symbol isPolytope => false,
     symbol isSimp => isSimp newfacets,
     symbol edim => C.edim,
     symbol dim => -1+C.dim,
     symbol facets => newfacets,
     symbol isEquidimensional => true,
     symbol noBoundary=>true
--     symbol dualComplex=>newEmptyComplex(C.dualComplex.simplexRing);
     };
 return(new Complex from L)
);
 newfaces:=replaceDim(fc(C),dim(C)+1,{});
 fc0:=new ScriptedFunctor from {subscript => i-> (newfaces)_(i+1)};
 newfvector:=replaceDim(C.fvector,dim(C)+1,0);
 L={
     symbol simplexRing => C.simplexRing,
     symbol grading => R.grading,
     symbol isPolytope => false,
     symbol isSimp => isSimp newfacets,
     symbol edim => C.edim,
     symbol dim => -1+C.dim,
     symbol fvector => newfvector,
     symbol facets => newfacets,
     symbol fc => fc0,
     symbol isEquidimensional => true,
     symbol noBoundary=>true
--     symbol dualComplex=>newEmptyComplex(C.dualComplex.simplexRing);
     };
 new Complex from L
)

-*
R=QQ[x_0..x_4]
addCokerGrading R
C=simplex(R)
dC=dualize C
bC=boundaryOfPolytope(C)
dbC=dualize(bC)

C=boundaryOfPolytope simplex(R,computeFaces=>false)
*-



-----------------------------------------------------------------------------



------------------------------------------------------------------------------
-- Stanley-Reisner correspondence

idealToComplex=method()

-- >> export
idealToComplex(Ideal):=(I)->(
idealToComplex monomialIdeal I)

-*
idealToComplex(MonomialIdeal):=(I)->(
n:=-1+rank source vars ring I;
A:=raysPPn(n);
idealToComplex(I,A))

idealToComplex(Ideal,Matrix):=(I,A)->(
idealToComplex(monomialIdeal I,A))
*-
--idealToComplex(MonomialIdeal,Matrix):=(I,A)->(
--if rank target A != rank source vars ring I then error("number of rows does not match the number of variables");
--if isSquareFree(I)==false then error("Expected squarefree monomial ideal");

-- >> export
idealToComplex(MonomialIdeal):=(I)->(
L:={};
L1:={};
R:=ring I;
C0:=simplex(R,computeFaces=>false);
Cl:=newEmptyComplex(R);
A:=R.grading;
--addCokerGrading(R,A);
v:=(entries vars R)#0;
n:=#v;
tst:=true;
pfc:=symbol pfc;
d:=0;
j:=0;
while d<=n do (
 if tst==true then (
  pfc=apply(subsets(v,d),j->face j);
  L1=select(pfc,j->isFaceIdeal(j,I));
  for j from 0 to #L1-1 do 
--        addComplexToFace(L1#j,Cl,d-1,j);
          addComplexToFace(L1#j,C0);
  --if #L1==0 then tst=false;
 ) else (
  L1={};
 );
 L=append(L,L1);
d=d+1);
addFaceDataToComplex(Cl,L);
Cl.ideal=I;
Cl)


isFaceIdeal=method()
isFaceIdeal(Face,MonomialIdeal):=(F,I)->(
tst:=false;
m:=faceToMonomial(F,ring I);
-- I==I+ideal(m);
gI:=gens I;
n:=rank source gI;
q:=0;
while q<n and tst==false do (
  if (degree(denominator(m/gI_(0,q))))#0==0 then tst=true;
q=q+1);
not tst)
--isFaceIdeal(face {x_0,x_1},I)

--idealToComplex(I)


idealToComplex(Ideal,Complex):=(I,C)->(
idealToComplex(monomialIdeal I,C))

idealToComplex(MonomialIdeal,Complex):=(I,C)->(
if member(symbol fc,keys C)==false then error("the complex must have face data");
L:={};
L1:={};
R:=ring I;
Cl:=newEmptyComplex(R);
A:=R.grading;
--addCokerGrading(R,A);
v:=(entries vars R)#0;
n:=#v;
tst:=true;
pfc:=symbol pfc;
d:=-1;
j:=0;
while d<n do (
 if tst==true then (
  L1=select(C.fc_d,j->isFaceIdeal(j,I));
  --for j from 0 to #L1-1 do addComplexToFace(L1#j,C);
  --if #L1==0 then tst=false;
 ) else (
  L1={};
 );
 L=append(L,L1);
d=d+1);
addFaceDataToComplex(Cl,L);
Cl.ideal=I;
Cl)

-*
C0=simplex R
C=idealToComplex(I,C0)
dualize C
C0=simplex(R,computeFaces=>false)
idealToComplex(I,C0)
*-


--------------------------------------------------------------
-- the minimal non-faces

-- this is of course equivalent to IdealToComplex if we
-- are dealing with a subcomplex of a simplex


-- get rid of faces C
-- >> export
minimalNonFaces=method(Options=>{embedded=>null})
minimalNonFaces(Complex):=opts->(C)->(
if class(opts.embedded)===Boolean then (
  if opts.embedded==true then return(minimalNonFacesEmbedded(C));
);
L:={};
j:=1;
jj:=0;
jjj:=0;
R:=simplexRing C;
v:=(entries vars R)#0;
nondivisiblenonfc:={};
tst:=symbol tst;
isface:=symbol isface;
fc:={};
start:=true;
while (j<=#v and #nondivisiblenonfc>0) or start==true do (
  --print(j);
  start=false;
  fc=subsets(v,j);
  nondivisiblenonfc={};
  for jj from 0 to #fc-1 do (
    tst=true;
    jjj=0;
    while jjj<#L and tst==true do (
      if isSubset(set vert L#jjj,set fc#jj)==true then tst=false;
    jjj=jjj+1);
    if tst==true then nondivisiblenonfc=append(nondivisiblenonfc,fc#jj);
  );
  for jj from 0 to #nondivisiblenonfc-1 do (
    isface=false;
    jjj=0;
    while jjj<#(C.fc_(j-1)) and isface==false do (
      if (set vert ((C.fc_(j-1))#jjj))==(set (nondivisiblenonfc#jj)) then (
          isface=true
      );
    jjj=jjj+1);
    if isface==false then (
       L=append(L,face nondivisiblenonfc#jj);
    );
  );
  --print(L);
j=j+1);
L)

--minimalNonFaces C

--C=IdealToComplex(I)
--minimalNonFaces(C)




minimalNonFacesEmbedded=method()
minimalNonFacesEmbedded(Complex):=(C)->(
aface:=findAface C;
C0:=(aface.ofComplex)#0;
L:={};
j:=0;
jj:=0;
jjj:=0;
R:=simplexRing C0;
nondivisiblenonfc:={};
tst:=symbol tst;
isface:=symbol isface;
fc:={};
start:=true;
while (j<=dim(C0) and #nondivisiblenonfc>0) or start==true do (
  start=false;
  fc=C0.fc_j;
  nondivisiblenonfc={};
  for jj from 0 to #fc-1 do (
    tst=true;
    jjj=0;
    while jjj<#L and tst==true do (
      if isSubset(set vert L#jjj,set vert fc#jj)==true then tst=false;
    jjj=jjj+1);
    if tst==true then nondivisiblenonfc=append(nondivisiblenonfc,fc#jj);
  );
  for jj from 0 to #nondivisiblenonfc-1 do (
    isface=false;
    jjj=0;
    while jjj<#(C.fc_j) and isface==false do (
      if (set vert ((C.fc_j)#jjj))==(set vert(nondivisiblenonfc#jj)) then (
          isface=true
      );
    jjj=jjj+1);
    if isface==false then (
       L=append(L,nondivisiblenonfc#jj);
    );
  );
  --print(L);
j=j+1);
L)

--minimalNonFacesEmbedded C

--C.fc_(dim C)

-- >> export
complexToIdeal=method(Options=>{embedded=>null})
complexToIdeal(Complex):=opts->(C)->(
R:=simplexRing C;
I:=ideal apply(minimalNonFaces(C,opts),j->faceToMonomial(j,R));
C.ideal=I;
I)

--C=IdealToComplex(I)
--complexToIdeal C
--complexToIdeal(C,embedded=>true)

-----------------------------------------------------------------------------
-- the geometric picture

-- SR complex is really a cocomplex (which is in inclusion reversing bijection to
-- a complex if sitting inside a simplex by taking the complement face
-- this is the Stanley-Reisner complex
-- but the cocomplex is always dual to a complex in the polytope
-- which is the true geometric strata of the vanishing locus of the ideal
-- in the Cox ring of the toric variety)

---------------------------------------------------------
-- cocomplexes:

-- >> export
CoComplex = new Type of Complex
CoComplex.synonym = "embedded co-complex"
CoComplex#{Standard,AfterPrint} = m -> (
 sp:="non-simplicial";
 eq:="equidimensional";
 if m.isEquidimensional==false then eq="non-equidimensional";
 lineprint:= lineNumber | " : ";
 lineprint2:=emptySpaces(lineprint|" ");
 if m.isSimp==true then sp="simplicial";
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << "co-complex of dim "|m.dim|" embedded in dim "|m.edim|" (printing facets)"
      << endl
      << lineprint2
      << eq|", "|sp|", F-vector "|toString(m.fvector)|", Euler = "|toString(eulerCharacteristic(m.fvector))
      << endl;)

coComplex=method()

-- >> export
coComplex(PolynomialRing,List,List,PolynomialRing) := (R,faceslist,facetlist,Rdual)-> (
 new CoComplex from complex(R,faceslist,facetlist,Rdual))

-- >> export
coComplex(PolynomialRing,List,PolynomialRing) := (R,faceslist,Rdual)-> (
coComplex(R,faceslist,facets(faceslist),Rdual))


-- >> export
coComplex(PolynomialRing,List) := (R,faceslist)-> (
coComplex(R,faceslist,facets(faceslist)))

-- >> export
coComplex(PolynomialRing,List,List) := (R,faceslist,facetlist)-> (
 new CoComplex from complex(R,faceslist,facetlist)
)



----------------------------

-- the geometric cocomplex
-- think of D as the faces over a Fano polytope

-- >> export
idealToCoComplex=method()

idealToCoComplex(MonomialIdeal):=(I)->(
D:=simplex ring I;
idealToCoComplex(I,D))

idealToCoComplex(Ideal):=(I)->(
idealToCoComplex(monomialIdeal I));


idealToCoComplex(Ideal,Complex):=(I,D)->(
idealToCoComplex(monomialIdeal I,D));

idealToCoComplex(MonomialIdeal,Complex):=(I,D)->(
if isSquareFree(I)==false then error("Expected squarefree monomial ideal");
if not(set((entries vars ring I)#0)== set((entries vars simplexRing D)#0) ) then error("Expected ideal and complex over rings with same variables");
tst:=false;
completecomplex:=false;
n:=edim(D);
gI:=gens I;
L:={};
L1:={};
for d from -1 to n do (
  L1={};
  for j from 0 to #(D.fc_d)-1 do (
    tst=false;
    if completecomplex==true then tst=true;
    --if tst==true then print(D.fc_d_j);
    if tst==false then (
      if containsFaceOf(D.fc_d_j,L)==true then tst=true;
    );
    if tst==false then (
      if testFaceCoComplex(D.fc_d_j,gI)==true then tst=true;
    );
    if tst==true then L1=append(L1,D.fc_d_j);
  );
  L=append(L,L1);
  if #L1==#(D.fc_d) then completecomplex=true;
);
C:=coComplex(simplexRing D,L);
C.ideal=I;
C);


containsFaceOf=method()
containsFaceOf(Face,List):=(F,L)->(
vF:=set vert F;
tst:=false;
q:=0;
qq:=0;
while q<#L and tst==false do (
  qq=0;
  while qq<#(L#q) and tst==false do (
    if isSubset(set vert(L#q#qq),vF)==true then tst=true;
  qq=qq+1);
q=q+1);
tst);

testFaceCoComplex=method()
testFaceCoComplex(Face,Matrix):=(F,gI)->(
vF:=vert F;
--print(vF,gI);
tst:=true;
tst2:=false;
q:=0;
qq:=0;
while q<rank source gI and tst==true do (
  --print(gI_(0,q));
  tst2=false;
  qq=0;
  while qq<#vF and tst2==false do (
     if (degree(denominator(gI_(0,q)/vF#qq)))#0==0 then tst2=true;
  qq=qq+1);
  if tst2==false then tst=false; 
q=q+1); 
tst)



coComplexToIdeal=method()
coComplexToIdeal(CoComplex):=(C)->(
fc:=facets(C);
I:=monomialIdeal(1_(simplexRing C));
q:=0;
qq:=0;
while q<#fc do (
  qq=0;
  while qq<#(fc_q) do (
    I=intersect(I,monomialIdeal vert fc_q_qq);
  qq=qq+1);
q=q+1);
I=ideal I;
C.ideal=I;
I)

--store dual face in face structure

---------------------------------------------------------------------------------
-- embedded cocomplexes


--------------------------------------------------
-- computing the facets of a CoComplex

isNonFacetCoComplex=method()

isNonFacetCoComplex(ZZ,List,List):=(qF,F,L)->(
tst:=false;
q:=qF;
qq:=0;
L2:={};
--print(q,F,L#q);
while q>=0 and tst==false do (
  L2={};
  qq=0;
  while qq<#(L#q) and tst==false do (
    if isSubset(L#q#qq,F) then tst=true;
  qq=qq+1); 
q=q-1);
tst)

isNonFacetCoComplex(ZZ,Face,List):=(qF,F,L)->(
tst:=false;
q:=qF;
qq:=0;
L2:={};
--print(q,F,L#q);
while q>=0 and tst==false do (
  L2={};
  qq=0;
  while qq<#(L#q) and tst==false do (
    if isSubset(vert L#q#qq, vert F) then tst=true;
  qq=qq+1); 
q=q-1);
tst)

-- L=fc C
--isNonFacetCoComplex(2,L#2#0,L)

facetsCoComplex=method()
facetsCoComplex(List):=(L)->(
q:=0;
qq:=0;
L1:={};
L2:={};
while q<#L do (
  L2={};
  qq=0;
  while qq<#(L#q) do (
    if isNonFacetCoComplex(q-1,L#q#qq,L)==false then L2=append(L2,L#q#qq);
  qq=qq+1);
  L1=append(L1,L2);
q=q+1);
L1)


---------------------------------------------------------------------------------

complement(Complex):=(C)->(
if member(symbol complementComplex,keys C)==true then return(C.complementComplex);
aface:=findAface C;
C0:=(aface.ofComplex)#0;
d:=dim(C0);
q:=0;
L:={};
L1:={};
while d>=-1 do (
  q=0;
  L1={};
  while q<#(C.fc_d) do (
    L1=append(L1,complement(C.fc_d_q));
  q=q+1);
  L=append(L,L1);
d=d-1);
cC:=complex(C0.simplexRing,L);
if #(L#0)==0 then cC=new CoComplex from cC;
C.complementComplex=cC;
cC.complementComplex=C;
cC)



complement(Face):=(F)->(
C0:=(F.ofComplex)#0;
R:=C0.simplexRing;
v:=(entries(vars R))#0;
n:=#v;
if C0.isSimp==false or n!=dim(C0)+1 then error("expected a subcomplex of a simplex");
face(toList((set v)-(set vert F)),C0))

-*
C=dCP(3,QQ[x_0..x_4])
F=C.fc_1_0
complement F
cC=complement C
dcC=dualize cC
cC.fc_2
dcC.fc_1
C.fc_1
coordinates dcC.fc_1_0
coordinates cC.fc_1_0
*-

---------------------------------------------------------------------------------


-*
R=QQ[x_0..x_15]
addCokerGrading(R)
C0=simplex(R)
boundaryOfPolytope(C0)


A=matrix {{-1, -1, -1, -1}, {1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0},{0,0,0,1}}
R=QQ[x_0..x_4]
addCokerGrading(R,A)
C0=simplex(R)


R=QQ[x_0..x_4]
addCokerGrading(R)
C0=simplex(R)

I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)

C=idealToComplex(I)
embeddingComplex C
idealToComplex(I,C0)
complexToIdeal(C)
cC=idealToCoComplex(I,C0)
cC==complement C
I==coComplexToIdeal(cC)
dualize cC

R=QQ[x_0..x_5]
C=boundaryCyclicPolytope(3,R)
I=complexToIdeal(C)
betti(cc=res I)
cc.dd_2


I=ideal(product (entries vars R)#0)
C=idealToComplex(I)
--fc C
cC=idealToCoComplex(I)
--fc cC
dualize cC

*-


--------------------------------------------------------------------------------------

-->export
selectFaces=method()
selectFaces(Complex,Function):=(C,f)->(
L1:={};
L2:={};
q:=-1;
qq:=0;
while q<=edim(C) do (
  qq=0;
  L2={};
  while qq<#(C.fc_q) do (
    if f(C.fc_q_qq)==true then L2=append(L2,C.fc_q_qq);
  qq=qq+1);
  L1=append(L1,L2);
q=q+1);
complex(C.simplexRing,L1))


isLinkFace=method()
isLinkFace(Face,Face,Complex):=(G,F,C)->(
if dim(intersectFaces(F,G))>-1 then return(false);
q:=0;
qq:=0;
gf:=set vert(F) + set vert(G);
while q<=edim(C) do (
  qq=0;
  while qq<#(C.fc_q) do (
    if (set vert(C.fc_q_qq))==gf then (return(true));
  qq=qq+1);
q=q+1);
false)
--link(F,C)

-- > export
link=method()
link(Face,Complex):=(F,C)->(
selectFaces(C,j->isLinkFace(j,F,C)))





-- caveat: this is really a cocomplex in C, fix this
openStar=method()
openStar(Face,Complex):=(F,C)->(
selectFaces(C,j->isSubface(F,j)))


isClosedStarFace=method()
isClosedStarFace(Face,Face,Complex):=(G,F,C)->(
q:=0;
qq:=0;
gf:=set vert(F) + set vert(G);
while q<=edim(C) do (
  qq=0;
  while qq<#(C.fc_q) do (
    if (set vert(C.fc_q_qq))==gf then (return(true));
  qq=qq+1);
q=q+1);
false)


closedStar=method()
closedStar(Face,Complex):=(F,C)->(
selectFaces(C,j->isClosedStarFace(j,F,C)))

-*
R=QQ[x_0..x_4]
C=boundaryOfPolytope simplex(R)
F=C.fc_0_0
link(F,C)
closedStar(F,C)
F=C.fc_1_0
link(F,C)
closedStar(F,C)

R=QQ[x_0..x_4]
I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
C=idealToComplex I
F=C.fc_0_0
link(F,C)
closedStar(F,C)
F=C.fc_1_0
link(F,C)
closedStar(F,C)

R=QQ[x_0..x_4]
I=ideal(x_0*x_1*x_2,x_3*x_4)
C=idealToComplex I
F=C.fc_0_0
link(F,C)
closedStar(F,C)
F=C.fc_1_0
link(F,C)
closedStar(F,C)
F=C.fc_2_0
link(F,C)
closedStar(F,C)

*-

---------------------------------------------------------------------------------------



possibleDenominators=method()
possibleDenominators(List,List):=(fc,v)->(
degreeList:=sort(apply(fc,j->(degree j)#0));
maxDegree:=max degreeList;
L:={};
for q from 1 to maxDegree do (
  L=append(L,select(subsets(v,q),j->dividesGenerator(j,fc)));
);
L);

dividesGenerator=method()
dividesGenerator(List,List):=(m,L)->(
pm:=product m;
for q from 0 to #L-1 do (
  if denominator(L#q/pm)==1 then return(true);
);
false)

--> export
possibleDenominators(Ideal):=(I)->(
possibleDenominators((entries mingens I)#0,(entries vars ring I)#0))

--> export
possibleDenominators(Ideal,List):=(I,v)->(
possibleDenominators((entries mingens I)#0,v))

-*
R=QQ[x_0..x_4]
I=ideal(x_0*x_1*x_2,x_3*x_4)
possibleDenominators(I)
possibleDenominators(I,{x_0,x_1,x_3})
C=idealToComplex I
F=C.fc_0_0
*-


deformationsFace=method()
deformationsFace(Face,Complex):=(F,C)->(
I:=symbol I;
if member(symbol ideal,keys C)==false then (
  I=complexToIdeal C;
) else (
  I=C.ideal;
);
deformationsFace(F,C,I));



faceToIndices=method()
faceToIndices(Face):=(F)->(
R:=(ofComplex F).simplexRing;
vt:=vert F;
apply(vt,j->findIndexOfVertex(j,R))
)

listToIndices=method()
listToIndices(List):=(F)->(
R:=ring(F#0);
apply(F,j->findIndexOfVertex(j,R))
)


findIndexOfVertex=method()
findIndexOfVertex(RingElement,PolynomialRing):=(m,R)->(
v:=(entries vars R)#0;
for qq from 0 to #v-1 do (
  if m==v#qq then return(qq);
))

--findIndexOfVertex(x_1,R)

--faceToIndices(F)



deformationsFace(Face,Complex,Ideal):=(F,C,I)->(
if member(symbol deform,keys F)==true then (
  if (F.deform)#0===C then return((F.deform)#1);
  --return((F.deform)#1);
);
if dim F==-1 then return({});
lk:=link(F,C);
vlk:=vert lk;
mg:=gens I;
pden:=possibleDenominators(I,vlk);
pden=select(pden,j->(#j>0));
pden=apply(join toSequence pden,j->vector (exponents product(j))#0);
A:=C.simplexRing.grading;
idxF:=faceToIndices(F);
defs:={};
for j from 0 to #pden-1 do (
  H0vF:=globalSections(A,pden#j,idxF);
  defs=join(defs,apply(H0vF,j->firstOrderDeformation(mg,j)));
);
L:=select(defs,j->(dim j>0));
F.deform={C,L};
L)

-*
R=QQ[x_0..x_4]
I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
C1=idealToComplex I
F=C1.fc_0_1
lkF=link(F,C1)
L=deformationsFace(F,C1)

*-

applyC(Complex,Function):=(C,f)->(
apply(fc C,L1->apply(L1,jj->f(jj))))


deform=method()
deform(Complex):=(C)->(
if member(symbol deform,keys C)==true then (return(C.deform));
L:={};
applyC(C,F->(
defs:=deformationsFace(F,C);
L=setJoin(L,defs);
));
L=setJoin(L,trivialDeformations(C));
C.deform=L;
L)
--deform(C1)

setJoin=method()
setJoin(List,List):=(L,F)->(
q:=0;
L1:=L;
while q<#F do (
  L1=setAppend(L1,F#q);
q=q+1);
L1)

----------------------------------------------------------
-- save deformation data

saveDeformations=method()
saveDeformations(Complex,String):=(C,fn)->(
defs:=C.deform;
S:=apply(apply(defs,bigTorusDegree),j->(entries transpose ((toList j)#0))#0);
F := openOut(fn);
F<<toString S<<endl;
close F;
defs)
--saveDeformations(C1,"defsC23.m2")

loadDeformations=method()
loadDeformations(Complex,String):=(C,fn)->(
F := openIn(fn);
S:=value(get(F));
mg:=gens complexToIdeal C;
C.deform=apply(S,j->firstOrderDeformation(mg,vector j)))

--loadDeformations(C1,"defsC23.m2")



--------------------------------------------------------------------------------------

-- trivial deformations

trivialDeformations=method()

trivialDeformations(Matrix):=(A)->(
n:=rank target A;
M:=(ring A)^n;
L:={};
for j from 0 to -1+n do (
  L=setAppend(L,removeFromList(globalSections(A,M_j),0));
);
L);

trivialDeformations(Ideal,Matrix):=(I,A)->(
n:=rank target A;
M:=(ring A)^n;
L:={};
for j from 0 to -1+n do (
  L=join(L,removeFromList(globalSections(A,M_j),0_M));
);
mg:=gens I;
--L=apply(L,j->vector (entries(transpose j))#0);
L=apply(L,j->firstOrderDeformation(mg,j)));

removeFromList=method()
removeFromList(List,Thing):=(L,m)->(
L1:={};
for j from 0 to #L-1 do (
  if L#j!=m then L1=append(L1,L#j);
);
L1)


trivialDeformations(Complex):=(C)->(
if member(symbol trivialDeformations,keys C)==true then (
  return(C.trivialDeformations)
);
R:=simplexRing C;
A:=grading C;
I:=symbol I;
if member(symbol ideal,keys C)==false then (
  I=complexToIdeal C;
) else (
  I=C.ideal;
);
tdef:=trivialDeformations(I,A);
C.trivialDeformations=tdef;
tdef);

--trivialDeformations C1


-- global sections

globalSectionsPPn=method()
globalSectionsPPn(Vector):=(v)->(
M:=entries v;
n:=#M;
d:=sum(M);
g:=symbol g;
R:=QQ[g_1..g_n];
L:=apply((entries basis(d,R))#0,monomialToVector);
apply(L,j->j-v));

globalSectionsPPn(Vector,ZZ):=(v,d)->(
M:=entries v;
n:=#M;
g:=symbol g;
R:=QQ[g_1..g_n];
L:=apply((entries((product toList(g_1..g_n))*basis(d-n,R)))#0,monomialToVector);
L);



monomialToVector=method()
monomialToVector(RingElement):=(m)->(
vector((exponents(m))#0))

--globalSectionsPPn(b)

globalSectionsPPn(Vector,List):=(v,var)->(
M:=entries v;
n:=#M;
d:=sum(M);
v1:=selectCoordinates(v,var);
L:=globalSectionsPPn(v1,d);
L=apply(L,j->extendCoordinates(j,var,n));
apply(L,j->j-v));




globalSectionsPPn(List,Vector):=(degs,v)->(
M:=entries v;
n:=#M;
d:=sum toList(apply(0..n-1,j->degs#j*M#j));
g:=symbol g;
R:=QQ[g_1..g_n,Degrees=>degs];
--print(d,basis(d,R));
L:=apply((entries basis(d,R))#0,monomialToVector);
apply(L,j->j-v));

--degs={1,1,2,2,3}
--globalSectionsPPn(degs,vector {0,0,1,1,0})



globalSectionsPPn(List,Vector,List):=(degs,v,var)->(
M:=entries v;
n:=#M;
d:=sum toList(apply(0..n-1,j->degs#j*M#j));
v1:=selectCoordinates(v,var);
degs1:=selectCoordinates(degs,var);
L:=globalSectionsPPn(degs1,v1,d);
L=apply(L,j->extendCoordinates(j,var,n));
apply(L,j->j-v));

globalSectionsPPn(List,Vector,ZZ):=(degs,v,d)->(
M:=entries v;
n:=#M;
g:=symbol g;
R:=QQ[g_1..g_n,Degrees=>degs];
d1:=sum degs;
L:=apply((entries((product toList(g_1..g_n))*basis(d-d1,R)))#0,monomialToVector);
L);

--globalSectionsPPn(degs,vector {0,0,1,1,0},{1,2})




-*
A=raysPPn(4)
b=vector {2,3,0,0,0}
globalSections(A,b,{2,3})
b=vector {1,1,0,0,0}
globalSections(A,b,{2,3,4})
globalSectionsPPn(b,6)
*-

selectCoordinates=method()
selectCoordinates(Vector,List):=(v,var)->(
L:=entries v;
vector apply(var,j->L#j))

selectCoordinates(List,List):=(L,var)->(
apply(var,j->L#j))

extendCoordinates=method()
extendCoordinates(Vector,List,ZZ):=(v,var,n)->(
L:={};
jj:=0;
for j from 0 to n-1 do (
  if member(j,var)==false then (
    L=append(L,0);
  ) else (
    L=append(L,v_jj);
    jj=jj+1;
  );
);
vector L)

--extendCoordinates(vector {3,4},{2,7},8)

--> export
globalSections=method()
globalSections(Matrix,Vector):=(A,v)->(
if A==raysPPn(-1+rank target A) then return(globalSectionsPPn(v));
degs:=prepend(1,(entries(-A))#0);
if A==raysPPn(degs) then return(globalSectionsPPn(degs,v));
le:=linesEquations(A,v);
be:=boundaryEquations(A);
C:=posHull(be,le);
dC:=-(C#"dualgens")#0;
L:=apply(entries transpose dC,j->cutRay vector j);
Lv:=apply(L,j->sub(j,ZZ)-v);
preLv:=apply(Lv,j->preImage(A,j));
vt:=joinVectors(preLv);
P:=convexHull vt;
lP:=latticePoints P;
apply(lP,j->matrixToVector(A*j)));

-*
A=matrix {{1, 0}, {0, 1}, {-1, -1}}
b=vector {2,0,0}
globalSections(A,b)

A=matrix {{1, 0}, {0, 1}, {-1, -1},{1,1}}
b=vector {2,2,0,0}
globalSections(A,b)

*-


globalSections(Matrix,Vector,List):=(A,v,var)->(
if A==raysPPn(-1+rank target A) then return(globalSectionsPPn(v,var));
degs:=prepend(1,(entries(-A))#0);
if A==raysPPn(degs) then return(globalSectionsPPn(degs,v,var));
cL:=sort toList(set(0..-1+rank target A) - set var);
le:=linesEquations(A,v);
be:=boundaryEquations(A);
for j from 0 to #cL-1 do (
  be=be|(-be_{cL#j});
);
C:=posHull(be,le);
dC:=-(C#"dualgens")#0;
L:=apply(entries transpose dC,j->cutRay vector j);
Lv:=apply(L,j->sub(j,ZZ)-v);
preLv:=apply(Lv,j->preImage(A,j));
vt:=joinVectors(preLv);
P:=convexHull vt;
-- this is needed for a bug in OldPolyhedra
if dim(P)==0 then (
  lP:={vertices P};
) else (
  lP=latticePoints P;
);
apply(lP,j->matrixToVector(A*j)));

-*
A=matrix {{1, 0}, {0, 1}, {-1, -1}}
b=vector {1,1,0}
globalSections(A,b,{0,1,2})
globalSections(A,b,{1,2})
globalSections(A,b,{1})
globalSections(A,b,{2})
*-

matrixToVector=method()
matrixToVector(Matrix):=(M)->(
vector (entries transpose M)#0)
--matrixToVector(matrix {{1},{2}})

joinVectors=method()
joinVectors(List):=(L)->(
LM:=apply(L,vectorToMatrix);
S:=apply(LM,ring);
if member(QQ,S)==true then LM=apply(LM,j->sub(j,QQ));
M:=LM#0;
for j from 1 to #L-1 do M=M|(LM#j);
M)

-*
v1=vector {1/2,1}
v2=vector {1,1}
joinVectors({v1,v2})
*-

cutRay=method()
cutRay(Vector):=(v)->(
L:=tolist v;
vector toList apply(0..#L-2,j->L#j/L#(#L-1)))

-*
cutRay(vector{1,2,0,-1})
*-

vectorToMatrix=method()
vectorToMatrix(Vector):=(v)->((toList v)#0)

linesEquations=method()
linesEquations(Matrix,Vector):=(A,v)->(
kA:=gens ker transpose A;
rh:=-transpose((transpose kA)*vectorToMatrix(v));
kA||rh)
-*
A=matrix {{1, 0}, {0, 1}, {-1, -1},{1,1}}
v=vector {2,0,0,0}
linesEquations(A,v)
*-

boundaryEquations=method()
boundaryEquations(Matrix):=(A)->(
n:=rank target A;
B:=id_(ZZ^n);
nv:=matrix {toList apply(0..n-1,j->0_ZZ)};
B||nv)

-*
A=matrix {{1, 0}, {0, 1}, {-1, -1}}
boundaryEquations(A)
*-

preImage=method()
preImage(Matrix,Vector):=(A,b)->(
n:=rank source A;
--print(ring A,ring ((toList(b))#0));
kh:=gens ker(A|sub((toList(b))#0,ring A));
if kh==0 then return(false);
--sub((kh_(n,0))^(-1)*sub(kh^(toList (0..n-1)),QQ),ZZ)
v:=-(kh_(n,0))^(-1)*kh^(toList (0..n-1));
vector (entries transpose v)#0)

-*
A=matrix {{1, 0}, {0, 1}, {-1, -1}}
b=vector {-2,2,0}
v=preImage(A,b)
A*v
*-

---------------------------------------------------------------------------------------
-- cones

hull=method(Options=>{file=>null})
hull(List):=opts->(L)->(
-- use "Convex" if available
if ((options SRdeformations).Configuration)#"UseConvex"==true then return(hullConvex(L,opts));
vA:=joinVectors(L);
P:=posHull vA;
d:=P#"dimension of the cone";
embdim:=P#"ambient dimension";
if embdim!=d or P#"dimension of lineality space">0 then error("expected cone of full dimension without lineality space");
A:=sub(transpose rays(P),ZZ);
Adual:=-sub(transpose rays dualCone P,ZZ);
n:=rank target A;
dn:=rank target Adual;
y := getSymbol "y";
R:=QQ[y_0..y_(n-1)];
R.grading=A;
v := getSymbol "v";
Rdual:=QQ[v_0..v_(dn-1)];
Rdual.grading=-Adual;
Cl:=newEmptyComplex(R);
dCl:=newEmptyComplex(Rdual);
--fc:=faces(1,P);
--fc=apply(fc,j->sub(transpose vertices j,ZZ));
--fc=apply(fc,j->face(matrixToVarlist(j,R),Cl));
--L1:={};
--for j from -1 to d-1 do (L1=append(L1,{}));
--L1=append(L1,{face((entries vars R)#0,Cl)});
--addFacetDataToComplex(Cl,L1);
--L1:={{},{face({},Cl,-1,0)}};
L1:={{face({},Cl,-1,0)}};
for j from 1 to d-1 do (
 fc:=faces(d-j,P);
 fc=apply(fc,j->sub(transpose rays j,ZZ));
 fc=apply(toList(0..(#fc-1)),jj->face(matrixToVarlist(fc#jj,R),Cl,j-1,jj));
 L1=append(L1,fc);
);
L1=append(L1,{face((entries vars R)#0,Cl,d-1,0)});
addFaceDataToComplex(Cl,L1);
Cl.polytopalFacets=L1#(#L1-2);
Cl.noBoundary=false;
Cl.isPolytope=true;
dCl.isPolytope=true;
dCl.dualComplex=Cl;
Cl.dualComplex=dCl;
Cl.edim=Cl.edim-1;
dCl.edim=dCl.edim-1;
Cl)

hull(String):=opts->(fn)->(
{A,Adual,fcL}:=toSequence readPosHullFaces(fn);
makeHull(A,Adual,fcL))

protect toFile

hullConvex=method(Options=>{file=>null})
hullConvex(List):=opts->(L)->(
{A,Adual,fcL}:=toSequence mPosHullFacesAndDuals(L,toFile=>opts.file);
makeHull(A,Adual,fcL))

makeHull=method()
makeHull(Matrix,Matrix,List):=(A,Adual,fcL)->(
n:=rank target A;
dn:=rank target Adual;
y:=symbol y;
R:=QQ[y_0..y_(n-1)];
R.grading=A;
v:= symbol v;
Rdual:=QQ[v_0..v_(dn-1)];
Rdual.grading=Adual;
Cl:=newEmptyComplex(R);
dCl:=newEmptyComplex(Rdual);
fcL1:=apply(toList(0..(#fcL-1)),j->apply(toList(0..(#(fcL#j)-1)),jj->(
    F:=face(numberFace(fcL#j#jj#0,R),Cl,j-1,jj);
    dF:=face(numberFace(fcL#j#jj#1,Rdual),dCl,#fcL-2-j,jj);
    F.dualFace=dF;
    dF.dualFace=F;
    {F,dF})
));
fcL2:=apply(fcL1,j->apply(j,jj->jj#0));
dfcL1:=apply(fcL1,j->apply(j,jj->jj#1));
dfcL1=apply(toList(1..#dfcL1),j->dfcL1#(#dfcL1-j));
addFaceDataToComplex(Cl,fcL2);
addFaceDataToComplex(dCl,dfcL1);
Cl.polytopalFacets=Cl.fc_(-1+dim Cl);
Cl.isPolytope=true;
dCl.isPolytope=true;
dCl.dualComplex=Cl;
Cl.dualComplex=dCl;
Cl.edim=Cl.edim-1;
dCl.edim=dCl.edim-1;
Cl)

----------------------------------------------------------------------------------------

convHull=method(Options=>{file=>null})
convHull(List):=opts->(L)->(
-- use "Convex" if available
if ((options SRdeformations).Configuration)#"UseConvex"==true then return(convHullConvex(L,opts));
vA:=joinVectors(L);
P:=convexHull vA;
d:=P#"dimension of polyhedron";
embdim:=P#"ambient dimension";
if embdim!=d or P#"dimension of lineality space">0 then error("expected polytope of full dimension without lineality space");
-- put QQ-ZZ test here !!!!!!!!!!!!
A:=sub(transpose vertices(P),ZZ);
n:=rank target A;
y := getSymbol "y";
R:=QQ[y_0..y_(n-1)];
R.grading=A;
Cl:=newEmptyComplex(R);
--fc:=faces(1,P);
--fc=apply(fc,j->sub(transpose vertices j,ZZ));
--fc=apply(fc,j->face(matrixToVarlist(j,R),Cl));
--L1:={};
--for j from -1 to d-1 do (L1=append(L1,{}));
--L1=append(L1,{face((entries vars R)#0,Cl)});
--addFacetDataToComplex(Cl,L1);
L1:={{face({},Cl,-1,0)}};
AdualL:={};
for j from 0 to d-1 do (
 fc:=faces(d-j,P);
 if j==d-1 then AdualL=apply(fc,j1->-1/((j1#"hyperplanes"#1)_(0,0))*(entries(j1#"hyperplanes"#0))#0);
 fc=apply(fc,j->sub(transpose vertices j,ZZ));
 fc=apply(toList(0..(#fc-1)),jj->face(matrixToVarlist(fc#jj,R),Cl,j,jj));
 L1=append(L1,fc);
);
L1=append(L1,{face((entries vars R)#0,Cl,d,0)});
addFaceDataToComplex(Cl,L1);
Cl.polytopalFacets=L1#(#L1-2);
Cl.noBoundary=false;
Cl.isPolytope=true;
--print(AdualL);
--Adual:=transpose vertices polar P;
--print(Adual);
Adual:=matrix AdualL;
dn:=rank target Adual;
v := getSymbol "v";
Rdual:=QQ[v_0..v_(dn-1)];
Rdual.grading=Adual;
dCl:=newEmptyComplex(Rdual);
dCl.isPolytope=true;
dCl.dualComplex=Cl;
Cl.dualComplex=dCl;
Cl)

convHull(String):=opts->(fn)->(
{A,Adual,fcL}:=toSequence readConvexHullFaces(fn);
makeConvHull(A,Adual,fcL))

convHullConvex=method(Options=>{file=>null})
convHullConvex(List):=opts->(L)->(
{A,Adual,fcL}:=toSequence mConvexHullFacesAndDuals(L,toFile=>opts.file);
makeConvHull(A,Adual,fcL))

makeConvHull=method()
makeConvHull(Matrix,Matrix,List):=(A,Adual,fcL)->(
n:=rank target A;
dn:=rank target Adual;
y:=symbol y;
R:=QQ[y_0..y_(n-1)];
R.grading=A;
v:= symbol v;
Rdual:=QQ[v_0..v_(dn-1)];
Rdual.grading=Adual;
Cl:=newEmptyComplex(R);
dCl:=newEmptyComplex(Rdual);
fcL1:=apply(toList(0..(#fcL-1)),j->apply(toList(0..(#(fcL#j)-1)),jj->(
    F:=face(numberFace(fcL#j#jj#0,R),Cl,j-1,jj);
    dF:=face(numberFace(fcL#j#jj#1,Rdual),dCl,#fcL-2-j,jj);
    F.dualFace=dF;
    dF.dualFace=F;
    {F,dF})
));
fcL2:=apply(fcL1,j->apply(j,jj->jj#0));
dfcL1:=apply(fcL1,j->apply(j,jj->jj#1));
dfcL1=apply(toList(1..#dfcL1),j->dfcL1#(#dfcL1-j));
addFaceDataToComplex(Cl,fcL2);
addFaceDataToComplex(dCl,dfcL1);
Cl.polytopalFacets=Cl.fc_(-1+dim Cl);
Cl.isPolytope=true;
dCl.isPolytope=true;
dCl.dualComplex=Cl;
Cl.dualComplex=dCl;
Cl)

numberFace=method()
numberFace(List,PolynomialRing):=(L,R)->(
apply(L,j->R_(j-1)))
--numberFace({1,3},QQ[x_0..x_6])

-*
installPackage "SRdeformations"
--L={vector {1,0,0},vector {0,1,0},vector {0,0,1},vector {-1,-1,-1}}
--L={vector {1,0,0,0,0},vector {0,1,0,0,0},vector {0,0,1,0,0},vector {0,0,0,1,0},vector {0,0,0,0,1},vector {-1,-1,-1,-1,-1}}
L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}}
P=convHull(L,file=>"tst2.txt")
convHull "tst2.txt"

*-



matrixToVarlist=method()
matrixToVarlist(Matrix,PolynomialRing):=(B,R)->(
A:=R.grading;
v:=(entries vars R)#0;
L:={};
for j from 0 to -1+rank target B do (
  for jj from 0 to #v-1 do (
     --print(A^{jj},B^{j});
     if A^{jj}==B^{j} then L=append(L,v#jj);
  );
);
L)

-*
L={vector {1,0},vector {0,1},vector {-1,-1},vector {1,1}}
--L={vector {1,0},vector {0,1},vector {-1,-1}}
L={vector {1,0},vector {0,1},vector {-1,0},vector {0,-1}}
P=convHull L
dP=boundaryOfPolytope P
fc dP

R=QQ[x_0..x_3]
I=ideal(x_0*x_3,x_1*x_2)
C=idealToComplex I
defs=deform C
defs=join(defs,{firstOrderDeformation(I,vector {0,1,-1,0}),
firstOrderDeformation(I,vector {0,-1,1,0}),
firstOrderDeformation(I,vector {-1,0,0,1}),
firstOrderDeformation(I,vector {1,0,0,-1})})
defslattice=apply(defs,j->preImage(R.grading,bigTorusDegree j))
P=convHull defslattice
dP=boundaryOfPolytope P
--computeFacesOfCompleteComplex dP


P=convexHull joinVectors defslattice
P.polytopalFacets
fP=faces(1,P)
fP=apply(fP,j->sub(transpose vertices j,ZZ))
Rv=QQ[v_0..v_7]
Rv.grading=sub(transpose vertices P,ZZ)
fP=apply(fP,j->matrixToVarlist(j,Rv))

*-




-------------------------------------------------------------------
-- deformations polytope, tropical faces of it

PT1=method(Options=>{file=>null})
PT1(Complex):=opts->(C)->(
A:=C.grading;
defs:=deform C;
defsX:=apply(defs,j->bigTorusDegree j);
defslattice:=apply(defsX,j->preImage(A,j));
convHull(defslattice,opts));
--PT1(C1)

-*
PT1(Complex):=(C)->(
A:=C.grading;
defs:=deform C;
defsX:=apply(defs,j->bigTorusDegree j);
defslattice:=apply(defsX,j->preImage(A,j));
convexHull joinVectors defslattice);
*-

tropDef=method()
tropDef(Complex,Complex):=(C,P)->(
defs:=deform C;
ge:=(entries (defs#0).generators)#0;
imgdefs:=apply(defs,j->apply(ge,jj->mapByLaurent(j,jj)));
--patt:=apply(apply(defs,toHom),toPattern);
patt:=apply(imgdefs,toPattern);
A:=C.grading;
ldefs:=apply(defs,j->preImage(A,j.bigTorusDegree));
B:=P.grading;
Bv:=apply(entries B,vector);
var:=(entries vars P.simplexRing)#0;
varPatt:=apply(Bv,j->latticeToPattern(j,ldefs,patt));
--print(var,varPatt);
L:={};
minimalTropicalFaces:={};
L1:={};
for j from -1 to edim(P) do (
  L1={};
  for jj from 0 to #(P.fc_j)-1 do (
    if isSuperfaceList(minimalTropicalFaces,P.fc_j_jj)==true then (
      L1=append(L1,P.fc_j_jj);
    ) else (
      if isTropical(P.fc_j_jj,var,varPatt)==true then (
          L1=append(L1,P.fc_j_jj);
          minimalTropicalFaces=append(minimalTropicalFaces,P.fc_j_jj);
          --print minimalTropicalFaces;
      );
    );
  );
L=append(L,L1));
coComplex(P.simplexRing,L,P.dualComplex.simplexRing))
--tropDef(C,defC)


isSuperfaceList=method()
isSuperfaceList(List,Face):=(L,F)->(
tst:=false;
j:=0;
while j<#L and tst==false do (
  if isSubface(L#j,F)==true then tst=true;
j=j+1);
tst)

isTropical=method()

isTropical(Face,List,List):=(F,var,varPatt)->(
if #vert(F)==0 then return(false);
-- !!!!!!!!!!! switch to lattice points instead of vertices;
contractAND(faceToPattern(F,var,varPatt)))
--isTropical(F,var,varPatt)

toPattern=method()
toPattern(Matrix):=(M)->(
apply((entries M)#0,j->j!=0))

toPattern(List):=(L)->(
apply(L,j->j!=0))

contractPattern=method()
contractPattern(List):=(L)->(
tst:=L#0;
for j from 1 to #L-1 do (
  tst=contractPair(tst,L#j);
);
tst)
--contractPattern(img)

contractPair=method()
contractPair(List,List):=(L1,L2)->(
apply(toList(0..#L1-1),j->(L1#j or L2#j)))
--contractPair({true,false},{false,false})

faceToPattern=method()
faceToPattern(Face,List,List):=(F,var,varPatt)->(
contractPattern(apply(vert F,j->variableToPattern(j,var,varPatt))))
--faceToPattern(F,var,varPatt)

contractAND=method()
contractAND(List):=(L)->(
tst:=L#0;
for j from 1 to #L-1 do (
  tst=tst and L#j;
  if tst==false then return(false);
);
tst)



latticeToPattern=method()
latticeToPattern(Vector,List,List):=(v,ldefs,patt)->(
patt#((select(0..#ldefs-1,j->(ldefs#j==v)))#0))
--latticeToPattern(Bv#0,ldefs,patt)

variableToPattern=method()
variableToPattern(Thing,List,List):=(v,var,varPatt)->(
j:=(select(0..#var-1,jj->(var#jj==v)))#0;
varPatt#j)
--variableToPattern(var#0,var,varPatt)

-*
installPackage "SRdeformations"
R=QQ[x_0..x_3]
C=idealToComplex ideal(x_0*x_1,x_2*x_3)
defC=PT1 C
TdefC=tropDef(C,defC)
dualize TdefC

defs=deform C;
patt=apply(apply(defs,toHom),toPattern)
A=C.grading
ldefs=apply(defs,j->preImage(A,j.bigTorusDegree))
B=TdefC.grading
Bv=apply(entries B,vector)
var=(entries vars TdefC.simplexRing)#0
varPatt=apply(Bv,j->latticeToPattern(j,ldefs,patt))
*-

--------------------------------------------------------------------
-- saving and loading complexes to files (todo)

-*
saveComplex=method()
saveComplex(Complex,String):=(C,filename)->(
F := openOut(filename|".txt");
F<<"new Complex from {";
F<<"symbol simplexRing =>"<<toString(C.simplexRing)<<","<<endl;
F<<"symbol dim=>"|toString(C.dim)<<","<<endl;
F<<"symbol edim=>"|toString(C.edim)<<","<<endl;
F<<"symbol isEquidimensional=>"|toString(C.isEquidimensional)<<","<<endl;
F<<"symbol isSimp=>"|toString(C.isSimp)<<","<<endl;
F<<"symbol fvector=>"|toString(C.fvector)<<","<<endl;
F<<"symbol grading=>"|toString(C.grading)<<"};"<<endl;
close F;
filename)

saveComplex(C,"testc")

toString(Face):=(F)->(
S:="";
S=S|
)
toString F

loadComplex=method()
loadComplex(String):=(filename)->(
F := openIn(filename|".txt");
value get(F))

loadComplex("testc")



*-

------------------------------------------------------------
-- homology of a complex

homology(Complex):=opts->(C)->(
if ((options SRdeformations).Configuration)#"UseConvex"==false then error("Only available via ConvexInterface and MapleInterface");
Lc:=apply(fc C,j->apply(j,coordinates));
mHomology Lc)



--------------------------------------------------------------------

-*
Copyright (C) [2009] [Janko Boehm]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-


beginDocumentation()

doc ///
  Key
    SRdeformations
  Headline
    Deformations of Stanley-Reisner rings and related computations.
  Description
    Text
      {\bf Overview:}
      
      The goal of this package is to provide a basic framework for deformations of Stanley-Reisner rings,
      the deformations polytope, its tropical subcomplex and tropical mirror symmetry in general.

      This is work in progress, many interesting pieces still missing and more testing is necessary.
      The efficiency and robustness of some of the pieces has to be improved.

      See the Macaulay 2 package repository for new versions of this file.

      All suggestions and contributions are welcome.

      {\bf Contents:}
      
      This package so far consists of the following parts:

      The first part provides a data structure for deformations of monomial ideals
      (including a toric grading).
      The class @TO FirstOrderDeformation@ stores (and computes the dimension of) a big torus graded 
      part of the vector space of first order deformations (specified by a Laurent monomial).

      The second part gives an implementation of (not necessarily simplicial) embedded
      complexes and co-complexes and their correspondence to monomial ideals. 
      The main focus here is on a flexible implementation, computing and 
      storing data only as soon as it is needed.
      See @TO Complex@, @TO CoComplex@, @TO Face@.

      The third part computes all first order deformations of Stanley-Reisner ideals using part one 
      and two via the results of Klaus Altmann and Jan Arthur Christophersen who reduce the problem to
      links of faces of simplicial complexes.
      See @TO deformationsFace@ and @TO deform@.

      The fourth part gives functions to compute the first order deformations polytope (see @TO PT1@)
      and its tropical subcocomplex (see @TO tropDef@).

      {\bf What' new:}

        {\it Jul 13, 2010 (Version 0.52)}
        
           Some changes to ensure compatibility with the current version of OldPolyhedra (1.1), in particular
           the method isSimplicial has been renamed to @TO isSimp@, as OldPolyhedra is now using the name
           name isSimplicial.


        {\it Jan 28, 2010 (Version 0.51)}
        
           Minor changes to ensure compatibility with the previous M2 version 1.2. Note that you will nevertheless
           need to install a more recent version of OldPolyhedra which also works with 1.2 (tested with 1.0.6).


        {\it Oct 24, 2009 (Version 0.50)}

           Added methods @TO (addCokerGrading,PolynomialRing,List)@ and @TO (raysPPn,List)@ to give the rays of the standard fan of weighted projective space.
           Fixed a bug in the multigrading.

        {\it Oct 18, 2009 (Version 0.49)}

           Fixed a problem in @TO convHull@ (correspondence of P.polytopalFacets with the vertices of the dual). This was only a problem when using OldPolyhedra.m2.

        {\it Oct 6, 2009 (Version 0.48)}

           Resolved a compatibility issue with the new version of the M2 package {\it OldPolyhedra}.


        {\it Oct 3, 2009 (Version 0.47)}

           Improvements to the documentation.

        {\it Sept 20, 2009 (Version 0.46)}

           Added a function @TO hull@ to compute the positive hull of a list of vectors.


        {\it Sept 8, 2009 (Version 0.45)}

           Adjusted the methods using ConvexInterface.m2 to version 0.25 which now
           indexes the vertices instead of using coordinates. This improves the performance substantially.

           Improved the computation of the dimension of a @TO FirstOrderDeformation@ using a much
           smaller linear system of equations.

           Fixed a bug in @TO (dim,Face)@.

           Exported the symbol @TO dualFace@ which stores the dual of a face to make this key available for the user.

           @TO convHull@ now stores in the faces their dimensions (see the key indices).

           Added an @TO Option@ @TO file@ to @TO convHull@ and @TO PT1@ to store the result in a file.
           @TO convHull@ applied to a @TO String@ reads a convex hull from a file.

           Improved speed of @TO globalSections@ when dealing with ideals in the Cox ring of projective space.

           Added an optional key deform to complexes storing the associated deformations, see @TO deform@.

           Added functions @TO saveDeformations@ and @TO loadDeformations@ to save to and load from a
           file the deformations associated to a @TO Complex@.
      
      {\bf Technical note:}
  
           In the future some of the methods and keys may change their name.

           At some point @TO Complex@ and @TO CoComplex@ will get a common ancestor.

      {\bf Installation:}

      @TO installPackage@("SRdeformations")

      {\bf Options:}

      Computations with polyhedra are used in this package so far in by following functions:

      @TO convHull@ -- The convex hull complex

      @TO globalSections@ -- The global sections of a toric divisor

      which again are called by @TO PT1@, @TO deform@ and @TO deformationsFace@.
      If @TO deform@ and @TO deformationsFace@ are used for @TO Complex@es inside a standard simplex,
      i.e., Stanley-Reisner ideals with the standard projective space grading, 
      then they don't rely on polyhedra computations.

      There are two choices of M2 packages to be called for computations with polyhedra:

      @TO OldPolyhedra@:

      Works without additional configuration, but is not very fast.

      It uses the M2 package {\it FourierMotzkin}.

      To use {\it OldPolyhedra} do

      @TO loadPackage@("SRdeformations",Configuration=>\{"UseConvex"=>false\})

      This is the standard configuration, i.e., it is synonymous to

      @TO loadPackage@("SRdeformations")

      {\it ConvexInterface}:

      This package has to be installed first, see its documentation for this.

      It calls the Maple package Convex and is faster than OldPolyhedra, hence the preferable choice.
      If you want to do non-trivial examples you have to go for it.

      To use it type

      @TO loadPackage@("SRdeformations",Configuration=>\{"UseConvex"=>true\})


      Note that you can change the standard option by editing the file init-SRdeformations.m2 in the .Macaulay directory in your home directory
      (it will be overwritten when you reinstall a newer version of the package, but there will be a backup).


      {\bf Examples:}

      @TO mirrorSphere@ uses the above functions to compute the mirror sphere of a Calabi-Yau.


      The author was supported by DFG (German Research Foundation) through Grant BO3330/1-1. 
       
///



---------------------------------------------------------------------
-- various

doc ///
  Key
    cokerElement
    (cokerElement,Vector,Matrix)
    (cokerElement,Matrix,Matrix)
  Headline
    Gives an element in the cokernel of a matrix.
  Usage
    cokerElement(v,A)
  Inputs
    v:Vector
    A:Matrix
  Outputs
    :Thing
  Description
   Text
        Gives the element in the @TO cokernel@ of A represented by v.
        The @TO Vector@ v has to lie in @TO target@ of A.

   Example
     A= matrix {{-1, -1, -1}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
     c1=cokerElement(vector {1,1,0,0},A)
     c2=cokerElement(vector {1,-1,0,0},A)
     c2==(0_(class c2))
     iszero c2
  SeeAlso
     inducedMap
///


doc ///
  Key
    iszero
    (iszero,Thing)
  Headline
    Tests whether something is zero.
  Usage
    iszero(c)
  Inputs
    c:Thing
  Outputs
    :Boolean
  Description
   Text
        Makes the test c == 0_@TO class@ c.
        Why does @TO zero@ not work for elements in the cokernel of a matrix?

   Example
     A= matrix {{-1, -1, -1}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
     c1=cokerElement(vector {1,1,0,0},A)
     c2=cokerElement(vector {1,-1,0,0},A)
     c2==(0_(class c2))
     iszero c2
     iszero c1
  SeeAlso
     zero
     cokerElement
///



doc ///
  Key
    vectorToMonomial
    (vectorToMonomial,Vector,PolynomialRing)
  Headline
    Converts an exponent vector into a monomial.
  Usage
    vectorToMonomial(v,R)
  Inputs
    v:Vector
    R:PolynomialRing
  Outputs
    :RingElement
  Description
   Text
        Converts the exponent vector v into a monomial in the variables of R.
        The number of variables of R has to match the length of v.

   Example
     R=QQ[x_0..x_4]
     m=vector {1,2,1,0,0}
     vectorToMonomial(m,R)
  SeeAlso
     laurent
///

doc ///
  Key
    laurent
    (laurent,Vector,PolynomialRing)
    (laurent,FirstOrderDeformation)
  Headline
    Converts an exponent vector or a deformation into a Laurent monomial.
  Usage
    laurent(v,R)
    laurent(f)
  Inputs
    v:Vector
    f:FirstOrderDeformation
    R:PolynomialRing
  Outputs
    :RingElement
  Description
   Text
        Converts the exponent vector v into a Laurent monomial in the variables of R.
        The result lies in @TO frac@ R.
        The number of variables of R has to match the length of v.
        
        If given a @TO FirstOrderDeformation@ it returns the corresponding laurent monomial.
        
   Example
     R=QQ[x_0..x_4]
     m=vector {1,-2,1,0,0}
     laurent(m,R)
   Text
   
   Example
     R=QQ[x_0..x_4]
     addCokerGrading(R);
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     mg=mingens I;
     f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
     laurent f
  SeeAlso
     laurent
///



-----------------------------------------------------------------------------------------
-- first order deformations of monomial ideals

doc ///
  Key
    FirstOrderDeformation
  Headline
   The class of all first order deformations of monomial ideals.
  Description
   Text
        The class of all first order deformations of reduced monomial ideals.
        Elements represent a big torus (i.e., torus on the variables of a @TO PolynomialRing@) 
        graded part of the vector space of first order deformations.  By results of Klaus Altmann 
        and Jan Arthur Christophersen the dimension is either 0 or 1 (for manifolds, though this is
        not required by the implementation).
        
        First order deformations can be created by @TO firstOrderDeformation@ by
        specifying a matrix with generators of a reduced monomial ideal and the exponent vector
        of a Laurent-monomial (i.e., a  big torus degree).
        

        {\bf Functions producing (sets of) first order deformations:}

        @TO deform@ -- Compute the deformations associated to a Stanley-Reisner complex.

        @TO deformationsFace@ -- Compute the deformations associated to a face

        @TO trivialDeformations@ -- Compute the trivial deformations

        @TO firstOrderDeformation@ -- Makes a first order deformation


        {\bf The data stored in a first order deformation f are}
        
        {\it f.gens}, a @TO matrix@ with generators of @TO source@ of the homomorphisms represented by f.
        
        {\it f.bigTorusDegree}, the exponent vector of the Laurent monomial.
                
        {\it f.degree}, the small torus (i.e., with respect to the grading added to R by @TO addCokerGrading@)
        degree of f.
                
        {\it f.isHomogeneous}, a @TO Boolean@ indicating if f.degree is zero.

        {\it f.relevantGens}, a @TO Matrix@ with those elements of f.gens which are relevant to the deformation f 
                              (i.e., those m which have numerator(m*laurent(f)) not in ideal(f.gens)).
        
        {\it f.relationsCoefficients}, @TO matrix@ of relations on coefficients of f.
        The rows correspond to the generators given in f.relevantGens.
        
        {\it f.parameters}, a @TO Matrix@ whose image is the @TO kernel@ of the @TO transpose@ of f.relationsCoefficients
                            extended by zeros for the elements of f.gens not in f.relevantGens.
                            The rows correspond to the generators given in f.gens.

        
        {\it f.dim}, the dimension of the f-graded part of the deformation space of @TO ideal@ f.gens.
        
        {\it f.isNonzero}, a @TO Boolean@ indicating whether f is non-zero.

        {\it f.isTrivial}, a @TO Boolean@ indicating whether f is trivial, i.e., @TO denominatorMonomial@ f has degree 1.
        
        For an example see @TO "Example first order deformation"@.

        This data can also be accessed by the methods listed below.
                
        @TO laurent@ represents f as a Laurent monomial, @TO toHom@ represents f as a homomorphism.
        
        @TO totalSpace@ computes the total space of f.
           
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        degree f
        dim f
        f1=firstOrderDeformation(mg,vector {-1,1,0,0,0})
        isTrivial f1
        f2=firstOrderDeformation(mg,vector {0,-1,-1,2,0})
        isNonzero f2
  SeeAlso
    simplexRing
    (target,FirstOrderDeformation)
    (source,FirstOrderDeformation)
    gensSource
    bigTorusDegree
    (numerator,FirstOrderDeformation)
    (denominator,FirstOrderDeformation)
    numeratorMonomial
    denominatorMonomial
    (degree,FirstOrderDeformation)
    (grading,FirstOrderDeformation)
    (isHomogeneous,FirstOrderDeformation)
    relationsCoefficients
    parameters
    (dim,FirstOrderDeformation)
    isNonzero
    isTrivial
    laurent
    toHom
    totalSpace
    trivialDeformations
  Caveat
        If we run into performance issues some of the redundant data will be removed, so
        for future compatibility access the data by the corresponding @TO method@ not via the @TO MutableHashTable@.
///


doc ///
  Key
    "Example first order deformation" 
  Headline
   Example accessing the data stored in a first order deformation.
  Description
   Text
        Example for accessing the data stored in a first order deformation:
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        f.gens
        f.bigTorusDegree
        simplexRing f
        target f
        source f
        numerator f
        denominator f
        bigTorusDegree f
        numeratorMonomial f
        degree f
        grading f
        isHomogeneous f
        relationsCoefficients f
        parameters f
        dim f
        isNonzero f
        isTrivial f

  SeeAlso
    simplexRing
    (target,FirstOrderDeformation)
    (source,FirstOrderDeformation)
    bigTorusDegree
    (numerator,FirstOrderDeformation)
    (denominator,FirstOrderDeformation)
    numeratorMonomial
    denominatorMonomial
    (degree,FirstOrderDeformation)
    (grading,FirstOrderDeformation)
    (isHomogeneous,FirstOrderDeformation)
    relationsCoefficients
    parameters
    (dim,FirstOrderDeformation)
    isNonzero
    isTrivial
    laurent
    toHom
    totalSpace
///


doc ///
  Key
    simplexRing
  Headline
    The underlying polynomial ring of a deformation or face or complex.
  Description
   Text
     The underlying polynomial ring of a deformation or face or complex.
///


doc ///
  Key
    (simplexRing,FirstOrderDeformation)
  Headline
    The underlying polynomial ring of a deformation or face.
  Usage
    simplexRing(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :PolynomialRing
  Description
   Text
     Returns the underlying @TO PolynomialRing@ of f.
     
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        simplexRing f
  SeeAlso
     FirstOrderDeformation
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    bigTorusDegree
    (bigTorusDegree,FirstOrderDeformation)
  Headline
    The big torus degree of a deformation.
  Usage
    bigTorusDegree(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Vector
  Description
   Text
     Returns the exponent vector of the Laurent monomial representing f.
     
   Example
        R=QQ[x_0..x_4]
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        bigTorusDegree f
  SeeAlso
     FirstOrderDeformation
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    (target,FirstOrderDeformation)
  Headline
    The target of a deformation.
  Usage
    target(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Module
  Description
   Text
     Returns the @TO target@ of the homomorphisms represented by f.
     
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        target f
  SeeAlso
     FirstOrderDeformation
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    (source,FirstOrderDeformation)
  Headline
    The source of a deformation.
  Usage
    source(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Module
  Description
   Text
     Returns the @TO source@ of the homomorphisms represented by f.
     
   Example
        R=QQ[x_0..x_4]
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        source f
  SeeAlso
     FirstOrderDeformation
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    (numerator,FirstOrderDeformation)
  Headline
    The numerator of a deformation as a vector.
  Usage
    numerator(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Vector
  Description
   Text
     Returns the numerator of a deformation as a vector.
     
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        numerator f
  SeeAlso
     FirstOrderDeformation
     numeratorMonomial
     denominatorMonomial
     (denominator,FirstOrderDeformation)
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    (denominator,FirstOrderDeformation)
  Headline
    The denominator of a deformation as a vector.
  Usage
    denominator(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Vector
  Description
   Text
     Returns the denominator of a deformation as a vector.
     
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        denominator f
  SeeAlso
     FirstOrderDeformation
     denominatorMonomial
     numeratorMonomial
     (numerator,FirstOrderDeformation)
     firstOrderDeformation
     addCokerGrading
///



doc ///
  Key
    numeratorMonomial
    (numeratorMonomial,FirstOrderDeformation)
  Headline
    The numerator monomial of a deformation.
  Usage
    numeratorMonomial(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :RingElement
  Description
   Text
     Returns the numerator monomial of a deformation.
     
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        numeratorMonomial f
  SeeAlso
     FirstOrderDeformation
     denominatorMonomial
     (denominator,FirstOrderDeformation)
     (numerator,FirstOrderDeformation)
     firstOrderDeformation
     addCokerGrading
///



doc ///
  Key
    denominatorMonomial
    (denominatorMonomial,FirstOrderDeformation)
  Headline
    The denominator monomial of a deformation.
  Usage
    denominatorMonomial(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :RingElement
  Description
   Text
     Returns the denominator monomial of a deformation.
     
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        denominatorMonomial f
  SeeAlso
     FirstOrderDeformation
     numeratorMonomial
     (denominator,FirstOrderDeformation)
     (numerator,FirstOrderDeformation)
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    (degree,FirstOrderDeformation)
  Headline
    The small torus degree of a deformation.
  Usage
    degree(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Thing
  Description
   Text
        Computes the small torus degree (i.e., with respect to the grading added to R = @TO simplexRing@ f by @TO addCokerGrading@)
        of f.   
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        degree f
  SeeAlso
    grading
    (isHomogeneous,FirstOrderDeformation)
    addCokerGrading
    firstOrderDeformation
///


doc ///
  Key
    grading
  Headline
    The grading of a first order deformation or complex or polynomial ring.
  Description
   Text
    The coker grading of a @TO FirstOrderDeformation@ or @TO Complex@ or @TO PolynomialRing@.
///

-*
  SeeAlso
    (grading,FirstOrderDeformation)
    (grading,Complex)
    (grading,PolynomialRing)
*-

doc ///
  Key
    (grading,PolynomialRing)
  Headline
    The coker grading of a polynomial ring.
  Usage
    grading(R)
  Inputs
    R:PolynomialRing
  Outputs
    :Matrix
  Description
   Text
        Returns the small torus grading (coker grading) of R , i.e., the grading added to R by @TO addCokerGrading@.
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        grading R
  SeeAlso
     addCokerGrading
///


doc ///
  Key
    (grading,FirstOrderDeformation)
  Headline
    The small torus grading of a deformation.
  Usage
    grading(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Matrix
  Description
   Text
        Returns the small torus grading of R = @TO simplexRing@ C, i.e., the grading added to R by @TO addCokerGrading@.
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        grading f
  SeeAlso
     (degree,FirstOrderDeformation)
     (isHomogeneous,FirstOrderDeformation)
     addCokerGrading
     firstOrderDeformation
///


doc ///
  Key
    (isHomogeneous,FirstOrderDeformation)
  Headline
    Check whether a deformation is homogeneous.
  Usage
    isHomogeneous(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Boolean
  Description
   Text
        Checks whether a deformation f is homogeneous with respect to the small torus gradin, i.e., the grading added to R = @TO simplexRing@ f by @TO addCokerGrading@.
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        isHomogeneous f
  SeeAlso
     (degree,FirstOrderDeformation)
     grading
     firstOrderDeformation
     addCokerGrading
///




doc ///
  Key
    (dim,FirstOrderDeformation)
  Headline
    Compute the dimension of a deformation.
  Usage
    dim(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :ZZ
      non-negative
  Description
   Text
        Computes the dimension of the big torus degree f part of the deformation space of @TO (source,FirstOrderDeformation)@ f.
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        dim f
  SeeAlso
       relationsCoefficients
       parameters
       firstOrderDeformation
       addCokerGrading
///

doc ///
  Key
    isNonzero
    (isNonzero,FirstOrderDeformation)
  Headline
    Check whether a deformation is non-zero.
  Usage
    isNonzero(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Boolean
  Description
   Text
        Check whether f is non-zero.

   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        isNonzero f
        f1=firstOrderDeformation(mg, vector {-1,-1,2,0,0})
        isNonzero f1
  SeeAlso
       dim
       isTrivial
       firstOrderDeformation
       addCokerGrading
///



doc ///
  Key
    isTrivial
    (isTrivial,FirstOrderDeformation)
    (isTrivial,Vector)
  Headline
    Check whether a deformation is trivial.
  Usage
    isTrivial(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Boolean
  Description
   Text
        Check whether f is trivial, i.e., @TO denominatorMonomial@ f has degree 1.

   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        isTrivial f
        f1=firstOrderDeformation(mg, vector {-1,1,0,0,0})
        isTrivial f1
  SeeAlso
       isNonzero
       denominatorMonomial
       firstOrderDeformation
       addCokerGrading
       trivialDeformations
///



doc ///
  Key
    (net,FirstOrderDeformation)
  Headline
    Pretty print for deformations.
  Usage
    net(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Net
  Description
   Text
        Pretty print for deformations. Prints @TO laurent@ f and @TO (dim,FirstOrderDeformation)@ f.
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
  SeeAlso
       Net
       net
///



doc ///
  Key
    gensSource
    (gensSource,FirstOrderDeformation)
  Headline
    Generators of the source of a deformation.
  Usage
    gensSource(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Matrix
  Description
   Text
        Returns generators of the @TO (source,FirstOrderDeformation)@ f.
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        gensSource f
  SeeAlso
     (source,FirstOrderDeformation)
     firstOrderDeformation
     addCokerGrading
///




doc ///
  Key
    totalSpace
    (totalSpace,FirstOrderDeformation,PolynomialRing)
    (totalSpace,List,PolynomialRing)
  Headline
    Total space of a deformation.
  Usage
    totalSpace(f,R)
    totalSpace(L,R)
  Inputs
    f:FirstOrderDeformation
    L:List
        of @TO FirstOrderDeformation@s.
    T:PolynomialRing
  Outputs
    :Ideal
  Description
   Text
        Compute the total space of a first order deformation f or a list of first order deformations. The polynomial ring T is used for the base. The number of variables
        of T should match @TO (dim,FirstOrderDeformation)@ f (respectively the sum over the deformations in L).
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        S=QQ[t]
        totalSpace(f,S)
        f1=firstOrderDeformation(mg, vector {0,-1,-1,0,2})
        S=QQ[t1,t2]
        totalSpace({f,f1},S)
  SeeAlso
       firstOrderDeformation
       addCokerGrading
///




doc ///
  Key
    toHom
    (toHom,FirstOrderDeformation)
  Headline
    Convert a first order deformation into a homomorphism.
  Usage
    toHom(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Matrix
  Description
   Text
        Convert f into a homomorphism @TO (source,FirstOrderDeformation)@ f -> @TO (target,FirstOrderDeformation)@ f.
        Requires @TO (dim,FirstOrderDeformation)@ f to be 0 or 1.
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,-1,0,2,0})
        fhom=toHom f
        source fhom
        target fhom
  SeeAlso
     (source,FirstOrderDeformation)
     (target,FirstOrderDeformation)
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    relationsCoefficients
    (relationsCoefficients,FirstOrderDeformation)
    (relationsCoefficients,Matrix,Vector)
  Headline
    Relations between the coefficients of a deformation.
  Usage
    relationsCoefficients(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Matrix
  Description
   Text
        Computes the @TO matrix@ whose columns generate the relations on the coefficients of the @TO image@s of the
        @TO generators@ of @TO (source,FirstOrderDeformation)@ f considering it as a homomorphism.
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,1,0,0,0})
        relationsCoefficients f
  SeeAlso
     (source,FirstOrderDeformation)
     parameters
     firstOrderDeformation
     addCokerGrading
///


doc ///
  Key
    parameters
    (parameters,FirstOrderDeformation)
    (parameters,Matrix,Vector)
  Headline
    Parameters of a deformation.
  Usage
    parameters(f)
  Inputs
    f:FirstOrderDeformation
  Outputs
    :Matrix
  Description
   Text
        Computes a @TO matrix@ whose columns correspond to the parameters of f (i.e., to the 
        coefficients of the @TO image@s of the @TO generators@ of @TO (source,FirstOrderDeformation)@ f considering it as a homomorphism.
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,1,0,0,0})
        relationsCoefficients f
        parameters f
  SeeAlso
     (dim,FirstOrderDeformation)
     relationsCoefficients
     firstOrderDeformation
     addCokerGrading     
///


doc ///
  Key
    firstOrderDeformation
    (firstOrderDeformation,Ideal,Vector)
    (firstOrderDeformation,MonomialIdeal,Vector)
    (firstOrderDeformation,Matrix,Vector)
  Headline
    Makes a first order deformation.
  Usage
    firstOrderDeformation(mg,v)
    firstOrderDeformation(I,v)
  Inputs
    I:Ideal
        monomial, reduced, in a @TO PolynomialRing@, or
    I:MonomialIdeal
        reduced
    mg:Matrix
        row, with generators of a reduced monomial ideal.
  Outputs
    :FirstOrderDeformation
  Description
   Text
        Generates a first order deformation @TO ideal@ mg. 
        If it is given an ideal I then @TO generators@ of I are stored
        to have a reference for @TO parameters@.
        You can also give non-minimal generators.
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f=firstOrderDeformation(mg, vector {-1,1,0,0,0})
  SeeAlso
     FirstOrderDeformation
     (dim,FirstOrderDeformation)
     addCokerGrading     
///

doc ///
  Key
    (symbol ==,FirstOrderDeformation,FirstOrderDeformation)
  Headline
   Compare two first order deformations.
  Usage
    f1==f2
  Inputs
    f1:FirstOrderDeformation
    f2:FirstOrderDeformation
  Outputs
    :Boolean
  Description
   Text
        Checks whether f1 and f2 are equal.
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        mg=mingens I;
        f1=firstOrderDeformation(mg, vector {-1,1,0,0,0})
        f2=firstOrderDeformation(mg, vector {0,2,0,-1,-1})
        f2==f2
        f2==f1
///

doc ///
  Key
    trivialDeformations
    (trivialDeformations,Complex)
    (trivialDeformations,Matrix)
    (trivialDeformations,Ideal,Matrix)
  Headline
   Compute the trivial deformations.
  Usage
    trivialDeformations(C)
  Inputs
    C:Complex
  Outputs
    :List
      of @TO FirstOrderDeformation@
  Description
   Text
        Compute the deformations induced by root automorphisms of the embedding space
        (i.e., excluding those coming from the torus).
        
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R)
        I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
        C=idealToComplex I
        T=trivialDeformations C
        tally apply(T,isTrivial)
///

-*
        Everything should also work for non-reduced monomial ideals, but not tested.
        *-
        


-------------------------------------------------------------------------------------------
-- complexes and cocomplexes



doc ///
  Key
    addCokerGrading
    (addCokerGrading,PolynomialRing)
    (addCokerGrading,PolynomialRing,List)
    (addCokerGrading,PolynomialRing,Matrix)
  Headline
    Stores a cokernel grading in a polynomial ring.
  Usage
    addCokerGrading(R)
    addCokerGrading(R,L)
    addCokerGrading(R,A)
  Inputs
    R:PolynomialRing
    L:List
    A:Matrix
  Outputs
    :Matrix
       the integer grading matrix.
  Description
   Text
       Stores a cokernel grading (Cox grading) in a polynomial ring.
       This data is accessed by @TO FirstOrderDeformation@ to compute the small torus
       @TO (degree,FirstOrderDeformation)@ of a deformation, and by
       @TO Complex@ and @TO CoComplex@ to store the vertices.
       
       The number or rows of A has to match the number of variables of R.
       
       If A is not specified, @TO raysPPn@ R is used.
       
       If L is specified then the grading of a weighted projective space is added.
       
       This command does not change the behaviour of R with respect to the standard
       Macaulay2 image grading, which we want to use independently.

   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R);
        R.grading
   Text
   
        Weighted projective space:
       
   Example
        R=QQ[x_0..x_4];
        addCokerGrading(R,{1,1,2,2,3});
        R.grading
  SeeAlso
     FirstOrderDeformation
     raysPPn
///



doc ///
  Key
    raysPPn
    (raysPPn,ZZ)
    (raysPPn,List)
  Headline
    The rays of the standard fan of projective space or weighted projective space.
  Usage
    raysPPn(n)
  Inputs
    n:ZZ
       positive
    L:List
       with at least 2 elements and first entry 1
  Outputs
    :Matrix
  Description
   Text
      Returns a matrix which has in its rows the rays of the standard fan of projective space of dimension n or weighted projective space with the weights L.
   Example
        raysPPn(2)
  SeeAlso
     addCokerGrading
///



doc ///
  Key
    Face
  Headline
   The class of all faces of complexes or co-complexes.
  Description
   Text
        The class of all faces of @TO Complex@es or @TO CoComplex@es.
        
        {\bf Creating faces:}

        Faces can be created by @TO face@ by
        specifying a @TO List@ or @TO Set@ of variables of a @TO PolynomialRing@ and some more optional data.
        
        Usually faces are accessed as faces of a @TO complex@ C using the @TO ScriptedFunctor@ C.fc with the subscripts "dimension of the face" and "index of the face".
        
        {\bf The data stored in a face F:}
        
        {\it F.vert}, a list with the vertices of F, which are variables of a @TO PolynomialRing@ R. We use a list to prevent Macaulay from reordering sets or monomials.
        
        {\it F.ofComplex}, a list with the @TO Complex@es and @TO CoComplex@es of which F is a face (optional).
        
        {}it F.indices}, a list with the indices of the face in its complexes. An index is a list {dim F, index of F in C.fc_(dim F)}.
        
        {\it F.dualFace}, the dual face of F (optional, is stored for later use when computed by @TO dualize@).
        Note that also the dual face dF of F then has dF.dualFace=F. 
        
        This data can also be accessed by the methods listed below.
        
        Note that the lattice data of the vertices is stored in R.grading via @TO addCokerGrading@.
        If no coker grading is present then many functions, like @TO simplex@, @TO idealToComplex@, @TO idealToCoComplex@ add the standard projective space grading @TO raysPPn@.

        {\bf Notes on the implementation:}
        
        In order to save memory identical faces are not created several times, e.g., 
        if sub@TO Complex@es are created from given ones any face of the subcomplex is identical to a face of the given one.

        So far the list F.ofComplex is only used for the complex which created F, but later we will perhaps append also all other complexes F is a face of.
        As we will append to the list this will be backwards compatible.
                   
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        F=C.fc_1_0
        F.vert
        coordinates F
        (F.ofComplex)#0==C
        bC=boundaryOfPolytope C
        F==bC.fc_1_0
  SeeAlso
    Complex
    CoComplex
    ofComplex
    vert
    dual
    addCokerGrading
    simplex
    boundaryOfPolytope
    coordinates
///

doc ///
  Key
    (dim,Complex)
  Headline
    Compute the dimension of a complex or co-complex.
  Usage
    dim(C)
  Inputs
    C:Complex
  Outputs
    :ZZ
      bigger or equal to -1
  Description
   Text
        Computes the dimension of a complex.

   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        dim C
        bC=boundaryOfPolytope C
        dim bC
        dbC=dualize bC
        dim dbC
  SeeAlso
       Complex
       CoComplex
///


doc ///
  Key
    (dim,Face)
  Headline
    Compute the dimension of a face.
  Usage
    dim(F)
  Inputs
    F:Face
  Outputs
    :ZZ
      bigger or equal to -1
  Description
   Text
        Computes the dimension of a face.
        If F.indices is present (usually the case by construction) this requires no computations.

        If F.indices is not present but a polynomial ring R can be associated to F (which is the case if
        F.ofComplex is present (or given as a second argument)
        or F is non-empty)
        then R.grading (which can be installed by @TO addCokerGrading@)
        is used to compute the dimension of the plane spanned by F.
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        bC=boundaryOfPolytope C
        F=bC.fc_2_0
        dim(face vert F,R)
  SeeAlso
       Complex
       CoComplex
  Caveat
        If F.indices is not present this returns a dimension as explained above
        but note that this does not check whether F is a face of the convex hull of the rows of R.grading.
///

doc ///
  Key
    (dim,Face,Complex)
  Headline
    Compute the dimension of a face.
  Usage
    dim(F,C)
  Inputs
    F:Face
    C:Complex
  Outputs
    :ZZ
      bigger or equal to -1
  Description
   Text
        Computes the dimension of a face.
        If F.indices is present (usually the case by construction) this requires no computations.

        If F.indices is not present but a polynomial ring R can be associated to F (which is the case if
        F.ofComplex is present (or given as a second argument)
        or F is non-empty)
        then R.grading (which can be installed by @TO addCokerGrading@)
        is used to compute the dimension of the plane spanned by F.
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        bC=boundaryOfPolytope C
        F=bC.fc_2_0
        dim(face vert F,C)
  SeeAlso
       Complex
       CoComplex
  Caveat
        If F.indices is not present this returns a dimension as explained above
        but note that this does not check whether F is a face of the convex hull of the rows of R.grading.
///



doc ///
  Key
    (dim,Face,PolynomialRing)
  Headline
    Compute the dimension of a face.
  Usage
    dim(F,R)
  Inputs
    F:Face
    R:PolynomialRing
         with coker grading.
  Outputs
    :ZZ
      bigger or equal to -1
  Description
   Text
        Computes the dimension of a face.
        If F.indices is present (usually the case by construction) this requires no computations.

        If F.indices is not present but a polynomial ring R can be associated to F (which is the case if
        F.ofComplex is present (or given as a second argument)
        or F is non-empty)
        then R.grading (which can be installed by @TO addCokerGrading@)
        is used to compute the dimension of the plane spanned by F.
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        bC=boundaryOfPolytope C
        F=bC.fc_2_0
        dim F
  SeeAlso
       Complex
       CoComplex
  Caveat
        If F.indices is not present this returns a dimension as explained above
        but note that this does not check whether F is a face of the convex hull of the rows of R.grading.
///


doc ///
  Key
    (complement,Face)
  Headline
    Compute the complement face of a simplex.
  Usage
    complement(F)
  Inputs
    F:Face
  Outputs
    :Face
  Description
   Text
        Computes the complement face of a face of a simplex (or subcomples thereof).
        
   Example
        R=QQ[x_0..x_4]
        C=simplex R
        bC=boundaryOfPolytope C
        F=bC.fc_2_0
        complement F
  SeeAlso
       (complement,Complex)
///



doc ///
  Key
    (complement,Complex)
  Headline
    Compute the complement CoComplex.
  Usage
    complement(C)
  Inputs
    C:Complex
  Outputs
    :CoComplex
  Description
   Text
        Given a @TO Complex@ it returns the @TO CoComplex@ of complement @TO Face@s and vice versa.
        
        The complement (co)complex cC is remembered in C.complementComplex=cC and cC.complementComplex=C.
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        bC=boundaryOfPolytope C
        cbC=complement bC
        complement cbC == bC
  SeeAlso
      (complement,Face)
///


doc ///
  Key
    coordinates
    (coordinates,Face)
    (coordinates,Face,Complex)
  Headline
    The coordinates of a face.
  Usage
    coordinates(C)
  Inputs
    F:Face
  Outputs
    :List
  Description
   Text
        Returns the coordinates of a face if some coker graded ring can be associated to F (if there is non one can give the second argument C).
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        bC=boundaryOfPolytope C
        F=bC.fc_2_0
        coordinates F
  SeeAlso
      addCokerGrading
///


doc ///
  Key
    dualize
    (dualize,Face)
    (dualize,Complex)
  Headline
    The dual of a face or complex.
  Usage
    dualize(F)
    dualize(C)
  Inputs
    F:Face
    C:Complex
    C:CoComplex
  Outputs
    :CoComplex
  Description
   Text
        Returns the dual of a face or a (co)complex. This is in the sense of dual face of Polytopes, so
        the faces of C have to be faces of a polytope.
        
        The dual (co)complex dC is stored in C.dualComplex=dC and dC.dualComplex=C.
        
        Note that if C is a Stanley-Reisner subcomplex of a simplex then dualize complement C is the isomorphic geometric complex of strata.
        
   Example
        R=QQ[x_0..x_4]
        addCokerGrading R
        C=simplex R
        bC=boundaryOfPolytope C
        F=bC.fc_2_0
        coordinates F
        dualize F
        coordinates dualize F
        dbC=dualize bC
        complement F
        coordinates complement F
        complement bC
        dualize complement bC
        bC
        coordinates dualize complement F
        coordinates F
  SeeAlso
      (complement,Face)
      (complement,Complex)
///


doc ///
  Key
    (indices,Face)
  Headline
    The indices of a face.
  Usage
    indices(F)
  Inputs
    F:Face
  Outputs
    :List
  Description
   Text
        Returns the indices of the face with respect to the first complex in F.ofComplex.
        The first entry d is the dimension and the second the index of F in the list (ofComplex F).fc_d.
        
   Example
        R=QQ[x_0..x_4];
        C=simplex R
        F=C.fc_2_0
        indices F
  SeeAlso
      ofComplex
///


doc ///
  Key
    ofComplex
    (ofComplex,Face)
  Headline
    The complex of a face.
  Usage
    ofComplex(F)
  Inputs
    F:Face
  Outputs
    :Complex
  Description
   Text
        Returns the complex of F, i.e., the first entry in F.ofComplex.
        
   Example
        R=QQ[x_0..x_4];
        C=simplex R
        F=C.fc_2_0
        ofComplex F
  SeeAlso
      (indices,Face)
///


doc ///
  Key
    (symbol ==,Face,Face)
  Headline
   Compare two faces.
  Usage
    F==G
  Inputs
    F:Face
    G:Face
  Outputs
    :Boolean
  Description
   Text
        Checks whether F and G are equal.

   Example
        R=QQ[x_0..x_4];
        face {x_0,x_1} == face {x_1,x_0}
        addCokerGrading R;
        C=simplex R;
        bC=boundaryOfPolytope C;
        F=bC.fc_2_0
        dF=dualize F
        F== dualize dF
  SeeAlso
     Face
     face
     Complex
     facets
     fc
///


doc ///
  Key
    (symbol ==,Complex,Complex)
  Headline
   Compare two complexes.
  Usage
    C1==C2
  Inputs
    C1:Complex
    C2:Complex
  Outputs
    :Boolean
  Description
   Text
        Checks whether C1 and C2 are equal.
        
        Uses the facets of C1 and C2 (as in many examples the Stanley-Reisner ideal cannot be computed as it is too big to write down).

   Example
        R=QQ[x_0..x_4];
        C=simplex R;
        bC=boundaryOfPolytope C
        dbC=dualize bC
        bC==dualize dbC
  SeeAlso
     (symbol ==,Face,Face)
     Complex
     dualize
     facets
///



doc ///
  Key
    faceToMonomial
    (faceToMonomial,Face)
    (faceToMonomial,Face,PolynomialRing)
  Headline
    The monomial of a face.
  Usage
    faceToMonomial(F)
  Inputs
    F:Face
    R:PolynomialRing
  Outputs
    :Complex
  Description
   Text
        Returns the product of the vertices of a face.
        
   Example
        R=QQ[x_0..x_4];
        C=simplex R
        F=C.fc_2_0
        vert F
        faceToMonomial F
  SeeAlso
      vert
///




doc ///
  Key
    intersectFaces
    (intersectFaces,Face,Face)
    (intersectFaces,List)
  Headline
    The intersection of two faces.
  Usage
    intersectFaces(F,G)
  Inputs
    F:Face
    G:Face
  Outputs
    :Face
  Description
   Text
        Returns the intersection of two faces or a list of faces.
        
   Example
        R=QQ[x_0..x_4];
        C=simplex R
        F=C.fc_2_0
        G=C.fc_2_1
        intersectFaces(F,G)
        H=C.fc_2_2
        intersectFaces({F,G,H})
  SeeAlso
      vert
///



doc ///
  Key
    (net,Face)
  Headline
    Printing faces.
  Usage
    net(F)
  Inputs
    F:Face
  Outputs
    :Net
  Description
   Text
        Prints a face. The vertices are printed without any brackets. The number of vertices is printed.
   Example
       R=QQ[x_0..x_4];
       C=simplex R;
       F=C.fc_2_0
  SeeAlso
     net
///



doc ///
  Key
    simplexDim
    (simplexDim,Face)
  Headline
    The dimension of a face inside the vertex simplex.
  Usage
    simplexDim(F)
  Inputs
    F:Face
  Outputs
    :ZZ
      bigger or equal to -1
  Description
   Text
        The dimension of a face inside the vertex simplex, i.e., the number of @TO vert@ F minus 1.

   Example
       R=QQ[x_0..x_4];
       C=simplex R;
       F=C.fc_2_0
       simplexDim F
  SeeAlso
     vert
     (dim,Face)
///

doc ///
  Key
    isSubface
    (isSubface,Face,Face)
  Headline
    Checks whether a face is a subface of another face.
  Usage
    isSubface(F,G)
  Inputs
    F:Face
    G:Face
  Outputs
    :Boolean
  Description
   Text
        Checks whether F is a subface of G.
   Example
       R=QQ[x_0..x_4];
       C=simplex R;
       G=C.fc_3_0
       F=C.fc_2_0
       H=C.fc_3_3
       isSubface(F,G)
       isSubface(F,H)
///


doc ///
  Key
    vert
    (vert,Face)
    (vert,Complex)
  Headline
    The vertices of a face or complex.
  Usage
    vert(F)
    vert(C)
  Inputs
    F:Face
    C:Complex
  Outputs
    :List
  Description
   Text
        The vertices of F or C.

   Example
       R=QQ[x_0..x_4];
       addCokerGrading R;
       C=simplex R;
       vert C
       F=C.fc_2_0
       vert F
  SeeAlso
      coordinates
///



doc ///
  Key
    (simplexRing,Face)
  Headline
    The underlying polynomial ring of a face.
  Usage
    simplexRing(F)
  Inputs
    F:Face
  Outputs
    :PolynomialRing
  Description
   Text
            The underlying polynomial ring of F.
   Example
       R=QQ[x_0..x_4];
       C=simplex R;
       F=C.fc_2_0
       simplexRing F
       R.grading
///

doc ///
  Key
    (simplexRing,Complex)
  Headline
    The underlying polynomial ring of a complex.
  Usage
    simplexRing(C)
  Inputs
    C:Complex
  Outputs
    :PolynomialRing
  Description
   Text
            The underlying polynomial ring of C.
   Example
       R=QQ[x_0..x_4];
       C=simplex R;
       simplexRing C
       R.grading
///


doc ///
  Key
    closedStar
    (closedStar,Face,Complex)
  Headline
    The closed star of a face of a complex.
  Usage
    closedStar(F)
  Inputs
    F:Face
    C:Complex
  Outputs
    :Complex
  Description
   Text
        The closed star of the face F of the complex C.
   Example
     R=QQ[x_0..x_4]
     C=boundaryOfPolytope simplex(R)
     F=C.fc_0_0
     link(F,C)
     closedStar(F,C)
     F=C.fc_1_0
     link(F,C)
     closedStar(F,C)
   Text
   
   Example
     R=QQ[x_0..x_4]
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex I
     F=C.fc_0_0
     link(F,C)
     closedStar(F,C)
     F=C.fc_1_0
     link(F,C)
     closedStar(F,C)
  SeeAlso
     link
///


doc ///
  Key
    link
    (link,Face,Complex)
  Headline
    The link of a face of a complex.
  Usage
    link(F)
  Inputs
    F:Face
    C:Complex
  Outputs
    :Complex
  Description
   Text
        The link of the face F of the complex C.

   Example
     R=QQ[x_0..x_4]
     C=boundaryOfPolytope simplex(R)
     F=C.fc_0_0
     link(F,C)
     closedStar(F,C)
     F=C.fc_1_0
     link(F,C)
     closedStar(F,C)
   Text
   
   Example
     R=QQ[x_0..x_4]
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex I
     F=C.fc_0_0
     link(F,C)
     closedStar(F,C)
     F=C.fc_1_0
     link(F,C)
     closedStar(F,C)
  SeeAlso
    closedStar
///



doc ///
  Key
    simplex
    (simplex,PolynomialRing)
    (simplex,PolynomialRing,PolynomialRing)
  Headline
    Simplex in the variables of a polynomial ring.
  Usage
    simplex(R)
    simplex(R,Rdual)
  Inputs
    R:PolynomialRing
    Rdual:PolynomialRing
  Outputs
    :Complex
  Description
   Text
        Returns a simplex on the variables of R.
        
        If R does not have a coker grading then the standard projective space fan rays are added, see @TO addCokerGrading@
        and @TO raysPPn@.
        
        The Option computeFaces=>false suppresses the computation of all faces.
        
        If Rdual is specified it is used for the vertices of the dual simplex, if not a new polynomial ring is created.
        It is graded by the coordinates of the vertices of the dual simplex.
        
        The dual simplex is always created without face data.
        
   Example
     R=QQ[x_0..x_4]
     C=simplex(R)
     grading C
     dC=C.dualComplex
     grading dC
     fc(dC);
     dC
  SeeAlso
     grading
     dualGrading
///

doc ///
  Key
    dualGrading
    (dualGrading,Complex)
  Headline
    The dual vertices of a polytope.
  Usage
    dualGrading(C)
  Inputs
    C:Complex
        a polytope
  Outputs
    :Matrix
  Description
   Text
       Returns the dual grading of (i.e., a matrix whose row are) the dual polytope of C.
       The rows are sorted according to the @TO polytopalFacets@ of C.
               
   Example
     R=QQ[x_0..x_4]
     C=simplex(R)
     grading C
     dA=dualGrading C
     dA===grading dualize C
     dA===C.dualComplex.simplexRing.grading
     pf=polytopalFacets C
     coordinates pf#0
     (dualGrading C)^{0}
  SeeAlso
     verticesDualPolytope
     grading
     polytopalFacets
     dualize
     isPolytope
     facets
  Caveat
     This just returns C.dualComplex.grading. If this data has not beed computed use @TO verticesDualPolytope@.
     Integrate into this verticesDualPolytope.
///



doc ///
  Key
    verticesDualPolytope
    (verticesDualPolytope,Complex)
  Headline
    The dual vertices of a polytope.
  Usage
    verticesDualPolytope(C)
  Inputs
    C:Complex
        a polytope
  Outputs
    :Matrix
  Description
   Text
       Computes the vertices of the dual polytope of C in the row of a matrix.
       The rows are sorted according to the @TO polytopalFacets@ of C.
        
   Example
     R=QQ[x_0..x_4]
     C=simplex(R)
     verticesDualPolytope C
  SeeAlso
     dualGrading
     grading
     polytopalFacets
     dualize
     isPolytope
     facets
///


doc ///
  Key
    computeFaces
    [simplex,computeFaces]
  Headline
    Compute faces of a simplex.
  Description
   Text
        Determines whether all faces of a simplex are being computed.
        
   Example
     R=QQ[x_0..x_4]
     simplex(R,computeFaces=>false)
  SeeAlso
     simplex
///


doc ///
  Key
    boundaryOfPolytope 
    (boundaryOfPolytope ,Complex)
  Headline
    The boundary of a polytope.
  Usage
    boundaryOfPolytope(C)
  Inputs
    C:Complex
  Outputs
    :Complex
  Description
   Text
        Returns the boundary of a complex C which is a polytope.
        
   Example
     R=QQ[x_0..x_4]
     C=simplex(R)
     isPolytope C
     boundaryOfPolytope C
///


doc ///
  Key
    boundaryCyclicPolytope  
    (boundaryCyclicPolytope  ,ZZ,PolynomialRing)
  Headline
    The boundary complex of a cyclic polytope.
  Usage
    boundaryCyclicPolytope(d,R)
  Inputs
    d:ZZ
       positive
    R:PolynomialRing
  Outputs
    :Complex
  Description
   Text
        Returns the boundary complex of a cyclic polytope of dimension d inside the standard simplex of the variables of R.
        So the vertices are those of the standard simplex.
        
   Example
     R=QQ[x_0..x_5]
     boundaryCyclicPolytope(3,R)
  SeeAlso
     fullCyclicPolytope
///

doc ///
  Key
    fc  
    (fc  ,Complex)
    (fc  ,Complex,ZZ)
  Headline
    The faces of a complex.
  Usage
    fc(C)
    fc(C,d)
  Inputs
    C:Complex
    d:ZZ
       positive
  Outputs
    :List
  Description
   Text
        Returns a list with entries lists with the faces of C starting in dimension -1.
        If d is given then faces of dimension d are returned.
        
   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     fc C
     fc(C,1)
///


doc ///
  Key
    fullCyclicPolytope   
    (fullCyclicPolytope,ZZ,PolynomialRing)
    (fullCyclicPolytope,ZZ,PolynomialRing,PolynomialRing)
  Headline
    Cyclic polytope.
  Usage
    fullCyclicPolytope(d,R)
  Inputs
    d:ZZ
       positive
    R:PolynomialRing
  Outputs
    C:Complex
  Description
   Text
        Returns the full cyclic polytope of dimension d with vertices the variables of R.
        A coker grading is added to R via the vertices of the moment curve
        (if R already has a coker grading then a warning is displayed)
        and translated such that 0 lies in the interior of C.
        
   Example
     R=QQ[x_0..x_5]
     C=fullCyclicPolytope(3,R)
  SeeAlso
     boundaryCyclicPolytope
///


doc ///
  Key
    convHull    
    (convHull,List)
    (convHull,String)
  Headline
    The convex hull complex.
  Usage
    convHull(L)
    convHull(fn)
  Inputs
    L:List
       of @TO Vector@s
    fn:String
  Outputs
    C:Complex
  Description
   Text
        Returns the convex hull of lattice vectors lying all in the same space.
        The output has C.isPolytope==true.

        If applied to the string fn the result of a previous computation stored via the option @TO file@ is read from the file fn.

        The 0-point has to lie in the convex hull.
        
   Example
    L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}}
    P=convHull(L)
    dP=boundaryOfPolytope P
  SeeAlso
     Complex
  Caveat
     This uses the package OldPolyhedra.m2 to compute the facets. Too slow compared to Maple/convex.

     If the package {\it ConvexInterface} is loaded, then this command calls Maple/Convex.
     See the corresponding option explained at @TO SRdeformations@.
///


doc ///
  Key
    polytopalFacets     
    (polytopalFacets ,Complex)
  Headline
    The facets of a polytope.
  Usage
    polytopalFacets(C)
  Inputs
    C:Complex
       a polytope
    R:PolynomialRing
  Outputs
    C:Complex
  Description
   Text
        The facets of a polytope. Note that this is not facets(C) (which is the polytope itself).
        
   Example
     R=QQ[x_0..x_5]
     C=fullCyclicPolytope(3,R)
     facets C
     polytopalFacets(C)
  SeeAlso
     isPolytope
     facets
///

doc ///
  Key
    facets     
    (facets,Complex)
    (facets,List)
  Headline
    The maximal faces of a complex.
  Usage
    facets(C)
  Inputs
    C:Complex
    R:PolynomialRing
  Outputs
    :List
  Description
   Text
        The maximal faces of C arranged in a list of lists with ascending dimension of the facets.
        
   Example
     R=QQ[x_0..x_5]
     C=fullCyclicPolytope(3,R)
     facets C
     dC=boundaryOfPolytope(C);
     facets dC
  SeeAlso
     fc
     polytopalFacets
///


doc ///
  Key
    isPolytope
    (isPolytope,Complex)
    (isPolytope,List)
  Headline
    Check whether a complex is a polytope.
  Usage
    isPolytope(C)
  Inputs
    C:Complex
  Outputs
    :Boolean
  Description
   Text
         Check whether a complex C is a polytope.
         
   Example
     R=QQ[x_0..x_5];
     C=fullCyclicPolytope(3,R)
     isPolytope C
///


doc ///
  Key
    eulerCharacteristic 
    (eulerCharacteristic ,Complex)
    (eulerCharacteristic ,List)
  Headline
    The Euler characteristic of a complex.
  Usage
    eulerCharacteristic(C)
  Inputs
    C:Complex
  Outputs
    :ZZ
  Description
   Text
      The Euler characteristic of C. This is printed always when printing a complex and the faces are known.
      
   Example
     R=QQ[x_0..x_5];
     C=fullCyclicPolytope(3,R);
     fvector C
     eulerCharacteristic C
     dC=boundaryOfPolytope(C);
     fvector dC
     eulerCharacteristic dC
  SeeAlso
     fvector
     fc
///

doc ///
  Key
    fvector 
    (fvector ,Complex)
    (fvector ,List)
  Headline
    The F-vector of a complex.
  Usage
    fvector(C)
  Inputs
    C:Complex
  Outputs
    :List
  Description
   Text
      The fvector of C. This is printed always when printing a complex and the faces are known.
      
   Example
     R=QQ[x_0..x_5];
     C=fullCyclicPolytope(3,R);
     fvector C
     eulerCharacteristic C
     dC=boundaryOfPolytope(C);
     fvector dC
     eulerCharacteristic dC
  SeeAlso
     eulerCharacteristic
     fc
///


doc ///
  Key
    globalSections  
    (globalSections,Matrix,Vector)
    (globalSections,Matrix,Vector,List)
  Headline
    The global sections of a toric divisor.
  Usage
    globalSections(A,v)
    globalSections(A,v,L)
  Inputs
    A:Matrix
    v:Vector
    L:List
  Outputs
    :List
  Description
   Text
      Computes the global sections of a toric Weil divisor D with coefficients v with respect to the coker grading by A.
      In the same way as v they are represented by vectors (exponent vectors of Laurent monomials in rank target A variables).
      
      If a list of indices L in {0..rank target A -1} is specified, then those Laurent monomial exponents
      are computed, which induce a linear equivalence of D to an effective divisor with support precisely on L.
      
   Example
     A=matrix {{1, 0}, {0, 1}, {-1, -1}}
     b=vector {2,0,0}
     globalSections(A,b)
     A=matrix {{1, 0}, {0, 1}, {-1, -1},{1,1}}
     b=vector {2,0,0,0}
     globalSections(A,b)
     globalSections(A,b,{1})
  Caveat
    This uses the package OldPolyhedra.m2 (if ConvexInterface.m2 is not present) to compute the lattice points of a convex hull.
    constructHilbertBasis of the package OldPolyhedra.m2 used by latticePoints overwrites global variable C.
    Fixed this in my local version.
     
///



doc ///
  Key
    coComplexToIdeal  
    (coComplexToIdeal,CoComplex)
  Headline
    The monomial ideal associated to a CoComplex.
  Usage
    coComplexToIdeal(C)
  Inputs
    C:CoComplex
  Outputs
    :Ideal
  Description
   Text
      The monomial ideal associated to a CoComplex, i.e., the intersection of all @TO ideal@ @TO vert@ F for @TO Face@s F of C.
      
   Example
     R=QQ[x_0..x_4]
     addCokerGrading(R)
     C0=simplex(R)
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex(I)
     embeddingComplex C
     idealToComplex(I,C0)
     complexToIdeal(C)
     cC=idealToCoComplex(I,C0)
     cC==complement C
     I==coComplexToIdeal(cC)
     dualize cC
  SeeAlso
     complexToIdeal
     idealToComplex
     idealToCoComplex
///

doc ///
  Key
    embedded
    [complexToIdeal,embedded]
    [minimalNonFaces,embedded]
  Headline
    Use only faces of the embedding complex.
  Description
   Text
      If the option embedded=>true is given, then only @TO Face@s of @TO embeddingComplex@ C are used.
      
  SeeAlso
     complexToIdeal
     minimalNonFaces
     coComplexToIdeal
     idealToComplex
     idealToCoComplex
     embeddingComplex
///



doc ///
  Key
    complexToIdeal  
    (complexToIdeal,Complex)
  Headline
    The monomial ideal associated to a complex.
  Usage
    complexToIdeal(C)
  Inputs
    C:Complex
  Outputs
    :Ideal
  Description
   Text
      The Stanley-Reisner ideal associated to a complex, i.e., the ideal of non-faces of C.
      
      If the option embedded=>true is given, then only @TO Face@s of @TO embeddingComplex@ C are used.
      
   Example
     R=QQ[x_0..x_4]
     addCokerGrading(R)
     C0=simplex(R)
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex(I)
     embeddingComplex C
     idealToComplex(I,C0)
     complexToIdeal(C)
     cC=idealToCoComplex(I,C0)
     cC==complement C
     I==coComplexToIdeal(cC)
     dualize cC
  SeeAlso
     coComplexToIdeal
     idealToComplex
     idealToCoComplex
     embeddingComplex
///


doc ///
  Key
    idealToComplex
    (idealToComplex,Ideal)
    (idealToComplex,MonomialIdeal)
    (idealToComplex,Ideal,Complex)
    (idealToComplex,MonomialIdeal,Complex)
  Headline
    The complex associated to a reduced monomial ideal.
  Usage
    idealToComplex(I)
    idealToComplex(I,C)
  Inputs
    I:Ideal
    I:MonomialIdeal
    C:Complex
  Outputs
    :Complex
  Description
   Text
      The Stanley-Complex associated to a reduced monomial ideal.
      
      This is a complex of faces of either C, or if C is not given, a complex of faces of @TO simplex@ @TO ring@ I.
      
   Example
     R=QQ[x_0..x_4]
     addCokerGrading(R)
     C0=simplex(R)
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex(I)
     embeddingComplex C
     idealToComplex(I,C0)
     complexToIdeal(C)
     cC=idealToCoComplex(I,C0)
     cC==complement C
     I==coComplexToIdeal(cC)
     dualize cC
  SeeAlso
     coComplexToIdeal
     complexToIdeal
     idealToCoComplex
     embeddingComplex
///


doc ///
  Key
    idealToCoComplex
    (idealToCoComplex,Ideal)
    (idealToCoComplex,MonomialIdeal)
    (idealToCoComplex,Ideal,Complex)
    (idealToCoComplex,MonomialIdeal,Complex)
  Headline
    The co-complex associated to a reduced monomial ideal.
  Usage
    idealToCoComplex(I)
    idealToCoComplex(I,C)
  Inputs
    I:Ideal
    I:MonomialIdeal
    C:Complex
  Outputs
    :CoComplex
  Description
   Text
      The co-complex of ideals of the strata of the locus of a reduced monomial ideal.
      
      This is a co-complex of faces of either C, or if C is not given, a complex of faces of @TO simplex@ @TO ring@ I.
      
   Example
     R=QQ[x_0..x_4]
     addCokerGrading(R)
     C0=simplex(R)
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex(I)
     embeddingComplex C
     idealToComplex(I,C0)
     complexToIdeal(C)
     cC=idealToCoComplex(I,C0)
     cC==complement C
     I==coComplexToIdeal(cC)
     dualize cC
  SeeAlso
     coComplexToIdeal
     idealToComplex
     complexToIdeal
     embeddingComplex
///


doc ///
  Key
    embeddingComplex
    (embeddingComplex,Complex)
  Headline
    The embedding complex of a complex or co-complex.
  Usage
    embeddingComplex(C)
  Inputs
    C:Complex
  Outputs
    :Complex
  Description
   Text
      The embedding complex of a complex or co-complex.
      
   Example
     R=QQ[x_0..x_4]
     addCokerGrading(R)
     C0=simplex(R)
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex(I)
     embeddingComplex C
     idealToComplex(I,C0)
     complexToIdeal(C)
     cC=idealToCoComplex(I,C0)
     cC==complement C
     I==coComplexToIdeal(cC)
     dualize cC
  SeeAlso
     complexToIdeal
///




doc ///
  Key
    Complex
  Headline
   The class of all embedded complexes.
  Description
   Text
        The class of all embedded complexes, not necessarily simplicial or compact or equidimensional.
        These are complexes with coordinates assigned to their vertices.

        {\bf Creating complexes:}

        The following functions return complexes:

        @TO simplex@ -- Simplex in the variables of a polynomial ring

        @TO boundaryCyclicPolytope@ -- The boundary complex of a cyclic polytope with standard projective space vertices

        @TO fullCyclicPolytope@ -- The full cyclic polytope with moment curve vertices

        @TO convHull@ -- The convex hull

        @TO hull@ -- The positive hull

        @TO boundaryOfPolytope@ -- The boundary of a polytope

        @TO newEmptyComplex@ -- Generates an empty complex.

        @TO idealToComplex@ -- The complex associated to a reduced monomial ideal

        @TO dualize@ -- The dual of a co-complex.

        @TO complement@ -- The complement of a co-complex.

        @TO complex@ -- Make a complex from a list of faces

        @TO complexFromFacets@ -- Make a complex from a list of facets

        @TO embeddingComplex@ -- The complex containing a subcomplex

        For examples see the documentation of these functions.


        {\bf The data stored in a complex C:}

        {\it C.simplexRing}, the polynomial ring of vertices of C.

        {\it C.grading}, is C.simplexRing.grading, a matrix with the coordinates of the vertices of C in its rows.

        {\it C.facets}, a list with the facets of C sorted into lists by dimension.

        {\it C.edim}, the embedding dimension of C, i.e., @TO rank@ @TO source@ C.grading.

        {\it C.dim}, the dimension of the complex.

        {\it C.isSimp}, a @TO Boolean@ indicating whether C is simplicial.

        {\it C.isEquidimensional}, a @TO Boolean@ indicating whether C is equidimensional.

        If not just the facets but the faces of C a known (e.g., after computed with @TO fc@) then the following data is present:

        {\it C.fc}, a @TO ScriptedFunctor@ with the faces of C sorted and indexed by dimension.

        {\it C.fvector}, a @TO List@ with the F-vector of C.

        The following may be present (if known due to creation of C or due to calling some function):

        {\it C.dualComplex}, the dual co-complex of C in the sense of dual faces of a polytope. See @TO dualize@.

        {\it C.isPolytope}, a @TO Boolean@ indicating whether C is a polytope.

        {\it C.polytopalFacets}, a @TO List@ with the boundary faces of the polytope C.

        {\it C.complementComplex}, the complement co-complex of C (if C is a subcomplex of a simplex). See @TO complement@.

   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     C.simplexRing
     C.grading
     C.fc_2
     C.facets
     dualize C
     complement C
   Text
     
   Example
     R=QQ[x_0..x_5]
     C=simplex R
     C.isPolytope
     C.polytopalFacets
  SeeAlso
    CoComplex
    Face
    (homology,Complex)
///


doc ///
  Key
    noBoundary
  Headline
    Optional data of Complex.
  Description
   Text
    Optional data in the @TO MutableHashTable@ @TO Complex@
    indicating whether the complex has no relative boundary.
  SeeAlso
    Complex
    CoComplex
///


doc ///
  Key
    complementComplex
  Headline
    Optional data of Complex.
  Description
   Text
    Optional data in the @TO MutableHashTable@ @TO Complex@
    storing the complement complex.
  SeeAlso
    Complex
    CoComplex
///

doc ///
  Key
    dualComplex
  Headline
    Optional data of Complex.
  Description
   Text
    Optional data in the @TO MutableHashTable@ @TO Complex@
    storing the dual complex.
  SeeAlso
    Complex
    CoComplex
///

doc ///
  Key
    dualFace
  Headline
    Optional data of Face.
  Description
   Text
    Optional data in the @TO MutableHashTable@ @TO Face@
    storing the dual face.
  SeeAlso
    dualize
///


doc ///
  Key
    CoComplex
  Headline
   The class of all embedded co-complexes.
  Description
   Text
        The class of all embedded co-complexes, not necessarily simplicial.

        {\bf Creating co-complexes:}

        The following functions return co-complexes:

        @TO idealToCoComplex@ -- The co-complex associated to a reduced monomial ideal

        @TO dualize@ -- The dual of a complex.

        @TO complement@ -- The complement of a complex.

        @TO coComplex@ -- Make a co-complex from a list of faces

        For further examples see the documentation of these functions.


        {\bf The data stored in a co-complex C:}

        {\it C.simplexRing}, the polynomial ring of vertices of C (note these are only faces of C if C is a polytope).

        {\it C.grading}, is C.simplexRing.grading, a matrix with the coordinates of the vertices of C in its rows.

        {\it C.facets}, a list with the facets of C sorted into lists by dimension.

        {\it C.edim}, the embedding dimension of C, i.e., @TO rank@ @TO source@ C.grading.

        {\it C.dim}, the dimension of C, i.e., the minimal dimension of the faces.

        {\it C.isSimp}, a @TO Boolean@ indicating whether C is simplicial.

        {\it C.isEquidimensional}, a @TO Boolean@ indicating whether C is equidimensional.

        {\it C.fc}, a @TO ScriptedFunctor@ with the faces of C sorted and indexed by dimension.

        {\it C.fvector}, a @TO List@ with the F-vector of C.

        The following may be present (if known due to creation of C or due to calling some function):

        {\it C.dualComplex}, the dual complex of C in the sense of dual faces of a polytope. See @TO dualize@.

        {\it C.isPolytope}, a @TO Boolean@ indicating whether C is a polytope.

        {\it C.polytopalFacets}, a @TO List@ with the boundary faces of the polytope C.

        {\it C.complementComplex}, the complement complex of C (if C is a subcocomplex of a simplex). See @TO complement@.

   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     C.simplexRing
     C.grading
     C.fc_2
     C.facets
     dC=dualize C
     cC=complement C
     dualize cC
  SeeAlso
    Complex
    Face
  Caveat
    So far a co-complex is of class complex and the methods checks of which type it really is. At some point both will have a common ancestor.
///

doc ///
  Key
    complex
    (complex,PolynomialRing,List)
    (complex,PolynomialRing,List,List)
    (complex,PolynomialRing,List,PolynomialRing)
    (complex,PolynomialRing,List,List,PolynomialRing)
  Headline
    Make a complex.
  Usage
    complex(R,facelist)
    complex(R,facelist,facetlist)
    complex(R,facelist,Rdual)
    complex(R,facelist,facetlist,Rdual)
  Inputs
    R:PolynomialRing
    facelist:List
         of the faces, sorted by dimension
    facetlist:List
         of the facets, sorted by dimension
    Rdual:PolynomialRing
  Outputs
    :Complex
  Description
   Text
      Make a complex from a list of faces and/or facets.

      This is mostly used internally but may be occasionally useful for the end user.
      
   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     fC=fc C
     C1=complex(R,fC)
     C==C1
  SeeAlso
     Face
     complexFromFacets
  Caveat
     If both the list of faces and facets is specified there is no consistency check.
///

doc ///
  Key
    complexFromFacets
    (complexFromFacets,PolynomialRing,List)
    (complexFromFacets,PolynomialRing,List,PolynomialRing)
  Headline
    Make a complex from its facets.
  Usage
    complexFromFacets(R,facetlist)
    complexFromFacets(R,facetlist,Rdual)
  Inputs
    R:PolynomialRing
    facetlist:List
         of the facets, sorted by dimension
    Rdual:PolynomialRing
  Outputs
    :Complex
  Description
   Text
      Make a complex from a list of faces and/or facets.

      This is mostly used internally but may be occasionally useful for the end user.
      
   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     fC=facets C
     C1=complexFromFacets(R,fC)
     C1==C
     fc C1;
     C1
  SeeAlso
     Face
     complex
     fc
  Caveat
     If both the list of faces and facets is specified there is no consistency check.
///


doc ///
  Key
    coComplex
    (coComplex,PolynomialRing,List)
    (coComplex,PolynomialRing,List,List)
    (coComplex,PolynomialRing,List,PolynomialRing)
    (coComplex,PolynomialRing,List,List,PolynomialRing)
  Headline
    Make a co-complex.
  Usage
    coComplex(R,facelist)
    coComplex(R,facelist,facetlist)
    coComplex(R,facelist,Rdual)
    coComplex(R,facelist,facetlist,Rdual)
  Inputs
    R:PolynomialRing
    facelist:List
         of the faces, sorted by dimension
    facetlist:List
         of the facets, sorted by dimension
    Rdual:PolynomialRing
  Outputs
    :CoComplex
  Description
   Text
      Make a co-complex from a list of faces and/or facets.

      This is mostly used internally but may be occasionally useful for the end user.
      
   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     grading R
     dC=dualize C
     fdC=fc dC
     Rdual=simplexRing dC
     grading Rdual
     dC1=coComplex(Rdual,fdC)
     dC==dC1
  SeeAlso
     Face
  Caveat
     If both the list of faces and facets is specified there is no consistency check.
///


doc ///
  Key
    variables
    (variables,Complex)
  Headline
    The variables of a complex or co-complex.
  Usage
    variables(C)
  Inputs
    C:Complex
  Outputs
    :Complex
  Description
   Text
      The variables of a complex of co-complex. Compare with @TO vert@.
      
   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     F=C.fc_0_0
     lkF=link(F,C)
     variables lkF
     vert lkF
  SeeAlso
     vert
///


doc ///
  Key
    edim
    (edim,Complex)
  Headline
    The embedding dimension of a complex or co-complex.
  Usage
    edim(C)
  Inputs
    C:Complex
  Outputs
    :ZZ
  Description
   Text
      The embedding dimension of a complex or co-complex.
      
   Example
     R=QQ[x_0..x_5]
     C=boundaryCyclicPolytope(3,R)
     dim C
     edim C 
  SeeAlso
     (dim,Complex)
///

doc ///
   Key
     isSimp
     (isSimp,Complex)
     (isSimp,List)
   Headline
     Check whether a complex or co-complex is simplicial.
   Usage
     isSimp(C)
   Inputs
     C:Complex
   Outputs
     :Boolean
   Description
    Text
       Check whether a complex or co-complex is simplicial.
    
    Example
      R=QQ[x_0..x_5]
      C=boundaryCyclicPolytope(3,R)
      isSimp C
      R=QQ[x_0..x_5]
      C1=fullCyclicPolytope(3,R)
      isSimp C1     
   SeeAlso
      boundaryCyclicPolytope
      fullCyclicPolytope
///

doc ///
  Key
    isEquidimensional
    (isEquidimensional,Complex)
    (isEquidimensional,List)
  Headline
    Check whether a complex or co-complex is equidimensional.
  Usage
    isEquidimensional(C)
  Inputs
    C:Complex
  Outputs
    :Boolean
  Description
   Text
      Check whether a complex or co-complex is equidimensional.
      
   Example
     R=QQ[x_0..x_5];
     C=boundaryCyclicPolytope(3,R)
     isEquidimensional(C)
     R=QQ[x_0..x_2];
     I=intersect(ideal(x_0),ideal(x_1,x_2))
     C=idealToComplex I
     isEquidimensional(C)     
  SeeAlso
     boundaryCyclicPolytope
     idealToComplex
///




doc ///
  Key
    face
    (face,List)
    (face,Set)
    (face,List,Complex)
    (face,List,Complex,ZZ,ZZ)
  Headline
    Generate a face.
  Usage
    face(L)
    face(S)
    face(L,C)
    face(L,C,d,j)
  Inputs
    L:List
    S:Set
    C:Complex
    d:ZZ
       bigger or equal to -1
    j:ZZ
       non-negative
  Outputs
    :Face
  Description
    Text
       Generate a @TO Face@ F from a list L (or set S) of vertices. If the additional argument C
       is given it sets F.ofComplex={C} to store the complex of which F is a face of.

       If d and j are given then F.indices={{d,j}} for storing the face dimension d and its index
       j in (F.ofComplex)#0.
      
    Example
     R=QQ[x_0..x_4]
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex I
     F=C.fc_1_0
     F==face(vert F,C,1,0)
  SeeAlso
     Face
     Complex
     CoComplex
///


doc ///
  Key
    addFaceDataToComplex
    (addFaceDataToComplex,Complex,List)
    (addFaceDataToComplex,Complex,List,List)
  Headline
    Adds to a complex face data.
  Usage
    addFaceDataToComplex(C,facelist)
    addFaceDataToComplex(C,facelist,facetlist)
  Inputs
    C:Complex
    facelist:List
      whose i-th entry is a list of the faces of C of dimension i-1
    facetlist:List
      whose i-th entry is a list of the facets of C of dimension i-1
  Outputs
    C:Complex
  Description
    Text
       Adds to a complex the face data and facet data (computed if not specified), i.e.,
       C.fc, C.facets, C.dim, C.fvector, C.isEquidimensional, C.isSimp are updated.

       This allows us to add the face data later to a complex generated previously by 
       @TO newEmptyComplex@.

       Note that the input C is modified by this method.

       This function is mainly used internally, but may occasionally be useful to the user and is hence exported.
      
    Example
       R=QQ[x_0..x_4]
       addCokerGrading(R)
       I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
       C=idealToComplex(I)
       Cl=newEmptyComplex(R)
       addFaceDataToComplex(Cl,fc C)
       Cl==C
  SeeAlso
    newEmptyComplex
    dim
    fvector
    fc
    isSimp
    isEquidimensional
  Caveat
    If both facelist and facetlist are specified this function does not make any consistency check.
///


doc ///
  Key
    addFacetDataToComplex
    (addFacetDataToComplex,Complex,List)
  Headline
    Adds to a complex facet data.
  Usage
    addFacetDataToComplex(C,facetlist)
  Inputs
    C:Complex
    facetlist:List
      whose i-th entry is a list of the facets of C of dimension i-1
  Outputs
    C:Complex
  Description
    Text
       Adds to a complex the facet data, i.e.,
       C.facets, C.dim, C.fvector, C.isEquidimensional, C.isSimp are updated.

       This allows us to add the facet data later to a complex generated previously by 
       @TO newEmptyComplex@.

       Note that the input C is modified by this method.

       This function is mainly used internally, but may occasionally be useful to the user and is hence exported.
      
    Example
       R=QQ[x_0..x_4]
       addCokerGrading(R)
       I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
       C=idealToComplex(I)
       Cl=newEmptyComplex(R)
       addFacetDataToComplex(Cl,facets C)
       Cl==C
  SeeAlso
    newEmptyComplex
    addFaceDataToComplex
    dim
    fvector
    facets
    isSimp
    isEquidimensional
///



doc ///
  Key
    newEmptyComplex
    (newEmptyComplex,PolynomialRing)
  Headline
    Generates an empty complex.
  Usage
    newEmptyComplex(R)
  Inputs
    R:PolynomialRing
       with coker grading added by @TO addCokerGrading@
  Outputs
    C:Complex
  Description
    Text
       Generates an empty @TO complex@.
       Face and facet data or just facet data can be added later by @TO addFaceDataToComplex@
       or @TO addFacetDataToComplex@.

       This function is mainly used internally to create an empty complex data structure
       to be filled later, but may occasionally be useful to the user and is hence exported.
      
    Example
       R=QQ[x_0..x_4]
       addCokerGrading(R)
       I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
       C=idealToComplex(I)
       Cl=newEmptyComplex(R)
       addFacetDataToComplex(Cl,facets C)
       Cl==C
  SeeAlso
    addFaceDataToComplex
    addFacetDataToComplex
    dim
    fvector
    facets
    isSimp
    isEquidimensional
///


doc ///
  Key
    (net,Complex)
  Headline
    Printing complexes.
  Usage
    net(C)
  Inputs
    C:Complex
  Outputs
    :Net
  Description
   Text
       Printing complexes. It prints the @TO facets@ of the complex C
       sorted by dimension.

   Example
     R=QQ[x_0..x_4]
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex I;
     net C
  SeeAlso
    net
///


doc ///
  Key
    (grading,Complex)
  Headline
    The grading of a complex.
  Usage
    grading(C)
  Inputs
    C:Complex
  Outputs
    :Matrix
  Description
   Text
       Returns C.simplexRing.grading. The output has the coordinates of the vertices of C in its rows.

   Example
     R=QQ[x_0..x_4]
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
     C=idealToComplex I;
     grading C
  SeeAlso
    idealToComplex
///



------------------------------------------------------------------------
-- deformations of Stanley-Reisner ideals

doc ///
  Key
    deformationsFace
    (deformationsFace,Face,Complex)
    (deformationsFace,Face,Complex,Ideal)
  Headline
    Compute the deformations associated to a face.
  Usage
    deformationsFace(F,C)
    deformationsFace(F,C,I)
  Inputs
    F:Face
    C:Complex
    I:Ideal
         reduced monomial
  Outputs
    :List
  Description
   Text
      Compute the homogeneous (i.e., @TO (degree,FirstOrderDeformation)@ zero) deformations 
      associated to a face F of the complex C. 
      
      The additional parameter I should be the Stanley-Reisner ideal of C and can be
      given to avoid computation of the Stanley-Reisner ideal if it is already known. 
      Usually this is not necessary: 
      Once I is computed it is stored in C.ideal,
      so deformationsFace(F,C,I) is equivalent to deformationsFace(F,C). Note also that all methods
      producing a complex from an ideal (like @TO idealToComplex@) store the ideal in C.ideal.
      
      The deformations and C are stored in F.deform = {C, deformations}. Note that usually C is not ofComplex F.
      
   Example
      R=QQ[x_0..x_4]
      I=ideal(x_0*x_1*x_2,x_3*x_4)
      C1=idealToComplex I
      F=C1.fc_0_0
      deformationsFace(F,C1)
      F=C1.fc_0_1
      deformationsFace(F,C1)
      F=C1.fc_1_0
      deformationsFace(F,C1)
      F=C1.fc_2_0
      deformationsFace(F,C1)
   Text
      
   Example
      R=QQ[x_0..x_4]
      I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
      C1=idealToComplex I
      F=C1.fc_0_1
      deformationsFace(F,C1)
      F=C1.fc_1_1
      deformationsFace(F,C1)
  SeeAlso
     deform
     link
     globalSections
  Caveat
     To homogenize the denominators of deformations (which are supported inside the link) we use
     globalSections to deal with the toric case. Speed of this should be improved. For ordinary 
     projective space globalSections works much faster.
///




doc ///
  Key
    deform
    (deform,Complex)
  Headline
    Compute the deformations associated to a Stanley-Reisner complex.
  Usage
    deform(C)
  Inputs
    C:Complex
  Outputs
    :List
  Description
   Text
      Compute a list with the homogeneous (i.e., @TO (degree,FirstOrderDeformation)@ zero) deformations associated to 
      Stanley-Reisner complex C.

      The result of the computation is stored in the key C.deform and this data can be stored to and loaded from a
      file by @TO saveDeformations@ and @TO loadDeformations@.

                  
   Example
      R=QQ[x_0..x_4]
      I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0)
      C1=idealToComplex I
      deform C1
  SeeAlso
     deformationsFace
     loadDeformations
     saveDeformations
     link
     globalSections
  Caveat
     To homogenize the denominators of deformations (which are supported inside the link) we use
     globalSections to deal with the toric case. Speed of this should be improved. For ordinary 
     projective space globalSections works much faster.
///



doc ///
  Key
    joinVectors
    (joinVectors,List)
  Headline
    Converts a list of vectors to a matrix.
  Usage
    joinVectors(C)
  Inputs
    L:List
       of @TO Vector@s
  Outputs
    :Matrix
  Description
   Text
      Puts the vectors in a list into a matrix.
                  
   Example
      joinVectors {vector {1,2},vector {3,4},vector {5,6}}
///



doc ///
  Key
    minimalNonFaces
    (minimalNonFaces,Complex)
  Headline
    The minimal non-faces of a complex.
  Usage
    minimalNonFaces(C)
  Inputs
    C:Complex
  Outputs
    :List
  Description
   Text
      The minimal non-faces of a complex.
      All subsets of the variables of @TO simplexRing@(C) are tested as standard.
      
      If the option embedded=>true is specified then only faces of @TO embeddingComplex@(C)
      are tested.
      
   Example
     R=QQ[x_0..x_4];
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     C=idealToComplex I
     minimalNonFaces C
  SeeAlso
     idealToComplex
     complexToIdeal
     embeddingComplex
///


doc ///
  Key
    possibleDenominators
    (possibleDenominators,Ideal)
    (possibleDenominators,Ideal,List)
    (possibleDenominators,List,List)
  Headline
    Possible denominators of deformations.
  Usage
    possibleDenominators(I)
    possibleDenominators(I,v)
  Inputs
    I:Ideal
        monomial, reduced.
    v:List
        of variables of @TO ring@ I.
  Outputs
    L:List
  Description
   Text
      Computes a list L of possible denominators of Laurent monomials representing 
      @TO FirstOrderDeformation@s of I. The list L is stratified into lists with 
      ascending degree of the denominators.
      Each denominator is represented as a list of variables.
      
      If the second argument v is given, one obtains only denominators contained
      (as a list) in v.
      
   Example
     R=QQ[x_0..x_4];
     I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     possibleDenominators(I)
     possibleDenominators(I,{x_0,x_1,x_2});
  SeeAlso
     deformationsFace
     deform
///


doc ///
  Key
    preImage
    (preImage,Matrix,Vector)
  Headline
    Compute the preimage.
  Usage
    preImage(A,v)
  Inputs
    A:Matrix
        of full rank.
    v:Vector
        in @TO target@ A.
  Outputs
    :Vector
        in @TO source@ A.
  Description
   Text
      Computes the preimage of v under A. If v is not in @TO image@ A then returns false.
      
   Example
     A=matrix {{1, 0}, {0, 1}, {-1, -1}}
     b=vector {-2,2,0}
     v=preImage(A,b)
     A*v
///

-*
doc ///
  Key
    selectFaces
    (selectFaces,Complex,Function)
  Headline
    Compute the subcomplex of faces satisfying a condition.
  Usage
    selectFaces(C,f)
  Inputs
    C:Complex
    f:Function
  Outputs
    :Complex
  Description
   Text
      Compute the subcomplex of faces satisfying a condition.
           
///
*-

doc ///
  Key
    (homology,Complex)
  Headline
    Compute the homology of a complex.
  Usage
    homology(C)
  Inputs
    C:Complex
  Outputs
    :List
      of finitely generated abelian groups (type defined in ConvexInterface)
  Description
   Text
     Computes the homology of a complex (not necessarily simplicial).
      
  Caveat
     To use this the package {\it ConvexInterface} has to be installed.
///



---------------------------------------------------------------------------------------
-- deformation polytope and its tropical sub-co-complex

doc ///
  Key
    PT1
    (PT1,Complex)
  Headline
    Compute the deformation polytope associated to a Stanley-Reisner complex.
  Usage
    PT1(C)
  Inputs
    C:Complex
  Outputs
    :Complex
  Description
   Text
      Compute the deformation polytope of C, i.e., the convex hull of all
       homogeneous (i.e., @TO (degree,FirstOrderDeformation)@ zero) deformations 
       associated to C, considering them as lattice monomials (i.e., their 
       preimages under C.grading).
                  
   Example
      R=QQ[x_0..x_3]
      I=ideal(x_0*x_1,x_2*x_3)
      C=idealToComplex I
      PT1C=PT1 C
  SeeAlso
     deformationsFace
     link
     globalSections
     tropDef
  Caveat
     To homogenize the denominators of deformations (which are supported inside the link) we use
     globalSections to deal with the toric case. The speed of this should be improved. For ordinary projective space 
     homogenization with support on F is done much faster.
///


doc ///
  Key
    tropDef
    (tropDef,Complex,Complex)
  Headline
    The co-complex of tropical faces of the deformation polytope.
  Usage
    tropDef(P,C)
  Inputs
    C:Complex
       a simplicial complex
    P:Complex
       the deformation polytope of C as returned by @TO PT1@ C.
  Outputs
    :CoComplex
  Description
   Text
    Computes the co-complex of tropical faces of the deformation polytope.

    This is work in progress.

   Example
      R=QQ[x_0..x_3]
      I=ideal(x_0*x_1,x_2*x_3)
      C=idealToComplex I
      PT1C=PT1 C
      tropDefC=tropDef(C,PT1C)
      tropDefC.grading
  SeeAlso
    PT1
    saveDeformations
    (convHull,String)
    deform
  Caveat
    The implementation of testing whether a face is tropical so far uses
    a trick to emulate higher order. For complicated (non-complete intersections
    and non-Pfaffians) examples this may lead to an incorrect result. Use with care.
    This will be fixed at some point.

    If using @TO OldPolyhedra@ to compute convex hulls and its faces instead of 
    {\it ConvexInterface} you are limited to rather simple examples.
///



-----------------------------------------------------------------------
-- example

doc ///
  Key
    mirrorSphere
  Headline
    Example how to compute the mirror sphere.
  Description
   Text
      Example how to compute the mirror sphere as an @TO Complex@.

      This is work in progress. Many interesting pieces are not yet implemented.
      
   Example
      R=QQ[x_0..x_4]
      I=ideal(x_0*x_1,x_2*x_3*x_4)
      C=idealToComplex I
      PT1C=PT1 C
      tropDefC=tropDef(C,PT1C)
      tropDefC.grading
      B=dualize tropDefC
      B.grading
      fvector C
      fvector B
  SeeAlso
     PT1
     tropDef
     (homology,Complex)
  Caveat
    The implementation of testing whether a face is tropical so far uses
    a trick to emulate higher order. For very complicated (non-complete intersections
    and non-Pfaffians) examples this may lead to an incorrect result. Use with care.
    This will be fixed at some point.

    If using @TO OldPolyhedra@ to compute convex hulls and its faces instead of 
    {\it ConvexInterface} you are limited to rather simple examples.
///



-*
   Text
      
   Example
      R=QQ[x_0..x_6]
      I=ideal(x_0*x_1,x_2*x_3,x_4*x_5*x_6)
      C=idealToComplex I
      PT1C=PT1 C
      tropDefC=tropDef(C,PT1C)
      tropDefC.grading
      B=dualize tropDefC
      B.grading
      fvector C
      fvector B
   Text
      
   Example
      R=QQ[x_0..x_4]
      I=ideal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
      C=idealToComplex I
      PT1C=PT1 C
      tropDefC=tropDef(C,PT1C)
      tropDefC.grading
      B=dualize tropDefC
      B.grading
      fvector C
      fvector B
   Text
      
   Example
      R=QQ[x_0..x_6]
      C=boundaryCyclicPolytope(4,R)
      I=complexToIdeal C
      betti res I
      def=deform C
      #def
      T1=select(def,j->(isTrivial j)==false)
      #T1
      for j from 0 to #T1-1 do print(j,toHom T1#j);
      PT1C=PT1 C
      tropDefC=tropDef(C,PT1C)
      tropDefC.grading
      B=dualize tropDefC
      B.grading
      fvector C
      fvector B

*-

doc ///
  Key
    file
    [convHull,file]
    [PT1,file]
    [hull,file]
  Headline
    Store result of a computation in a file.
  Description
   Text
      If the option file=>fn with a @TO String@ fn is given then the result of the computation is stored in the file fn.
      
  SeeAlso
     PT1
     convHull
     hull
  Caveat
    This works only when using the package ConvexInterface.
///

doc ///
  Key
    saveDeformations
    (saveDeformations,Complex,String)
  Headline
    Store the deformation data of a complex in a file.
  Usage
    saveDeformations(C,fn)
  Inputs
    C:Complex
    fn:String
       file name.
  Outputs
    :List
       with all @TO FirstOrderDeformation@s.
  Description
   Text
      Store the deformation data of a complex C in a file with name fn.
      
  SeeAlso
     loadDeformations
     deform
///

doc ///
  Key
    loadDeformations
    (loadDeformations,Complex,String)
  Headline
    Read the deformation data of a complex from a file.
  Usage
    loadDeformations(C,fn)
  Inputs
    C:Complex
    fn:String
       file name.
  Outputs
    :Complex
  Description
   Text
     Read the deformation data of a complex C from a file with name fn.

     The deformations are stored in C.deform.
      
  SeeAlso
     saveDeformations
     deform
  Caveat
     This does not change deformation data which may be stored in the individual faces.
///


doc ///
  Key
    hull    
    (hull,List)
    (hull,String)
  Headline
    The positive hull complex.
  Usage
    hull(L)
    hull(fn)
  Inputs
    L:List
       of @TO Vector@s lying all in the same space.
    fn:String
  Outputs
    C:Complex
  Description
   Text
        Returns the cone which is the hull of lattice vectors in L.
        The output has C.isPolytope==true.

        If applied to the string fn the result of a previous computation stored via the option @TO file@ is read from the file fn.

        The hull of L has to be strictly convex.
        
   Example
    L= {{0,1,1,0,0},{0,1,0,1,0},{0,1,0,0,0},{1,0,0,0,1},{1,0,-1,-1,-1},{1,0,0,0,0}};
    L=apply(L,vector)
    C=hull L
    C.grading
    dC=dualize C
    dC.grading
  SeeAlso
     Complex
  Caveat
     The cone is represented as a complex on its rays, hence if @TO (dim,Face)@ is applied to a @TO Face@ it
     will return the dimension of the corresponding cone minus one.
      
     This uses the package OldPolyhedra.m2 to compute the facets. Too slow compared to Maple/convex.

     If the package {\it ConvexInterface} is loaded, then this command calls Maple/Convex.
     See the corresponding option explained at @TO SRdeformations@.
///


-*
installPackage("SRdeformations")
installPackage("SRdeformations",RerunExamples=>true)
*-
