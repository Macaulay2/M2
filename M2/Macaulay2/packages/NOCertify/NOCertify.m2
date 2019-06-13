packageName="NOCertify"
newPackage(
        packageName,
        Version => "0.01", 
        Date => "July 20, 2018",
        Authors => {
	    {Name => "Tim Duff", 
	     Email => "tduff3@gatech.edu", 
             HomePage => "http://people.math.gatech.edu/~tduff3/"}
	      },
        Headline => "Certification of Overdetermined Systems via Newton-Okounkov Bodies",
	PackageImports => {},
	PackageExports => {"MonodromySolver","Polyhedra"},
        DebuggingMode => true
        )


export{-- LAlgebra types and methods
"LAlgebra","lAlgebra","FirstBasis","LastBasis","HilbertTable",
"NOBodyVRep","degreeYL","triangularBasis", "LastRays",
-- "numerical" types and methods
"createSquaredUpFamily","Qi", "makeQi","makeCC","taylorSeries","NewVariables"
}
   

-*
Code for L-Algebras.

General thoughts:

1) should I allow for 
2) 

*-

LAlgebra = new Type of MutableHashTable

--todo: keep track of all extreme rays computed so far
lAlgebra = method(Options=>{})
lAlgebra List := o -> L -> (
    L':=triangularBasis L;
    H:=new MutableHashTable from {1=>#L'};
    new LAlgebra from {Ring=>ring first L,
	FirstBasis=>L',LastBasis=>L',HilbertTable=>H,
	LastRays=>vertices convexHull NOBodyVRep L'}
    )
    
hilbertFunction (ZZ,LAlgebra) := (k,A) -> (
    k':=max keys A.HilbertTable;
    if k <= k' then return (A.HilbertTable)#k;
    (L,L'):=(A.FirstBasis,A.LastBasis);
    for i from k'+1 to k do (
	    L'=triangularBasis flatten for p in L list for q in L' list p*q;
--	    unique flatten apply(L,p->apply(L',q->p*q));
	    (A.HilbertTable)#i = #L';
	    A.LastRays = vertices convexHull (
		A.LastRays | (1/i)*NOBodyVRep L');
	);
    A.LastBasis=L';
    (A.HilbertTable)#k
    )
    
TEST ///
R=QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},3},Global=>false]
A=lAlgebra {x*y,x*z,y*z}
assert(hilbertFunction(5,A)==21)
///

    

Qi=QQ[i]/(i^2+1) -- global variable giving ring of Gaussian rationals
-*
makeQi = method(Options=>{})
makeQi CC := o -> z -> promote(realPart z,QQ)+i_Qi*promote(imaginaryPart z,QQ)
makeQi Point := o -> p ->  point{apply(p.Coordinates,c->makeQi c)}
makeQi List := o -> L -> apply(L,p->makeQi p)
--makeQi PolySystem TODO

makeCC = method(Options=>{})
makeCC Qi := o -> z -> (
    m:=sub(last coefficients z,CC);
    m_(1,0)+ii*m_(0,0)
    )
makeCC Point := o -> p ->  point{apply(p.Coordinates,c->makeCC c)}
makeCC List := o -> L -> apply(L,p->makeCC p)
makeCC (RingElement,Ring) := o -> (f,S) -> (
    if (Qi =!= coefficientRing ring f) then error("methods requires input w/ i coefficients");
    es:=exponents f;
    cs:=flatten entries last coefficients f;
    apply(cs,c->(makeCC sub(c,Qi))*apply(numgens S,i->(gens S)#i^(es#i)))

--makeCC PolySystem := o -> P -> (
  --  if coefficientRing ring P

input
promote(polySystem,CC) := (P,Qi) -> (
    R:=ring L;
    if (not instance(coefficientRing R,Qi) then error("polySystem

-- THIS WILL BE A SOFT CERTIFICATE UNLESS WE CONVERT P TO A QI-SYSTEM
alphaBetaGamma = (P,p) -> (
    J:=evaluate(jacobian P,makeQi p);
    beta:=norm(inverse J*evaluate(P,p));
    
*-

flattenJacobian = (M,X,numeqs) -> (
    n:=numcols X;
    while (numrows M > numeqs) do (
	m:=sub(numrows M/n,ZZ);
	M=sum apply(n,i->X_(0,i)*M^(toList(i*m..(i+1)*m-1)));
	);
    M*transpose X
    )

-- methods for storing the coefficients only?
taylorSeries=method(Options=>{NewVariables=>true,Verbose=>false})
taylorSeries (PolySystem,Point) := o -> (P,p) -> (
    R:=ring P;
    d:=max apply(equations P,e-> first degree e);
    J:=jacobian P;
    vec:=vars R;
    if not o.NewVariables then vec=vec - sub(matrix p,R);
    numeqs:=P.NumberOfPolys;
    g:=evaluate(P,p)+evaluate(J,p)*transpose vec;
    fac:=1;
    for i from 2 to d do(
	J=jacobian J;
	fac=(1/i)*fac;
	g=g+fac*flattenJacobian(sub(evaluate(J,p),R),vec,numeqs);
	);
    if (o.NewVariables and o.Verbose) then print("Taylor expansion in coordinates centered at" | toExternalString p.Coordinates);
    g
    )

-- NewVariables=>false should yield output=input
TEST ///
R=CC[x,y,z]
P=polySystem{x^2+1+z-y^3+x*y,x^2-1}
p=point{{0,0,0}}
assert(equations P==flatten entries taylorSeries(P,p))
///

triangularBasis = L -> (
    R:=ring first L;
    --if (options R).Global then error("use a local order!");
    mons:=rsort unique join flatten apply(L,f-> flatten entries first coefficients f);
    A:=matrix apply(L,f->flatten entries last coefficients(f,Monomials=>mons));
    WW:=symbol WW;
    S:=QQ[WW_1..WW_(numcols A)];
    delete(0_R,flatten entries(matrix{mons}*sub(last coefficients(gens gb ideal(sub(A,S)*transpose vars S)),R)))
    )

-- deprecated?
NOBodyVRep = L -> (
    R:=ring first L;
    --if (options R).Global then error("use a local order!");
    mons:=rsort unique join flatten apply(L,f-> flatten entries first coefficients f);
    A:=matrix apply(L,f->flatten entries last coefficients(f,Monomials=>mons));
    WW:=symbol WW;
    S:=QQ[WW_1..WW_(numcols A)];
    transpose matrix apply(flatten entries(matrix{mons}*sub(last coefficients(gens gb ideal(sub(A,S)*transpose vars S)),R)),p->flatten exponents leadMonomial p)
    )

    
degreeYL = L -> (
    V:=NOBodyVRep L;
    ind:=det smithNormalForm(matrix V,ChangeMatrix=>{false,false},KeepZeroes=>false);
    ind*latticeVolume convexHull V
    )

--thre circles intersecting in 1 pt
TEST ///
R=Qi[x,y,MonomialOrder=>{Weights=>{-1,-1},1},Global=>false]
L1=sort {(x^2+y^2-4),((x-1)^2+(y-1)^2-2),((x-3)^2+(y+1)^2-2)}
assert(degreeYL L1==1)
///

-- toric kodaira map, cusp in P^2
TEST /// 
R=Qi[x,MonomialOrder=>{Weights=>{-1},1},Global=>false]
assert(degreeYL {1_R,x^2,x^3}==3)
///


createSquaredUpFamily = L -> (
    W:=symbol W;
    R:=ring L#0;
    n:=numgens R;
    S:=CC[flatten apply(n,i->apply(#L,j->W_(i,j)))][gens R];
    polySystem apply(n,i->sum apply(#L,j->W_(i,j)*sub(L#j,S)))
    )	
    
end


--block which is useful for debugging
uninstallPackage packageName
restart
packageName="NOCertify"
installPackage(packageName,DebuggingMode=>true)
--check packageName
--symbolic

restart
needsPackage "NOCertify"
R=QQ[x,y,MonomialOrder=>{Weights=>{-1,-1},2},Global=>false]
L={x,x*y-y^2,x*y^2}
A=lAlgebra L
hilbertFunction(2,A)
NOBodyVRep A.FirstBasis,

hilbertFunction(4,A)
peek A
peek A.HilbertTable



-*
R&S Example 1-1
L={x,x*y-y^2,x*y^2}
degree of Kodaira is 2
for x<y NOBody and ind are both 2
for y<x NOBody and ind are both 1 (I think)
*-
L={x,x*y-y^2,x*y^2}
(V,npaths)=monodromySolve createSquaredUpFamily L
sols=points V.PartialSols
m=apply(sols,p->evaluate(polySystem L,p))
apply(m,p->(1/p_(0,0))*p)
#(points V.PartialSols)

vertices convexHull NOBodyVRep L


L3=flatten apply(L,a->flatten apply(L,b->apply(L,c->a*b*c)))
one=apply(triangularBasis L3,p->first exponents leadMonomial p)

f0=first L
f1=L#1
f2=last L
apply(L,p->first exponents leadMonomial p)
vertices convexHull NOBodyVRep L--BUG
latticeVolume convexHull NOBodyVRep L--BUG
L2=triangularBasis unique flatten apply(L,p->apply(L,q->p*q))
(1/2^2)*latticeVolume convexHull NOBodyVRep L2
L3=triangularBasis unique flatten apply(L2,p->apply(L,q->p*q))
two=apply(L3,p->first exponents leadMonomial p)
vertices convexHull NOBodyVRep L2
vertices convexHull((1/3) * vertices convexHull NOBodyVRep L3|(1/2) * vertices convexHull NOBodyVRep L2)

L4=triangularBasis unique flatten apply(L3,p->apply(L,q->p*q))
vertices convexHull((1/4)*sub(NOBodyVRep L4,QQ)|(1/3)*sub(NOBodyVRep L3,QQ)|(1/2)*sub(NOBodyVRep L2,QQ)|NOBodyVRep L)
sub(oo,RR)

L5=triangularBasis unique flatten apply(L4,p->apply(L,q->p*q))
(1/5) * vertices convexHull NOBodyVRep L5

L6=triangularBasis unique flatten apply(L5,p->apply(L,q->p*q))
(1/5) * vertices convexHull NOBodyVRep L5

L7=triangularBasis unique flatten apply(L6,p->apply(L,q->p*q))
(1/7) * vertices convexHull NOBodyVRep L7

L8=triangularBasis unique flatten apply(L7,p->apply(L,q->p*q))
latticeVolume convexHull NOBodyVRep L8


L9=triangularBasis unique flatten apply(L8,p->apply(L,q->p*q))
(1/9) * vertices convexHull NOBodyVRep L9

L10=triangularBasis unique flatten apply(L9,p->apply(L,q->p*q))
# apply(triangularBasis L10,


L11=triangularBasis unique flatten apply(L10,p->apply(L,q->p*q))
(1/11) * vertices convexHull NOBodyVRep L11


latticeVolume convexHull NOBodyVRep L9




netList L2
f3=L2#2
f0*f3-2*f1*f2

L3=flatten apply(L,p->apply(L2,q->p*q))
member(f0*f3,L3)
member(f1*f2,L3)

L4=triangularBasis unique flatten apply(L,p->apply(L3,q->p*q))
L5=triangularBasis unique flatten apply(L,p->apply(L4,q->p*q))
vertices convexHull NOBodyVRep L5


 -- should get xy^4 somewhere!
--make ^^ work


PPP = points (first monodromySolve(createSquaredUpFamily L,NumberOfNodes=>3)).PartialSols
m=apply(PPP,p->matrix p)
apply(m,n->(1/n_(0,0))*n)




(1/2^2) * latticeVolume  convexHull vertices latticeVolume convexHull NOBodyVRep L2
vertices convexHull NOBodyVRep L2
m=#L2
L3=unique flatten apply(L2,p->apply(L,q->p*q))
vertices convexHull NOBodyVRep L3
L4=unique flatten apply(L3,p->apply(L,q->p*q))
vertices convexHull NOBodyVRep L4

L5=unique flatten apply(L4,p->apply(L,q->p*q))
(1/25) * latticeVolume convexHull NOBodyVRep L5

L6=unique flatten apply(L5,p->apply(L,q->p*q))
(1/6^2) * latticeVolume convexHull NOBodyVRep L6

L7=unique flatten apply(L6,p->apply(L,q->p*q))
(1/7^2) * latticeVolume convexHull NOBodyVRep L7

L'=L
for j from 2 to 3 do (
    L'=unique flatten apply(L',p->apply(L,q->p*q))
    )
NOBodyVRep L'

L8=unique flatten apply(L7,p->apply(L,q->p*q))
vertices convexHull NOBodyVRep L8



S=Qi[apply(#L2,i->(symbol WW)_i)][gens R]
L2=apply(L2,x->sub(x,S))
level2=(sub(vars coefficientRing S,S)*transpose matrix{L2})_(0,0)
sub(level2,{WW_5=>0,WW_4=>0,WW_1=>0,WW_0=>0})

--numeric
restart
needsPackage "NOCertify"
R=CC[x,y,MonomialOrder=>{Weights=>{-1,-1},2},Global=>false]
MR2={x,x*y-y^2,x*y^2}--not all valuations give khovanskii basis
MR1={x+y,x*y,x*y^2}--no khovanskii basis? i think when y<x
x= points (first monodromySolve(createSquaredUpFamily MR1,NumberOfNodes=>4)).PartialSols






L2= sort {x^2+y^2-4,(x-1)^2+(y-1)^2-2,(x-2)^2+(y-2)^2-4}
degreeYL L2 -- BUG




V=NOBodyVRep L
D=convexHull V
vertices D
latticeVolume D
L'={y,x*y-x^2,y*x^2}
V'=NOBodyVRep L'
D'=convexHull V'
vertices D'
latticeVolume D
smithNormalForm V'

degreeYL L

netList points (first monodromySolve createSquaredUpFamily L).PartialSols



--alternating invariants
restart
needsPackage "NOCertify"
R=CC[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},3},Global=>false]
L={x+y+z,x*y+x*z+y*x,x*y*z,(x-y)*(x-z)*(y-z)}
x= points (first monodromySolve(createSquaredUpFamily L,Verbose=>true,NumberOfNodes=>4)).PartialSols
#x
ms=apply(x,p->evaluate(polySystem L,p))
netList apply(ms,m->(1/m_(0,0))*m)
realsols=solveSystem L
netList realsols

restart
R=QQ[x,y,z]
I=ideal{x+y+z,x*y+x*z+y*x,x*y*z,(x-y)*(x-z)*(y-z)}
multiplicity I

degreeYL L
V=NOBodyVRep L
latticeVolume convexHull V
latticeVolume convexHull transpose V
smithNormalForm V


R=Qi[x,y,MonomialOrder=>{Weights=>{-1,-1},2},Global=>false]
L1=sort {(x^2+y^2-4),((x-1)^2+(y-1)^2-2),((x-3)^2+(y+1)^2-2)}
solveSystem polySystem L1 -- ERROR

setRandomSeed 2
(V,npaths)=monodromySolve createSquaredUpFamily L1
ps=solveSystem V.SpecializedSystem
m=1/(first (makeQi first ps).Coordinates)*matrix makeQi first ps
n=1/(first (makeQi last ps).Coordinates)*sub(matrix makeQi last ps,frac Qi)
makeCC(m_(0,1)-n_(0,1))

1/(first (last ps).Coordinates)*matrix last ps
ps=solveSystem V.SpecializedSystem 
netList makeCC apply(makeQi ps,p->point evaluate(polySystem L1,p))

solveSystem V.SpecializedSystem

kod=apply(ps, p->evaluate(polySystem L,p))
(1/(first kod)_(0,0)*first kod,1/(last kod)_(0,0)*last kod)
makeQi first ps

#(points (first oo).PartialSols)

R=Qi[x,y,MonomialOrder=>{Weights=>{-1,-1},2},Global=>false]
L0={x^2+y^2-4,(x-1)^2+(y-1)^2-2,(x-3)^2+y^2-2}

setRandomSeed 2
V0 = first monodromySolve createSquaredUpFamily L0
ps=points V0.PartialSols
kod=apply(ps, p->evaluate(polySystem L0,p))
{1/(first kod)_(0,0)*first kod,1/(last kod)_(0,0)*last kod}

-*
--circles with 2 intersections
L2= sort {x^2+y^2-4,(x-1)^2+(y-1)^2-2,(x-2)^2+(y-2)^2-4}

--circles with 1 intersections
L1=sort {(x^2+y^2-4),((x-1)^2+(y-1)^2-2),((x-3)^2+(y+1)^2-2)}
*-

restart
R=QQ[z,t]
I=ideal{z^2*t,t}
integralClosure(R/I)
methods integralClosure 
S=QQ[A,B]

phi=map(R,S,{t,t*z^2})

phi A

phi(S)
integralClosure(S/(ker phi))
