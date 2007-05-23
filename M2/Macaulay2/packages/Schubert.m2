newPackage(
	"Schubert",
    	Version => "0.1",
    	Date => "May, 2007",
	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	     {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	     },
	HomePage => "http://www.math.uiuc.edu/Macaulay2/",
    	Headline => "computations of characteristic classes for varieties without equations",
    	DebuggingMode => true
    	)

export {AbstractVariety, AbstractVarietyMap, AbstractSheaf, flagVariety, intersectionRing, ch, 
     ChernClass, point, DIM, logg, expp, todd, integral, ct}

AbstractVariety = new Type of MutableHashTable
AbstractVariety.synonym = "abstract variety"
globalAssignment AbstractVariety
net AbstractVariety := X -> (
     if ReverseDictionary#?X then toString ReverseDictionary#X
     else "--an abstract variety--")

AbstractVarietyMap = new Type of MutableHashTable
AbstractVarietyMap.synonym = "abstract variety map"

AbstractSheaf = new Type of MutableHashTable
AbstractSheaf.synonym = "abstract sheaf"

intersectionRing = method()
intersectionRing AbstractVariety := X -> X.intersectionRing

ct = method()
ct AbstractSheaf := F -> F.ChernClass

chernClassValues = new MutableHashTable
ChernClass = new Type of BasicList
baseName ChernClass := identity
installMethod(symbol <-, ChernClass, (c,x) -> chernClassValues#c = x)
value ChernClass := c -> if chernClassValues#?c then chernClassValues#c else c
c = method()
c(ZZ,Symbol) := (n,E) -> value new ChernClass from {n,E}
c(ZZ,AbstractSheaf) := (n,E) -> part(n,E.ChernClass)
expression ChernClass := c -> new FunctionApplication from {new Subscript from {symbol c,c#0}, c#1}
net ChernClass := net @@ expression

OO(AbstractVariety) := X -> new AbstractSheaf from {
     symbol AbstractVariety => X,
     symbol ChernClass => 1_(intersectionRing X),
     symbol rank => 1     
     }

AbstractSheaf ^ ZZ := (E,n) -> new AbstractSheaf from {
     symbol AbstractVariety => E.AbstractVariety,
     symbol ChernClass => E.ChernClass ^ n,
     symbol rank => E.rank * n
     }
rank AbstractSheaf := E -> E.rank

pointRing := QQ						    --  or QQ[] ?
pointRing.DIM = 0;

point = new AbstractVariety from {
     global intersectionRing => pointRing
     }

flagVariety = method()
flagVariety(ZZ,List,List) := (rk,bundleNames,bundleRanks) -> flagVariety(point,rk,bundleNames,bundleRanks)
flagVariety(AbstractVariety,ZZ,List,List) := (X,rk,bundleNames,bundleRanks) -> flagVariety(OO_X^rk,bundleNames,bundleRanks)
flagVariety(AbstractSheaf,List,List) := (E,bundleNames,bundleRanks) -> (
     X := E.AbstractVariety;
     n := #bundleRanks;
     if n =!= #bundleNames then error "name list and rank list should have same length";
     if rank E =!= sum bundleRanks then error "expected rank of bundle to equal sum of bundle ranks";
     vrs := apply(bundleNames, bundleRanks, (E,r) -> apply(toList(1 .. r), i -> new ChernClass from {i,E}));
     dgs := flatten apply(bundleRanks, r -> 1 .. r);
     T := (intersectionRing X)[flatten vrs,Degrees=>dgs,MonomialOrder => ProductOrder bundleRanks];
     (A,F,G) := flattenRing T;
     rlns := F (product apply(vrs, x -> 1+sum(x,value)) - promote(ct E,T));
     rlns = apply(1 .. first degree rlns, i -> part_i rlns);
     B := A/rlns;
     B.DIM = (intersectionRing X).DIM + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     use B;
     point := (basis_(B.DIM) B)_(0,0);			    -- not right, sign could be wrong!
     B.point = point;
     new AbstractVariety from {
	  global intersectionRing => B,
	  global point => point
	  }
     )

Grassmannian(ZZ,AbstractSheaf,List) := opts -> (k,E,bundleNames) -> flagVariety(E,bundleNames,{k,rank E-k})
Grassmannian(ZZ,ZZ,AbstractVariety,List) := opts -> (k,n,X,bundleNames) -> Grassmannian(k,OO_X^n,bundleNames)
Grassmannian(ZZ,ZZ,List) := opts -> (k,n,bundleNames) -> Grassmannian(k,n,point,bundleNames)

Proj(AbstractSheaf,List) := (E,bundleNames) -> Grassmannian(E.rank - 1,E,bundleNames)
Proj(ZZ,AbstractVariety,List) := (n,X,bundleNames) -> Proj(OO_X^(n+1),bundleNames)
Proj(ZZ,List) := (n,bundleNames) -> Proj(OO_point^(n+1),bundleNames)

integral = f -> coefficient((ring f).point, f)		    -- not right, sign could be wrong!

logg = method()
logg RingElement := (C) -> (
     -- C is the total chern class in an intersection ring
     -- The chern character of C is returned.
     if not (ring C).?DIM then error "expected a ring with DIM set";
     d := (ring C).DIM;
     p := new MutableList from splice{d+1:0}; -- p#i is (-1)^i * (i-th power sum of chern roots)
     e := for i from 0 to d list part(i,C); -- elem symm functions in the chern roots
     for n from 1 to d do
         p#n = -n*e#n - sum for j from 1 to n-1 list e#j * p#(n-j);
     sum for i from 1 to d list 1/i! * (-1)^i * p#i
     )

expp = method()
expp RingElement := (A) -> (
     -- A is the chern character
     -- the total chern class of A is returned
     if not (ring A).?DIM then error "expected a ring with DIM set";
     d := (ring A).DIM;
     p := for i from 0 to d list (-1)^i * i! * part(i,A);
     e := new MutableList from splice{d+1:0};
     e#0 = 1;
     for n from 1 to d do
	  e#n = - 1/n * sum for j from 1 to n list p#j * e#(n-j);
     sum toList e
     )

todd = method()
todd RingElement := (A) -> (
     -- A is the chern character
     -- the (total) todd class is returned
     if not (ring A).?DIM then error "expected a ring with DIM set";
     d := (ring A).DIM;
     -- step 1: find the first part of the Taylor series for t/(1-exp(-t))
     denom := for i from 0 to d list (-1)^i /(i+1)!;
     invdenom := new MutableList from splice{d+1:0};
     invdenom#0 = 1;
     for n from 1 to d do 
       invdenom#n = - sum for i from 1 to n list denom#i * invdenom#(n-i);
     -- step 2.  logg.  This is more complicated than desired.
     R := QQ (monoid[t]);
     R.DIM = d;
     td := logg sum for i from 0 to d list invdenom#i * R_0^i;
     td = for i from 0 to d list coefficient(R_0^i,td);
     -- step 3.  exp
     A1 := sum for i from 0 to d list i! * td#i * part(i,A);
     expp A1
     )

beginDocumentation()

end

compactMatrixForm = false
loadPackage "Schubert"

Proj(3,{Q,R})
P3 = Proj(3,{Q,R})
A = intersectionRing P3
prune oo
basis A
P3.point

G24 = Grassmannian(2,4,{Q,R})
C = intersectionRing G24
transpose presentation C

F22 = flagVariety(4,{Q,R},{2,2})
A = intersectionRing F22
transpose presentation A
basis A

F222 = flagVariety(6,{Q,R,S},{2,2,2})
B = intersectionRing F222
transpose presentation B
transpose basis B
(c_1 Q)^3 * (c_1 R)^5 * (c_1 S)^4
integral ((c_1 Q)^3 * (c_1 R)^5 * (c_1 S)^4)
F222.point
B.point

compactMatrixForm = false
loadPackage "Schubert"
X = new AbstractVariety from {
     global intersectionRing => QQ[e1,e2,Degrees=>{1,2}]/(e1^4,e2^2,e1^2*e2)
     }
X.intersectionRing.DIM = 3
E = new AbstractSheaf from {
     global AbstractVariety => X,
     global rank => 2,
     global ChernClass => 1 + e1 + e2
     }
c_0 E,c_1 E,c_2 E,c_3 E
P1 = Proj(E,{Q,R})
A = intersectionRing P1
x = c_1 Q
y = c_1 R
x*y

R = QQ[c1,c2,c3,c4,Degrees=>{1,2,3,4},MonomialOrder=>GRevLex=>{1,2,3,4}]
R.DIM = 4
c = 1 + sum gens R
a = logg c
expp a
todd a
