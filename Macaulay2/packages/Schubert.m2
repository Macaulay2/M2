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
     ChernClass, chernClass, point, DIM, logg, expp, integral}

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

chernClass = method()
chernClass AbstractSheaf := F -> F.ChernClass

chernClassValues = new MutableHashTable
ChernClass = new Type of BasicList
baseName ChernClass := identity
installMethod(symbol <-, ChernClass, (c,x) -> chernClassValues#c = x)
value ChernClass := c -> if chernClassValues#?c then chernClassValues#c else c
c = method()
c(ZZ,Symbol) := (n,E) -> value new ChernClass from {n,E}
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

pointRing := QQ[]
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
     A := (intersectionRing X)[flatten vrs,Degrees=>dgs];
     rlns := product apply(vrs, x -> 1+sum(x,value)) - promote(chernClass E,A);
     rlns = apply(1 .. first degree rlns, i -> part_i rlns);
     B := A/rlns;
     B.DIM = (intersectionRing X).DIM + sum(n, i -> sum(i+1 .. n-1, j -> bundleRanks#i * bundleRanks#j));
     use B;
     point := (basis_(B.DIM) B)_(0,0);
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

beginDocumentation()

end

compactMatrixForm = false

loadPackage "Schubert"
Proj(3,{Q,R})
P3 = Proj(3,{Q,R})
A = intersectionRing P3
prune oo
P3.point

G24 = Grassmannian(2,4,{Q,R})
C = intersectionRing G24
transpose presentation C

F22 = flagVariety(4,{Q,R},{2,2})
A = intersectionRing F22
transpose presentation A
A.DIM
basis_(A.DIM) A
F222 = flagVariety(6,{Q,R,S},{2,2,2})
B = intersectionRing F222
transpose presentation B
B.DIM
basis_(B.DIM) B
(c_1 Q)^3 * (c_1 R)^5 * (c_1 S)^4
integral ((c_1 Q)^3 * (c_1 R)^5 * (c_1 S)^4)
F222.point
B.point
