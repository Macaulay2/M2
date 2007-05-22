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

export {AbstractVariety, AbstractVarietyMap, AbstractSheaf, flagVariety, intersectionRing, cc, ch, ChernClass, chernClass, point}

AbstractVariety = new Type of MutableHashTable
AbstractVarietyMap = new Type of MutableHashTable
AbstractSheaf = new Type of MutableHashTable
intersectionRing = method()
intersectionRing AbstractVariety := X -> X.intersectionRing

chernClass = method()
chernClass AbstractSheaf := F -> F.ChernClass

chernClassValues = new MutableHashTable
ChernClass = new Type of BasicList
baseName ChernClass := identity
installMethod(symbol <-, ChernClass, (c,x) -> chernClassValues#c = x)
value ChernClass := c -> if chernClassValues#?c then chernClassValues#c else c
cc = method()
cc(ZZ,Symbol) := (n,E) -> value new ChernClass from {n,E}
expression ChernClass := c -> new FunctionApplication from {new Subscript from {cc,c#0}, c#1}

OO(AbstractVariety) := X -> new AbstractSheaf from {
     symbol AbstractVariety => X,
     symbol ChernClass => 1_(intersectionRing X)
     }

AbstractSheaf ^ ZZ := (E,n) -> new AbstractSheaf from {
     symbol AbstractVariety => X,
     symbol ChernClass => E.ChernClass ^ n
     }

point = new AbstractVariety
point.intersectionRing = QQ

flagVariety = method()
flagVariety(AbstractVariety,AbstractSheaf,List,List) := (X,E,bundleNames,bundleRanks) -> (
     vrs := apply(bundleNames, bundleRanks, (E,r) -> apply(toList(1 .. r), i -> cc_i E));
     dgs := flatten apply(bundleRanks, r -> 1 .. r);
     A := (intersectionRing X)[flatten vrs,Degrees=>dgs];
     rlns := product apply(vrs, x -> 1+sum(x,value)) - promote(chernClass E,A);
     rlns = apply(1 .. first degree rlns, i -> part_i rlns);
     B := A/rlns;
     use B;
     B)

beginDocumentation()

end

loadPackage "Schubert"
errorDepth = 0
flagVariety(point,OO_point^4,{Q,R},{2,2})
