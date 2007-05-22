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

export {AbstractVariety, AbstractVarietyMap, AbstractSheaf, flagVariety, intersectionRing, cc, ch, ChernClass}

AbstractVariety = new Type
AbstractVarietyMap = new Type
AbstractSheaf = new Type
intersectionRing = method()
intersectionRing AbstractVariety := X -> X.intersectionRing
ChernClass = new Type of BasicList
cc = method()
cc(ZZ,Symbol) := (n,E) -> new ChernClass from {n,E}

flagVariety = method()
flagVariety(AbstractVariety,AbstractSheaf,List,List) := (X,E,bundleNames,bundleRanks) -> (
     
     )

beginDocumentation()

end
