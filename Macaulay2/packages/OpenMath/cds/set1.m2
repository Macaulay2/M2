--done: set,emptyset, in, union, notin, map, size
--not done: cartesian_product, notprsubset, notsubset, 
--          prsubset, setdiff, subset, suchthat

--- From OpenMath ---
OMSEvaluators#"set1" = new MutableHashTable;
OMSEvaluators#"set1"#"set" = (args, attrs) -> new Set from apply(args, fromOpenMath)
OMSEvaluators#"set1"#"emptyset" = (new Set from {})
OMSEvaluators#"set1"#"in" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	(a#1)#?(a#0)
)
OMSEvaluators#"set1"#"notin" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	not (a#1)#?(a#0)
)
OMSEvaluators#"set1"#"size" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	#(a#0)
)
OMSEvaluators#"set1"#"union" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	r := a#0;
	for i in 1..(#a-1) do
		r = merge(r, a#i, (p,q)->1);
	r
)
OMSEvaluators#"set1"#"map" = (args, attrs) -> (
	--slightly messy, but o/w is incompatible with the rest of the code.
	--sorry.
	f := fromOpenMath(args#0);
	l := fromOpenMath(args#1);
	new Set from apply(keys(l), i -> f({toOpenMath i}, null))
)



--- To OpenMath ---
toOpenMath Set := l -> (
	OMA("set1", "set", apply(keys(l), toOpenMath))
)
