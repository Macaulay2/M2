--done: list, map
--not done: suchthat

--- From OpenMath ---
OMSEvaluators#"list1" = new MutableHashTable;
OMSEvaluators#"list1"#"list" = (args, attrs) -> apply(args, fromOpenMath)
OMSEvaluators#"list1"#"map" = (args, attrs) -> (
	--slightly messy, but o/w is incompatible with the rest of the code.
	--sorry.
	f := fromOpenMath(args#0);
	l := fromOpenMath(args#1);
	apply(l, i -> f({toOpenMath i}, null))
)

--- To OpenMath ---
toOpenMath BasicList := l -> (
	OMA("list1", "list", apply(toList l, toOpenMath))
)

--Doesn't work since sequences are special.
--toOpenMath Sequence := l -> (
--	OMA("list1", "list", new List from (apply(l, toOpenMath)))
--)
