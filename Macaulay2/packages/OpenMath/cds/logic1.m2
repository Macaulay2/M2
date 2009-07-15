-- done: false, true, and, or, not
-- not done / maybe todo: equivalent, implies, xor

--- From OpenMath ---
OMSEvaluators#"logic1" = new MutableHashTable;
OMSEvaluators#"logic1"#"true" = true
OMSEvaluators#"logic1"#"false" = false
OMSEvaluators#"logic1"#"and" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) and (a#1) )
OMSEvaluators#"logic1"#"or" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) or (a#1) )
OMSEvaluators#"logic1"#"not" = (args, attrs) -> ( a := apply(args, fromOpenMath); not(a#0))

--- To OpenMath ---
toOpenMath Boolean := x -> if x then OMS("logic1", "true") else OMS("logic1", "false")
