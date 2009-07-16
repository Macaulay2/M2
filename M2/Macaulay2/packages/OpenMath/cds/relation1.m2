-- done: eq, geq, gt, leq, lt, neq
-- not done / maybe todo: approx, 

--- From OpenMath ---
OMSEvaluators#"relation1" = new MutableHashTable;
OMSEvaluators#"relation1"#"eq" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) == (a#1) )
OMSEvaluators#"relation1"#"geq" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) >= (a#1) )
OMSEvaluators#"relation1"#"gt" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) > (a#1) )
OMSEvaluators#"relation1"#"leq" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) <= (a#1) )
OMSEvaluators#"relation1"#"lt" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) < (a#1) )
OMSEvaluators#"relation1"#"neq" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) != (a#1) )

