-- done: 
-- not done / maybe todo: approx, eq, geq, gt, leq, lt, neq

--- From OpenMath ---
OMSEvaluators#"relation1" = new MutableHashTable;
OMSEvaluators#"relation1"#"eq" = args -> ( a := apply(args, fromOpenMath); (a#0) == (a#1) )
OMSEvaluators#"relation1"#"geq" = args -> ( a := apply(args, fromOpenMath); (a#0) >= (a#1) )
OMSEvaluators#"relation1"#"gt" = args -> ( a := apply(args, fromOpenMath); (a#0) > (a#1) )
OMSEvaluators#"relation1"#"leq" = args -> ( a := apply(args, fromOpenMath); (a#0) <= (a#1) )
OMSEvaluators#"relation1"#"lt" = args -> ( a := apply(args, fromOpenMath); (a#0) < (a#1) )
OMSEvaluators#"relation1"#"neq" = args -> ( a := apply(args, fromOpenMath); (a#0) != (a#1) )

