-- todo: 
-- done: factorial, factorof, quotient, remainder

OMSEvaluators#"integer1" = new MutableHashTable;
OMSEvaluators#"integer1"#"factorial" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0)! )
OMSEvaluators#"integer1"#"factorof" = (args, attrs) -> ( a := apply(args, fromOpenMath); 0 == (a#1) % (a#0) )
OMSEvaluators#"integer1"#"quotient" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) // (a#1) )
OMSEvaluators#"integer1"#"remainder" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0) % (a#1) )

