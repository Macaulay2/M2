-- symbols defined here: abs, divide, gcd, lcm, minus, plus, power, product, root, sum, times, unary_minus

--- From OpenMath ---
OMSEvaluators#"arith1" = new MutableHashTable;
OMSEvaluators#"arith1"#"abs" = args -> ( a := apply(args, fromOpenMath); abs(a#0))
OMSEvaluators#"arith1"#"divide" = args -> ( a := apply(args, fromOpenMath); a#0/a#1 )
OMSEvaluators#"arith1"#"gcd" = args -> gcd(apply(args, fromOpenMath))
OMSEvaluators#"arith1"#"lcm" = args -> lcm(apply(args, fromOpenMath))
OMSEvaluators#"arith1"#"minus" = args -> ( a := apply(args, fromOpenMath); a#0 - a#1 )
OMSEvaluators#"arith1"#"plus" = args -> sum(apply(args, fromOpenMath))
OMSEvaluators#"arith1"#"power" = args -> ( a := apply(args, fromOpenMath); (a#0)^(a#1) )
OMSEvaluators#"arith1"#"times" = args -> ( a := apply(args, fromOpenMath); (a#0)*(a#1) )
OMSEvaluators#"arith1"#"unary_minus" = args -> ( a := apply(args, fromOpenMath); -(a#0))

-- OMSEvaluators#"arith1.product" = args -> 
-- OMSEvaluators#"arith1.root" = args -> 
-- OMSEvaluators#"arith1.sum" = args -> 

