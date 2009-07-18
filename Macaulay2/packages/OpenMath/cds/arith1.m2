-- done: abs, divide, gcd, lcm, minus, plus, power, times, unary_minus
-- not done / maybe todo: product, root, sum

--- From OpenMath ---
OMSEvaluators#"arith1" = new MutableHashTable;
OMSEvaluators#"arith1"#"abs" = (args, attrs) -> ( a := apply(args, fromOpenMath); abs(a#0))
OMSEvaluators#"arith1"#"divide" = (args, attrs) -> ( a := apply(args, fromOpenMath); a#0/a#1 )
OMSEvaluators#"arith1"#"gcd" = (args, attrs) -> gcd(apply(args, fromOpenMath))
OMSEvaluators#"arith1"#"lcm" = (args, attrs) -> lcm(apply(args, fromOpenMath))
OMSEvaluators#"arith1"#"minus" = (args, attrs) -> ( a := apply(args, fromOpenMath); a#0 - a#1 )
OMSEvaluators#"arith1"#"plus" = (args, attrs) -> sum(apply(args, fromOpenMath))
OMSEvaluators#"arith1"#"power" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0)^(a#1) )
OMSEvaluators#"arith1"#"times" = (args, attrs) -> ( a := apply(args, fromOpenMath); (a#0)*(a#1) )
OMSEvaluators#"arith1"#"unary_minus" = (args, attrs) -> ( a := apply(args, fromOpenMath); -(a#0))

-- OMSEvaluators#"arith1.product" = (args, attrs) -> 
-- OMSEvaluators#"arith1.root" = (args, attrs) -> 
-- OMSEvaluators#"arith1.sum" = (args, attrs) -> 

