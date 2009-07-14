-- symbols defined here: abs, divide, gcd, lcm, minus, plus, power, product, root, sum, times, unary_minus

--- From OpenMath ---
OMSParsers#"arith1.abs" = args -> ( a := apply(args, fromOpenMath); abs(a#0))
OMSParsers#"arith1.divide" = args -> ( a := apply(args, fromOpenMath); a#0/a#1 )
OMSParsers#"arith1.gcd" = args -> gcd(apply(args, fromOpenMath))
OMSParsers#"arith1.lcm" = args -> lcm(apply(args, fromOpenMath))
OMSParsers#"arith1.minus" = args -> ( a := apply(args, fromOpenMath); a#0 - a#1 )
OMSParsers#"arith1.plus" = args -> sum(apply(args, fromOpenMath))
OMSParsers#"arith1.power" = args -> ( a := apply(args, fromOpenMath); (a#0)^(a#1) )
OMSParsers#"arith1.times" = args -> ( a := apply(args, fromOpenMath); (a#0)*(a#1) )
OMSParsers#"arith1.unary_minus" = args -> ( a := apply(args, fromOpenMath); -(a#0))

-- OMSParsers#"arith1.product" = args -> 
-- OMSParsers#"arith1.root" = args -> 
-- OMSParsers#"arith1.sum" = args -> 


