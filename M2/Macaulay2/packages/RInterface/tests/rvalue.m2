-- RValue: basic evaluation
assert Equation(value RValue "TRUE", true)
assert Equation(value RValue "1L", 1)
assert Equation(value RValue "\"hello\"", "hello")

-- RValue: Sequence input (elements are concatenated as strings)
n = 5
assert Equation(value RValue("sum(1:", n, "L)"), 15)

-- RValue: Environment option
env = RObject hashTable {"x" => 3, "y" => 4}
assert Equation(value RValue("x + y", Environment => env), 7)
assert Equation(value RValue("x * y", Environment => env), 12)

-- RObject from HashTable: creates an R environment
env2 = RObject hashTable {"a" => 2_ZZ}
assert Equation(value RValue("a^3", Environment => env2), 8)

-- RContext: empty constructor
ctx = new RContext

-- RContext: constructor from string
ctx = RContext "p <- 7L"
assert Equation(value ctx_"p", 7)

-- RContext: evaluate more code in the same context
ctx "q <- p + 1L"
assert Equation(value ctx_"q", 8)

-- RContext: (RContext, String) returns result of evaluation
result = ctx "p * q"
assert Equation(value result, 56)

-- RContext: subscript operator
ctx2 = RContext "val <- 42L"
assert Equation(value ctx2_"val", 42)

-- listSymbols: check it runs without error on RContext
ctx3 = RContext "m <- 1L; n <- 2L"
listSymbols ctx3

-- listSymbols: check it runs without error on RObject (environment)
listSymbols ctx3.Environment

-- use RContext: imports variables into M2 symbol table
ctxUse = RContext "rval1 <- 5L; rval2 <- 10L"
use ctxUse
assert Equation(value rval1, 5)
assert Equation(value rval2, 10)
