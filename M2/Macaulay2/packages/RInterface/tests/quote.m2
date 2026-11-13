assert Equation(toString RQuote "x", "x")
assert Equation(toString RQuote x, "x")
assert Equation(toString RQuote log, "log")
assert Equation((RFunction log) RObject 1, RObject 0)
