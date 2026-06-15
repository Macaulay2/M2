assert Equation(toList RObject null, {})
c = RFunction "c"
assert Equation(value c(true, false), {true, false})
assert Equation(value c(1, 2, 3), {1, 2, 3})
assert Equation(value c(1.5, 2.5), {1.5, 2.5})
assert Equation(value c(ii, 2*ii), {ii, 2*ii})
assert Equation(value c("foo", "bar"), {"foo", "bar"})
list' = RFunction "list"
assert Equation(value list'(true, 2, pi), {true, 2, numeric pi})
x = RObject {10, 20, 30}
i = iterator x
assert Equation(next i, RObject 10)
assert Equation(next i, RObject 20)
assert Equation(next i, RObject 30)
assert Equation(length RSymbol "letters", 26)
assert Equation(length RSymbol letters, 26)
