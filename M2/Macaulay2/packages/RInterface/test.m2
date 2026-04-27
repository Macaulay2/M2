----------------------
-- RInterface tests --
----------------------

-----------
-- tests --
-----------

TEST ///
-- basic conversions
assert BinaryOperation(symbol ===, value RObject null, null)
assert Equation(value RObject (1, 2, 3), (1, 2, 3))
assert BinaryOperation(symbol ===,
    value RObject ("foo" => 1, "bar" => 2), ("foo" => 1, "bar" => 2))
assert Equation(value RObject true, true)
assert Equation(value RObject false, false)
assert Equation(value RObject 5, 5)
assert Equation(value RObject 0.2, 0.2)
assert Equation(value RObject(1/2), 0.5)
assert Equation(value RObject pi, numeric pi)
assert Equation(value RObject(2 + 3*ii), 2 + 3*ii)
assert Equation(value RObject "foo", "foo")
assert Equation(value RObject {1, 2, 3}, {1, 2, 3})
assert BinaryOperation(symbol ===,
    value RObject {"foo" => 1, "bar" => 2}, {"foo" => 1, "bar" => 2})
///


TEST ///
-- iterators
assert Equation(toList RObject null, {})
c = RFunction "c"
assert Equation(value c(true, false), {true, false})
assert Equation(value c(1, 2, 3), {1, 2, 3})
assert Equation(value c(1.5, 2.5), {1.5, 2.5})
assert Equation(value c(ii, 2*ii), {ii, 2*ii})
assert Equation(value c("foo", "bar"), {"foo", "bar"})
list' = RFunction "list"
assert Equation(value list'(true, 2, pi), {true, 2, numeric pi})
///

TEST ///
-- subscripting
x = RObject {2, 4, 6, 8, 10}
assert Equation(x_1, RObject 2)
assert Equation(x_2, RObject 4)
assert Equation(x_3, RObject 6)
assert Equation(x_4, RObject 8)
assert Equation(x_5, RObject 10)
assert Equation(x[1], RObject 2)
assert Equation(x[1, 3, 5], RObject {2, 6, 10})
assert Equation(x_1 = 3, RObject {3, 4, 6, 8, 10})
assert Equation(x[1, 3, 5] = {3, 7, 11}, RObject {3, 4, 7, 8, 11})
///

TEST ///
-- binary operators
assert Equation(RObject 5 + RObject 2, RObject 7)
assert Equation(RObject 5 + 2, RObject 7)
assert Equation(5 + RObject 2, RObject 7)
assert Equation(RObject 5 - RObject 2, RObject 3)
assert Equation(RObject 5 - 2, RObject 3)
assert Equation(5 - RObject 2, RObject 3)
assert Equation(RObject 5 * RObject 2, RObject 10)
assert Equation(RObject 5 * 2, RObject 10)
assert Equation(5 * RObject 2, RObject 10)
assert Equation(RObject 5 / RObject 2, RObject 2.5)
assert Equation(RObject 5 / 2, RObject 2.5)
assert Equation(5 / RObject 2, RObject 2.5)
assert Equation(RObject 5^(RObject 2), RObject 25)
assert Equation(RObject 5^2, RObject 25)
assert Equation(5^(RObject 2), RObject 25)
assert Equation(RObject 5 % RObject 2, RObject 1)
assert Equation(RObject 5 % 2, RObject 1)
assert Equation(5 % RObject 2, RObject 1)
assert Equation(RObject 5 // RObject 2, RObject 2)
assert Equation(RObject 5 // 2, RObject 2)
assert Equation(5 // RObject 2, RObject 2)
assert Equation(RObject 5 ?? RObject 2, RObject 5)
assert Equation(RObject null ?? RObject 2, RObject 2)
///

TEST ///
-- logic operators
assert Equation(RObject true and RObject true, RObject true)
assert Equation(RObject true and true, RObject true)
assert Equation(true and RObject true, RObject true)
assert Equation(RObject true or RObject true, RObject true)
assert Equation(RObject true or true, RObject true)
assert Equation(true or RObject true, RObject true)
assert Equation(RObject true xor RObject true, RObject false)
assert Equation(RObject true xor true, RObject false)
assert Equation(true xor RObject true, RObject false)
assert Equation(not RObject true, RObject false)
///

TEST ///
-- comparison operators
assert Equation(RObject 2, RObject 2)
assert Equation(RObject 2, 2)
assert Equation(2, RObject 2)
assert BinaryOperation(symbol <, RObject 2, RObject 3)
assert BinaryOperation(symbol <, RObject 2, 3)
assert BinaryOperation(symbol <, 2, RObject 3)
assert BinaryOperation(symbol <=, RObject 2, RObject 3)
assert BinaryOperation(symbol <=, RObject 2, 3)
assert BinaryOperation(symbol <=, 2, RObject 3)
assert BinaryOperation(symbol >, RObject 3, RObject 2)
assert BinaryOperation(symbol >, RObject 3, 2)
assert BinaryOperation(symbol >, 3, RObject 2)
assert BinaryOperation(symbol >=, RObject 3, RObject 2)
assert BinaryOperation(symbol >=, RObject 3, 2)
assert BinaryOperation(symbol >=, 3, RObject 2)
assert BinaryOperation(symbol !=, RObject 2, RObject 3)
assert BinaryOperation(symbol !=, RObject 2, 3)
assert BinaryOperation(symbol !=, 2, RObject 3)
///

TEST ///
-- matrices/arrays
c = RFunction "c"
matrix' = RFunction "matrix"
array = RFunction "array"
assert Equation(value matrix'(c(1, 2, 3, 11, 12, 13), "nrow" => 2, "ncol" => 3),
    {{1, 2}, {3, 11}, {12, 13}})
assert Equation(value array(c splice(5, 9, 3, 10..15), "dim" => c(3, 3, 2)), {
	{{5, 9, 3}, {10, 11, 12}, {13, 14, 15}},
	{{5, 9, 3}, {10, 11, 12}, {13, 14, 15}}})
///

TEST ///
-- functions
assert Equation(+RObject 1, RObject 1)
assert Equation(-RObject 1, RObject(-1))
assert Equation(abs RObject(-1), RObject 1)
assert Equation(acos RObject 1, RObject 0)
assert Equation(acosh RObject 1, RObject 0)
assert Equation(asin RObject 0, RObject 0)
assert Equation(asinh RObject 0, RObject 0)
assert Equation(atan RObject 0, RObject 0)
assert Equation(atanh RObject 0, RObject 0)
assert Equation(ceiling RObject 2.5, RObject 3)
assert Equation(cos RObject 0, RObject 1)
assert Equation(cosh RObject 0, RObject 1)
assert Equation(exp RObject 0, RObject 1)
assert Equation(expm1 RObject 0, RObject 0)
assert Equation(floor RObject 2.5, RObject 2)
assert Equation(log RObject 1, RObject 0)
assert Equation(log1p RObject 0, RObject 0)
assert Equation(max RObject {1, 3, 5}, RObject 5)
assert Equation(min RObject {1, 3, 5}, RObject 1)
assert Equation(round RObject 2.6, RObject 3)
assert Equation(sin RObject 0, RObject 0)
assert Equation(sinh RObject 0, RObject 0)
assert Equation(sqrt RObject 1, RObject 1)
assert Equation(sum RObject {1, 3, 5}, RObject 9)
assert Equation(tan RObject 0, RObject 0)
assert Equation(tanh RObject 0, RObject 0)
assert Equation((RObject 1)!, RObject 1)
assert Equation((RObject 1)~, RObject(-2))
assert Equation(conjugate RObject ii, RObject(-ii))
assert(abs(Digamma RObject 1 + RObject EulerConstant) <
    RObject(1e-15))
assert Equation(Gamma RObject 1, RObject 1)
assert Equation(imaginaryPart RObject ii, RObject 1)
assert Equation(lngamma RObject 1, RObject 0)
assert Equation(product RObject {1, 3, 5}, RObject 15)
assert Equation(realPart RObject ii, RObject 0)
assert Equation(truncate RObject 2.5, RObject 2)
assert Equation(value((RObject 1):(RObject 5)), {1, 2, 3, 4, 5})
assert Equation(value((RObject 1):5), {1, 2, 3, 4, 5})
assert Equation(value((RObject 1)..(RObject 5)), {1, 2, 3, 4, 5})
assert Equation(value((RObject 1)..5), {1, 2, 3, 4, 5})
assert Equation(value(1..(RObject 5)), {1, 2, 3, 4, 5})
assert Equation(RObject 3 & RObject 5, RObject 1)
assert Equation(RObject 3 & 5, RObject 1)
assert Equation(3 & RObject 5, RObject 1)
assert Equation(RObject 3 | RObject 5, RObject 7)
assert Equation(RObject 3 | 5, RObject 7)
assert Equation(3 | RObject 5, RObject 7)
assert Equation(RObject 3 ^^ RObject 5, RObject 6)
assert Equation(RObject 3 ^^ 5, RObject 6)
assert Equation(3 ^^ RObject 5, RObject 6)
assert Equation(RObject 3 << RObject 5, RObject 96)
assert Equation(RObject 3 << 5, RObject 96)
assert Equation(3 << RObject 5, RObject 96)
assert Equation(RObject 10 >> RObject 1, RObject 5)
assert Equation(RObject 10 >> 1, RObject 5)
assert Equation(10 >> RObject 1, RObject 5)
assert Equation(atan2(RObject 0, RObject 1), RObject 0)
assert Equation(atan2(RObject 0, 1), RObject 0)
assert Equation(atan2(0, RObject 1), RObject 0)
assert Equation(Beta(RObject 1, RObject 1), RObject 1)
assert Equation(Beta(RObject 1, 1), RObject 1)
assert Equation(Beta(1, RObject 1), RObject 1)
assert Equation(binomial(RObject 1, RObject 1), RObject 1)
assert Equation(binomial(RObject 1, 1), RObject 1)
assert Equation(binomial(1, RObject 1), RObject 1)
///
