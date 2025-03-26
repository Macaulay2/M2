-----------------
-- logical xor --
-----------------
assert Equation(true xor true, false)
assert Equation(true xor false, true)
assert Equation(false xor true, true)
assert Equation(false xor false, false)

-- test precedence (and > xor)
assert Equation(false and true xor true, true)
assert Equation(true xor true and false, true)
assert Equation(false and false xor true, true)
assert Equation(true xor false and false, true)
-- (xor > or)
assert Equation(true xor true or true, true)
assert Equation(true or true xor true, true)
assert Equation(true xor false or true, true)
assert Equation(true or false xor true, true)

-----------------
-- bitwise xor --
-----------------
assert Equation(1 ^^ 1, 0)
assert Equation(1 ^^ 0, 1)
assert Equation(0 ^^ 1, 1)
assert Equation(0 ^^ 0, 0)

-- test precedence (& > ^^)
assert Equation(0 & 1 ^^ 1, 1)
assert Equation(1 ^^ 1 & 0, 1)
assert Equation(0 & 0 ^^ 1, 1)
assert Equation(1 ^^ 0 & 0, 1)
-- (^^ > |)
assert Equation(1 ^^ 1 | 1, 1)
assert Equation(1 | 1 ^^ 1, 1)
assert Equation(1 ^^ 0 | 1, 1)
assert Equation(1 | 0 ^^ 1, 1)


-----------------
-- bitwise not --
-----------------
assert Equation(1138~, -1139)
assert Equation((-1139)~, 1138)

-----------------
-- expressions --
-----------------
assert Equation(value BinaryOperation(symbol xor, true, true), false)
assert Equation(value BinaryOperation(symbol ^^, 1, 1), 0)

---------------
-- functions --
---------------
T = x -> true
F = x -> false
assert Equation((T and T)(), true)
assert Equation((T and F)(), false)
assert Equation((F and T)(), false)
assert Equation((F and F)(), false)
assert Equation((T or T)(), true)
assert Equation((T or F)(), true)
assert Equation((F or T)(), true)
assert Equation((F or F)(), false)
assert Equation((T xor T)(), false)
assert Equation((T xor F)(), true)
assert Equation((F xor T)(), true)
assert Equation((F xor F)(), false)
assert Equation((not T)(), false)
assert Equation((not F)(), true)
