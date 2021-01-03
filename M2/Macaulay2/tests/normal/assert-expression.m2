assert Equation(2 + 2, 4)
assert not Equation(2 + 2, 5)
assert BinaryOperation {symbol <, 1, 3}
assert BinaryOperation {symbol <=, 1, 3}
assert BinaryOperation {symbol >, 3, 1}
assert BinaryOperation {symbol >=, 3, 1}
assert BinaryOperation {symbol ===, 2, 2}
assert BinaryOperation {symbol =!=, 2, 2.0}
assert(hold true and true)
assert(hold true or false)
