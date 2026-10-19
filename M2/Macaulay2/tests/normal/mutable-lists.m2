-- select w/ mutable lists
M = new MutableList from 1..10
M' = select(M, even)
assert Equation(#M', 5)
for i to 4 do assert Equation(M'#i, 2*(i + 1))
M'#0 = 5
assert Equation(M'#0, 5)
M'' = select(3, M, even)
assert Equation(#M'',3)
for i to 2 do assert Equation(M''#i, 2*(i + 1))
M''#0 = 5
assert Equation(M''#0, 5)

-- ensure that we clean up when removing elements
x = new MutableList from {1, 2, 3, 4}
assert Equation(length x, 4)
assert Equation(remove(x, 0), 1)
assert Equation(toList x, {2, 3, 4})
assert Equation(remove(x, 2), 4)
x#3 = 5
assert Equation(toList x, {2, 3, null, 5}) -- 4 should be gone
