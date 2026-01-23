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

-- growing a list must not affect sequences made from it, even when
-- there is spare capacity to grow in place
grow = () -> (y := new MutableList; for i to 4 do y##y = i; y)
x = grow()
s = new Sequence from x
x##x = 5
x#0 = 100
assert Equation(s, (0, 1, 2, 3, 4))
