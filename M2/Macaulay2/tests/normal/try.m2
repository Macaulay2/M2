f = x -> 1/x
assert Equation(
    apply(-5..5, try f),
    (-1/5, -1/4, -1/3, -1/2, -1, null, 1, 1/2, 1/3, 1/4, 1/5))
assert Equation(
    apply(-5..5, try f then 2),
    (2, 2, 2, 2, 2, null, 2, 2, 2, 2, 2))
assert Equation(
    apply(-5..5, try f else 3),
    (-1/5, -1/4, -1/3, -1/2, -1, 3, 1, 1/2, 1/3, 1/4, 1/5))
assert Equation(
    apply(-5..5, try f then 2 else 3),
    (2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2))
assert Equation(
    apply(-5..5, try f except err do 4),
    (-1/5, -1/4, -1/3, -1/2, -1, 4, 1, 1/2, 1/3, 1/4, 1/5))
assert Equation(
    apply(-5..5, try f then 2 except err do 4),
    (2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2))

