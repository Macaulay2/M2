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
assert Equation(remove(x, 0), 1)
assert Equation(toList x, {2, 3, 4})
assert Equation(remove(x, 2), 4)
x#3 = 5
assert Equation(toList x, {2, 3, null, 5}) -- 4 should be gone
-- ... including when removing shrinks the underlying sequence
for k from 1 to 40 do (
    x = new MutableList from 0..99;
    while #x > k do remove(x, 0);
    x#(k + 1) = 0;
    assert(x#k === null))

-- growing a list must not affect sequences made from it, even when
-- there is spare capacity to grow in place
grow = () -> (y := new MutableList; for i to 4 do y##y = i; y)
x = grow()
s = new Sequence from x
x##x = 5
x#0 = 100
assert Equation(s, (0, 1, 2, 3, 4))

-- functions that call f on a mutable list work on a snapshot of it, even
-- when f grows or shrinks the list
visits = (change, loop) -> (
    x = grow();
    seen = {};
    loop(e -> (change x; seen = append(seen, e)));
    seen)
for change in {y -> (y##y = 5), y -> remove(y, 0)} do (
    assert Equation(visits(change, f -> select(x, e -> (f e; true))), toList(0..4));
    assert Equation(visits(change, f -> select(10, x, e -> (f e; true))), toList(0..4));
    assert Equation(visits(change, f -> any(x, e -> (f e; false))), toList(0..4));
    assert Equation(visits(change, f -> any(x, toList(0..4), (e, d) -> (f e; false))), toList(0..4));
    assert Equation(visits(change, f -> any(toList(0..4), x, (d, e) -> (f e; false))), toList(0..4));
    assert Equation(visits(change, f -> scan(x, f)), toList(0..4));
    assert Equation(visits(change, f -> apply(x, f)), toList(0..4));
    assert Equation(visits(change, f -> for e in x do f e), toList(0..4));
    z = new MutableList;
    scan({(0, 1), (0, 2), (1, 3)}, p -> (z##z = p));
    assert BinaryOperation(symbol ===, hashTable((a, b) -> (change z; a + b), z), hashTable {0 => 3, 1 => 3}))
