assert = x -> if not x then error "assertion failed "
y := 66							    -- variable locally defined
assert (y === 66)					    -- checking integrity of lexical scope

