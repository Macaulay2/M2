i12 : A = new MutableMatrix from matrix{{1_RR}}

o12 = | 1 |

o12 : MutableMatrix

i13 : clean(0.1,A)
stdio:13:1:(3): error: unknown engine error

A = mutableMatrix({{1_RR}}, Dense=>true)
clean(0.1,A) -- works fine
A = mutableMatrix({{1_RR}}, Dense=>false)
assert try (clean(0.1,A);false) else true  -- not yet implemented.
