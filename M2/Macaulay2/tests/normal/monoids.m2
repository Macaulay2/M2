--- checking variable processing
S = monoid[a_(0,0)..c_(2,2),x_0..z_3]
assert(numgens S == 39)
assert match(regexQuote "a_(0,0)..c_(2,2), x_0..z_3", toExternalString S)
---
monoid[a,b,c]
monoid[a..c,x..z]
monoid[(a,b,c),(x,y,z)]
monoid[{a,b,c},{x,y,z}]
monoid[vars(0..2,23..25)]
monoid[vars(0..2),vars(23..25)]
monoid[a_0..a_3,b_0..b_3] -- FIXME: this would fail if QQ[a,b,c] was defined before
---
monoid[a,b, WeylAlgebra => a=>b]
monoid[a,b, WeylAlgebra => 0=>1]
monoid[Variables => 4, WeylAlgebra => {{0, 2}, {1, 3}}]
monoid[Variables => 4, WeylAlgebra => {(0, 2), (1, 3)}]
monoid[Variables => 4, WeylAlgebra => {0 => 2, 1 => 3}]
---
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => false]
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => true]
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => 0..3]
monoid[Variables => 4, VariableBaseName => "e", SkewCommutative => {0,1,2,3}]
monoid[vars(0..3), VariableBaseName => "e", SkewCommutative => {0,1,2,3}]
monoid[vars(0..3), VariableBaseName => "e", SkewCommutative => vars(0..3)]
---

---- checking degreeLength, degreesMonoid, degreesRing
assert(degreeLength degreesMonoid 0 == 0)
assert(degreeLength degreesMonoid 2 == 0)
assert(degreeLength degreesMonoid{} == 0)
assert(degreeLength degreesMonoid{1} == 1)
assert(degreeLength degreesMonoid{1,2} == 1)

assert(degreeLength degreesRing 0 == 0)
assert(degreeLength degreesRing 2 == 0)
assert(degreeLength degreesRing{} == 0)
assert(degreeLength degreesRing{1} == 1)
assert(degreeLength degreesRing{1,2} == 1)

assert(degreeLength(monoid[]) == 1)
assert(degreeLength(monoid[a]) == 1)
assert(degreeLength(monoid[a,b]) == 1)
