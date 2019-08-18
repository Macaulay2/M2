-- Some tests of minimalPresentation of rings
-- including the exclude list

R = ZZ/101[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,2,3})
assert(numgens S == 3)
use ambient S
assert(ideal S == ideal"a-c-d")

R = ZZ/101[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,2})
describe S
assert(numgens S == 2)
assert(ideal S == 0)

R = ZZ/101[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {1,2})
describe S
assert(numgens S == 3)
use ambient S
assert(ideal S == ideal"b-c3")

R = ZZ/101[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,1,2})
describe S
assert(numgens S == 3)
use ambient S
assert(ideal S == ideal"b-c3")

A = ZZ/101[symbol s, symbol t]/(s-t-1)

R = A[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,2,3})
describe S
assert(numgens S == 4)
use ambient S
assert(ideal S == ideal"a-c-d")

R = A[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,2,3,4})
describe S
assert(numgens S == 4)
use ambient S
assert(ideal S == ideal"a-c-d")

R = A[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,2,3,5})
describe S
assert(numgens S == 4)
use ambient S
assert(ideal S == ideal"a-c-d")

R = A[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,2})
describe S
assert(numgens S == 3)
assert(ideal S == 0)

R = A[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {1,2})
describe S
assert(numgens S == 4)
use ambient S
assert(ideal S == ideal"b-c3")

R = A[vars(0..3)]/(a-d-c, b-c^3)
S = prune(R, Exclude => {0,1,2,4,5})
describe S
assert(numgens S == 5)
use ambient S
assert(ideal S == ideal"b-c3,s-t-1")

