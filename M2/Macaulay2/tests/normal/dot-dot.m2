R = QQ[b,c,  e,f]
S = QQ[  c,d,  f]
-*
(print; print generateAssertions ///
a..a
a..b
a..c
a..d
a..e
a..f
a..g
b..b
b..c
b..d
b..e
b..f
b..g
c..c
c..d
c..e
c..f
c..g
d..d
d..e
d..f
d..g
e..e
e..f
e..g
f..f
f..g
g..g
a..<a
a..<b
a..<c
a..<d
a..<e
a..<f
a..<g
b..<b
b..<c
b..<d
b..<e
b..<f
b..<g
c..<c
c..<d
c..<e
c..<f
c..<g
d..<d
d..<e
d..<f
d..<g
e..<e
e..<f
e..<g
f..<f
f..<g
g..<g
///)
*-

assert( (a..b) === (a,symbol b) )
assert( (a..c) === (a,symbol b,symbol c) )
assert( (a..d) === (a,symbol b,symbol c,symbol d) )
assert( (a..e) === (a,symbol b,symbol c,symbol d,symbol e) )
assert( (a..f) === (a,symbol b,symbol c,symbol d,symbol e,symbol f) )
assert( (a..g) === (a,symbol b,symbol c,symbol d,symbol e,symbol f,g) )
assert( (b..b) === 1:(b) )
assert( (b..c) === (symbol b,symbol c) )
assert( (b..d) === (symbol b,symbol c,symbol d) )
assert( (b..e) === (symbol b,symbol c,symbol d,symbol e) )
assert( (b..f) === (symbol b,symbol c,symbol d,symbol e,symbol f) )
assert( (b..g) === (symbol b,symbol c,symbol d,symbol e,symbol f,g) )
assert( (c..c) === 1:(c) )
assert( (c..d) === (c,d) )
assert( (c..e) === (symbol c,symbol d,symbol e) )
assert( (c..f) === (symbol c,symbol d,symbol e,symbol f) )
assert( (c..g) === (symbol c,symbol d,symbol e,symbol f,g) )
assert( (d..d) === 1:(d) )
assert( (d..e) === (symbol d,symbol e) )
assert( (d..f) === (symbol d,symbol e,symbol f) )
assert( (d..g) === (symbol d,symbol e,symbol f,g) )
assert( (e..e) === 1:(e) )
assert( (e..f) === (symbol e,symbol f) )
assert( (e..g) === (symbol e,symbol f,g) )
assert( (f..f) === 1:(f) )
assert( (f..g) === (symbol f,g) )
assert( (g..g) === 1:(g) )
assert( (a..<a) === () )
assert( (a..<b) === 1:(a) )
assert( (a..<c) === (a,symbol b) )
assert( (a..<d) === (a,symbol b,symbol c) )
assert( (a..<e) === (a,symbol b,symbol c,symbol d) )
assert( (a..<f) === (a,symbol b,symbol c,symbol d,symbol e) )
assert( (a..<g) === (a,symbol b,symbol c,symbol d,symbol e,symbol f) )
assert( (b..<b) === () )
assert( (b..<c) === 1:(b) )
assert( (b..<d) === (symbol b,symbol c) )
assert( (b..<e) === (symbol b,symbol c,symbol d) )
assert( (b..<f) === (symbol b,symbol c,symbol d,symbol e) )
assert( (b..<g) === (symbol b,symbol c,symbol d,symbol e,symbol f) )
assert( (c..<c) === () )
assert( (c..<d) === 1:(c) )
assert( (c..<e) === (c,d) )
assert( (c..<f) === (symbol c,symbol d,symbol e) )
assert( (c..<g) === (symbol c,symbol d,symbol e,symbol f) )
assert( (d..<d) === () )
assert( (d..<e) === 1:(d) )
assert( (d..<f) === (symbol d,symbol e) )
assert( (d..<g) === (symbol d,symbol e,symbol f) )
assert( (e..<e) === () )
assert( (e..<f) === 1:(e) )
assert( (e..<g) === (symbol e,symbol f) )
assert( (f..<f) === () )
assert( (f..<g) === 1:(f) )
assert( (g..<g) === () )

T = QQ[x_1,y,x_2,z,x_3]
U = QQ[x_1,y,x_2,z,x_3]
-*
print generateAssertions ///
T_0 .. T_4
U_0 .. U_4
///
*-
assert( (T_0 .. T_4) === (new IndexedVariable from {symbol x,1},new IndexedVariable from {symbol x,2},new IndexedVariable from {symbol x,3}) )
assert( (U_0 .. U_4) === (x_1,x_2,x_3) )
