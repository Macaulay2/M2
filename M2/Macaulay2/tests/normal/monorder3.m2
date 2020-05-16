-- 
debug Core
numParts = method()
numParts Monoid := (M) -> rawMonoidNumberOfBlocks raw M
numParts PolynomialRing := (R) -> numParts monoid R

assert(1 == numParts(R = ZZ/101[a..d]))
assert(1 == numParts(ZZ/101[a..e]))
assert(1 == numParts(ZZ/101[a..e, MonomialOrder => {Lex, Position=>Up}]))
assert(1 == numParts(ZZ/101[a..e, MonomialOrder => {MonomialSize => 32, Lex, Position=>Up}]))
assert(3 == numParts(ZZ/101[a..e, MonomialOrder => {MonomialSize => 32, Lex=>3, Lex=>1, Position=>Up}]))
assert(4 == numParts(ZZ/101[a..d, MonomialOrder => {Weights => {1,1}, Weights => {0,0,1,1}, MonomialSize => 32, Lex=>3, Lex=>1}]))
-*
S = ZZ/101[a..e, MonomialOrder => {Lex=>3, Lex=>1}]
S = ZZ/101[x_1..x_10, MonomialOrder => {Lex=>6, Lex=>4}]
S = ZZ/101[x_1..x_10, MonomialOrder => {Lex=>6, Lex=>3}]
S = ZZ/101[x_1..x_10, MonomialOrder => {Eliminate 3, Lex=>6, Lex=>3}]
S = ZZ/101[x_1..x_10, MonomialOrder => {Weights => {0,0,0,0,0,0,1,1,1}, Lex=>6, Lex=>4}]
S = ZZ/101[a..d, MonomialOrder=> {RevLex=>4}, Global=>false]
S = ZZ/101[a..d, MonomialOrder=> {GRevLex=>4}]
S = ZZ/101[a..d, MonomialOrder=> {1,1,2}]
S = ZZ/101[a..d, MonomialOrder=> {Position=>Up,1,1,2}]
M = matrix{{a,b,c,d}}
selectInSubring(1, M ++ M)
selectInSubring(2, M ++ M)
selectInSubring(3, M ++ M)
debug Core
3 == rawMonoidNumberOfBlocks raw monoid S
S = ZZ/101[a..d, MonomialOrder=> {Position=>Up, Lex=>2, MonomialSize=>8, Lex => 2}]
raw monoid S
*-



