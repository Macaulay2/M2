-- Test of monomial ordering code, added 15 April 2013.
-- After M2 1.6 is released, this file should be placed in Macaulay2Doc/test

debug Core
R = ZZ/101[a..d, MonomialOrder=>{Weights=>{1,2,3,4},Lex=>4}]
monomialOrderMatrix R
monomialOrderMatrix monoid R
assert((matrix{{1,2,3,4}},Lex) == monomialOrderMatrix R)

R = ZZ/101[a..d, MonomialOrder=>{Weights=>{1,2,3,4}, Weights=>{1,1},4}]
ans = (matrix{{1, 2, 3, 4}, {1, 1, 0, 0}, {1, 1, 1, 1}}, RevLex)
ans == monomialOrderMatrix R

R = ZZ/101[a..d, MonomialOrder=>{Weights=>{1,2,3,4}, 3, 1}]
ans = (matrix{{1, 2, 3, 4}, {1, 1, 1, 0}, {0, 0, -1, 0}, {0, -1, 0, 0}, {0, 0, 0, 1}},
       RevLex)
assert(ans == monomialOrderMatrix R)
  
R = ZZ/101[a..d, Degrees=>{1,3,5,7}, MonomialOrder=>{GRevLex=>2, GRevLex=>2}]
monomialOrderMatrix R

R = ZZ/101[a..d, Degrees=>{1,3,5,7}, MonomialOrder=>{2,2}]
monomialOrderMatrix R

R = ZZ/101[a..d, Degrees=>{1,3,5,7}, MonomialOrder=>{2,Lex=>2}]
monomialOrderMatrix R

R = ZZ/101[vars(0..9), Degrees=>{1,3,5,7,9,11,12,13,2,1}, MonomialOrder=>{Eliminate 4 }]
monomialOrderMatrix R
