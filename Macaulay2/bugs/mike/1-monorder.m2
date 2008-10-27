-- All these give monomial overflow
-- Desired behavior: should give an error (the weights refer to variables after the 4 given).
R = ZZ/101[x_1..x_4, MonomialOrder =>{GRevLex => 4, Weights => {1,1,1,1}}]
R = ZZ/101[x_1..x_4, MonomialOrder =>{GRevLex, Weights => {1,1,1,1}}]

-- In this example, GRevLexTiny, GRevLexSmall appear
R = QQ[x_1..x_8,MonomialOrder=>{MonomialSize=>8, GRevLex=>4, MonomialSize=>32, MonomialSize=>16, GRevLex=>4}]
(options monoid R).MonomialOrder  -- should be {GRevLex=>4, MonomialSize=>16, GRevLex=>4}
(options monoid R).MonomialSize  -- should be 8?
debug Core
raw R
