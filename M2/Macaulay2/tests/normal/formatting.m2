-- 
R = ZZ [x..z, t_1 .. t_4, s_(1,1) .. s_(2,2), Degrees=>{1,2,3,4,1,2,3, 4: 1}, MonomialOrder=>Lex]
assert (
     toExternalString R == 
     "ZZ(monoid[x..z, t_1..t_4, s_(1,1)..s_(2,2), Degrees => {1..4, 1..3, 4:1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, Lex => 11, Position => Up}, DegreeRank => 1])"
     )

QQ[a][x]
assert( toString (x-a) == "x-a" )
assert( class expression (-a) === Minus )
