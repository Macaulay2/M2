-- 
R = ZZ [x..z, t_1 .. t_4, s_(1,1) .. s_(2,2), Degrees=>{1,2,3,4,1,2,3, 4: 1}, MonomialOrder=>Lex]
assert ( toExternalString R == "ZZ [x, y, z, t_1, t_2, t_3, t_4, s_(1,1), s_(1,2), s_(2,1), s_(2,2), Degrees => {{1}, {2}, {3}, {4}, {1}, {2}, {3}, {1}, {1}, {1}, {1}}, MonomialOrder => Lex, Heft => {1}]" )

