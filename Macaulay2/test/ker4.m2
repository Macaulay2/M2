R = ZZ/101[a,b,c];
S = ZZ/101[x,y];
f = map(S,R,{x^2,x*y,y^2});
ker f
assert( degrees source gens ker f == {{2}} )
assert( degrees target gens ker f == {{0}} )
-- Local Variables:
-- compile-command: "make ker4.okay"
-- End:
