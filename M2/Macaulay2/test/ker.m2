R=ZZ/101[a,b,c]
S=ZZ/101[t]
assert( ker map(S,R,{t^4,t^5,t^6}) == ideal matrix {{b^2-a*c, a^3-c^2}} )
-- Local Variables:
-- compile-command: "make ker.okay"
-- End:
