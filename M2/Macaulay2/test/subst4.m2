R = ZZ[a,b][x,y];
f = a+x;
substitute(f, { a => b } )
a' = a + 0_R
leadMonomial a'
baseName a'
-- Local Variables:
-- compile-command: "make subst4.okay"
-- End:
