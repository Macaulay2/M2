S = ZZ/101[d,e,Degrees=>{{1,2},{2,0}}]
R = ZZ/101[a,b,c]/(a^2,b^2)
I = monomialIdeal gens ideal(c)
sendgg(ggPush I, ggcodim)
