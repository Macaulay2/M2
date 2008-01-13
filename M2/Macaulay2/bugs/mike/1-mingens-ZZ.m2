-- Fixed 13 Jan 2008

R = ZZ[x,y]
I = ideal(x^2, x*y-2*y^2, y^3)
gens gb I
J = trim I
gens gb J -- this should be the same as I.

assert(gens gb I == gens gb J)
assert(I == J)



-- 'mingens' when computng a GB over ZZ[x,y]
-- is not considering the coefficients...
-- It needs to be more conservative...


-- Here is another possible mingens bug:
-- It seems to be handled OK.  I was worried that
-- when deferring a reduction, the 'mingens' flag
-- might be lost.  But apparently not...
gbTrace=10
R = ZZ/32003[a..d]
I = ideal"a2-b2,a30"
gens gb I
mingens I
