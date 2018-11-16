-- blow up of P5 along the Veronese
P5 = flagBundle({1,5})
P2 = flagBundle({1,2})
incl = map(P5, P2, OO_P2(2))
(Ytilde,PN,PNmap,Ymap) = blowup(incl)
ct = ctop tangentBundle Ytilde
integral Ymap_* ct
assert( oo == 12 )
Ediv = chern(1,exceptionalDivisor Ytilde)
quadric = chern(1,OO_P5(2))
propertransform = (Ymap^* quadric) - Ediv
-- conics tangent to 5 lines
assert (integral propertransform^5 == 1)
sextic = chern(1,OO_P5(6))
propertransform = (Ymap^* sextic) - 2* Ediv
-- conics tangent to 5 general conics
integral propertransform^5
assert (integral propertransform^5 == 3264)

-- blow up a point on P^2
X = flagBundle({1,0})
Y = flagBundle({1,2})
i = map(Y,X, OO_X)
(Ytilde, PN, PNmap, Ymap) = blowup(i)
Ediv = chern(1,exceptionalDivisor Ytilde)
assert (integral (Ediv^2) == -1)
assert (integral PNmap^* Ediv == -1)
assert (integral ctop tangentBundle Ytilde == 4)

-- blow up a point in P^7
X = flagBundle({1,0})
Y = flagBundle({1,7})
i = map(Y,X, dual first bundles X)
(Ytilde, PN, PNmap, Ymap) = blowup(i)
assert (integral ctop tangentBundle Ytilde == 14)

-- Blow up a twisted cubic in P^3, then check that the proper transforms
-- of three quadric surfaces containing that twisted cubic have product == 0
X = flagBundle({1,1})
Y = flagBundle({1,3})
i = map(Y, X, OO_X(3))
(Ytilde, PN, PNmap, Ymap) = blowup(i)
quadric = chern(1,OO_Y(2))
Ediv = chern(1,exceptionalDivisor Ytilde)
propertransform = (Ymap^* quadric) - Ediv
assert(propertransform^3 == 0)
cubic = chern(1,OO_Y(3))
-- the same formula (see Eisenbud and Harris' book, section on
-- intersections of surfaces in P3 containing a curve) gives that intersecting
-- the proper transforms of two quadric surfaces containing our twisted cubic
-- with the proper transform of a cubic containing it should give 1
-- To visualize this, consider taking the cubic surface to be a quadric union
-- a hyperplane.  Then the hyperplane intersects the twisted cubic in three
-- points, so it intersects the other two quadrics in one point off of the
-- twisted cubic, and this formula counts that point.
assert(integral (propertransform^2 *((Ymap^* cubic) - Ediv)) == 1)

--The same check, with variables.  Again, see Eisenbud and Harris
B = base(r,s,t)
X = flagBundle({1,1},B)
Y = flagBundle({1,3},B)
i = map(Y,X,OO_X(3))
(Ytilde, PN, PNmap, Ymap) = blowup(i)
rsurf = chern(1,OO_Y(r))
ssurf = chern(1,OO_Y(s))
tsurf = chern(1,OO_Y(t))
Ediv = chern(1,exceptionalDivisor Ytilde)
rtrans = (Ymap^* rsurf) - Ediv
strans = (Ymap^* ssurf) - Ediv
ttrans = (Ymap^* tsurf) - Ediv
rtrans*strans*ttrans
integral oo
assert(oo == r*s*t - 3 * (r + s + t) + 10)


-- G(2,5) is cut out by 5 quadrics in P^9
X = flagBundle({2,3})
S = first bundles X
L = exteriorPower(2, dual S)
Y = flagBundle({1,9})
i = map(Y,X,L)
(Ytilde, PN, PNmap, Ymap) = blowup(i)
Ediv = chern(1,exceptionalDivisor Ytilde)
quadric = chern(1,OO_Y(2))
propertransform = (Ymap^* quadric) - Ediv
-- 5 generic quadrics containing the Grassmannian cut it out
propertransform^5
assert (propertransform^5 == 0)
assert (Ediv^5 != 0)

--The same excess intersection example of three surfaces of degrees r,s,t,
--but now the common curve they contain is a curve of degree d and genus
--c*(c-1)/2.  Obviously we build this curve as a plane curve and re-embed it
BB = base(r,s,t,c,d)
P2 = abstractProjectiveSpace(2,BB)
P3 = abstractProjectiveSpace(3,BB)
C = sectionZeroLocus(OO_P2(c)) -- plane curve of degree c
g = (c-1)*(c-2)/2 -- and its genus
 -- we map with degree d*c, but since d is a free variable, we can later replace
 -- it by d/c to get the desired answer
L = (C.StructureMap)^* OO_P2(d)
incl = map(P3, C, L)
(Ytilde, PN, PNmap, Ymap) = blowup(incl)
Ediv = chern(1,exceptionalDivisor Ytilde)
(rsurf, ssurf, tsurf) = (x -> chern(1,OO_P3(x))) \ (r,s,t)
(ptr, pts, ptt) = (x -> (Ymap^* x) - Ediv) \ oo
integral(ptr*pts*ptt)
sub(oo, d=>(d/c)) -- replace d by d/c to account for the issue above
assert(oo == r*s*t - d*(r+s+t) + (2*g - 2 + 4 * d))