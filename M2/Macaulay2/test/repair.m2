r = QQ[x,y,Degrees => {{2,1},{1,3}}, Heft => {1,1}]
R = QQ[X,Y,Degrees => {{2,1},{1,3}}]
assert( degrees source vars R === degrees source vars r )
assert( degrees R^{{2,3},{4,1},{1,0},{0,1}} === degrees r^{{2,3},{4,1},{1,0},{0,1}} )
assert( degree x === degree X )
assert( degree y === degree Y )
assert( poincare r === poincare R )
assert( poincare (r/(x)) === poincare (R/(X)) )
assert( poincare (r^1/(x)) === poincare (R^1/(X)) )
assert( poincare (r^1/(y)) === poincare (R^1/(Y)) )
assert( poincare (r^1/(x,y)) === poincare (R^1/(X,Y)) )
assert( poincare (r^1/(x^2,y^2)) === poincare (R^1/(X^2,Y^2)) )
assert( poincare (r^1/(x^3,x*y^2,y^3)) === poincare (R^1/(X^3,X*Y^2,Y^3)) )
assert( hilbertSeries R === hilbertSeries r )
assert( hilbertSeries (R^1/(X)) === hilbertSeries (r^1/(x)) )
assert( hilbertSeries (R^1/(Y)) === hilbertSeries (r^1/(y)) )
assert( hilbertSeries (R^1/(X,Y)) === hilbertSeries (r^1/(x,y)) )
assert( hilbertSeries (R^1/(X^2,Y^2)) === hilbertSeries (r^1/(x^2,y^2)) )

s = QQ[x,Degrees => {{2,1}}, Heft => {1,1}]
f = map(s,r,{x,0})
assert( isHomogeneous f )
F = r^{{1,0},{0,1}}
f ** F
degrees F
degrees (f ** F)
G = F/y
degrees source gens G
degrees source gens (f ** G)
H = coker transpose vars r
degrees source gens H
f ** H
degrees source gens (f ** H)

-- here is an example of using it to get negative degrees.

S = QQ[a,b,Degrees => {{-2,10},{0,10}}, Heft => {1,1}]
poincare (S/a)
poincare (S/b)
poincare (S/(a^2))
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test repair.out"
-- End:
