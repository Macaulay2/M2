needsPackage "Bertini"

--To solve a positive dimensional system use the bertiniPosDimSolve command
-- the input is a list of equations 
-- the outpt is a numerical variety
R=QQ[x,y,z]
outHyper=bertiniPosDimSolve(
     {(z+2)*x*(x^2+y^2-1),(y-1)*(2+y)*(x^2+y^2-1),(2+y)*(x-3)*(x^2+y^2-1)})

numDecomp=for i from 0 to 2 list  apply(outHyper#i,degree)
assert(member({1},numDecomp))
assert(member({1,1},numDecomp))
assert(member({2},numDecomp))

-- testing that dim and deg of witness sets is correct
R=CC[x,y,z,w,a]
F={(x+1)*z,(x+1)*y,(x+1)*w,(x+1)*a*(a+1)}
comps = components bertiniPosDimSolve F
assert(
    comps / dim == {1,1,4} and
    comps / degree == {1,1,1}
    )
--TODO: We need a test to check if it produces correct slices
--TODO: We need to check if the witness sets work with the other stuff in NumericalAlgebraicGeometry.

-- affine

R = CC[x,y,z,t]
I = ideal(x + 3, y+1)
nv = bertiniPosDimSolve(I_*, Verbose => true)
w  = first components nv
assert all(points w, p->norm evaluate(polySystem slice w,p) < 0.0001)

R = CC[x,y,z,t]
I = ideal(x + 3, y+1, z, t-0.5*ii)
nv = bertiniPosDimSolve(I_*, Verbose => true)
w  = first components nv
assert(slice w == {})

-- projective 

R = CC[x,y,z,t]
I = ideal(x + z, y+z)
nv = bertiniPosDimSolve(I_*, Verbose => true, IsProjective=>1)
w  = first components nv
assert all(points w, p->norm evaluate(polySystem slice w,p) < 0.0001)

I = ideal(x + z, y+x, z)
nv = bertiniPosDimSolve(I_*, Verbose => true, IsProjective=>1)
w  = first components nv
assert(slice w == {})
