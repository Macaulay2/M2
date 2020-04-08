restart
needsPackage "NumericalAlgebraicGeometry"
R = CC_53[x_1..x_4, y_1..y_4]
I = ideal(
    y_3+y_4,    
    x_3*y_2+x_4*y_2,
    x_4*y_1+x_4*y_2-x_1*y_4-x_2*y_4-y_4,
    x_3*y_1-x_4*y_2+x_1*y_4+x_2*y_4+y_4,
    x_4^2+y_4^2-1,
    x_3^2+y_4^2-1,
    x_2^2+y_2^2-1,
    x_1^2+y_1^2-1
    )
	    			    
-- This one appears correct:
W1 = numericalIrreducibleDecomposition I
for d in keys W1 list {d, #W1#d}
    				    
-- The following is WRONG:
-- There are 8 components of dimension 0, not dimension 1.
-- but W2 has 8 dim=1 components, and one dim=2 component of degree 14.
errorDepth = 2
numericalIrreducibleDecomposition(I, Software=>BERTINI)
W2 = oo
for d in keys W2 list if instance(d, ZZ) then {d, #W2#d} else continue
				    
-- doing it using the Bertini interface:
-- gives the same as the BERTINI version above.
needsPackage "Bertini"
bertiniPosDimSolve I_*
    				      
-- Using exact arithmetic, we see what the answer is.
RQQ = QQ[x_1..x_4, y_1..y_4]
IQQ = ideal(y_3+y_4,x_3*y_2+x_4*y_2,x_4*y_1+x_4*y_2-x_1*y_4-x_2*y_4-y_4,x_3*y_1-x_4*y_2+x_1*y_4+x_2*y_4+y_4,x_4^2+y_4^2-1,x_3^2+y_4^2-1,x_2^2+y_2^2-1,x_1^2+y_1^2-1)
dim IQQ
degree IQQ
radical IQQ == IQQ -- true: so no non-reduced components.
comps = (decompose IQQ)/trim
comps/(i -> (dim i,degree i))
tally oo
-- This IQQ is a radical ideal, with 8 isolated solutions, and one
-- component of dim 2 and degree 14 (over QQ, but the numerical decomp
-- indicates that this is irreducible over CC as well).
    				      
-- could it be variable name problems?
R1 = CC_53[vars(0..7)]
I1 = ideal(
    y_3+y_4,
    x_3*y_2+x_4*y_2,
    x_4*y_1+x_4*y_2-x_1*y_4-x_2*y_4-y_4,
    x_3*y_1-x_4*y_2+x_1*y_4+x_2*y_4+y_4,
    x_4^2+y_4^2-1,
    x_3^2+y_4^2-1,
    x_2^2+y_2^2-1,
    x_1^2+y_1^2-1
    )
I1 = sub(I1, vars R1)
needsPackage "Bertini"
bertiniPosDimSolve I1_* -- NO: this gives the same problem...
bertiniPosDimSolve(I1_*, RandomSeed=>12313) -- NO: this gives the same problem...
									      
