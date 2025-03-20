----------------------------------------------------
--- Method Handles for the Macaulay Package: ------

connectionMatrices = method()


----------------------------------------------------
--pfaffians computes Pfaffian system for D-ideals
----------------------------------------------------
-- c.f. [Theorem 1.4.22, SST]
connectionMatrices(Ideal) := List => (I) -> (
    D := ring I;
    createDpairs D;
    w := (((options(D)).MonomialOrder)#1)#1;
    -- warning: multiplication in R isn't correct,
    -- but this acts as the associated graded ring of R
    R := rationalWeylAlgebra(D);
    -- gb with respect to an elimination
    -- weight order tie broken by RevLex?
    G := gens gb I;
    r := holonomicRank(w, M := comodule I);
    if r === infinity then error "system is not finite dimensional";
    B := sub(M.cache#"basis", R);
    A := apply(D.dpairVars#1,
	dt -> transpose concatCols apply(flatten entries B,
	    s -> last coefficients(
		-- essentially compute: (dt * s) % G
		normalForm(dt_R * s, first entries G), Monomials => B)));
    apply((A/entries)/matrix, p ->sub(p,fractionField D))
)

-- gives the pfaffians system with respect to a new basis B
connectionMatrices(List,Ideal) := (B,I)->(
    W := ring I;
    G := gaugeMatrix(I,B);
    invG := inverse G;
    n := dim W//2;
    C := connectionMatrices I;
    for i from 1 to n list(
        dxi := W_(n+i-1);
        diffMatrixWeyl(dxi, G)*invG + G*(C#(i-1))*invG
    )
)

----------------------------------------------------
--connectionMatrix computes connection matrix of I
----------------------------------------------------
connectionMatrix = method()
connectionMatrix(Ideal) := List => (I) -> (
    P := pfaffians(I);
    R := rationalWeylAlgebra(ring I);
    var := gens R;
    net(sum((for i from 0 to length(var)-1 list var_i*P_i )))
)


-- allows for Pfaffian system P as input
connectionMatrix(List) := List => (P) -> (
    R := rationalWeylAlgebra(makeWA(coefficientRing(ring P_0)[gens ring P_0]));
    var := gens R;
    net(sum((for i from 0 to length(var)-1 list var_i*sub(P_i,R))))
)

----------------------------------------------------
--stdMon computes std monomials wrt. to weight order
----------------------------------------------------
standardMonomials = method()
standardMonomials(Ideal) := (I) -> (
    D := ring I;
    w := (((options(D)).MonomialOrder)#1)#1;
    M := comodule I;
    r := holonomicRank(w, M);
    B := sub(M.cache#"basis", D);
    return flatten entries B;
);


end--
restart
needs "./pfaffians.m2"


------------------------------
-- ALS notes, Example 7.16
D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1); -- doesn't commute
A = pfaffians(I);

-- i2 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1});
-- i3 : A = pfaffians(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
-- Grobner basis:
-- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
-- Standard monomials:
-- | 1 dy |
-- o3 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
--       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

-- i4 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,1});
-- i5 : A = pfaffians(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
-- Grobner basis:
-- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
-- Standard monomials:
-- | 1 dy |
-- o5 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
--       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

-- i6 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
-- i7 : A = pfaffians(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
-- Grobner basis:
-- | ydy+xdx+1 xdxdy+xdx^2+dy+dx x2dx^2-xydx^2+3xdx-ydx+1 |
-- Standard monomials:
-- | 1 dx |
-- o7 = {{-1} | 0            1               |, {-1} | (-1)/y    (-x)/y        |}
--       {-1} | (-1)/(x2-xy) (-3x+y)/(x2-xy) |  {-1} | 1/(xy-y2) (x+y)/(xy-y2) |


--------------------------------------------------------
-- Example: GKZ system 1                                -- Q: Over which matrix (before {{1,2,3}} stated.)
D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,1})
pfaffians(w, ideal (x*dx+2*y*dy-1, dx^2-dy))             -- Checked in Mathematica
-- i6 : pfaffians ideal (x*dx+2*y*dy-1, dx^2-dy)
-- Grobner basis:
-- | xdx+2ydy-1 4y2dy^2-x2dy+2ydy 2ydxdy+xdy dx^2-dy |
-- Standard monomials:
-- | 1 dy |
-- o6 = {{-1} | 1/x (-2y)/x |, {-1} | 0 1           |}
--      {-1} | 0   (-x)/2y |  {-1} | 0 (x2-2y)/4y2 |

pfaffians ideal (x*dx+2*y*dy-1, dx^2-dy)            -- Shortcut for: pfaffians({0,0,1,1},  ideal (x*dx+2*y*dy-1, dx^2-dy))

--------------------------------------------------------

--------------------------------------------------------
-- Example:                                         -- Q: What is this the example for?
-- permutation matrices?
pfaffians ideal (dy^2-1, dx^5-dy)                   -- Output (??)
pfaffians ideal (dy^3-1, dx^2-dy)                   -- Output (??)
--------------------------------------------------------


--------------------------------------------------------
-- Example 1.4.24 in SST                                        --> ERROR with recursive version of normalForm (reduce.m2)
                                                                --> No error with iterative version.
D = makeWA(QQ[a,b,c,c', DegreeRank => 0][x,y])
I = ideal(
    dx*(x*dx + c  - 1) - (x*dx + y*dy + a)*(x*dx + y*dy + b),
    dy*(y*dy + c' - 1) - (x*dx + y*dy + a)*(x*dx + y*dy + b))
A = pfaffians({0,0,1,1}, I);
netList apply(A, mat -> sub(mat, {a => 10, b => 4/5, c => -2, c' => 3/2}))
gens gb sub(I, {a => 10, b => 4/5, c => -2, c' => 3/2})         --> Q: How to check this? [SSt, p.40] only contains a statement that one of the basis elements is of a certain form.

checkSystem(D, A)

pfaffians AppellF1 {10,4/5,-2,3/2}   -- Q: What is this? How to confirm the above?
--------------------------------------------------------

-- partially fixed version
--------------------------------------------------------
-- Example 1.4.24 in SST                                        --> ERROR with recursive version of normalForm (reduce.m2)
                                                                --> No error with iterative version.
D = makeWA(frac(QQ[a,b,c,c', DegreeRank => 0])[x,y],{0,0,1,1})
I = ideal(dx*(x*dx + c  - 1) - (x*dx + y*dy + a)*(x*dx + y*dy + b),dy*(y*dy + c' - 1) - (x*dx + y*dy + a)*(x*dx + y*dy + b))
A = pfaffians(I);
netList apply(A, mat -> sub(mat, {a => 10, b => 4/5, c => -2, c' => 3/2}))
gens gb sub(I, {a => 0, b => 4/5, c => -2, c' => 3/2})         --> Q: How to check this? [SSt, p.40] only contains a statement that one of the basis elements is of a certain form.

checkSystem(D, A)

pfaffians AppellF1 {10,4/5,-2,3/2}   -- Q: What is this? How to confirm the above?
--------------------------------------------------------


--------------------------------------------------------
-- Example page 28, ideal generated by equations (5.14) from https://arxiv.org/pdf/2303.11105
-- P2 in the paper is pfaffian_0
w={0,0,1,1}
D = makeWeylAlgebra(QQ[x,y],w)
I = ideal(x^2*dx^2+2*x*y*dx*dy+(y-1)*y*dy^2+3*x*dx+(3*y-1)*dy+1, x*dx^2-y*dy^2+dx-dy)
P = pfaffians I
P_0
--------------------------------------------------------

--------------------------------------------------------
-- Example equation (11) from https://arxiv.org/pdf/2410.14757
w = {0,0,0,1,1,1}
D = makeWeylAlgebra((QQ[e,DegreeRank=>0])[x,y,z],w)
delta1 = (x^2-z^2)*dx^2+2*(1-e)*x*dx-e*(1-e)
delta2 = (y^2-z^2)*dy^2+2*(1-e)*y*dy-e*(1-e)
delta3 = (x+z)*(y+z)*dx*dy-e*(x+z)*dx-e*(y+z)*dy+e^2
h = x*dx+y*dy+z*dz-2*e
I = ideal(delta1+delta3, delta2+delta3,h)
r = holonomicRank I;                                        -- WRONG: Gives "infinity" (instead of 4)
P = pfaffians I;                                            -- (old: ERROR: Does not terminate.)

--------------------------------------------------------

--------------------------------------------------------
-- Example equation (11) from https://arxiv.org/pdf/2410.14757     (MODIFIED from above, with frac(QQ[e]))
w = {0,0,0,1,1,1}
D = makeWeylAlgebra(frac(QQ[e,DegreeRank=>0])[x,y,z],w)
delta1 = (x^2-z^2)*dx^2+2*(1-e)*x*dx-e*(1-e)
delta2 = (y^2-z^2)*dy^2+2*(1-e)*y*dy-e*(1-e)
delta3 = (x+z)*(y+z)*dx*dy-e*(x+z)*dx-e*(y+z)*dy+e^2
h = x*dx+y*dy+z*dz-2*e
I = ideal(delta1+delta3, delta2+delta3,h)
r = holonomicRank I;                                        -- CORRECT: Outputs 4 as holonomic rank.
P = connectionMatrices I;                                            -- (old: ERROR:  reduce.m2:38:54:(3):[4]: error: not implemented yet: fraction fields of polynomial rings over rings other than ZZ, QQ, or a finite field)

--------------------------------------------------------





--------------------------------------------------------
--------------------------------------------------------
------ OLD EXAMPLES (graded reverse lex order)   -------
--------------------------------------------------------

--------------------------------------------------------
-- Example:

D = makeWA(QQ[x])
pfaffians ideal (dx^2 - x)                              -- Output (??)
--------------------------------------------------------

--------------------------------------------------------
-- Example:

D = makeWA(QQ[x_1,x_2,x_3])
-- FIXME: why zero?
netList pfaffians stafford ideal (dx_1, dx_2, dx_3)     -- Output (??)
-----------------------------

restart
needs "./pfaffians.m2"

-- Example 1.2.9 in SST, pp. 14
D = makeWA(QQ[a,b,c,DegreeRank => 0][x_1..x_4])
I = ideal(
    dx_2*dx_3 - dx_1*dx_4,
    x_1*dx_1 - x_4*dx_4 + 1 - c,
    x_2*dx_2 + x_4*dx_4 + a,
    x_3*dx_3 + x_4*dx_4 + b)
I = sub(I, {a => 1/2, b => 1/2, c => 1})
--WeylClosure I
netList(A = pfaffians I)

-- example
R = (frac extractVarsAlgebra D)(monoid[D.dpairVars#1])
G = gb sub(I, R);
B = sub((comodule I).cache#"basis", R)
dt = last D.dpairVars#1
s = last flatten entries B
last coefficients(sub(dt, R) * s % G, Monomials => B)

<< texMath A_1
checkSystem(D, A)

-- Example 1.4.23 in SST, pp. 39
netList apply(A, mat -> sub(mat, {a => 1/2, b => 1/2, c => 1}))


--examples from 25/11
D = makeWA(QQ[a,DegreeRank=>0][x])
pfaffians ideal (x^5*dx - a)


D = makeWA(QQ[a,b,DegreeRank=>0][x,y])
pfaffians(I=ideal (x^5*dx^2 - a,y^4*dy^3-b*dx))
holonomicRank I

f=symbol f
g=symbol g
h=symbol h
D = makeWA(QQ[f,g,h,DegreeRank=>0][x,y])
I=ideal(f*dx+g*dy+h,dx^2-dx^3+dy^3)
characteristicIdeal I
pfaffians I

D = makeWA(QQ[x,y])
g=x*y
f=x^2+y^2
h=x+y
I=ideal(f*dx+g*dy+h,dx^2+dy^3)
holonomicRank I
pfaffians I



D = QQ[x,y,dx,dy, WeylAlgebra =>{x=>dx,y=>dy}, Weights=>{0,0,2,1}]
P=x*dx^2-y*dy^2+dx-dy
Q=x*dx+y*dy+1
I=ideal(P,Q)
pfaffians I
gens gb I
leadTerm I

D' = QQ[x,y,dx,dy, WeylAlgebra =>{x=>dx,y=>dy}]
P=x*dx^2-y*dy^2+dx-dy
Q=x*dx+y*dy+1
I'=ideal(P,Q)
gens gb I'
leadTerm I'
pfaffians(I')
M'=comodule I'
holonomicRank M'
peek(M'.cache)




D = QQ[x,y,dx,dy, WeylAlgebra =>{x=>dx,y=>dy}]
P=x*dx^2-y*dy^2+2*dx-2*dy
Q=x*dx+y*dy+1
I=ideal(P,Q)
pfaffians I
M=comodule I
holonomicRank M
peek(M.cache)

--------------------------------------------------------
--------------------------------------------------------
-- END OF OLD WA EXAMPLES ------------------------------
--------------------------------------------------------
