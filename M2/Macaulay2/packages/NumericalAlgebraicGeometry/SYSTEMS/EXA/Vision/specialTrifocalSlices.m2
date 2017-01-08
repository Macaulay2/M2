restart
printWidth = 500
-- trifocal map
R = QQ[a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23,
    b00, b01, b02, b03, b10, b11, b12, b13, b20, b21, b22, b23,
    c00, c01, c02, c03, c10, c11, c12, c13, c20, c21, c22, c23];
S = QQ[t_(0,0,0)..t_(2,2,2)]

A = transpose genericMatrix(R,a00,4,3)
B = transpose genericMatrix(R,b00,4,3)
C = transpose genericMatrix(R,c00,4,3)
M = A||B||C

Is = reverse subsets(3,2);
phi = map(R,S,flatten flatten apply(3, i -> 
	apply(3, j ->
	    apply(3, k -> (-1)^(sum(Is#i)+j+k) *det M^(Is#i|{j+3}|{k+6})
	    	-- what is the sign convention?   
	    	)))
    )	   
ABC1 = random(QQ^1,QQ^(numgens R))
M1 = sub(M,ABC1)
--X1 = transpose matrix{{1,0,0,0}}
X1 = random(QQ^4,QQ^1) -- a world point
xyz1 = M1*X1 -- 3 views of X1
phiM1 = sub(matrix phi,ABC1)
tangent = sub(jacobian matrix phi,ABC1)
rank tangent
normal = gens ker tangent

--- special trifocal slices
bigS =S[x1,x2,x3,y1,y2,y3,z1,z2,z3]
T = apply(3,i->table(3,3,(j,k)->t_(i,j,k))) 
epsilon = x -> (
    (x1,x2,x3) := toSequence flatten entries x;
    - matrix {{0,x3,-x2},{-x3,0,x1},{x2,-x1,0}}
    )
xyz = genericMatrix(bigS,9,1)

L = matrix\T
L1 = L/(l->sub(l,phiM1))
contract (List,Matrix) := (L,xyz) -> (
    x := xyz^{0,1,2};
    y := xyz^{3,4,5};
    z := xyz^{6,7,8};
    sum(#L,l-> x_(l,0)*epsilon(y)*L#l*epsilon(z))
    --sum(#L,l-> L#l*x_(l,0))    
    --sum(#L,l-> transpose y'*L#l*z'*x_(l,0))
    )
contract(L1,xyz1)

I = ideal flatten entries contract(L,xyz)    

toSxyz1 = map(S,bigS,transpose xyz1)
genI = toSxyz1 I
codim genI
slice = sub(last coefficients gens genI,QQ)

-- what is the drop in dimension?
rank slice -- expected
rank (slice | normal) - rank normal -- actual


--------------
----------------
ABC1 = matrix{
    {1,0,0,0},
    {0,1,0,0},
    {0,0,1,0},
    {0,0,0,1},
    {0,0,1,0},
    {0,1,0,0},
    {0,1,1,1},
    {1,0,1,1},
    {1,1,0,1}
    }
    
