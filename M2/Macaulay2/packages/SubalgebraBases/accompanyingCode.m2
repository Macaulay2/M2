restart
-*
Example 2.2
*-
needsPackage "SubalgebraBases";
R = QQ[x_1..x_3];
A = subring {x_1+x_2+x_3, x_1^2+x_2^2+x_3^2, x_1^3+x_2^3+x_3^3};
SB = sagbi A
isSAGBI SB
A = subring(gens SB, GeneratorSymbol => g);
f = x_1^4 + x_2^4 + x_3^4;
q = f // A
r = f % A

-*
Example 2.3
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x_1,x_2];
SB = subalgebraBasis({x_1+x_2, x_1*x_2, x_1*x_2^2}, Limit => 7)
isSAGBI SB

-*
Example 2.5
*-
restart
needsPackage "SubalgebraBases";
n = 3;
R = QQ[a,b,c,d,u_1 .. u_n, v_1 .. v_n, MonomialOrder => Lex];
S = R / ideal(a*d - b*c - 1);
G = flatten for i from 1 to n list {a*u_i + b*v_i, c*u_i + d*v_i};
SB = subalgebraBasis G
isSAGBI SB

-*
Example 3.1
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x,y];
A = subring {x+y, x^6, y^6}
SB = sagbi(A, Limit => 5)
isSAGBI SB
SB' = sagbi(A, Limit => 100)
isSAGBI SB'
gens SB'

-*
Example 3.3
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x,y];
G = {x^2 + x, y^2 + 1};
subduction(G, x^2*y^2 + x^3*y)

-*
Examples 3.4 and 3.5
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x,y];
A = subring {x+y, x*y, x*y^2};
SB = sagbi(A, Limit => 5);
f = x*y^3 + x*y^4 + x*y^5 + x*y^6;
f % A
f % SB
SB = sagbi(A, Limit => 7);
f % SB
groebnerMembershipTest(f, A)

-*
Example 3.6
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x,y];
I = ideal(x^3 + x*y^2 + y^3);
S = R/I;
A1 = subring {x^2, x*y};
A2 = subring {x, y^2};
A = intersect(A1, A2)
gens A
isFullIntersection A

-*
Example 4.1
*-
restart
needsPackage "SubalgebraBases";
R = QQ[t_1..t_3, w_1..w_3, v_1..v_3, MonomialOrder=>{Eliminate 3, Lex}];
SB = sagbi {w_1, w_2, w_3, -t_3*w_2+t_2*w_3+v_1, t_3*w_1-t_1*w_3+v_2, -t_2*w_1+t_1*w_2+v_3};
isSAGBI SB
SB' = selectInSubring(1, gens SB)

-- multi-screw translational invariants
screwsExample = n -> (
    t := symbol t;
    w := symbol w;
    v := symbol v;
    R := QQ[t_1,t_2,t_3,w_(1,1)..w_(n,3),v_(1,1)..v_(n,3),MonomialOrder=>{Eliminate(3),Lex}];
    vs := for i from 1 to n list transpose matrix {{v_(i,1),v_(i,2),v_(i,3)}};
    ws := for i from 1 to n list transpose matrix {{w_(i,1),w_(i,2),w_(i,3)}};
    T := matrix{
        {0,-t_3,t_2},
        {t_3,0,-t_1},
        {-t_2,t_1,0}
    };
    cfold := L -> fold(L,(a,b)->a||b);
    ys := cfold for i from 1 to n list cfold {ws#(i-1), T*ws#(i-1)+vs#(i-1)};
    B := sagbi(transpose ys, Limit=>100, SubductionMethod => "Engine");
    (B, selectInSubring(1, gens B))
);

-*
Example 4.2
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x_1 .. x_6, y_1 .. y_6];
L124 = y_3*x_5*x_6 + x_3*y_5*x_6 - x_3*x_5*y_6;
L135 = y_2*x_4*x_6 - x_2*y_4*x_6 + x_2*x_4*y_6;
L236 = y_1*x_4*x_5 + x_1*y_4*x_5 - x_1*x_4*y_5;
L456 = y_1*x_2*x_3 + x_1*y_2*x_3 + x_1*x_2*y_3;
M16 = y_2*x_3*x_4*x_5 + x_2*y_3*x_4*x_5 - x_2*x_3*y_4*x_5 + x_2*x_3*x_4*y_5;
M25 = y_1*x_3*x_4*x_6 + x_1*y_3*x_4*x_6 + x_1*x_3*y_4*x_6 - x_1*x_3*x_4*y_6;
M34 = y_1*x_2*x_5*x_6 + x_1*y_2*x_5*x_6 - x_1*x_2*y_5*x_6 + x_1*x_2*x_5*y_6;
RG = subring {x_1 .. x_6, L124, L135, L236, L456, M16, M25, M34};
isSAGBI RG

-*
Example 4.3
*-
restart
needsPackage "SubalgebraBases";
R = QQ[x_(1,1)..x_(3,6),
    MonomialOrder => {Weights => {0,0,0,0,0,0,0,15,3,12,9,6,0,7,14,21,28,35}}];
X = transpose genericMatrix(R, 6, 3);
A = subring for s in subsets(6, 3) list det X_s;
SB = sagbi(A, Limit => 20, SubductionMethod => "Engine")
isSAGBI SB
