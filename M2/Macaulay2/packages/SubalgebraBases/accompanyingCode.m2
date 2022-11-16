-*
This file accompanies the paper "SubalgebraBases in Macaulay2"
Code snippets from the paper can be found in this file.
Save this file and load it into Macaulay2 using:
load "accompanyingCode.m2"
To run a code snippet: call the corresponding function found below.
*-   

needsPackage "SubalgebraBases";

---------------------------------------
-- Code from Section 1: Introduction --
---------------------------------------
introduction1 = () -> (
    x := symbol x;
    R = QQ[x_1..x_3];
    f = x_1^2 + x_2^2 + x_3^2;
    A = subring {x_1+x_2+x_3, x_1*x_2 + x_1*x_3 + x_2*x_3, x_1*x_2*x_3};
    print("sagbi A: ", sagbi A);
    print("f // A, f % A: ", f // A, f % A);
    A
    )
--------------------------------------------------------
-- Code from Section 2: Subalgebra Bases in Macaulay2 --
--------------------------------------------------------
-- introduction to the function: subalgebraBasis
example1 = () -> (
    x := symbol x;
    R = QQ[x_1 .. x_3];
    subalgebraBasis {x_1+x_2+x_3, x_1^2+x_2^2+x_3^2, x_1^3+x_2^3+x_3^3}
    )
-- introduction to the function: isSAGBI
example2 = () -> (
    x := symbol x;
    y := symbol y;
    R = QQ[x,y];
    M = subalgebraBasis({x+y, x*y, x*y^2}, Limit => 15);
    print("generators: ", M);
    print("isSAGBI: ", isSAGBI M);
    M
    )
-- introduction to the functions: sagbi and isSAGBI
-- shows how to resume a previously started computation
example3 = () -> (
    x := symbol x;
    y := symbol y;
    R = QQ[x,y];
    S = subring {x+y, x^6, y^6};
    SB = sagbi(S, Limit => 5);
    print("isSAGBI SB: ", isSAGBI SB);
    SB' = sagbi(S, Limit => 100);
    print("isSAGBI SB': ", isSAGBI SB');
    print("gens SB' :", gens SB');
    SB'
    )
-- quotient ring example from [Example 2, Stillman and Tsai]
exampleQuotient = n -> (
    R := QQ[a,b,c,d,u_1 .. u_n, v_1 .. v_n, MonomialOrder => Lex];
    Q := R / ideal(a*d - b*c - 1);
    S := subring flatten for i from 1 to n list {a*u_i + b*v_i, c*u_i + d*v_i};
    sagbi S
    )
-- introduction to: % (modulo a subring and modulo a SAGBIBasis)
example4 = () -> (
    x := symbol x;
    y := symbol y;
    R = QQ[x,y];
    S = subring {x+y, x*y, x*y^2};
    B = sagbi(S, Limit => 5);
    f = x*y^3 + x*y^4 + x*y^5 + x*y^6;
    print("f % S: ", f % S);
    print("f % B: ", f % B);
    B
    )
-- introduction to: subduction
example5 = () -> (
    x := symbol x;
    y := symbol y;
    R := QQ[x,y];
    F := {x^2 + x, y^2 + 1};
    subduction(F, x^2*y^2 + x^3*y)
    )
-- introduction to: groebnerMembershipTest
example6 = () -> (
    example4();
    print("groebnerMembershipTest: ", groebnerMembershipTest(f, S));
    )
-- quotient example for groebnerMembershipTest:
example6a = () -> (
    x := symbol x;
    y := symbol y;
    R := QQ[x,y];
    I := ideal(x^3 + x*y + y^3);
    Q := R / I;
    use Q;
    S := subring {x^2*y^3 - x*2 - y^2 + x, x*y^4 + y^3 - y};
    f := y^10+x*y^8-x^2*y^6+x*y^6+2*x^2*y^4+y^5+x*y^3-y^3-x*y;
    print("groebnerMembershipTest: ", groebnerMembershipTest(f, S));
    print("f % (subalgebraBasis(S, Limit => 10)): ", f % (subalgebraBasis(S, Limit => 10)));
    S
    )
-- introduction to: subringIntersection
example7 = () -> (
    x := symbol x;
    y := symbol y;
    R = QQ[x,y];
    I = ideal(x^3 + x*y^2 + y^3);
    Q = R/I;
    S1 = subring {x^2, x*y};
    S2 = subring {x, y^2};
    subringIntersection(S1, S2, PrintLevel => 1)
    )
-- subringIntersection whose intersection has a finite subalgebra basis
-- but the computation requires an infinite subalgebra basis
example8 = () -> (
    x := symbol x;
    y := symbol y;
    R = QQ[x,y];
    S1 = subring {x^3, x^2*y};
    S2 = subring {x^4, y};
    subringIntersection(S1, S2, Limit => 15, PrintLevel => 1)
    )
-----------------------------------
-- Code from Section 3: Examples --
-----------------------------------
-- Cox-Nagata Ring example (Example 2.6 from Sturmfels-Xu)
coxNagata = () -> (
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
    )
-- Grassmannian(3,6) example
gr36hexagon = () -> (
    x := symbol x;
    R = QQ[x_(1,1) .. x_(3,6), MonomialOrder => {Weights => {0,0,0,0,0,0,0,15,3,12,9,6,0,7,14,21,28,35}}];
    X = transpose genericMatrix(R, 6, 3);
    S = subring for s in subsets(6, 3) list det X_s;
    SB = sagbi(S, Limit => 100, SubductionMethod => "Engine");
    print("isSAGBI: ", isSAGBI SB);
    SB
    )
-- Adjoint action of SE(3) on se(3) - single screw example
oneScrew = () -> (
    t := symbol t;
    w := symbol w;
    v := symbol v;
    R = QQ[t_1 .. t_3, w_1 .. w_3, v_1 .. v_3, MonomialOrder=>{Eliminate(3),Lex}];
    A = subring {w_1, w_2, w_3,
	-t_3*w_2+t_2*w_3+v_1, t_3*w_1-t_1*w_3+v_2, -t_2*w_1+t_1*w_2+v_3};
    B = selectInSubring(1, subalgebraBasis A)
    )
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
