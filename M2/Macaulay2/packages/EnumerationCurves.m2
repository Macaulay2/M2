-- -*- coding: utf-8 -*-

newPackage(
        "EnumerationCurves",
        Version => "0.1",
        Date => "December 10, 2013",
        Authors => {{Name => "Hiep Dang",
                    Email => "hiepdt_tt@dlu.edu.vn"}},
        Headline => "Enumeration of rational curves via torus actions",
	Keywords => {"Commutative Algebra"},
        DebuggingMode => false
        )

export {"rationalCurve","linesHypersurface","multipleCover"}

rationalCurve = method()
rationalCurve(ZZ,List) := (d,D) -> (
    r := #D + 3;
    F := fixedPoints(r,d);
    result := 0;
    for f in F do (
        s := product(#D,i->contributionBundle(r,f#0,D#i));
        t := (f#1)*normalBundle(r,f#0);
        result = result + s/t;
        );
    result
    )
rationalCurve(ZZ) := (d) -> rationalCurve(d,{5});

multipleCover = method()
multipleCover(ZZ) := (d) -> (
    F := fixedPoints(1,d);
    result := 0;
    for f in F do (
        s := contributionBundle'(f#0);
        t := (f#1)*normalBundle(1,f#0);
        result = result + s/t;
        );
    result
    )

linesHypersurface = method()
linesHypersurface(ZZ) := (n) -> (
    F := fixedPoints(n,1);
    result := 0;
    for f in F do (
        s := contributionBundle(n,f#0);
        t := normalBundle(n,f#0);
        result = result + s/t;
        );
    result
    )

fixedPoints = method(TypicalValue => List)
--return a list of graphs corresponding the fixed point components of a torus
--action on a moduli space of stable maps.
fixedPoints(ZZ,ZZ) := (r,d) -> (
    S := subsets(for i to r list i,2);
    F := for s in S list {graph1(d,s#0,s#1),d};
    if d == 2 then (
        for i to r do (
            for j to r do (
                for k to r do (
                    if i != j and j != k then (
                        F = append(F,{graph2({1,1},i,j,k),2});
                        );
                    );
                );
            );
        );
    if d == 3 then (
        for i to r do (
            for j to r do (
                for k to r do (
                    if i != j and j != k then (
                        F = append(F,{graph2({2,1},i,j,k),2});
                        for h to r do (
                            if k != h then (
                                F = append(F,{graph31({1,1,1},i,j,k,h),2});
                                );
                            if j != h then (
                                F = append(F,{graph32({1,1,1},i,j,k,h),6});
                                );
                            );
                        );
                    );
                );
            );
        );
    if d == 4 then (
        for i to r do (
            for j to r do (
                for k to r do (
                    if i != j and j != k then (
                        F = append(F,{graph2({3,1},i,j,k),3});
                        F = append(F,{graph2({2,2},i,j,k),8});
                        for h to r do (
                            if k != h then (
                                F = append(F,{graph31({2,1,1},i,j,k,h),2});
                                F = append(F,{graph31({1,2,1},i,j,k,h),4});
                                );
                            if j != h then (
                                F = append(F,{graph32({2,1,1},i,j,k,h),4});
                                );
                            for m to r do (
                                if k != h and h != m then (
                                    F = append(F,{graph41({1,1,1,1},i,j,k,h,m),2});
                                    );
                                if k != h and k != m then (
                                    F = append(F,{graph42({1,1,1,1},i,j,k,h,m),2});
                                    );
                                if j != h and j != m then (
                                    F = append(F,{graph43({1,1,1,1},i,j,k,h,m),24});
                                    );
                                );
                            );
                        );
                    );
                );
            );
        );
    if d == 5 then (
        for i to r do (
            for j to r do (
                for k to r do (
                    if i != j and j != k then (
                        F = append(F,{graph2({4,1},i,j,k),4});
                        F = append(F,{graph2({3,2},i,j,k),6});
                        for h to r do (
                            if k != h then (
                                F = append(F,{graph31({3,1,1},i,j,k,h),3});
                                F = append(F,{graph31({1,3,1},i,j,k,h),6});
                                F = append(F,{graph31({2,2,1},i,j,k,h),4});
                                F = append(F,{graph31({2,1,2},i,j,k,h),8});
                                );
                            if j != h then (
                                F = append(F,{graph32({3,1,1},i,j,k,h),6});
                                F = append(F,{graph32({2,2,1},i,j,k,h),8});
                                );
                            for m to r do (
                                if k != h and h != m then (
                                    F = append(F,{graph41({2,1,1,1},i,j,k,h,m),2});
                                    F = append(F,{graph41({1,2,1,1},i,j,k,h,m),2});
                                    );
                                if k != h and k != m then (
                                    F = append(F,{graph42({2,1,1,1},i,j,k,h,m),4});
                                    F = append(F,{graph42({1,2,1,1},i,j,k,h,m),4});
                                    F = append(F,{graph42({1,1,2,1},i,j,k,h,m),2});
                                    );
                                if j != h and j != m then (
                                    F = append(F,{graph43({2,1,1,1},i,j,k,h,m),12});
                                    );
                                for n to r do (
                                    if k != h and h != m and m != n then (
                                        F = append(F,{graph51({1,1,1,1,1},i,j,k,h,m,n),2});
                                        );
                                    if k != h and h != m and h != n then (
                                        F = append(F,{graph52({1,1,1,1,1},i,j,k,h,m,n),2});
                                        );
                                    if k != h and h != m and k != n then (
                                        F = append(F,{graph53({1,1,1,1,1},i,j,k,h,m,n),2});
                                        );
                                    if k != h and k != m and k != n then (
                                        F = append(F,{graph54({1,1,1,1,1},i,j,k,h,m,n),6});
                                        );
                                    if k != h and j != m and k != n then (
                                        F = append(F,{graph55({1,1,1,1,1},i,j,k,h,m,n),8});
                                        );
                                    if j != h and j != m and j != n then (
                                        F = append(F,{graph56({1,1,1,1,1},i,j,k,h,m,n),120});
                                        );
                                    );
                                );
                            );
                        );
                    );
                );
            );
        );
    if d == 6 then (
        for i to r do (
            for j to r do (
                for k to r do (
                    if i != j and j != k then (
                        F = append(F,{graph2({5,1},i,j,k),5});
                        F = append(F,{graph2({4,2},i,j,k),8});
                        F = append(F,{graph2({3,3},i,j,k),18});
                        for h to r do (
                            if k != h then (
                                F = append(F,{graph31({4,1,1},i,j,k,h),4});
                                F = append(F,{graph31({1,4,1},i,j,k,h),8});
                                F = append(F,{graph31({3,2,1},i,j,k,h),6});
                                F = append(F,{graph31({2,1,3},i,j,k,h),6});
                                F = append(F,{graph31({1,3,2},i,j,k,h),6});
                                F = append(F,{graph31({2,2,2},i,j,k,h),16});
                                );
                            if j != h then (
                                F = append(F,{graph32({4,1,1},i,j,k,h),8});
                                F = append(F,{graph32({3,2,1},i,j,k,h),6});
                                F = append(F,{graph32({2,2,2},i,j,k,h),48});
                                );
                            for m to r do (
                                if k != h and h != m then (
                                    F = append(F,{graph41({3,1,1,1},i,j,k,h,m),3});
                                    F = append(F,{graph41({1,3,1,1},i,j,k,h,m),3});
                                    F = append(F,{graph41({2,2,1,1},i,j,k,h,m),4});
                                    F = append(F,{graph41({2,1,2,1},i,j,k,h,m),4});
                                    F = append(F,{graph41({2,1,1,2},i,j,k,h,m),8});
                                    F = append(F,{graph41({1,2,2,1},i,j,k,h,m),8});
                                    );
                                if k != h and k != m then (
                                    F = append(F,{graph42({3,1,1,1},i,j,k,h,m),6});
                                    F = append(F,{graph42({1,3,1,1},i,j,k,h,m),6});
                                    F = append(F,{graph42({1,1,3,1},i,j,k,h,m),3});
                                    F = append(F,{graph42({2,2,1,1},i,j,k,h,m),8});
                                    F = append(F,{graph42({1,1,2,2},i,j,k,h,m),8});
                                    F = append(F,{graph42({1,2,2,1},i,j,k,h,m),4});
                                    F = append(F,{graph42({2,1,2,1},i,j,k,h,m),4});
                                    );
                                if j != h and j != m then (
                                    F = append(F,{graph43({3,1,1,1},i,j,k,h,m),18});
                                    F = append(F,{graph43({2,2,1,1},i,j,k,h,m),16});
                                    );
                                for n to r do (
                                    if k != h and h != m and m != n then (
                                        F = append(F,{graph51({2,1,1,1,1},i,j,k,h,m,n),2});
                                        F = append(F,{graph51({1,2,1,1,1},i,j,k,h,m,n),2});
                                        F = append(F,{graph51({1,1,2,1,1},i,j,k,h,m,n),4});
                                        );
                                    if k != h and h != m and h != n then (
                                        F = append(F,{graph52({2,1,1,1,1},i,j,k,h,m,n),4});
                                        F = append(F,{graph52({1,2,1,1,1},i,j,k,h,m,n),4});
                                        F = append(F,{graph52({1,1,2,1,1},i,j,k,h,m,n),4});
                                        F = append(F,{graph52({1,1,1,2,1},i,j,k,h,m,n),2});
                                        );
                                    if k != h and h != m and k != n then (
                                        F = append(F,{graph53({2,1,1,1,1},i,j,k,h,m,n),2});
                                        F = append(F,{graph53({1,2,1,1,1},i,j,k,h,m,n),2});
                                        F = append(F,{graph53({1,1,1,1,2},i,j,k,h,m,n),4});
                                        );
                                    if k != h and k != m and k != n then (
                                        F = append(F,{graph54({2,1,1,1,1},i,j,k,h,m,n),12});
                                        F = append(F,{graph54({1,2,1,1,1},i,j,k,h,m,n),12});
                                        F = append(F,{graph54({1,1,2,1,1},i,j,k,h,m,n),4});
                                        );
                                    if k != h and j != m and k != n then (
                                        F = append(F,{graph55({2,1,1,1,1},i,j,k,h,m,n),4});
                                        F = append(F,{graph55({1,2,1,1,1},i,j,k,h,m,n),16});
                                        );
                                    if j != h and j != m and j != n then (
                                        F = append(F,{graph56({2,1,1,1,1},i,j,k,h,m,n),48});
                                        );
                                    for p to r do (
                                        if k != h and h != m and m != n and n != p then (
                                            F = append(F,{graph61({1,1,1,1,1,1},i,j,k,h,m,n,p),2});
                                            );
                                        if k != h and h != m and m != n and m != p then (
                                            F = append(F,{graph62({1,1,1,1,1,1},i,j,k,h,m,n,p),2});
                                            );
                                        if k != h and h != m and m != n and h != p then (
                                            F = append(F,{graph63({1,1,1,1,1,1},i,j,k,h,m,n,p),1});
                                            );
                                        if k != h and h != m and h != n and h != p then (
                                            F = append(F,{graph64({1,1,1,1,1,1},i,j,k,h,m,n,p),6});
                                            );
                                        if k != h and h != m and k != n and k != p then (
                                            F = append(F,{graph65({1,1,1,1,1,1},i,j,k,h,m,n,p),4});
                                            );
                                        if k != h and h != m and k != n and n != p then (
                                            F = append(F,{graph66({1,1,1,1,1,1},i,j,k,h,m,n,p),6});
                                            );
                                        if k != h and h != m and j != n and h != p then (
                                            F = append(F,{graph67({1,1,1,1,1,1},i,j,k,h,m,n,p),8});
                                            );
                                        if k != h and h != m and k != n and h != p then (
                                            F = append(F,{graph68({1,1,1,1,1,1},i,j,k,h,m,n,p),2});
                                            );
                                        if k != h and j != m and k != n and k != p then (
                                            F = append(F,{graph69({1,1,1,1,1,1},i,j,k,h,m,n,p),12});
                                            );
                                        if k != h and k != m and k != n and k != p then (
                                            F = append(F,{graph610({1,1,1,1,1,1},i,j,k,h,m,n,p),24});
                                            );
                                        if j != h and j != m and j != n and j != p then (
                                            F = append(F,{graph611({1,1,1,1,1,1},i,j,k,h,m,n,p),720});
                                            );
                                        );
                                    );
                                );
                            );
                        );
                    );
                );
            );
        );
    F
    )

torusList = method(TypicalValue => List)
--create a list of integers corresponding the weights of a torus action on 
--moduli spaces of stable maps.
torusList(ZZ) := (r) -> apply(r+1, i -> 10^i)

contributionBundle = method()
--return a number corresponding to the contribution bundle on a moduli space of
--stable maps at a fixed point component (graph).
contributionBundle(ZZ,List,ZZ) := (r,G,k) -> (
    L := torusList(r);
    V := product(#G#0,i->(k*L#(G#0#i#0))^(G#0#i#1 - 1));
    E := product(#G#1,j->product(k*G#1#j#2+1,i->(i*L#(G#1#j#0)+(k*G#1#j#2-i)*L#(G#1#j#1))/G#1#j#2));
    E/V
    )
contributionBundle(ZZ,List) := (r,G) -> contributionBundle(r,G,2*r-3)

contributionBundle' = method()
contributionBundle'(List) := (G) -> (
    L := torusList(1);
    V := product(#G#0,i->(-L#(G#0#i#0))^(G#0#i#1 - 1));
    E := product(#G#1,j->product(1..G#1#j#2-1,i->(-i*L#(G#1#j#0)-(G#1#j#2-i)*L#(G#1#j#1))/G#1#j#2));
    (E*V)^2
    )

normalBundle = method()
--a number corresponding to the normal bundle on a moduli space of stable maps 
--at a fixed point component (graph).
normalBundle(ZZ,List) := (r,G) -> (
    L := torusList(r);
    N := 1;
    for e in G#1 do (
        E := (-1)^(e#2)*(e#2)!^2*(L#(e#0)-L#(e#1))^(2*(e#2))/(e#2)^(2*(e#2));
        for k to r do (
            if k != e#0 and k != e#1 then (
                E = E*product(e#2+1,i->(i*L#(e#0)+(e#2-i)*L#(e#1))/(e#2) - L#k);
                );
            );
        N = N*E;
        );
    for v in G#0 do (
        F := product(1..v#1,i->(L#(v#(-i)#0)-L#(v#(-i)#1))/v#(-i)#2);
        if v#1 == 1 then (N = N/F;) else (
            z := 1;
            for i to r do (
                if i != v#0 then (z = z*(L#(v#0)-L#i);)
                );
            if v#1 == 3 then (N = N*F/z^2;) else (
                G := sum(1..v#1,i->v#(-i)#2/(L#(v#(-i)#0)-L#(v#(-i)#1)));
                N = N*F*G^(3-v#1)/z^(v#1-1);
                );
            );
        );
    N
    )

--------------------------------------------------------------------------------
----------------- Methods concerned with graphs ------------------------------
--------------------------------------------------------------------------------

graph1 = (d,i,j) -> (
    f1 := {i,j,d};
    f2 := {j,i,d};
    v1 := {i,1,f1};
    v2 := {j,1,f2};
    vertices := {v1,v2};
    edges := {f1};
    {vertices,edges}
    )

graph2 = (d,i,j,k) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,1,f4};
    vertices := {v1,v2,v3};
    edges := {f1,f3};
    {vertices,edges}
    )

graph31 = (d,i,j,k,h) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,1,f6};
    vertices := {v1,v2,v3,v4};
    edges := {f1,f3,f5};
    {vertices,edges}
    )

graph32 = (d,i,j,k,h) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {j,h,d#2};
    f6 := {h,j,d#2};
    v1 := {i,1,f1};
    v2 := {j,3,f2,f3,f5};
    v3 := {k,1,f4};
    v4 := {h,1,f6};
    vertices := {v1,v2,v3,v4};
    edges := {f1,f3,f5};
    {vertices,edges}
    )

graph41 = (d,i,j,k,h,m) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,2,f6,f7};
    v5 := {m,1,f8};
    vertices := {v1,v2,v3,v4,v5};
    edges := {f1,f3,f5,f7};
    {vertices,edges}
    )

graph42 = (d,i,j,k,h,m) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {k,m,d#3};
    f8 := {m,k,d#3};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,3,f4,f5,f7};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    vertices := {v1,v2,v3,v4,v5};
    edges := {f1,f3,f5,f7};
    {vertices,edges}
    )

graph43 = (d,i,j,k,h,m) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {j,h,d#2};
    f6 := {h,j,d#2};
    f7 := {j,m,d#3};
    f8 := {m,j,d#3};
    v1 := {i,1,f1};
    v2 := {j,4,f2,f3,f5,f7};
    v3 := {k,1,f4};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    vertices := {v1,v2,v3,v4,v5};
    edges := {f1,f3,f5,f7};
    {vertices,edges}
    )

graph51 = (d,i,j,k,h,m,n) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {m,n,d#4};
    f10 := {n,m,d#4};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,2,f6,f7};
    v5 := {m,2,f8,f9};
    v6 := {n,1,f10};
    vertices := {v1,v2,v3,v4,v5,v6};
    edges := {f1,f3,f5,f7,f9};
    {vertices,edges}
    )

graph52 = (d,i,j,k,h,m,n) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {h,n,d#4};
    f10 := {n,h,d#4};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,3,f6,f7,f9};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    vertices := {v1,v2,v3,v4,v5,v6};
    edges := {f1,f3,f5,f7,f9};
    {vertices,edges}
    )

graph53 = (d,i,j,k,h,m,n) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,3,f4,f5,f9};
    v4 := {h,2,f6,f7};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    vertices := {v1,v2,v3,v4,v5,v6};
    edges := {f1,f3,f5,f7,f9};
    {vertices,edges}
    )

graph54 = (d,i,j,k,h,m,n) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {k,m,d#3};
    f8 := {m,k,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,4,f4,f5,f7,f9};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    vertices := {v1,v2,v3,v4,v5,v6};
    edges := {f1,f3,f5,f7,f9};
    {vertices,edges}
    )

graph55 = (d,i,j,k,h,m,n) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {j,m,d#3};
    f8 := {m,j,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    v1 := {i,1,f1};
    v2 := {j,3,f2,f3,f7};
    v3 := {k,3,f4,f5,f9};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    vertices := {v1,v2,v3,v4,v5,v6};
    edges := {f1,f3,f5,f7,f9};
    {vertices,edges}
    )

graph56 = (d,i,j,k,h,m,n) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {j,h,d#2};
    f6 := {h,j,d#2};
    f7 := {j,m,d#3};
    f8 := {m,j,d#3};
    f9 := {j,n,d#4};
    f10 := {n,j,d#4};
    v1 := {i,1,f1};
    v2 := {j,5,f2,f3,f5,f7,f9};
    v3 := {k,1,f4};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    vertices := {v1,v2,v3,v4,v5,v6};
    edges := {f1,f3,f5,f7,f9};
    {vertices,edges}
    )

graph61 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {m,n,d#4};
    f10 := {n,m,d#4};
    f11 := {n,p,d#5};
    f12 := {p,n,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,2,f6,f7};
    v5 := {m,2,f8,f9};
    v6 := {n,2,f10,f11};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph62 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {m,n,d#4};
    f10 := {n,m,d#4};
    f11 := {m,p,d#5};
    f12 := {p,m,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,2,f6,f7};
    v5 := {m,3,f8,f9,f11};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph63 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {m,n,d#4};
    f10 := {n,m,d#4};
    f11 := {h,p,d#5};
    f12 := {p,h,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,3,f6,f7,f11};
    v5 := {m,2,f8,f9};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph64 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {h,n,d#4};
    f10 := {n,h,d#4};
    f11 := {h,p,d#5};
    f12 := {p,h,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,2,f4,f5};
    v4 := {h,4,f6,f7,f9,f11};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph65 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    f11 := {k,p,d#5};
    f12 := {p,k,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,4,f4,f5,f9,f11};
    v4 := {h,2,f6,f7};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph66 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    f11 := {n,p,d#5};
    f12 := {p,n,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,3,f4,f5,f9};
    v4 := {h,2,f6,f7};
    v5 := {m,1,f8};
    v6 := {n,2,f10,f11};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph67 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {j,n,d#4};
    f10 := {n,j,d#4};
    f11 := {h,p,d#5};
    f12 := {p,h,d#5};
    v1 := {i,1,f1};
    v2 := {j,3,f2,f3,f9};
    v3 := {k,2,f4,f5};
    v4 := {h,3,f6,f7,f11};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph68 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {h,m,d#3};
    f8 := {m,h,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    f11 := {h,p,d#5};
    f12 := {p,h,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,3,f4,f5,f9};
    v4 := {h,3,f6,f7,f11};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph69 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {j,m,d#3};
    f8 := {m,j,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    f11 := {k,p,d#5};
    f12 := {p,k,d#5};
    v1 := {i,1,f1};
    v2 := {j,3,f2,f3,f7};
    v3 := {k,4,f4,f5,f9,f11};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph610 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {k,h,d#2};
    f6 := {h,k,d#2};
    f7 := {k,m,d#3};
    f8 := {m,k,d#3};
    f9 := {k,n,d#4};
    f10 := {n,k,d#4};
    f11 := {k,p,d#5};
    f12 := {p,k,d#5};
    v1 := {i,1,f1};
    v2 := {j,2,f2,f3};
    v3 := {k,5,f4,f5,f7,f9,f11};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

graph611 = (d,i,j,k,h,m,n,p) -> (
    f1 := {i,j,d#0};
    f2 := {j,i,d#0};
    f3 := {j,k,d#1};
    f4 := {k,j,d#1};
    f5 := {j,h,d#2};
    f6 := {h,j,d#2};
    f7 := {j,m,d#3};
    f8 := {m,j,d#3};
    f9 := {j,n,d#4};
    f10 := {n,j,d#4};
    f11 := {j,p,d#5};
    f12 := {p,j,d#5};
    v1 := {i,1,f1};
    v2 := {j,6,f2,f3,f5,f7,f9,f11};
    v3 := {k,1,f4};
    v4 := {h,1,f6};
    v5 := {m,1,f8};
    v6 := {n,1,f10};
    v7 := {p,1,f12};
    vertices := {v1,v2,v3,v4,v5,v6,v7};
    edges := {f1,f3,f5,f7,f9,f11};
    {vertices,edges}
    )

beginDocumentation()

doc ///
  Key
    EnumerationCurves
  Headline
    Enumeration of rational curves via torus actions
  Description
    Text
      
      {\it EnumerationCurves} is a package to compute the physical numbers of rational curves on Calabi-Yau threefolds
      via torus actions. 
      
      The implementation uses the formulas given in the paper of Kontsevich "{\it Enumeration of rational curves via torus actions}".
      
      The main idea is to apply the localization theorem of Atiyah and Bott to reduce the computation to integrations over 
      graphs.
///

doc ///
  Key
    rationalCurve
    (rationalCurve, ZZ)
    (rationalCurve, ZZ, List)
  Headline
    Rational curves on Calabi-Yau threefolds
  Usage
    rationalCurve(d)
    rationalCurve(d,D)
  Inputs
    d:ZZ
      the degree d of a rational curve
    D:List
      a list of positive integers corresponding to the type of a complete intersection
  Outputs
    :QQ
      the physical number of rational curves on a general Calabi-Yau threefold
  Description
    Text
      Computes the physical number of rational curves on a general complete intersection 
      Calabi-Yau threefold in some projective space.

      There are five types of such the complete intersections: quintic hypersurface in \mathbb P^4,
      complete intersections of types (4,2) and (3,3) in \mathbb P^5, complete intersection of type (3,2,2) in \mathbb P^6,
      complete intersection of type (2,2,2,2) in \mathbb P^7.
      
      For lines:
    
    Example
      rationalCurve(1)
      T = {{5},{4,2},{3,3},{3,2,2},{2,2,2,2}}
      for D in T list rationalCurve(1,D)
    Text
      This gives the numbers of lines on general complete intersection Calabi-Yau threefolds.
      
      For conics:
    
    Example
      rationalCurve(2)
      for D in T list rationalCurve(2,D)
    Text
      The number of conics on a general quintic threefold can be computed as follows:
      
    Example
      rationalCurve(2) - rationalCurve(1)/8
    Text
      The numbers of conics on general complete intersection Calabi-Yau threefolds can be computed as follows:
    
    Example
      time for D in T list rationalCurve(2,D) - rationalCurve(1,D)/8
    Text
      For rational curves of degree 3:
    
    Example
      time rationalCurve(3)
      time for D in T list rationalCurve(3,D)
    Text
      The number of rational curves of degree 3 on a general quintic threefold can be computed as follows:
      
    Example
      time rationalCurve(3) - rationalCurve(1)/27
    Text
      The numbers of rational curves of degree 3 on general complete intersection Calabi-Yau threefolds can be computed as follows:
    
    Example
      time for D in T list rationalCurve(3,D) - rationalCurve(1,D)/27
    Text
      For rational curves of degree 4:
    
    Example
      time rationalCurve(4)
      time rationalCurve(4,{4,2})
    Text
      The number of rational curves of degree 4 on a general quintic threefold can be computed as follows:
      
    Example
      time rationalCurve(4) - rationalCurve(2)/8
    Text
      The numbers of rational curves of degree 4 on general complete intersections of types (4,2) and (3,3) in \mathbb P^5 can be computed as follows:
    
    Example
      time rationalCurve(4,{4,2}) - rationalCurve(2,{4,2})/8
      time rationalCurve(4,{3,3}) - rationalCurve(2,{3,3})/8
///

doc ///
  Key
    multipleCover
    (multipleCover, ZZ)
  Headline
    Multiple coverings of rational curves on Calabi-Yau threefolds
  Usage
    multipleCover(d)
  Inputs
    d:ZZ
      the degree d of a rational curve
  Outputs
    :QQ
      the contribution of multiple coverings of rational curves on Calabi-Yau threefolds
  Description
    Text
      Computes the contribution of multiple coverings of a smooth rational curve as a Gromov-Witten invariant.
      
    Example
      for d from 1 to 6 list multipleCover(d)
///

doc ///
  Key
    linesHypersurface
    (linesHypersurface, ZZ)
  Headline
    Lines on hypersurfaces
  Usage
    linesHypersurface(n)
  Inputs
    n:ZZ
      the dimension of ambient projective space
  Outputs
    :ZZ
      the number of lines on a general hypersurface of degree 2n - 3 in \mathbb P^n
  Description
    Text
      Computes the number of lines on a general hypersurface of degree 2n - 3 in \mathbb P^n.

    Example
      time for n from 2 to 10 list linesHypersurface(n)
///

TEST ///
    assert(rationalCurve(1) == 2875)
    assert(rationalCurve(2) == 4876875/8)
    assert(rationalCurve(3) == 8564575000/27)
    assert(rationalCurve(4) == 15517926796875/64)
--    assert(rationalCurve(5) == 229305888887648)
    assert(multipleCover(1) == 1)
    assert(multipleCover(2) == 1/8)
    assert(multipleCover(3) == 1/27)
    assert(multipleCover(4) == 1/64)
    assert(multipleCover(5) == 1/125)
    assert(multipleCover(6) == 1/216)
    assert(linesHypersurface(3) == 27)
    assert(linesHypersurface(4) == 2875)
    assert(linesHypersurface(5) == 698005)
    assert(linesHypersurface(6) == 305093061)
///

end

restart
uninstallPackage "EnumerationCurves"
notify=true
installPackage "EnumerationCurves"
viewHelp EnumerationCurves
