-- PURPOSE : Computing the Minkowskisum of two polyhedra in the same ambient space
minkowskiSum = method(TypicalValue => Polyhedron)

--   INPUT : '(P1,P2)',  two polyhedra
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
     -- Checking for input errors
     if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same space");
     if isEmpty P1 or isEmpty P2 then emptyPolyhedron ambDim P1 else if P1 == P2 then 2 * P1 else if ambDim P1 <= 3 then oldMinkSum(P1,P2) else newMinkSum(P1,P2))


oldMinkSum = (P1,P2) -> (
     -- Saving the vertices and rays
     V1 := vertices P1;
     V2 := vertices P2;
     R := promote(rays P1 | rays P2,QQ) | map(target V1,QQ^1,0);
     Vnew := map(target V1,QQ^0,0);
     -- Collecting all sums of vertices of P1 with vertices of P2
     Vnew = matrix {unique flatten apply(numColumns V1, i -> apply(numColumns V2, j -> V1_{i}+V2_{j}))};
     convexHull(Vnew,R))


newMinkSum = (P,Q) -> (
     facePairBuilder := (k,P) -> (
        L := faceBuilder(k,P);
        HS := halfspaces P;
        HS = apply(numRows HS#0, i -> ((HS#0)^{i},(HS#1)^{i}));
        apply(L, l -> (
             l = (toList l#0,toList l#1);
             (l,select(HS, hs -> all(l#0, v -> (hs#0)*v - hs#1 == 0) and all(l#1, r -> (hs#0)*r == 0)))))
     );
     uniqueColumns := M -> (
        if M!=0 then matrix{(unique apply(numColumns M, i -> M_{i}))} else map(ZZ^(numRows M),ZZ^0,0)
	  );
     block1 := (P, hyperplanesTmpQ) -> (
          entP := flatten entries((hyperplanesTmpQ#0)*(rays P));
          maxP := flatten entries((hyperplanesTmpQ#0)*(vertices P));
          if all(entP, e -> e == 0) then {(hyperplanesTmpQ#0,matrix{{max maxP}} + hyperplanesTmpQ#1),(-hyperplanesTmpQ#0,-(matrix{{min maxP}} + hyperplanesTmpQ#1))}
          else if all(entP, e -> e <= 0) then {(hyperplanesTmpQ#0,matrix{{max maxP}} + hyperplanesTmpQ#1)} 
          else if all(entP, e -> e >= 0) then {(-hyperplanesTmpQ#0,-(matrix{{min maxP}} + hyperplanesTmpQ#1))}
          else 0
     );
     block2 := (f, P, hyperplanesTmpQ) -> (
          if f#1 == {} then (
              L := block1(P,hyperplanesTmpQ);
              if L=!=0 then L else 0
          )
          else if all(flatten entries((f#1#0#0)*(rays P)), e -> e <= 0) then (
             mP := max flatten entries((f#1#0#0)*(vertices P));
             --mP = transpose makePrimitiveMatrix transpose(f#1#0#0|(f#1#0#1 + matrix{{mP}}));
             {(f#1#0#0,f#1#0#1 + matrix{{mP}})}
          ) else 0
     );
     sanitizeHyperplanes := (hyperplanes, hyperplanesTmp, n) -> (
        if numRows hyperplanes#0 == numRows hyperplanesTmp#0 then (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
             kPP := (transpose mingens ker(hyperplanesTmp#0 * transpose hyperplanes#0))_{0..(numRows hyperplanes#0)-1};
             (kPP * hyperplanes#0,kPP * hyperplanes#1)
        )
     );
     --
     -- Start of main method.
     --
     n := ambDim P;
     hyperplanesTmpP := hyperplanes P;
     hyperplanesTmpQ := hyperplanes Q;
     hyperplanesTmp := if hyperplanesTmpP == (0,0) or hyperplanesTmpQ == (0,0) then (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
        k := transpose mingens ker transpose(hyperplanesTmpP#0|| -hyperplanesTmpQ#0);
        if k == 0 then (map(ZZ^0,ZZ^n,0),map(ZZ^0,ZZ^1,0)) else (
             dhyperplanesTmpP := numRows hyperplanesTmpP#0;
             (k_{0..dhyperplanesTmpP-1} * hyperplanesTmpP#0,k*(hyperplanesTmpP#1||hyperplanesTmpQ#1)))
     );
     d := n - numRows(hyperplanesTmp#0);
     if d != n then (
        hyperplanesTmpP = sanitizeHyperplanes(hyperplanesTmpP, hyperplanesTmp, n);
        hyperplanesTmpQ = sanitizeHyperplanes(hyperplanesTmpQ, hyperplanesTmp, n);
     );
     LP := reverse apply(dim P + 1, k -> facePairBuilder(k,P));
     LP = LP | toList(max(0,d-#LP):{});
     LQ := reverse apply(dim Q + 1, k -> facePairBuilder(k,Q));
     LQ = LQ | toList(max(0,d-#LQ):{});
     HS := unique flatten apply(d, i -> (
	       if i == 0 then flatten for f in LQ#(d-1) list (
             L := block2(f,P,hyperplanesTmpQ);
             if L=!= 0 then L else continue
          )
	       else if i == d-1 then flatten for f in LP#(d-1) list (
             L := block2(f,Q,hyperplanesTmpP);
             if L=!= 0 then L else continue
          )
	       else flatten for Pface in LP#i list (
             for Qface in LQ#(d-i-1) list (
                -- This fixes the descending vertex number bug. We forgot to add the common hyperplanes.
                hyperplanesTmpPp := hyperplanes P;
                PfaceHS := if Pface#1 != {} then (matrix apply(Pface#1, f -> {f#0}) || hyperplanesTmpPp#0,matrix apply(Pface#1, f -> {f#1}) || hyperplanesTmpPp#1) else hyperplanesTmpPp;
                QfaceHS := if Qface#1 != {} then (matrix apply(Qface#1, f -> {f#0}) || hyperplanesTmpQ#0,matrix apply(Qface#1, f -> {f#1}) || hyperplanesTmpQ#1) else hyperplanesTmpQ;
                dP := rank PfaceHS#0;
                dQ := rank QfaceHS#0;
                PfaceHS = ((PfaceHS#0)^{0..dP-1},(PfaceHS#1)^{0..dP-1});
                QfaceHS = ((QfaceHS#0)^{0..dQ-1},(QfaceHS#1)^{0..dQ-1});
                kPQ := transpose mingens ker transpose(PfaceHS#0|| -QfaceHS#0); 
                if numRows kPQ != 1 then continue else (
                     dPfaceHS := numRows PfaceHS#0;
                     newHS := kPQ_{0..dPfaceHS-1} * PfaceHS#0 | kPQ*(PfaceHS#1||QfaceHS#1);
                     --newHS = transpose makePrimitiveMatrix newHS;
                     newHS = (submatrix'(newHS,{n}),newHS_{n});
                     checkValueP := (newHS#0 *(Pface#0#0#0))_(0,0);
                     checkValueQ := (newHS#0 *(Qface#0#0#0))_(0,0);
                     if all(flatten entries(newHS#0 *(vertices P)), e -> e <= checkValueP) and all(flatten entries(newHS#0 *(vertices Q)), e -> e <= checkValueQ) then (
                     if all(Pface#0#1, r -> (newHS#0 *r)_(0,0) <= 0) and all(Qface#0#1, r -> (newHS*r)_(0,0) <= 0) then newHS else continue) 
                     else if all(flatten entries(newHS#0 *(vertices P)), e -> e >= checkValueP) and 
                        all(flatten entries(newHS#0 *(vertices Q)), e -> e >= checkValueQ) then (
                        if all(Pface#0#1, r -> (newHS#0 *r)_(0,0) >= 0) and 
                           all(Qface#0#1, r -> (newHS*r)_(0,0) >= 0) then (-(newHS#0),-(newHS#1)) 
                        else continue
                     ) 
                     else continue
                  )
               )
            )
         ));
     HS = (matrix apply(HS, e -> {first e}),matrix apply(HS, e -> {last e}));
     V := matrix {unique flatten apply(numColumns vertices P, i -> apply(numColumns vertices Q, j -> (vertices P)_{i}+(vertices Q)_{j}))};
     -- Maybe the following line is needed as well:
     -- if V==0 then V = map(ZZ^(ambDim P),ZZ^1,0);
     -- This fixes the wrong ring bug.
     R := promote(rays P | rays Q,QQ) | map(target promote(V,QQ),QQ^1,0);
     V = (map(QQ^1,source promote(V,QQ),(i,j)->1) || promote(V,QQ)) | (map(QQ^1,source R,0) || R);
     HS = sort makePrimitiveMatrix transpose(-(HS#1)|HS#0);
     HS = uniqueColumns HS;
     hyperplanesTmp = sort makePrimitiveMatrix transpose(-(hyperplanesTmp#1)|hyperplanesTmp#0);
     hyperplanesTmp = uniqueColumns hyperplanesTmp;
     polyhedronBuilder reverse fMReplacement(V,HS,hyperplanesTmp))

--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : The Minkowskisum as a cone
minkowskiSum(Cone,Cone) := (C1,C2) -> (
     -- Checking for input errors
     if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same space");
     -- Saving the vertices and rays
     R := C1#"rays" | C2#"rays";
     LS := C1#"linealitySpace" | C2#"linealitySpace";
     posHull(R,LS))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Cone,Polyhedron) := (C,P) -> (
     -- Checking for input errors
     if ambDim(C) =!= ambDim(P) then error("Cone and polyhedron must lie in the same space");
     -- Saving the vertices and rays
     V := P#"vertices";
     R := P#"rays" | C#"rays" | C#"linealitySpace" | -(C#"linealitySpace");
     convexHull(V,R))


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Cone) := (P,C) -> (
     -- Checking for input errors
     if ambDim(C) =!= ambDim(P) then error("Cone and polyhedron must lie in the same space");
     -- Saving the vertices and rays
     V := P#"vertices";
     R := P#"rays" | C#"rays" | C#"linealitySpace" | -(C#"linealitySpace");
     convexHull(V,R))


Polyhedron + Polyhedron := minkowskiSum
Polyhedron + Cone := minkowskiSum
Cone + Polyhedron := minkowskiSum
Cone + Cone := minkowskiSum
