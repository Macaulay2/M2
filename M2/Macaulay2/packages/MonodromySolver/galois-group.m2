-- code for dealing w/ examples
pCompose = method()
pCompose (MutableHashTable, MutableHashTable) := (H1, H2) -> (
    new MutableHashTable from apply(keys H2,k-> if H1#?(H2#k) then k=> H1#(H2#k))
    )
///TEST
H1 = new MutableHashTable from {0=>1, 1=>2, 2=>0}
H2 = new MutableHashTable from {0=>2, 1=>1, 2=>0}
H1H2=pCompose(H1,H2)
H2H1=pCompose(H2,H1)
assert(H1H2#1 == 2)
assert(H2H1#1 == 0)
///

inverse MutableHashTable := H -> new MutableHashTable from apply(keys H, values H, (k,v) -> v=>k)

isIncident = (v, e) -> if v === e.Node1 or v === e.Node2 then true else false
neighbor = (v, e) -> if v === e.Node1 then e.Node2 else if v === e.Node2 then e.Node1 else error "edge not incident at vertex";

completeCorrespondences = (rc, G) -> reverse toList select(G.Edges, e -> (
        ve1 := delete(,unique values e.Correspondence12); 
        ve2 := delete(,values e.Correspondence21); 
        rc == length ve1 and rc == length ve2 
        )
    )

monodromyGroup = method(Options=>{FileName=>null,"RootCount"=>null,"msOptions"=>MonodromyOptions})
monodromyGroup HomotopyGraph := o -> G -> (
    rc := if instance(o#"RootCount", Nothing) then max apply(toList G.Vertices, v-> length v.PartialSols) else o#"RootCount";
    EG := completeCorrespondences(rc, G);
    goodVs := select(1, G.Vertices, v-> 2 == # select(2, EG, e -> isIncident(v, e)));
    if #goodVs > 0 then (
        goodV := first goodVs;
        getPermutations(goodV, length goodV.PartialSols, FileName => o.FileName)
        ) else {}
    )
monodromyGroup System := o -> GS -> (
    monOpts := new MutableHashTable from o#"msOptions";
    monOpts.EdgesSaturated = true;
    V := first monodromySolve(GS, new OptionTable from monOpts);
    monodromyGroup(V.Graph, o)
    )    
monodromyGroup (System, Point, List)  := o -> (GS, p0, x0s) -> (
    monOpts := new MutableHashTable from o#"msOptions";
    monOpts.EdgesSaturated = true;
    V := first monodromySolve(GS, p0, x0s, new OptionTable from monOpts);
    monodromyGroup(V.Graph, o)
    )    

getPermutations = method(Options => {FileName => null})
getPermutations (HomotopyNode, ZZ) := o -> (V, rc) -> (
    perms := {};
    if rc == length V.PartialSols then (
        idPerm := new MutableHashTable from for i from 0 to rc-1 list i => i;
        -- step 0: extract subgraph of complete correspondences
        G := V.Graph;
        -- EG = edges with complete correspondence
        EG := completeCorrespondences(rc, G);
        -- VG = connected component of V in subgraph of EG
        VG := set(apply(EG, e -> e.Node1) | apply(EG, e -> e.Node2));
        -- STEP 1: BUILD SPANNING TREE on (VG, EG)
        (uncoveredV, uncoveredE) := (VG-set{V},set EG);
        T := new MutableHashTable from {};
        while (#uncoveredV > 0) do (
            -- select an uncovered vertex adjacent to a covered vertex
            vList := select(1, keys uncoveredV, v -> any(v.Edges, e -> (u := neighbor(v, e); not member(u, uncoveredV) and member(e, uncoveredE))));
            if # vList == 1 then (
                v := first vList;
                -- select an edge w/ complete correspondence
                eList := select(1, reverse v.Edges, e -> (u := neighbor(v, e); not member(u, uncoveredV) and member(e, uncoveredE)));
                if #eList == 1 then (
                    T#v = first eList; 
                    uncoveredE = uncoveredE - set{T#v};
                    );
                );
            uncoveredV = uncoveredV - set{v};
            );
        -- STEP 2: extract permutations from cycle basis
        local ei;
        perms = values \ apply(keys uncoveredE, e -> (
                (u, v) := (e.Node1, e.Node2);
                uPath := idPerm;
                while T#?u do (
                    ei = T#u;
                    uPath = if u === ei.Node1 then pCompose(ei.Correspondence12, uPath) else pCompose(ei.Correspondence21, uPath);
                    u = neighbor(u, T#u);
                    );
                vPath := idPerm;
                while T#?v do (
                    ei = T#v;
                    vPath = if v === ei.Node1 then pCompose(ei.Correspondence12, vPath) else pCompose(ei.Correspondence21, vPath);
                    v = neighbor(v, T#v);
                    );
                pCompose(vPath, pCompose(e.Correspondence12, inverse uPath))
                )
            );
        filename := if instance(o.FileName, Nothing) then temporaryFileName() else (
            assert instance(o.FileName, String);
            o#FileName);
        writePermutations(perms, filename);
        );
    perms
   )


writePermutations = method(Options=>{"PathToFile"=>""})
writePermutations (List, String) := o -> (perms, filename) -> (
    file := openOut(o#"PathToFile"|filename);
    if #perms > 0 then (
        GAPperms := perms/(P->P/(i->i+1)); -- increment letters by 1 for GAP
        for i from 0 to #perms-1 do file << "p" << i << ":= PermList(" << toString(new Array from GAPperms#i) << ");" << endl;
        file << "G:=Group(";
        for i from 0 to #perms-2 do file << "p" << i << ", ";
        file << "p" << #perms-1 << ");";
        );
    close file
    )

