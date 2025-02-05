newPackage(
    "ThinSincereQuivers",
    Headline => "Construction of flow polytopes and their associated quivers",
    Version => "0.1",
    Date => "January 18, 2025",
    Authors => {
        {Name => "Mary Barker",
         Email => "marybarker103@gmail.com",
         HomePage => "https://github.com/marybarker"}, 
        {Name => "Patricio Gallardo",
         Email => "pgallard@ucr.edu",
         HomePage => "http://patriciogallardo.com/"
        }
    },
    PackageImports => {"Graphs", "Polyhedra", "LatticePolytopes"},
    Keywords => {"Toric Geometry"}
)
export {
-- Methods/Functions
    "allSpanningTrees",
    "basisForFlowPolytope",
    "bipartiteQuiver",
    "chainQuiver",
    "coneSystem",
    "flowPolytopeVertices",
    "getWeights",
    "incInverse",
    "isAcyclic",
    "isClosedUnderArrows",
    "isSemistable",
    "isStable",
    "isTight",
    "makeTight",
    "maxCodimensionUnstable",
    "maximalNonstableSubquivers",
    "maximalUnstableSubquivers",
    "mergeOnArrow",
    "mergeOnVertex",
    "potentialWalls",
    "primitiveArrows",
    "quiverIncidenceMatrix",
    "quiverEdges",
    "quiverFlow",
    "quiverVertices",
    "quiverWeights",
    "referenceThetas",
    "referenceWeights",
    "samePolytope",
    "stableTrees",
    "subquivers",
    "threeVertexQuiver",
    "wallQPlus",
    "wallType",
-- Options
    "AsSubquiver",
    "Flow",
    "Height",
    "ReturnSingletons",
-- Quiver objects 
    "ToricQuiver",
    "toricQuiver"
}
-----------------------------------------------------------
-- PACKAGE GLOBALS: special variable names, types, and constants
-----------------------------------------------------------
protect Axis
protect EdgesAdded
protect flow
protect IncidenceMatrix
protect NonSingletons
protect Oriented
protect Q0
protect Q1
protect Qplus
protect RavelLoops
protect Replacement
protect SavePath
protect Singletons
protect WallType
protect weights

ToricQuiver = new Type of HashTable
ToricQuiver.synonym = "toric quiver"
Wall = new Type of HashTable
Wall.synonym = "wall"

-----------------------------------------------------------


-----------------------------------------------------------
-- PACKAGE METHODS/FUNCTIONS:
-----------------------------------------------------------

-----------------------------------------------------------
-- Toric Quiver constructor
-----------------------------------------------------------
toricQuiver = method(Options => {Flow => "Default", Height => 10})
-- construct ToricQuiver from incidence matrix
toricQuiver(Matrix) := ToricQuiver => opts -> Q -> (
    F := 0.5*sumList(for x in entries(Q) list(for y in x list(abs(y))), Axis => "Col");
    if opts.Flow == "Canonical" then (
        F = toList(numColumns(Q):1);
    ) else if opts.Flow == "Random" then (
        F = for i in (0..#F - 1) list(random(opts.Height));
    );
    -- set Q to be unit valued to apply flow
    Q = matrix(for e in entries(Q) list(for x in e list(if abs(x) > 0 then x/abs(x) else 0)));
    Qp := matrix(for e in entries(Q) list(for x in e list abs x));
    -- verify that each column in Q has 2 nonzero entries, which are -1 and 1.
    if any(sum entries Qp, x -> x != 2) then (
        error "Matrix is not a graph incidence matrix";
    );
    if any(sum entries Q, x -> x != 0) then (
        error "Matrix is not a graph incidence matrix";
    );

    -- find lexicographic ordering on edges for standard quiver representation
    Qi := sortedIndices(graphEdges(Q, Oriented => true));
    new ToricQuiver from hashTable{
        IncidenceMatrix => Q_Qi,
        Q0 => toList(0..numRows(Q) - 1),
        Q1 => sort(graphEdges(Q, Oriented => true)),
        flow => F_Qi,
        weights => sumList(entries(Q*diagonalMatrix(F)), Axis => "Row")
    }
)
-- construct ToricQuiver from incidence matrix and a flow
toricQuiver(Matrix, List) := ToricQuiver => opts -> (Q, F) -> (
    -- set Q to be unit valued to apply flow
    Q = matrix(for e in entries(Q) list(for x in e list(if abs(x) > 0 then x/abs(x) else 0)));
    Qp := matrix(for e in entries(Q) list(for x in e list abs x));
    -- verify that each column in Q has 2 nonzero entries, which are -1 and 1.
    if any(sum entries Qp, x -> x != 2) then (
        error "Matrix is not a graph incidence matrix";
    );
    if any(sum entries Q, x -> x != 0) then (
        error "Matrix is not a graph incidence matrix";
    );
    -- find lexicographic ordering on edges for standard quiver representation
    Qi := sortedIndices(graphEdges(Q, Oriented => true));
    new ToricQuiver from hashTable{
        IncidenceMatrix => Q_Qi,
        Q0 => toList(0..numRows(Q) - 1),
        Q1 => sort(graphEdges(Q, Oriented => true)),
        flow => F_Qi,
        weights => sumList(entries(Q*diagonalMatrix(F)), Axis => "Row")
    }
)
-- construct ToricQuiver from Toric quiver to copy graph
toricQuiver(ToricQuiver) := ToricQuiver => opts -> Q -> (
    toricQuiver(Q.IncidenceMatrix, Q.flow, Flow => opts.Flow)
)
-- construct ToricQuiver from Toric quiver (to copy graph) and list(giving a flow)
toricQuiver(ToricQuiver, List) := ToricQuiver => opts -> (Q, F) -> (
    toricQuiver(Q.IncidenceMatrix, F)
)
-- construct ToricQuiver from list of edges
toricQuiver(List) := ToricQuiver => opts -> E -> (
    Q := graphFromEdges(sort(E), Oriented => true);
    F := toList(#E:1);
    if opts.Flow == "Random" then (
        F = for i in (0..#E - 1) list(random(opts.Height));
    );
    new ToricQuiver from hashTable{
        IncidenceMatrix => Q,
        Q0 => toList(0..numRows(Q) - 1),
        Q1 => E,
        flow => F,
        weights => sumList(entries(Q*diagonalMatrix(F)), Axis => "Row")
    }
)
-- construct ToricQuiver from list of edges and a flow
toricQuiver(List, List) := ToricQuiver => opts -> (E, F) -> (
    -- find lexicographic ordering on edges for standard quiver representation
    Qi := sortedIndices(E);

    Q := matrix graphFromEdges(E_Qi, Oriented => true);
    new ToricQuiver from hashTable{
        IncidenceMatrix => Q,
        Q0 => toList(0..numRows(Q) - 1),
        Q1 => E_Qi,
        flow => F_Qi,
        weights => sumList(entries(Q*diagonalMatrix(F)), Axis => "Row")
    }
)
-- construct ToricQuiver from a Graph object
toricQuiver(Graph) := ToricQuiver => opts -> G -> (
    E := for e in edges(G) list toList(e);
    toricQuiver(E, Flow => opts.Flow)
)
-- construct ToricQuiver from a Graph and a flow
toricQuiver(Graph, List) := ToricQuiver => opts -> (G, F) -> (
    E := for e in edges(G) list toList(e);
    toricQuiver(E, F)
)
-- subquiver of a ToricQuiver by taking a subset of the arrows, represented as a "child" of the original quiver
ToricQuiver ^ List := ToricQuiver => (TQ, L) -> (
    if any(L, x -> (x < 0) or (x >= #TQ.Q1)) then {
      error "invalid range for subsetting quiver edges";
    };
    newFlow := TQ.flow;
    Lc := toList(set(0..#TQ.flow - 1) - set(L));
    for i in Lc do(newFlow = replace(i, 0, newFlow));
    toricQuiver(TQ.IncidenceMatrix, newFlow)
)
-- subquiver of a ToricQuiver by removing all vertices/arrows not in the subquiver
ToricQuiver _ List := ToricQuiver => (TQ, L) -> (
    if any(L, x -> (x < 0) or (x >= #TQ.Q1)) then {
      error "invalid range for subsetting quiver edges";
    };
    M := matrix(for x in entries(TQ.IncidenceMatrix_L) list(if any(x, y-> y != 0) then (x) else (continue;)));
    toricQuiver(M)
)
-- equality of two quivers:
ToricQuiver == ToricQuiver := Boolean => (TQ1, TQ2) -> (
    (TQ1.Q1 === TQ2.Q1) and (TQ1.flow == TQ2.flow)
)
------------------------------------------------------------




------------------------------------------------------------
-- OTHER EXPORTED FUNCTIONS
------------------------------------------------------------


------------------------------------------------------------
allSpanningTrees = (TQ) -> (
    Q := TQ.IncidenceMatrix;
    Q0 := numRows(Q);
    Q1 := numColumns(Q);

    --  edges of quiver Q represented as a list of tuples
    allEdges := graphEdges(Q, Oriented => true);
    allNodes := toList(0..Q0-1);

    trees := {};
    edgeIndices := {};
    
    -- in any tree, the number of edges should be #vertices - 1, 
    -- and so we need to remove Q1 - (Q0-1) edges to obtain a tree

    d := Q1 - Q0 + 1;
    if d > 0 then (
        -- try removing every combination of d edges and see if result is a tree
        dTuplesToRemove := combinations(d, toList(0..#allEdges-1), Replacement => false, Order => false);
        edgesKept := {};
        edgesRemoved := {};

        trees = for dTuple in dTuplesToRemove list (
            edgeIndices = toList(set(0..#allEdges - 1) - set(dTuple));
            edgesKept = allEdges_edgeIndices;
            edgesRemoved = allEdges_dTuple;

            reducedG := transpose(matrix(for e in edgesKept list(
                t := e#0;
                h := e#1;
                localE := toList(Q0:0);
                localE = replace(h,  1, localE);
                localE = replace(t, -1, localE);
                localE
            )));
            if numColumns(reducedG) > 1 then (
                notAnyCycles := not existsUnorientedCycle(reducedG);
                if isGraphConnected(reducedG) and notAnyCycles then (
                    edgeIndices
                ) else ( 
                    continue;
                )
            ) else (
                edgeIndices
            )
        );
    );
    trees
)
------------------------------------------------------------


------------------------------------------------------------
-- creates a basis for the |Q1|-|Q0|+1 dimensional subspace of R^|Q1| 
-- to write the associated flow polytopes in. 
basisForFlowPolytope = method()
basisForFlowPolytope (List, ToricQuiver) := Matrix => (ST, Q) -> (
    removedEdges := toList(set(0..#Q.Q1 - 1) - set(ST));
    es := ST | removedEdges;

    f := for i from 0 to #removedEdges - 1 list(
        edge := Q.Q1_removedEdges#i;
        edgeList := ST | {removedEdges#i};
        cycle := primalUndirectedCycle(Q.Q1_ST | {edge});

        fi := #es:0;
        for j in cycle do (
            if j >= 0 then (
                fi = replace(edgeList#j, 1, fi)
            ) else (
                k := -(1 + j);
                fi = replace(edgeList#k, -1, fi)
            );
        );
        fi
    );
    output := for j from 0 to #es - 1 list(
        for i from 0 to #removedEdges - 1 list(
            ff := f#i;
            ff#j
        )
    );
    matrix output
)
basisForFlowPolytope ToricQuiver := Matrix => Q -> (
    (sT, removedEdges) := spanningTree(Q.IncidenceMatrix);
    return basisForFlowPolytope(sT, Q)
)
------------------------------------------------------------


------------------------------------------------------------
bipartiteQuiver = {Flow => "Canonical", Height => 10} >> opts -> (a, b) -> (
    if instance(opts.Flow, List) then (
        if #opts.Flow != a*b then (
            print("error: provided flow is not correct length.");
            return;
        );
        toricQuiver(flatten(for ai from 0 to a - 1 list(for bi from 0 to b - 1 list({ai, a+bi}))), opts.Flow)
    ) else (
        toricQuiver(flatten(for ai from 0 to a - 1 list(for bi from 0 to b - 1 list({ai, a+bi}))), Flow => opts.Flow, Height => opts.Height)
    )
)
------------------------------------------------------------


------------------------------------------------------------
chainQuiver = {Flow => "Canonical", Height => 10} >> opts -> (numEdges) -> (
    Es := flatten for v from 0 to #numEdges - 1 list(
        numEs := numEdges#v;
        for j from 1 to numEs list({v, v+1})
    );
    if instance(opts.Flow, List) then (
        if #opts.Flow != sum(numEdges) then (
            print("error: provided flow is not correct length.");
            return;
        );
        return toricQuiver(Es, opts.Flow)
    ) else (
        return toricQuiver(Es, Flow => opts.Flow, Height => opts.Height)
    )
)
------------------------------------------------------------


------------------------------------------------------------
coneSystem = Q -> (
    STs := allSpanningTrees(Q);
    allArrows := entries transpose Q.IncidenceMatrix;
    QDim := #Q.Q1 - #Q.Q0 + 1;
    coneDim := #Q.Q0 - 1;

    -- create list of cones CT for each tree T
    treeChambers := unique for T in STs list(transpose matrix allArrows_T);
    treeChambers = for T in treeChambers list(coneFromVData T);

    if #treeChambers > 1 then (
        -- find all pairs of cones with full-dimensional intersection
        aij := for i in 0..#treeChambers-1 list(
            tci := treeChambers#i;
            for j in i+1..#treeChambers-1 list(

                tcj := treeChambers#j;
                irs := intersection(tci, tcj);
                if dim irs >= coneDim then (j) else (continue;)
            )
        );

        -- now add each cone CT to the list of admissible subcones 
        -- before adding every possible intersection to the list as well.
        ssets := for t in treeChambers list(rays t);
        addedTo := unique treeChambers; -- keep track of cones that have already been added
        lastList := for tIdx in 0..#treeChambers-1 list(t:=treeChambers#tIdx; {t, tIdx});
        -- generate all possible intersections of cones
        for ctr in 0..#STs-1 do(
            -- stopping condition: if all intersections tried in the latest loop iteration are up lower-dim
            allEmpty := true; 

            -- create a list of intersecting pairs from intersecting treeChamber elements with lastList elements
            currentList := flatten for LE in lastList list(
                i := round LE#1; -- make index an int
                TI := LE#0; -- current cone
                if toString aij#i != "null" then ( -- loop through all cones that intersect with cone i nontrivially
                    for j in aij#i list(
                        TJ := treeChambers#j;
                        TIJ := intersection(TI, TJ);
                        if (dim TIJ >= coneDim) then (
                            allEmpty = false;
                            if not isIn(TIJ, addedTo) then (
                                addedTo = addedTo | {TIJ};
                                {TIJ, j}
                            ) else (continue;)
                        ) else (continue;)
                    )
                ) else (continue;)
            );
            if allEmpty then (
               break;
            ) else (
               lastList = currentList;
               ssets = ssets | apply(lastList, x -> rays x#0);
            );
        );

        -- now take the finest (in terms of containment) subsets to partition the cone system
        finestSubsets := for i in 0..#ssets-1 list(
            ssi := coneFromVData ssets#i;
            containsSomething := false;

            for j in 0..#ssets-1 do(
                if not (i == j) then (
                    ssj := coneFromVData ssets#j;
                    if contains(ssi, ssj) then (
                        containsSomething = true;
                    )
                );
            );
            if not containsSomething then (
                ssi
            ) else (continue;)
        );
        finestSubsets
    ) else (
        rays first treeChambers
    )
)
------------------------------------------------------------


------------------------------------------------------------
flowPolytopeVertices = method(Options => {Format => "SimplifiedBasis"})
flowPolytopeVertices(List, ToricQuiver) := opts -> (th, Q) -> (
    if #th != numRows Q.IncidenceMatrix then (
        print("error: the provided weight is in incorrect dimension");
        return {};
    );
    -- vertices of flow polytope correspond to regular flows on spanning trees
    allTrees := allSpanningTrees(Q);
    regularFlows := unique for x in allTrees list(
        if all(incInverse(th, Q^x), y -> y >= 0) then (
            incInverse(th, Q^x)
        ) else (continue;)
    );
    if #regularFlows < 1 then (
        {}
    ) else (
        -- simplified basis option reduces the dimension to what is strictly necessary.
        -- Recall we can represent the polytope in a lower dimensional subspace of R^Q1, 
        -- since the polytope has dimension |Q1|-|Q0|+1 <= |Q1|
        if instance(opts.Format, String) and (toString(opts.Format) != "SimplifiedBasis") then (
            regularFlows
        ) else (

            -- first generate a basis (ether user-specified or generated from first spanning tree)
            fpb := {};
            if instance(opts.Format, List) then ( -- if Format is a spanning tree
                fpb = basisForFlowPolytope(opts.Format, toricQuiver(Q, incInverse(th, Q)));
            ) else ( -- if Format is the string "SimplifiedBasis" 
                fpb = basisForFlowPolytope(toricQuiver(Q, incInverse(th, Q)));
            );

            -- translate regularFlows to contain origin by subtracting of first of them from all
            kerF := flatten for f in regularFlows list(
                ff := f - regularFlows#0;
                entries transpose solve(fpb, matrix(for fff in ff list ({round fff})))
            );

            -- translate interior point to origin(if interior lattice point exists)
            ip := interiorLatticePoints convexHull transpose matrix kerF;
            if #ip > 0 then (
                ip = first entries transpose first ip;
                for e in kerF list(e - ip)
            ) else (
                kerF
            )
        )
    )
)
flowPolytopeVertices ToricQuiver := opts -> Q -> (
    flowPolytopeVertices(Q.weights, Q, Format => opts.Format)
)
------------------------------------------------------------


------------------------------------------------------------
-- preimage of the weight th under the incidence map. 
-- NOTE: This function assumes that th is indeed a weight
incInverse = method()
incInverse(List, ToricQuiver) := List => (th, tQ) -> (
    nonzeroFlows := for t in tQ.flow list(if t != 0 then 1 else 0);
    a := tQ.IncidenceMatrix;
    a = a * diagonalMatrix(nonzeroFlows);
    b := matrix(for t in th list {floor t}) **QQ;
    F := solve(a **QQ, b);
    flatten entries F
)
------------------------------------------------------------


------------------------------------------------------------
isAcyclic = method()
isAcyclic(Matrix) := Boolean => Q -> (
    not existsOrientedCycle(Q)
)
isAcyclic(ToricQuiver) := Boolean => Q -> (
    not existsOrientedCycle(Q.IncidenceMatrix)
)
------------------------------------------------------------


------------------------------------------------------------
isClosedUnderArrows = method()
isClosedUnderArrows (Matrix, List) := Boolean => (Q, V) -> (
    Qt := transpose(Q);
    sQ := entries(Qt_V);
    all(sumList(sQ, Axis => "Row"), x -> x >= 0)
)
isClosedUnderArrows (List, Matrix) := Boolean => (V, Q) -> (
    isClosedUnderArrows(Q, V)
)
isClosedUnderArrows (List, ToricQuiver) := Boolean => (V, Q) -> (
    isClosedUnderArrows(Q.IncidenceMatrix, V)
)
isClosedUnderArrows (Matrix, ToricQuiver) := Boolean => (SQ, Q) -> (
    SQM := entries transpose SQ;
    V := positions(SQM, x -> all(x, y-> y != 0));
    isClosedUnderArrows(Q.IncidenceMatrix, V)
)
isClosedUnderArrows (ToricQuiver, ToricQuiver) := Boolean => (SQ, Q) -> (
    SQM := entries (SQ.IncidenceMatrix*diagonalMatrix(SQ.flow));
    V := positions(SQM, x -> any(x, y -> y != 0));
    isClosedUnderArrows(Q.IncidenceMatrix, V)
)
------------------------------------------------------------


------------------------------------------------------------
isSemistable = method()
isSemistable(List, ToricQuiver) := Boolean => (subQ, Q) -> (
    Qcm := Q.IncidenceMatrix;

    -- get the vertices in the subquiver
    subQVertices := positions(entries(Qcm_subQ), x -> any(x, y -> y != 0));

    -- weights of the original quiver
    Qweights := Q.weights;

    -- inherited weights on the subquiver
    weights := Qweights_subQVertices;

    -- negative weights in Q_0 \ subQ_0
    otherVertices := toList(set(0..#Qweights - 1) - set(subQVertices));
    minWeight := sum(apply({0} | toList(Qweights_otherVertices), x -> if(x <= 0) then x else 0));

    subMat := Qcm_subQ;
    tSubMat := transpose(subMat);
    subMat = transpose(tSubMat_subQVertices);

    sums := for subset in subsetsClosedUnderArrows(subMat) list(sumList(weights_subset));
    all(sums, x -> x + minWeight >= 0)
)
isSemistable(ToricQuiver, ToricQuiver) := Boolean => (subQ, Q) -> (
    nonZeroEntries := positions(subQ.flow, x -> (x > 0) or (x < 0));
    isSemistable(nonZeroEntries, Q)
)
------------------------------------------------------------


------------------------------------------------------------
isStable = method()
isStable(List, ToricQuiver) := Boolean => (subQ, Q) -> (
    Qcm := Q.IncidenceMatrix;

    -- get the vertices in the subquiver
    subQVertices := positions(entries(Qcm_subQ), x -> any(x, y -> y != 0));

    -- weights of the original quiver
    Qweights := Q.weights;

    -- inherited weights on the subquiver
    weights := Qweights_subQVertices;

    -- negative weights in Q_0 \ subQ_0
    otherVertices := toList(set(0..#Qweights - 1) - set(subQVertices));
    minWeight := sum(apply({0} | toList(Qweights_otherVertices), x -> if(x <= 0) then x else 0));

    subMat := Qcm_subQ;
    tSubMat := transpose(subMat);
    subMat = transpose(tSubMat_subQVertices);

    sums := for subset in subsetsClosedUnderArrows(subMat) list(sumList(weights_subset));
    all(sums, x -> x + minWeight > 0)
)
isStable(ToricQuiver, ToricQuiver) := Boolean => (subQ, Q) -> (
    nonZeroEntries := positions(subQ.flow, x -> (x > 0) or (x < 0));
    isStable(nonZeroEntries, Q)
)
------------------------------------------------------------


------------------------------------------------------------
isTight = method(Options => {Format => "Flow"})
isTight(ToricQuiver) := Boolean => opts -> Q -> (
    numArrows := #Q#Q1;
    maxUnstSubs := maximalUnstableSubquivers(Q, ReturnSingletons => true);
    if numArrows > 1 then (
        all(maxUnstSubs#NonSingletons, x -> #x != (numArrows - 1))
    ) else (
        #maxUnstSubs#Singletons < 1
    )
)
isTight(ToricQuiver, List) := Boolean => opts -> (Q, F) -> (
    if opts.Format == "Flow" then (
        isTight(toricQuiver(Q.IncidenceMatrix, F))
    ) else (
        FF := incInverse(F, Q);
        isTight(toricQuiver(Q.IncidenceMatrix, FF))
    )
)
isTight(List, ToricQuiver) := Boolean => opts -> (F, Q) -> (
    if opts.Format == "Flow" then (
        isTight(toricQuiver(Q.IncidenceMatrix, F))
    ) else (
        FF := incInverse(F, Q);
        isTight(toricQuiver(Q.IncidenceMatrix, FF))
    )
)
------------------------------------------------------------


------------------------------------------------------------
isWellDefined(ToricQuiver) := Boolean => Q -> (
    -- check that the vertices are consecutive
    vertexRange := toList(0..#Q.Q0 - 1),
    if any(Q.Q0, x -> not isIn(x, vertexRange)) then (
        print("error: Toric Quiver is missing vertices");
        return false;
    );
    -- check that each edge is defined in terms of two vertices
    if any(Q.Q1, e -> (#e < 2) or (not (isIn(e#0, vertexRange) and isIn(e#1, vertexRange)))) then (
        print("error: edge is has invalid endpoint");
        return false;
    );
    -- check that the flow has the same dimension as the number of edges
    if #Q.flow != #Q.Q1 then (
        print("error: the flow of the Toric Quiver has incorrect dimension.");
        return false;
    );
    if #Q.flow != numgens source Q.IncidenceMatrix then (
        print("error: the flow of the Toric Quiver has incorrect dimension.");
        return false;
    );
    -- check that the weights have the same dimension as the number of vertices
    if #Q.weights != #Q.Q0 then (
        print("error: the weights of the Toric Quiver have incorrect dimension.");
        return false;
    );
    if #Q.weights != #entries Q.IncidenceMatrix then (
        print("error: the weights of the Toric Quiver have incorrect dimension.");
        return false;
    );
    -- check that the weights match the flow and incidence matrix
    computedWeights := sumList(entries(Q.IncidenceMatrix*diagonalMatrix(Q.flow)), Axis => "Row");
    if any(0..#Q.weights - 1, x -> computedWeights#x != Q.weights#x) then (
        print("error: Toric Quiver weights do not match its flow and incidence matrix");
        return false;
    );
    -- check that the incidence matrix is an actual incidence matrix
    Qp := matrix(for e in entries(Q.IncidenceMatrix) list(for x in e list abs x));
    if any(sum entries Qp, x -> x != 2) then (
        print("error: Matrix is not a graph incidence matrix");
        return false;
    );
    if any(sum entries Q.IncidenceMatrix, x -> x != 0) then (
        print("error: Matrix is not a graph incidence matrix");
        return false;
    );
    return true;
)
------------------------------------------------------------


------------------------------------------------------------
makeTight = (W, Q) -> (
    potentialF := incInverse(W, Q);
    k := entries generators kernel Q.IncidenceMatrix;
    potentialF = potentialF + flatten entries transpose(matrix({sumList(k, Axis => "Row")}));

    -- this function calls itself recursively until a tight quiver is produced
    if isTight(Q, potentialF) then (
        return toricQuiver(Q.IncidenceMatrix, potentialF);
    ) else (
        if (#stableTrees(W, Q) < 1) then (
            print("Error: provided weight W is not in C(Q) and so does not admit a tight toric quiver");
            return ;
        );

        -- find a maximal unstable subquiver, called R, of codimension 1 (this exists if not tight)
        Qcm := graphFromEdges(Q.Q1, Oriented => true)*diagonalMatrix(potentialF);
        maxUnstSubs := maximalUnstableSubquivers(toricQuiver(Q.IncidenceMatrix, potentialF), ReturnSingletons => true);
        R := first(maxUnstSubs#NonSingletons);
        Rvertices := toList set flatten Q.Q1_R;
        S := {};

        -- generate a connected subset of R0 that is closed under arrows and has weight sum <= 0
        if #R < 1 then (
            -- this is for the case of quivers with 1 arrow or similar
            Rvertices = first(maxUnstSubs#Singletons);
            S = Rvertices;
        ) else (
            success := false;
            for i from 1 to #Rvertices - 1 do (
                combs := combinations(#Rvertices - i, Rvertices, Replacement => false, Order => false);
                for c in combs do (
                    if sumList(W_c) <= 0 then (
                        if isClosedUnderArrows(c, Q_R) then (
                            success = true;
                            S = c;
                            break;
                        );
                    );
                    if success then break;
                );
                if success then break;
            );
        );

        -- alpha is an arrow in Q1 with head in R and tail in Q\R
        alpha := first toList (set(0..#Q.Q1-1) - set(R));
        a := sort(Q.Q1_alpha);
        {aMinus, aPlus} := (a_0, a_1);

        -- contract the arrow alpha to create a new quiver
        newRows := entries(Q.IncidenceMatrix);
        newCols := drop(toList(0..#Q.Q1 - 1), {alpha, alpha});
        newM := matrix(for e in Q.Q0 list(
            if e == aMinus then (
                nRs := sumList(Q.IncidenceMatrix^{aPlus, aMinus}, Axis => "Col");
                nRs_newCols
            ) else if e == aPlus then (
                continue;
            ) else (
                nR := newRows_e;
                nR_newCols
            )
        ));
        newFlow := drop(potentialF, {alpha, alpha});
        newQ := toricQuiver(newM);
        newW := getWeights(newQ.IncidenceMatrix*diagonalMatrix(newFlow));

	nonEmptyEdges := for i in 0..#newQ.Q1 - 1 list (
		e := newQ.Q1#i;
		if toString e#0 == "null" then (
			continue;
		) else (
			i
		)
	);
        return makeTight(newW, newQ_nonEmptyEdges);
    );
)
------------------------------------------------------------


------------------------------------------------------------
maxCodimensionUnstable = method()
maxCodimensionUnstable ToricQuiver := Number => (Q) -> (
    numArrows := #Q.Q1;
    maxUnstables := maximalUnstableSubquivers(Q, ReturnSingletons => true);
    if #(maxUnstables#Singletons) > 0 then (
        #Q.Q1
    ) else (
        maxUnstables = maxUnstables#NonSingletons;
        k := max(
            for sQ in maxUnstables list(
                numArrows - #sQ
            )
        );
        k
    )
)
------------------------------------------------------------


------------------------------------------------------------
maximalNonstableSubquivers = {Format => "list", ReturnSingletons => false} >> opts -> (Q) -> (
    NonstableList := nonStableSubquivers(Q, Format => "list");

    withArrows := for subQ1 in NonstableList.NonSingletons list (
        IsMaximal := true;
        for subQ2 in NonstableList#NonSingletons do (
            if isProperSubset(subQ1, subQ2) then (
                IsMaximal = false;
            );
        );
        if IsMaximal then (
            if (opts.Format == "list") then (
                subQ1) else (
                Q^subQ1
            )
        ) else (continue;)
    );
    if opts.ReturnSingletons then (
        containedSingletons := flatten for subQ1 in NonstableList#NonSingletons list (
            for x in Q.Q1_subQ1 list ({x})
        );
        withoutArrows := toList(set(NonstableList#Singletons) - set(containedSingletons));

        hashTable {NonSingletons => withArrows, Singletons => withoutArrows}
    ) else (
        hashTable {NonSingletons => withArrows}
    )
)
------------------------------------------------------------


------------------------------------------------------------
maximalUnstableSubquivers = {Format => "list", ReturnSingletons => false} >> opts -> (Q) -> (
    unstableList := unstableSubquivers(Q, Format => "list");

    withArrows := for subQ1 in unstableList.NonSingletons list (
        IsMaximal := true;
        for subQ2 in unstableList#NonSingletons do (
            if isProperSubset(subQ1, subQ2) then (
                IsMaximal = false;
            );
        );
        if IsMaximal then (
            if (opts.Format == "list") then (
                subQ1) else (
                Q^subQ1
            )
        ) else (continue;)
    );
    if opts.ReturnSingletons then (
        containedSingletons := flatten for subQ1 in unstableList#NonSingletons list (
            for x in Q.Q1_subQ1 list ({x})
        );
        withoutArrows := toList(set(unstableList#Singletons) - set(containedSingletons));
    
        hashTable {NonSingletons => withArrows, Singletons => withoutArrows}
    ) else (
        hashTable {NonSingletons => withArrows}
    )
)
------------------------------------------------------------


------------------------------------------------------------
mergeOnArrow = method()
mergeOnArrow(Matrix, ZZ, Matrix, ZZ) := ToricQuiver => (Q1, a1, Q2, a2) -> (
    if (a1 >= numColumns(Q1)) then {
      error "invalid arrow value provided";
    };
    if (a2 >= numColumns(Q2)) then {
      error "invalid arrow value provided";
    };
    Q1nr := numRows(Q1);
    Q2nr := numRows(Q2);
    Q1nc := numColumns(Q1);
    Q2nc := numColumns(Q2);
    nrow := Q1nr + Q2nr - 2;
    ncol := Q1nc + Q2nc - 1;

    q1E := toList(graphEdges(Q1, Oriented => true))_a1;
    q2E := toList(graphEdges(Q2, Oriented => true))_a2;

    c1 := toList(join(drop(0..Q1nc - 1, {a1, a1}), {a1}));
    c2 := toList(drop(0..Q2nc - 1, {a2, a2}));

    r1 := toList(join(toList(set(0..Q1nr - 1) - set(q1E)), q1E));
    r2 := toList(join(q2E, toList(set(0..Q2nr - 1) - set(q2E))));

    Q1 = entries(Q1^r1)_c1;
    Q2 = entries(Q2^r2)_c2;

    paddingSize := 0;
    toricQuiver matrix(
        for row from 0 to nrow - 1 list(
            if row < (Q1nr - 2) then (
                paddingSize = Q2nc - 1;
                join(Q1_row, toList(paddingSize:0))

            ) else if row < Q1nr then (
                join(Q1_row, Q2_(2 + row - Q1nr))
            ) else (
                j := (row - Q1nr) + 2;
                paddingSize = Q1nc;
                toList(join(paddingSize:0, Q2_j))
            )
        )
    )
)
mergeOnArrow(ToricQuiver, ZZ, Matrix, ZZ) := ToricQuiver => (Q1, a1, Q2, a2) -> (
    mergeOnArrow(Q1.IncidenceMatrix, a1, Q2, a2)
)
mergeOnArrow(Matrix, ZZ, ToricQuiver, ZZ) := ToricQuiver => (Q1, a1, Q2, a2) -> (
    mergeOnArrow(Q1, a1, Q2.IncidenceMatrix, a2)
)
mergeOnArrow(ToricQuiver, ZZ, ToricQuiver, ZZ) := ToricQuiver => (Q1, a1, Q2, a2) -> (
    mergeOnArrow(Q1.IncidenceMatrix, a1, Q2.IncidenceMatrix, a2)
)
------------------------------------------------------------


------------------------------------------------------------
mergeOnVertex = method()
mergeOnVertex(Matrix, ZZ, Matrix, ZZ) := ToricQuiver => (Q1, v1, Q2, v2) -> (
    if (v1 >= numRows(Q1)) then {
      error "invalid vertex value provided";
    };
    if (v2 >= numRows(Q2)) then {
      error "invalid vertex value provided";
    };
    nrow := numRows(Q1) + numRows(Q2) - 1;
    ncol := numColumns(Q1) + numColumns(Q2);
    Q1rs := numRows(Q1);
    Q1cs := numColumns(Q1);
    Q2cs := numColumns(Q2);

    i1 := toList(join(drop(0..numRows(Q1) - 1, {v1, v1}), {v1}));
    i2 := toList(join({v2}, drop(0..numRows(Q2) - 1, {v2, v2})));

    Q1 = entries(Q1^i1);
    Q2 = entries(Q2^i2);

    paddingSize := ncol - Q1cs;
    r := 0;
    toricQuiver matrix(
        for row in (0..nrow - 1) list(
            if row < (Q1rs - 1) then (
                Q1_row | toList(paddingSize:0)
            ) else if (row < Q1rs) then (
                Q1_row | Q2_0
            ) else (
                r = row - (Q1rs - 1);
                paddingSize = ncol - Q2cs;
                toList(paddingSize:0) | Q2_r 
            )
        )
    )
)
mergeOnVertex(ToricQuiver, ZZ, Matrix, ZZ) := ToricQuiver => (Q1, v1, Q2, v2) -> (
    mergeOnVertex(Q1.IncidenceMatrix, v1, Q2, v2)
)
mergeOnVertex(Matrix, ZZ, ToricQuiver, ZZ) := ToricQuiver => (Q1, v1, Q2, v2) -> (
    mergeOnVertex(Q1, v1, Q2.IncidenceMatrix, v2)
)
mergeOnVertex(ToricQuiver, ZZ, ToricQuiver, ZZ) := ToricQuiver => (Q1, v1, Q2, v2) -> (
    mergeOnVertex(Q1.IncidenceMatrix, v1, Q2.IncidenceMatrix, v2)
)
------------------------------------------------------------


------------------------------------------------------------
potentialWalls = method()
potentialWalls(Matrix) := List => (Q) -> (
    nv := numRows(Q);
    nvSet := set(0..nv - 1);
    subs := (1..ceiling(nv/2));

    -- create list of possible Qminus
    Qms := flatten(for i from 1 to floor(nv/2) list (
        combinations(i, toList(nvSet), Replacement => false, Order => false)
    ));

    alreadyMet := set ();
    Qedges := graphEdges(Q, Oriented => true);

    for Qm in Qms list(
        if member(Qm, alreadyMet) then ( 
            continue;
        ) else (
            -- restrict to only vertices/edgesin Qm
            mSums := sumList(Q^Qm, Axis => "Col");
            connectsToQm := positions(entries transpose Q^Qm, x -> any(x, v -> v != 0));
            -- find the edges that have head and tail in Qm
            QmEdgeIndices := for s in connectsToQm list(if (mSums_s == 0) then (s) else (continue;));
            Qp := toList(nvSet - set(Qm));
            alreadyMet = alreadyMet + set ({Qp}) + set ({Qm});

            if isGraphConnected(Q^Qm_QmEdgeIndices) then (
                pSums := sumList(Q^Qp, Axis => "Col");
                connectsToQp := positions(entries transpose Q^Qp, x -> any(x, v -> v != 0));

                QpEdgeIndices := for s in connectsToQp list(if (pSums_s == 0) then (s) else (continue;));
                if (#Qp < 2) or (isGraphConnected(Q^Qp_QpEdgeIndices)) then (
                   new Wall from hashTable ({Qplus => Qp, WallType => wallType(Qp, Q)})
                ) else (continue;)
            ) else (continue;)
        )
    )
)
potentialWalls(ToricQuiver) := List => (Q) -> (
    potentialWalls(Q.IncidenceMatrix*diagonalMatrix(Q.flow))
)
------------------------------------------------------------


------------------------------------------------------------
primitiveArrows = Q -> (
    Es := Q.Q1;
    PAs := for i from 0 to #Es - 1 list(
        a := Es#i;
        (isCycle, cycle) := isPathBetween(a#0, a#1, drop(Es, {i, i}), Oriented => true, SavePath => true);
        if isCycle and (#set(cycle) > 1) then (
            continue;
        ) else (
            i
        )
    );
    return PAs
)
------------------------------------------------------------


------------------------------------------------------------
quiverIncidenceMatrix = method()
quiverIncidenceMatrix(ToricQuiver) := Matrix => Q -> (
    return Q.IncidenceMatrix
)
------------------------------------------------------------


------------------------------------------------------------
quiverEdges = method()
quiverEdges(ToricQuiver) := List => Q -> (
    return Q.Q1
)
------------------------------------------------------------


------------------------------------------------------------
quiverFlow = method()
quiverFlow(ToricQuiver) := List => Q -> (
    return Q.flow
)
------------------------------------------------------------


------------------------------------------------------------
quiverVertices = method()
quiverVertices(ToricQuiver) := List => Q -> (
    return Q.Q0
)
------------------------------------------------------------


------------------------------------------------------------
quiverWeights = method()
quiverWeights(ToricQuiver) := List => Q -> (
    return Q.weights
)
------------------------------------------------------------


------------------------------------------------------------
-- this routine routines an interior point for each 
-- chamber of the coneSystem QCS associated to a toric quiver Q
referenceThetas = method()
referenceThetas(List) := List => QCS -> (
    for c in QCS list(flatten entries interiorVector c)
)
------------------------------------------------------------


------------------------------------------------------------
-- this routine routines an interior point for each 
-- chamber of the coneSystem QCS associated to a toric quiver Q
referenceWeights = method()
referenceWeights(List) := List => QCS -> (
    for c in QCS list(flatten entries interiorVector c)
)
------------------------------------------------------------


------------------------------------------------------------
-- this function checks if the weights theta1 and theta2 
-- belong to the same polytope in the wall chamber decomposition for Q
samePolytope = method()
samePolytope(List, List, ToricQuiver) := Boolean => (theta1, theta2, Q) -> (
    treesTheta1 := stableTrees(theta1, Q);
    treesTheta2 := stableTrees(theta2, Q);
    if (#treesTheta1 < 1) then (
        if (#treesTheta2 < 1) then (
            return true
        ) else (
            return false
        );
    ) else if (#treesTheta2 < 1) then (
         return false
    ) else if all(0..#treesTheta1 - 1, x -> treesTheta1#x == treesTheta2#x) then (
        return true
    ) else (
        p1 := flowPolytopeVertices(theta1, Q);
        p2 := flowPolytopeVertices(theta2, Q);
        if areIsomorphic(convexHull transpose matrix p1, convexHull transpose matrix p2) then (
            return true
        ) else (
            error "cannot be determined";
        )
    )
)
------------------------------------------------------------


------------------------------------------------------------
-- this function lists all of the spanning trees T of TQ
-- such that T admits a regular flow in the preimage of weight th
stableTrees = (th, TQ) -> (
    allTrees := allSpanningTrees(TQ);
    for x in allTrees list(if all(incInverse(th, TQ_x), y -> y > 0) then sort(x) else continue )
)
------------------------------------------------------------


------------------------------------------------------------
-- yield the subquivers of a given quiver Q
subquivers = method(Options => {Format => "quiver", AsSubquiver => false})
subquivers Matrix := opts -> Q -> (
    numArrows := numColumns(Q);
    arrows := 0..(numArrows - 1);
    QFlow := 0.5*sumList(for x in entries(Q) list(for y in x list(abs(y))), Axis => "Col");

    flatten(
        for i from 1 to numArrows - 1 list (
            for c in combinations(i, arrows, Order => false, Replacement => false) list (
                if opts.Format == "list" then (
                    c
                ) else (
                    if opts.AsSubquiver then (
                        toricQuiver(Q, QFlow)^c
                    ) else (
                        toricQuiver(Q)_c
                    )
                )
            )
        )
    )
)
subquivers ToricQuiver := opts -> Q -> (
    numArrows := #Q.Q1;
    arrows := 0..(numArrows - 1);

    flatten(
        for i from 1 to numArrows - 1 list (
            for c in combinations(i, arrows, Order => false, Replacement => false) list (
                if opts.Format == "list" then (
                    c
                ) else (
                    if opts.AsSubquiver then (
                        Q^c
                    ) else (
                        Q_c
                    )
                )
            )
        )
    )
)
------------------------------------------------------------


------------------------------------------------------------
-- return ordered list of the weights for the vertices of quiver Q
getWeights = method()
getWeights(ToricQuiver) := List => Q -> (
    return Q.weights
)
getWeights(Matrix) := List => Q -> (
    return sumList(entries(Q), Axis => "Row")
)
------------------------------------------------------------


------------------------------------------------------------
threeVertexQuiver = method(Options => {Flow => "Canonical", Height => 10})
threeVertexQuiver(List) := ToricQuiver => opts -> (numEdges) -> (
    if #numEdges != 3 then (
        print("error: need a list of 3 numbers, denoting the number of edges between each pair of vertices");
        return;
    );
    Es0 := for i from 0 to numEdges#0 - 1 list({0, 1});
    Es1 := for i from 0 to numEdges#1 - 1 list({1, 2});
    Es2 := for i from 0 to numEdges#2 - 1 list({0, 2});
    Es := Es0 | Es2 | Es1;

    if instance(opts.Flow, List) then (
        if #opts.Flow != sum(numEdges) then (
            print("error: provided flow is not correct length.");
            return;
        );
        return toricQuiver(Es, opts.Flow)
    ) else (
        return toricQuiver(Es, Flow => opts.Flow, Height => opts.Height)
    )
)
------------------------------------------------------------


------------------------------------------------------------
wallQPlus = method()
wallQPlus(Wall) := Sequence => W -> (
    return W.Qplus
)
------------------------------------------------------------



------------------------------------------------------------
wallType = method()
wallType(List, Matrix) := Sequence => (Qp, Q) -> (
    tp := number(sumList(Q^Qp, Axis => "Col"), x -> x < 0);
    tm := number(sumList(Q^Qp, Axis => "Col"), x -> x > 0);
    return (tp, tm)
)
wallType(List, ToricQuiver) := Sequence => (Qp, Q) -> (
    return wallType(Qp, Q.IncidenceMatrix*diagonalMatrix(Q.flow))
)
wallType(Wall) := Sequence => W -> (
    return W.WallType
)
------------------------------------------------------------




------------------------------------------------------------
-- NON-EXPORTED AUXILIARY FUNCTIONS
------------------------------------------------------------


------------------------------------------------------------
adjacencyToIncidence = (A) -> (
    E := for i in (0..numRows(A) - 1) list(for j in (0..numColumns(A) - 1) list(if A_{j}^{i} != 0 then (i, j)));
    matrix(graphFromEdges(E), Oriented => true)

)

-- cast object x as a list, even when x is not an iterable
asList = x -> (
    if instance(x, List) then(
        return x
    )
    else if instance(x, Sequence) then(
        return toList(x)
    )
    else if instance(x, Set) then(
        return toList(x)
    )
    else
        return {x}
)

-- take all possible combinations of length k from list l -- 
-- optional arguments: 
-- -- Replacement(true/false) = with replacement
-- -- Order(true/false) = whether or not the ordering of combination values matters
combinations = {Replacement => true, Order => true} >> opts -> (k, l) -> (
    combs := {};
    combs1 := {};
    combs2 := {};
    if k > 1 then (
        -- if we are using combinations with replacement -- 
        if opts.Replacement then (
           combs = flatten(join(for i in l list(for j from 0 to k - 1 list(i))));
           combs1 = unique(subsets(combs, k));
           combs2 = unique(subsets(combs, k));
           for i in combs2 do (combs1 = append(combs1, reverse(i)));
        )
        else (
           combs = flatten(for i in l list(i));
           combs1 = unique(subsets(combs, k));
           combs2 = unique(subsets(combs, k));
           for i in combs2 do (combs1 = append(combs1, reverse(i)));
        );
    )
    else combs1 = for i in l list(asList(i));

    if opts.Order != true then (
        combs = unique(
            for i in combs1 list(sort(i))
        );
    ) else (
        combs = unique(flatten for c in combs1 list permutations(c));
    );
    combs
)

-- yield the edges (from a list of edges of the form {{v0,v1},..{vi, vj}}) that adjoin point p
edgesOutOfPoint = {Oriented => false} >> opts -> (p, E) -> (
    if opts.Oriented then (
        for i from 0 to #E - 1 list(e := E#i; if p != e#0 then (continue;) else (i, e))
    )
    else (
        for i from 0 to #E - 1 list(e := E#i; if (number(e, j -> j == p) < 1) then (continue;) else (i, e))
    )
)

-- check if there exists a cycle in a (possibly unconnected)
-- oriented graph, passed in matrix form. 
existsOrientedCycle = (G) -> (
    retVal := false;
    E := graphEdges(G, Oriented => true);
    V := toList(0..numRows(G)-1);
    for firstV in V do (
        visited := replace(firstV, 1, toList(#V:0));
        result := findCycleDFS(firstV, visited, E);
        if result then (
            retVal = true;
            break;
        )
    );
    retVal
)

existsUnorientedCycle = (G) -> (
    retVal := false;
    E := graphEdges(G, Oriented => false);
    for i from 0 to #E - 1 do (
        if isEdgeInCycle(i, E) then (
            retVal = true;
            break;
        );
    );
    retVal
)

-- DFS search to find cycle in directed graph:
findCycleDFS = (startV, visited, E) -> (
    retVal := false;
    edgesOut := edgesOutOfPoint(startV, E, Oriented => true);
    for edge in edgesOut do (
        currentVisited := toList(visited);
        edgeVerts := edge#1;
        endV := edgeVerts#1;
        if visited#endV == 1 then (
            retVal = true;
            break;
        );
        if retVal then break;
        currentVisited = replace(endV, 1, visited);
        retVal = findCycleDFS(endV, currentVisited, E);
    );
    retVal
)

-- yield the edges of a graph in the form of a list of pairs 
-- (v1, v2), where edge E is from v1 to v2
graphEdges = method(Options => {Oriented => false, RavelLoops => false});
graphEdges Matrix := List => opts -> (G) -> (
    E := {};
    if opts.Oriented == true then (
        E = for e in entries(transpose(G)) list(
            {position(e, i -> i < 0), 
             position(e, i -> i > 0)}
        );
    )
    else (
        E = for e in entries(transpose(G)) list(
            positions(e, i -> (i > 0 or i < 0))
        );
        if opts.RavelLoops == true then (
            E = for e in E list(if #e > 1 then e else toList(2:e#0));
        );
    );
    return E
)
graphEdges ToricQuiver := List => opts -> (G) -> (
    graphEdges(G.IncidenceMatrix, Oriented => opts.Oriented, RavelLoops => opts.RavelLoops)
)

graphVertices = method();
graphVertices Matrix := List => M -> (
    toList(0..numRows(M) - 1)
)
graphVertices ToricQuiver := List => Q -> (
    Q.Q0
)

-- yield the matrix rep of graph, given a list of edges as ordered 
-- pairs (this is the opposite of graphEdges() function. 
graphFromEdges = {Oriented => false} >> opts -> E -> (
    -- first, if oriented graph, then make sure this is reflected. 
    tailVal := 1;
    if opts.Oriented == true then (
        tailVal = -1;
    );

    nVerts := max(flatten(E))+1;
    cols := for i in E list(
        row := (nVerts:0);
        toList(replace(i#0, tailVal, replace(i#1, 1, row)))
    );
    transpose(matrix(cols))
)

-- checks if there is a cycle containing given edge. 
isEdgeInCycle = (i, E) -> (
    if #E > 1 then (
        e := E#i;
        if #e > 1 then (
            p := e#0;
            q := e#1;
        )
        else (
            p = e#0;
            q = e#0;
        );
        indicesToSave := drop(toList(0..(#E-1)), {i,i});
        isPathBetween(p, q, E_indicesToSave)
    )
    else (
        false
    )
)

-- check if graph is connected
isGraphConnected = G -> (
    gEdges := graphEdges(G, Oriented => false);
    gPoints := graphVertices(G);

    if (#gPoints < 2) then (
        true
    ) else (
        -- reorder the edges to list loops first
        lens := sortedIndices(for e in gEdges list(-#e));
        gEdges = gEdges_lens;

        -- check if there is a non-loop edge first
        if max(for e in gEdges list(#e)) > 1 then (
            isConnected(graph(gEdges))
        -- if there are only loops/empty edges
        ) else (
            false
        )
    )
)

isIn = (v, l) -> (
    p := positions(l, x -> x == v);
    #p > 0
)

isMaximal = method()
isMaximal(List, Matrix) := Boolean => (Qlist, Q) -> (
    returnVal := true;
    for Q2 in Qlist do (
        if isProperSubset(Q, Q2) then (
            returnVal = false;
        );
    );
    returnVal
)
isMaximal(List, ToricQuiver) := Boolean => (Qlist, Q) -> (
    Ms := for Qm in Qlist list(Qm.IncidenceMatrix);
    isMaximal(Ms, Q.IncidenceMatrix)
)

-- check if there exists a path between p and q by appending 
-- edges in E(which is a list of pairs (v1, v2). 
-- optional arguments: 
-- -- Oriented(true/false) = whether or not the graph should be oriented
-- -- SavePath(true/false) = whether or not to return the edges involved in the path
-- -- EdgesAdded(list) = internal mechanism for computing for SavePath
isPathBetween = {Oriented => false, SavePath => false, EdgesAdded => {}} >> opts -> (p, q, E) -> (
    ifPath := false;
    existsPath := false;
    currentEdges := {};
    pathsToSee := edgesOutOfPoint(p, E, Oriented => opts.Oriented);

    for edge in pathsToSee do (
        --- get the edge index and endpoints
        i := edge#0;
        e := edge#1;
        v := e#1;
        if p == e#1 then (
            v = e#0;
        );
        if opts.SavePath then (
            currentEdges = append(toList(opts.EdgesAdded), {p, v});
        );

        if q == v then (
            existsPath = true;
            break;
        )
        else (
            thisPath := {};
            remainingEdges := for j from 0 to #E - 1 list(if j == i then (continue;) else E#j);

            if opts.SavePath then (
                (ifPath, thisPath) = isPathBetween(v, q, remainingEdges, Oriented => opts.Oriented, SavePath => true, EdgesAdded => currentEdges);
            )
            else (
                ifPath = isPathBetween(v, q, remainingEdges, Oriented => opts.Oriented, EdgesAdded => currentEdges);
            );
            if ifPath then (
                existsPath = true;
                currentEdges = currentEdges | thisPath;
                break;
            );
        );
    );
    if opts.SavePath then (
        return (existsPath, currentEdges);
    )
    else (
        return existsPath;
    )
)

isProperSubset = (Q1, Q2) -> (
    S1 := set(Q1);
    S2 := set(Q2);
    if S1 === S2 then (
        false
    ) else (
        isSubset(S1, S2)
    )
)

-- gives the edges that comprise an undirected cycle in the graph G, 
-- (which is assumed to contain a single cycle) and returns the ordered cycle
--  input: G(list of tuples): edges of graph G
--  output: cycle(list of tuples): tuple representation of the edges contained in the cycle
primalUndirectedCycle = (G) -> (
    if existsUnorientedCycle(graphFromEdges(G)) then (
        for i from 0 to #G - 1 do (
            edge := G#i;
            (isCycle, cycle) := isPathBetween(edge#1, edge#0, drop(G, {i, i}), 
                                              Oriented => false, SavePath => true, EdgesAdded => {edge});
            if isCycle then (
                edgeIndices := {};
                metEdges := {};

                for cE in cycle do (
                    for gI in toList(0..#G - 1) do (
                        if isIn(gI, metEdges) then (
                            continue;
                        ) else (
                            gE := G#gI;
                            if (gE#0 == cE#0) and (gE#1 == cE#1) then (
                                metEdges = metEdges | {gI};
                                edgeIndices = edgeIndices | {gI};
                                break;
                            ) else if (gE#1 == cE#0) and (gE#0 == cE#1) then (
                                metEdges = metEdges | {gI};
                                edgeIndices = edgeIndices | {-(gI+1)};
                                break;
                            );
                        );
                    );
                );
                return edgeIndices;
            );
        );
        return {};
    ) else (
        return G;
    );
)


nonStableSubquivers = method(Options => {Format => "list"})
nonStableSubquivers(ToricQuiver) := opts -> Q -> (
    numArrows := #Q.Q1;
    arrows := toList(0..numArrows - 1);

    L := flatten(for i from 1 to numArrows - 1 list (
        combinations(numArrows - i, arrows, Replacement => false, Order => false) 
    ));

    sqsWithArrows := for sQ in L list(
        if not isStable(toList(sQ), Q) then (
            if (opts.Format == "list") then (
                sQ
            ) else (
                Q^sQ
            )
        ) else (continue;)
    );
    singletonUnstableSqs := for x in positions(Q.weights, x -> x <= 0) list ({x});

    return hashTable({NonSingletons => sqsWithArrows, Singletons => singletonUnstableSqs})
)

-- return the indices of the list l in order the values occur 
-- in the sorted list sort(l)
sortedIndices = (l) -> (
    sortedVals := unique(sort(l));
    flatten(for i in sortedVals list(positions(l, x -> x == i)))
)

-- Returns a spanning tree(the first one that is encountered) of 
-- the quiver Q with |Q_1| - |Q_0| + 1 edges removed. 
-- NOTE: if such a spanning tree is not possible, then it returns empty lists
--
-- input: 
--     - Q: Matrix representation of quiver
-- outputs:
--     - edges_kept(list of tuples): list of the edges in the spanning tree
--     - edges_removed(list of tuples)): list of the edges in the complement of the spanning tree
--
spanningTree = (Q) -> (
    Q0 := numRows(Q);
    Q1 := numColumns(Q);

    --  edges of quiver Q represented as a list of tuples
    allEdges := graphEdges(Q, Oriented => true);
    allNodes := toList(0..Q0-1);

    -- number of edges to remove from spanning tree
    d := Q1 - Q0 + 1;

    edgeIndices := {};
    if d > 0 then (
        dTuplesToRemove := combinations(d, toList(0..#allEdges-1), Replacement => false, Order => false);
        edgesKept := {};
        edgesRemoved := {};
        foundTree := false;

        for dTuple in dTuplesToRemove do (
            edgeIndices = toList(set(0..#allEdges - 1) - set(dTuple));
            edgesKept = allEdges_edgeIndices;
            edgesRemoved = allEdges_dTuple;

            reducedG := transpose(matrix(for e in edgesKept list(
                t := e#0;
                h := e#1;
                localE := toList(Q0:0);
                localE = replace(h,  1, localE);
                localE = replace(t, -1, localE);
                localE
            )));
            if numColumns(reducedG) > 1 then (
                notAnyCycles := not existsUnorientedCycle(reducedG);

                if isGraphConnected(reducedG) and notAnyCycles then (
                    foundTree = true;
                    break;
                );
            ) else (
                foundTree = true;
                break;
            );
        );
        if foundTree then (
            dTuple := toList(set(0..#allEdges - 1) - set(edgeIndices));
            return (edgeIndices, dTuple);
        ) else (
            return ({}, {});
        );
    ) else (
        return (allEdges, {});
    );
)

-- list the subsets of a quiver Q that are closed under arrows
subsetsClosedUnderArrows = method()
subsetsClosedUnderArrows Matrix := List => (Q) -> (
    currentVertices := 0..(numRows(Q) - 1);

    flatten(for i from 1 to #currentVertices - 1 list(
        for c in combinations(i, currentVertices, Order => false, Replacement => false) list(
            if isClosedUnderArrows(c, Q) then (
                c
            )
            else(continue;)
        )
    ))
)
subsetsClosedUnderArrows ToricQuiver := List => (Q) -> (
    return subsetsClosedUnderArrows(Q.IncidenceMatrix)
)

-- add all elements of a list x together, and specify Axis (row/col) if x is actually a matrix or list of lists -- 
sumList = {Axis => "None"} >> opts -> x -> (
    s := 0;
    if opts.Axis == "Row" then (
        s = flatten(for i in x list(sumList(i)));
    )
    else if opts.Axis == "Col" then (
       pivoted := entries(transpose(matrix(x)));
       s = flatten(for i in pivoted list(sumList(i)));
    )
    else (
        s = sum(toList(x));
    );
    return s
)

unstableSubquivers = method(Options => {Format => "list"})
unstableSubquivers(ToricQuiver) := opts -> Q -> (
    numArrows := #Q.Q1;
    arrows := toList(0..numArrows - 1);

    L := flatten(for i from 1 to numArrows - 1 list (
        combinations(numArrows - i, arrows, Replacement => false, Order => false) 
    ));

    sqsWithArrows := for sQ in L list(
        if not isSemistable(toList(sQ), Q) then (
            if (opts.Format == "list") then (
                sQ
            ) else (
                Q^sQ
            )
        ) else (continue;)
    );
    singletonUnstableSqs := for x in positions(Q.weights, x -> x < 0) list ({x});

    return hashTable({NonSingletons => sqsWithArrows, Singletons => singletonUnstableSqs})
)


------------------------------------------------------------
beginDocumentation()
multidoc ///
    Node
        Key 
            ThinSincereQuivers
        Headline
            creating and manipulating Toric Quivers
        Description
            Text
                {\em ThinSincereQuivers} is a package for creating and manipulating toric quivers.
            Text   
                For further details in the theory, we suggest the following articles and the references within them:
            Text
                @UL { 
                      {"Lutz Hille, ", HREF{"https://doi.org/10.1016/S0024-3795(02)00406-8", EM "Quivers, cones and polytopes, "}, "
                       Linear algebra and its applications 365 (2003): 215-237."},
                      {"Mátyás Domokos and  Dániel Joó, ", HREF{"https://arxiv.org/abs/1402.5096v1", 
                            EM "On the equations and classification of toric quiver varieties"},",  
                          Proceedings. Section A, Mathematics-The Royal Society of Edinburgh 146.2 (2016): 265."
                    }
                }@
            Text
                @SUBSECTION "Menu"@
            Text
                @UL {
                    {TO "Creating toric quivers"},
                    {TO "Creating subquiver representations"},
                }@
        Subnodes
            ToricQuiver
            "Creating toric quivers"
            "Creating subquiver representations"
            toricQuiver
            (symbol _, ToricQuiver, List)
            (symbol ^, ToricQuiver, List)
            (symbol ==, ToricQuiver, ToricQuiver)
            bipartiteQuiver
            chainQuiver
            threeVertexQuiver
            allSpanningTrees
            basisForFlowPolytope
            coneSystem
            flowPolytopeVertices
            incInverse
            isAcyclic
            isClosedUnderArrows
            isSemistable
            isStable
            isTight
            isWellDefined
            makeTight
            maxCodimensionUnstable
            maximalNonstableSubquivers
            maximalUnstableSubquivers
            mergeOnArrow
            mergeOnVertex
            potentialWalls
            primitiveArrows
            quiverIncidenceMatrix
            quiverEdges
            quiverFlow
            quiverVertices
            quiverWeights
            referenceThetas
            referenceWeights
            samePolytope
            stableTrees
            subquivers
            wallQPlus
            wallType
            getWeights
            AsSubquiver
            Flow
            ReturnSingletons
    Node
        Key
            ToricQuiver
        Headline
            the ToricQuiver datatype
        Description
            Text
                A toric quiver is an acyclic directed graph with edge set $Q_1$ and
                vertices $Q_0$, equipped with a flow, which is a vector of values
                associated to each edge of the graph, and a weight, which is a vector of
                values associated to each vertex in the graph.
                The weights are obtained from the flow and graph as the image of the incidence map, defined as follows:
                $$
                \text{inc}(\mathbf{w})(i)
                := \sum_{a\in Q_1 \atop a^{+} = i} \mathbf{w}(a)
                - \sum_{a\in Q_1 \atop a^{-} = i}
                \mathbf{w}(a) \quad \text{ for all } i \in Q_0.
                $$
                The ToricQuiver data type is a type of Hash Table with the following keys:
            Text
                @UL {
                    {TT "IncidenceMatrix:", "matrix representation of the connected graph underlying the quiver"},
                    {TT "flow:              ", "list of integers representing the flow associated to each edge of the quiver"},
                    {TT "Q0:                ", "the list of vertices"},
                    {TT "Q1:                ", "the list of edges "},
                    {TT "weights:           ", "the values on each vertex induced by the flow"},
                }@
        SeeAlso
            "toricQuiver"
            "bipartiteQuiver"
            "threeVertexQuiver"
            "chainQuiver"

    Node
        Key
            toricQuiver
            (toricQuiver, List)
            (toricQuiver, List, List)
            (toricQuiver, Matrix)
            (toricQuiver, Matrix, List)
            (toricQuiver, Graph)
            (toricQuiver, Graph, List)
            (toricQuiver, ToricQuiver)
            (toricQuiver, ToricQuiver, List)
            [toricQuiver, Flow]
            [toricQuiver, Height]
        Headline
            the toricQuiver constructor
        Usage
            Q = toricQuiver E
            Q = toricQuiver (E, F)
            Q = toricQuiver M
            Q = toricQuiver (M, F)
            Q = toricQuiver G
            Q = toricQuiver (G, F)
            Q = toricQuiver T
            Q = toricQuiver (T, F)
        Inputs
            E: List
                of pairs {\tt (V1, V2)} giving the edges of the quiver in terms of the vertices
            F: List 
                the flow on the quiver given as a list of integers
            G: Graph
            M: Matrix 
                of integers giving the connectivity structure of the quiver. If specified, this matrix should be a well-defined incidence matrix for the graph underlying the quiver
            T: ToricQuiver 
            Flow => String
                that specifies the flow for the polytope. 
                options are 
                {\tt Default}, which takes the flow from values in the matrix, 
                {\tt Canonical}, which sets the flow to 1 for each edge, and 
                {\tt Random}, which assigns a random integer between 0 and {\tt Height} to each edge
            Height => ZZ
                used in conjunction with the option {\tt Flow => Random} to set the upper limit of the random number generator
        Outputs
            Q: ToricQuiver
        Description
            Text
                A toric quiver is a directed graph {\tt Q=(Q_0, Q_1) } where 
                {\tt Q_0} is the set of vertices associated to {\tt Q} and {\tt Q_1} is the set of arrows. 
                Also included in $Q$ is a flow, which associates an integer value to each edge. 
                The canonical flow gives a weight of 1 to each edge. 
            Text
                The ToricQuiver data type is stored as a hash table with the following keys: 
            Text
                @UL {
                    {TT "IncidenceMatrix:", "matrix representation of the connected graph underlying the quiver"},
                    {TT "flow:              ", "list of integers representing the flow associated to each edge of the quiver"},
                    {TT "Q0:                ", "the list of vertices"},
                    {TT "Q1:                ", "the list of edges "},
                    {TT "weights:           ", "the values on each vertex induced by the flow"},
                }@

            Example
                -- create a toric quiver from matrix
                Q = toricQuiver matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}})
            Example
                -- create a toric quiver from matrix with specified flow
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), {3, 1, 0, 5})
            Example
                -- create a toric quiver from a list of edges
                Q = toricQuiver {{0,1},{0,1},{0,2},{0,2}}
            Example
                -- create a toric quiver from a list of edges and a flow
                Q = toricQuiver ({{0,1},{0,1},{0,2},{0,2}}, {1,2,3,4})
            Example
                -- create a toric quiver from a matrix with keyword flow
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), Flow => "Canonical")
            Example
                -- create a toric quiver from a matrix with random flow with values in the range [0, 10)
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random")
            Example
                -- create a toric quiver from a matrix with random flow with values in the range [0, 200)
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random", Height => 201)
            Example
                -- create a toric quiver copied from another one
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random");
                R = toricQuiver(Q)
            Example
                -- create a toric quiver copied from another, but with alternative flow
                R = toricQuiver(Q, {1,2,3,4})
        SeeAlso
            "bipartiteQuiver"
    Node
        Key
            "Creating toric quivers"
        Description
            Text
                Toric quivers are represented as a type of HashTable with the following keys:
            Text
                @UL{  
                    {TT "IncidenceMatrix: ","weighted incidence matrix giving the vertex-edge connectivity structure of $Q$"},
                    {TT "Q0: ","list of vertices"},
                    {TT "Q1: ","list of edges"},
                    {TT "flow: ","list of integers giving the flow on each edge"},
                    {TT "weights: ","induced weights on vertices given by the image of the flow"},
                }@

            Text
                One can generate the quiver {\tt Q} associated to the bipartite graph 
                {\tt K_{2,3}} with a random flow {\tt w} as follows:

            Example
                Q0 = {{0,2},{0,3},{0,4},{1,2},{1,3},{1,4}};
                Q = toricQuiver(Q0, Flow => "Random")
            Text
                Alternatively, one can construct a toric quiver using any of the following constructions:
            Text
                create a toric quiver from matrix
            Example
                Q = toricQuiver matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}})
            Text
                create a toric quiver from matrix with specified flow
            Example
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), {3, 1, 0, 5})
            Text
                create a toric quiver from a list of edges
            Example
                Q = toricQuiver {{0,1},{0,1},{0,2},{0,2}}
            Text
                create a toric quiver from a list of edges and a flow
            Example
                Q = toricQuiver ({{0,1},{0,1},{0,2},{0,2}}, {1,2,3,4})
            Text
                create a toric quiver from a matrix with keyword flow
            Example
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), Flow => "Canonical")
            Text
                create a toric quiver from a matrix with random flow with values in the range [0, 100]
            Example
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random", Height => 101)
            Text
                create a toric quiver copied from another one
            Example
                R = toricQuiver(Q)
            Text
                create a toric quiver copied from another, but with alternative flow
            Example
                R = toricQuiver(Q, {1,2,3,4})
    Node
        Key
            "Creating subquiver representations"
        Description
            Text
                A subquiver is a subgraph of a quiver. Specifically, it is formed by selecting a subset $I$
                of the arrows from the original quiver, ensuring that the tails and heads of the chosen arrows 
                align with the selected vertices. In this context, there are two ways to approach a subquiver. 
                One approach is to recall the original quiver and represent the subquiver as a subset of its arrows 
                and vertices, denoted as $Q^I$. The flow of the resultant quiver will be zero along the arrows not in $I$. 
                Alternatively, we can disregard the original quiver and focus solely on the arrows and vertices of the new subquiver, represented as $Q_I$. 
                The weight of the new quiver $Q_I$ is derived from the flows of the original quiver $Q$.  
            Text
                The two methods corresponding to these ideas are referenced in the examples below. 
            Example
                Q = bipartiteQuiver(2, 3);
                -- show the subquiver consisting of arross 0, 1 and 3, by removing all of the others
                SQ = Q_{0,1,3}
            Example
                Q = bipartiteQuiver(2,3);
                -- show the subquiver consisting of arross 0, 1 and 3, by setting flow to 0 on all other arrows 
                SQ = Q^{0,1,3}
        SeeAlso
            (symbol ^, ToricQuiver, List)
            (symbol _, ToricQuiver, List)
    Node
        Key
            (symbol _, ToricQuiver, List)
        Headline
            taking a subquiver by indexing
        Usage
            Q_L
        Inputs
            Q: ToricQuiver
            L: List
               of integers specifying which arrows to subset
        Outputs
            S: ToricQuiver
                the subquiver in question
        Description
            Text
                This method returns a the subquiver of the quiver {\tt Q} 
                that is made up of the arrows in the list {\tt L}. Note that 
                this method re-orders the subquiver labels to create a standalone quiver.
                To retain the original quiver labels on the subquiver, see @TO2 {(symbol ^, ToricQuiver, List), "(symbol ^, ToricQuiver, List)"}@. 
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- extract the subquiver consisting of the arrows with indices 0, 1, 3
                SQ = Q_{0,1,3}
        SeeAlso
            (symbol ^, ToricQuiver, List)
    Node
        Key
            (symbol ^, ToricQuiver, List)
        Headline
            taking a subquiver by indexing
        Usage
            Q^L
        Inputs
            Q: ToricQuiver
            L: List
               of integers specifying which arrows to subset
        Outputs
            S: ToricQuiver
                the subquiver in question
        Description
            Text
                This method returns a the subquiver of the quiver {\tt Q} 
                that is made up of the arrows in the list {\tt L}. 
            Example
                Q = bipartiteQuiver(2, 3);
                -- extract the subquiver consisting of the arrows with indices 0, 1, 3
                SQ = Q^{0,1,3}
        SeeAlso
            (symbol _, ToricQuiver, List)
    Node
        Key
            (symbol ==, ToricQuiver, ToricQuiver)
        Headline
            whether two toric quivers are equal.
        Usage
            Q1 == Q2
        Inputs
            Q1: ToricQuiver
            Q2: ToricQuiver
        Outputs
            B: Boolean
        Description
            Text
                This method takes two toric quivers and returns the 
                boolean of the statement {\tt Q1} is equal to {\tt Q2}. 
            Example
                Q = bipartiteQuiver(2, 3);
                R = bipartiteQuiver(2, 2);
                -- check if the two quivers are the same
                Q == R
            Example
                Q = toricQuiver {{0, 2}, {0, 3}, {1, 2}, {1, 3}};
                R = bipartiteQuiver(2, 2);
                -- check if the two quivers are the same
                Q == R
        SeeAlso
            toricQuiver
    Node
        Key
            bipartiteQuiver
            [bipartiteQuiver, Flow]
            [bipartiteQuiver, Height]
        Headline
            make a toric quiver on underlying bipartite graph
        Usage
            bipartiteQuiver (N, M)
        Inputs
            N: ZZ
                number of vertices that are sources
            M: ZZ
                number of vertices that are sinks
            Flow => 
                specify what form of flow to use. This input can be 
                either a string with values {\tt Canonical} or {\tt Random}, 
                or else a list of integer values. 
            Height => ZZ
                used in conjunction with the option {\tt Flow => Random} to set the upper limit of the random number generator
        Outputs
            Q: ToricQuiver
        Description
            Text
                This function creates the unique toric quiver whose underlying graph 
                is the complete bipartite graph with 
                {\tt N} source vertices and {\tt M} sink vertices.
            Example
                -- create a bipartite quiver with 2 sources and 3 sinks
                Q = bipartiteQuiver (2, 3)
            Example
                -- create a bipartite quiver with 2 sources and 3 sinks and a random flow
                Q = bipartiteQuiver (2, 3, Flow => "Random")
            Example
                -- create a bipartite quiver with 2 sources and 3 sinks and a specified flow
                Q = bipartiteQuiver (2, 3, Flow => {1, 2, 1, 3, 1, 4})
        SeeAlso
            toricQuiver
    Node
        Key
            chainQuiver
            [chainQuiver, Flow]
            [chainQuiver, Height]
        Headline
            make a toric quiver on underlying graph in the form of a chain
        Usage
            chainQuiver E
        Inputs
            E: List
                number of edges linking each vertex to the next
            Flow => 
                specify what form of flow to use. This input can be 
                either a string with values {\tt Canonical} or {\tt Random}, 
                or else a list of integer values. 
            Height => ZZ
                used in conjunction with the option {\tt Flow => Random} to set the upper limit of the random number generator
        Outputs
            Q: ToricQuiver
        Description
            Text
                A chain quiver is a quiver where each vertex {\tt N} is connected to
                its predecessor vertex {\tt N - 1} by arrows pointing from {\tt N - 1}
                to {\tt N}, and connected to its successor vertex {\tt N + 1} by arrows
                pointing to {\tt N + 1}.
                This results in a quiver in the shape of a chain, where each pair of vertices
                along the chain is connected by a variable number of edges.
            Example
                -- create a chain quiver with 3 vertices and 6 edges
                Q = chainQuiver {1,2,3}
            Example
                -- create a chain quiver with 3 vertices and 6 edges and a random flow
                Q = chainQuiver ({1,2,3}, Flow => "Random")
            Example
                -- create a chain quiver with 3 vertices and 6 edges and a specified flow
                Q = chainQuiver ({1,2,3}, Flow => {1, 2, 1, 3, 1, 4})
        SeeAlso
            toricQuiver
    Node
        Key
            threeVertexQuiver
            (threeVertexQuiver, List)
            [threeVertexQuiver, Flow]
            [threeVertexQuiver, Height]
        Headline
            make a toric quiver on underlying graph with three vertices and a specified number of edges between each
        Usage
            threeVertexQuiver E
        Inputs
            E: List
                number of edges between each pair of vertices
            Flow => 
                specify what form of flow to use. This input can be 
                either a string with values {\tt Canonical} or {\tt Random}, 
                or else a list of integer values. 
            Height => ZZ
                used in conjunction with the option {\tt Flow => Random} to set the upper limit of the random number generator
        Outputs
            Q: ToricQuiver
        Description
            Text
                This method can be used to create any quiver in the  family of quivers that can be
                built by defining three vertices and any nonzero number of edges joining each pair of edges
            Example
                -- create a quiver with 3 vertices and 6 edges
                Q = threeVertexQuiver {1,2,3}
            Example
                -- create a quiver with 3 vertices and 6 edges and a random flow
                Q = threeVertexQuiver ({1,2,3}, Flow => "Random")
            Example
                -- create a quiver with 3 vertices and 6 edges and a specified flow
                Q = threeVertexQuiver ({1,2,3}, Flow => {1, 2, 1, 3, 1, 4})
        SeeAlso
            toricQuiver
    Node
        Key
            allSpanningTrees
        Headline
            find the spanning trees of the underlying graph
        Usage
            allSpanningTrees Q
        Inputs
            Q: ToricQuiver
        Outputs
            L: List
                each spanning tree of the underlying graph is represented as a list of arrow indices, and the set of all spanning trees is a list of lists
        Description
            Text
                This method returns all of the spanning trees of the 
                underlying graph of the quiver {\tt Q}. Trees are 
                represented as lists of arrow indices.
            Text
                The algorithm performs a depth-first search on each vertex 
                and checks if the result is a connected graph.
            Example
                Q = bipartiteQuiver(2, 3)
                -- calculate the spanning trees for the underlying graph of the quiver Q
                allSpanningTrees(Q)
        SeeAlso
            toricQuiver
    Node
        Key
            basisForFlowPolytope
            (basisForFlowPolytope, ToricQuiver)
            (basisForFlowPolytope, List, ToricQuiver)
        Headline
            compute the necessary basis vectors for the hyperplane of a flow polytope
        Usage
            basisForFlowPolytope Q
            basisForFlowPolytope (T, Q)
        Inputs
            Q: ToricQuiver
            T: List
                of edges comprising the spanning tree to use to generate the basis. 
        Outputs
            B: Matrix
                whose rows are the basis vectors.
        Description
            Text
                For a generic weight, theta, in $C(Q)$, the flow polytope has the same dimension as the kernel of the inc map, 
                which is $|Q_0| - |Q_1| + 1$. Moreover, given a spanning tree of the quiver, there exists a natural basis 
                for the kernel of the inc map constructed from the combinatorics of the quiver, 
                see 
                @{HREF{
                "https://www.etd.ceu.edu/2015/joo_daniel.pdf", 
                "Dániel Joó, Toric Quiver Varieties, Ph.D thesis, 
                2015."}}@
                Therefore, we can translate the flow polytope 
                to this kernel and express the polytope on such basis. 
                With basisForFlowPolytope Q, we calculate the basis for inc map from a spanning tree of it. 
                If none is provided, then one is randomly chosen.

 
            Example
                basisForFlowPolytope bipartiteQuiver(2, 3)
            Example
                basisForFlowPolytope ({0,1,4,5},  bipartiteQuiver(2, 3))
        SeeAlso
            flowPolytopeVertices
    Node
        Key
            coneSystem
        Headline
            compute the chamber decomposition of weights
        Usage
            coneSystem Q
        Inputs
            Q: ToricQuiver
        Outputs
            L: List
                of Cones
        Description
            Text
                The set of weights {\tt th} for which the polytope {\tt (Q,th)} is 
                nonempty lies in a fan {\tt CQ}. This fan is partitioned into cones by the 
                walls of the toric quiver {\tt Q}, and for each partition there exists 
                a unique flow polytope. For a detailed discussion see
                Lutz Hille  
                @{HREF{
                "https://doi.org/10.1016/S0024-3795(02)00406-8",
                "\"Quivers, cones and polytopes\", Linear algebra and its applications 365 (2003): 215-237."
                }}@ 

            Example
                Q = toricQuiver {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
                CS = coneSystem Q
        SeeAlso
            flowPolytopeVertices
    Node
        Key
            flowPolytopeVertices
            (flowPolytopeVertices, ToricQuiver)
            (flowPolytopeVertices, List, ToricQuiver)
            [flowPolytopeVertices, Format]
        Headline
            generate the polytope associated to a toric quiver
        Usage
            flowPolytopeVertices Q
            flowPolytopeVertices(W, Q)
        Inputs
            Q: ToricQuiver
            W: List
                of integer values specifying a weight on the vertices
            Format => String
                specifying what basis to use. The default value {\tt SimplifiedBasis} 
                returns the polytope in a basis giving the minimal degree 
                necessary for polytope dimension. 
        Outputs
            : 
                Depending on the option for Format, this method returns a Matrix or List of Lists
                giving the coordinates of the vertices defining the flow polytope. 
                If the flow polytope does not exist for the given weight. Then, it returns an empty list.
        Description
            Text
                Associated with every acyclic toric quiver and weight pair is a flow polytope. This polytope can be translated to the kernel of the inc map, 
                so it is full dimensional. By default, the output lists the vertices within this vector space. To obtain the presentation in the original fiber, 
                use {\tt Format => "FullBasis"}.    
            Example
                F = flowPolytopeVertices(bipartiteQuiver(2, 3))
            Text 
                We can vary the weight of the quiver to define the flow polytope
            Example
                F = flowPolytopeVertices({-3,-3,2,2,2}, bipartiteQuiver(2,3))
            Text 
                The user can also recover the polytope $\Delta(Q,\theta)$ within the inverse of the corresponding inc map, that is within $inc^{-1}(\theta)$. 
            Example
                F = flowPolytopeVertices( bipartiteQuiver(2,3), Format => "FullBasis")
        SeeAlso
            basisForFlowPolytope
    Node
        Key
            incInverse
            (incInverse, List, ToricQuiver)
        Headline
            compute a flow in the preimage for a given weight
        Usage
            incInverse(W, Q)
        Inputs
            W: List
                of integers, specifying the weight on each vertex
            Q: ToricQuiver
        Outputs
            F: List
                giving a flow in the preimage of the input weight.
        Description
            Text
                Every integral flow induces a weight on the vertices $Q_0$ as follows: Let $a \in Q_1$ be an arrow. 
                The symbol  $a^{+} \in Q_0$ denotes its head and the symbol $a^{-} \in Q_0$ denotes its tail. 
                The so called incidence map generates a weight from the flow $\mathbf{w}$ by
                $$
                \text{inc}(\mathbf{w})(i) 
                := \sum_{a\in Q_1 \atop a^{+} = i} \mathbf{w}(a)  
                - \sum_{a\in Q_1 \atop a^{-} = i} 
                \mathbf{w}(a) \quad \text{ for all } i \in Q_0.
                $$
                The function incInverse calculates a vector in the preimage of such a map.    
            
            Example
                Q = toricQuiver(bipartiteQuiver(2,3));
                th = {-5,-1,2,2,2};
                -- calculate the preimage of the weight th for the quiver Q
                incInverse(th, Q)
        SeeAlso
            toricQuiver
            getWeights
    Node
        Key
            isAcyclic
            (isAcyclic, Matrix)
            (isAcyclic, ToricQuiver)
        Headline
            whether a toric quiver has no cycles
        Usage
            isAcyclic M
            isAcyclic Q
        Inputs
            Q: ToricQuiver
        Outputs
            : Boolean
                true if the quiver is acyclic
        Description
            Text
                This method determines whether a quiver is free from oriented cycles.
            Example
                isAcyclic bipartiteQuiver(2, 3)
            Example
                isAcyclic toricQuiver matrix({{-1, 1, -1, -1}, {1, -1, 0, 0}, {0, 0, 1, 1}})
        SeeAlso
            toricQuiver
    Node
        Key
            isClosedUnderArrows
            (isClosedUnderArrows, Matrix, List)
            (isClosedUnderArrows, List, Matrix)
            (isClosedUnderArrows, List, ToricQuiver)
            (isClosedUnderArrows, Matrix, ToricQuiver)
            (isClosedUnderArrows, ToricQuiver, ToricQuiver)
        Headline
            whether a subquiver closed under arrows
        Usage
            isClosedUnderArrows (M, V)
            isClosedUnderArrows (V, M)
            isClosedUnderArrows (V, Q)
            isClosedUnderArrows (M, Q)
            isClosedUnderArrows (SQ, Q)
        Inputs
            M: Matrix
                incidence matrix of subquiver or quiver to check
            Q: ToricQuiver
            SQ: ToricQuiver
                subquiver of Q to check 
            V: List
                set of vertices corresponding to the subquiver
        Outputs
            : Boolean
                true if the subquiver is closed under arrows with respect to the quiver, and false otherwise.
        Description
            Text
                Checks that a set of vertices is closed under arrows with respect to the toricQuiver {\tt Q}. 
                That is, for any $v\in V$, then any arrow in $Q_1$ with tail $v$ must have head in $V$ as well. 
                Note that this does not require that $V\subset Q_0$.
            Text
                Note also that the attribute closed under arrows relates to the underlying graph. 
                Arrows with flow of 0 (which occur in cases where using the quiver subset form: {\tt Q^S} 
                rather than {\tt Q_S}) are considered as valid arrows. 
            Example
                -- check if the subset {0, 2, 3} of the vertices of the quiver is closed under arrows
                isClosedUnderArrows ({0, 2, 3}, bipartiteQuiver(2,3))
            Example
                -- check if the subset {2, 3, 4} of the vertices of the quiver is closed under arrows
                isClosedUnderArrows ({2, 3, 4}, bipartiteQuiver(2,3))
            Example
                Q = threeVertexQuiver {1, 2, 3};
                SQ = Q_{0,1};
                -- check if the subquiver consisting of arrows {0, 1} is closed under arrows
                isClosedUnderArrows (SQ, Q)
            Example
                Q = threeVertexQuiver {1, 2, 3};
                SQ = Q^{0,1};
                -- check if the subquiver consisting of arrows {0, 1} is closed under arrows
                isClosedUnderArrows (SQ, Q)
        SeeAlso
            toricQuiver
    Node
        Key
            isSemistable
            (isSemistable, List, ToricQuiver)
            (isSemistable, ToricQuiver, ToricQuiver)
        Headline
            whether a subquiver is semistable with respect to a given weight
        Usage
            isSemistable (L, Q)
            isSemistable (sQ, Q)
        Inputs
            L: List
                of the indices of arrows in {\tt Q} that make up the subquiver in question
            Q: ToricQuiver
            SQ: ToricQuiver
                A subquiver of the quiver {\tt Q}
        Outputs
            : Boolean
                true if the subquiver is semistable, and false otherwise.
        Description
            Text
                This function determines if a given subquiver 
                is semistable with respect to the weight saved on {\tt Q}. 
                A subquiver {\tt SQ} of the quiver {\tt Q} is semistable if for every subset 
		{\tt V} of the vertices of {\tt Q} that is also {\tt SQ}-successor closed, 
		the sum of the weights associated to {\tt V} is nonnegative. 
            Example
                isSemistable ({0, 1}, bipartiteQuiver(2,3))
            Example
                isSemistable ({0, 1, 3, 4, 5}, bipartiteQuiver(2,3))
            Example
                -- create a quiver Q
                Q = bipartiteQuiver(2, 3);
                -- extract a subquiver
                S = first(subquivers(Q, Format => "quiver", AsSubquiver => true))
                -- check if the subquiver is semistable
                isSemistable (S, Q)
        SeeAlso
    Node
        Key
            isStable
            (isStable, List, ToricQuiver)
            (isStable, ToricQuiver, ToricQuiver)
        Headline
            whether a subquiver is semistable with respect to a given weight
        Usage
            isStable (L, Q)
            isStable (SQ, Q)
        Inputs
            L: List
                of the indices of arrows in {\tt Q} that make up the subquiver in question
            Q: ToricQuiver
            SQ: ToricQuiver
                A subquiver of the quiver {\tt Q}
        Outputs
            : Boolean
                true if the subquiver is stable, and false otherwise.
        Description
            Text 
                This function determines if a given subquiver 
                is stable with respect to the weight saved on {\tt Q}. 
                A subquiver {\tt SQ} of the quiver {\tt Q} is stable if for every subset 
		{\tt V} of the vertices of {\tt Q} that is also {\tt SQ}-successor closed, 
		the sum of the weights associated to {\tt V} is positive. 
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- extract a subquiver
                P = Q^{0,1,4,5};
                -- check if the subquiver is stable
                isStable(P, Q)
            Example
                isStable ({0, 1}, bipartiteQuiver(2, 3))
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- extract a subquiver of the quiver
                S = first(subquivers(Q, Format => "quiver", AsSubquiver => true));
                -- check  if this subquiver is stable
                isStable (S, Q)
        SeeAlso
    Node
        Key
            isTight
            (isTight, ToricQuiver)
            (isTight, ToricQuiver, List)
            (isTight, List, ToricQuiver)
            [isTight, Format]
        Headline
            determine if toric quiver is tight
        Usage
            isTight Q
            isTight(Q, W)
            isTight(W, Q)
            isTight(W, Q, Format => "Flow")
            isTight(W, Q, Format => "Weight")
        Inputs
            Q: ToricQuiver
            W: List
            Format => 
                specify whether input W is a flow (integer values associated to each arrow) 
                or the image of the flow under the map {\tt weights} (integer values associated to each vertex). 
        Outputs
            : Boolean
                true if the toric quiver is tight with respect to the specified flow, and false otherwise
        Description
            Text
                A toric quiver $Q$ is tight with respect to a given flow if there is no maximal 
                unstable subquiver of codimension 1. That is, every unstable subquiver of $Q$ 
                has at most $|Q_1|-2$ arrows. This method determines if a toric quiver $Q$ is 
                tight with respect to the vertex weights induced by its flow. 
            Example
                isTight bipartiteQuiver(2, 3)
            Example
                isTight bipartiteQuiver(2, 3, Flow => "Random")
            Example
                isTight (bipartiteQuiver(2, 3), {2,1,2,3,2,3})
            Example
                isTight ({0,0,0,0,0,1}, bipartiteQuiver(2, 3))
        SeeAlso
    Node
        Key
            (isWellDefined, ToricQuiver)
        Headline
            determine if toric quiver is correctly constructed
        Usage
            isWellDefined Q
        Inputs
            Q: ToricQuiver
        Outputs
            : Boolean
                true if the vertices and edges of the toric quiver agree, and if the flow for the quiver is a preimage for the weight. Otherwise, false.
        Description
            Text
                This method checks that the various attributes associated to the given toric 
                quiver are in the correct dimension. It checks that the dimensions of the 
                incidence matrix agree with the number of vertices and edges, as do the flow and weights. 
                It also checks that the weights for the quiver are induced by the flow, and that the 
                vertices and edges are constructed without gaps or missing data. 
            Example
                isWellDefined bipartiteQuiver(2, 3, Flow => "Random")
        SeeAlso
    Node
        Key
            makeTight
        Headline
            return a tight quiver with the same flow polytope
        Usage
            makeTight(W, Q)
        Inputs
            W: List
               of values corresponding to a weight on each arrow of {\tt Q}
            Q: ToricQuiver
        Outputs
            Q: ToricQuiver
                that is tight with respect to the flow on the input, and which has the same flow polytope as the input.
        Description
            Text
                Let $\theta$ be an integral weight assigned to the vertices of a quiver $Q$. The quiver $Q$ is called $\theta$-tight if for every arrow $\alpha$, 
                the subquiver $Q\setminus \alpha$ is $\theta$-stable. Every quiver can be tightened by contraction of certain arrows in Q and changing the weight accordingly, 
                see Section 4 at
                @{HREF{"https://link.springer.com/article/10.1007/s00229-009-0255-6", 
                "Altmann, Klaus, and Duco van Straten. \"Smoothing of quiver varieties.\" manuscripta mathematica 129 (2009): 211-230."}}@ 
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- create a list of weights corresponding to the arrows of Q
                w = {-5,-1,2,2,2};
                -- create a tight quiver with the same flow polytope
                makeTight(w, Q)
        SeeAlso
    Node
        Key
            maxCodimensionUnstable
            (maxCodimensionUnstable, ToricQuiver)
        Headline
            compute the maximal codimension of the unstable loci of a quiver
        Usage
            maxCodimensionUnstable Q
        Inputs
            Q: ToricQuiver
        Outputs
            : ZZ
                the maximal codimension of the unstable loci of the quiver
        Description
            Text
                It computes the maximal codimension of the unstable loci a given quiver {\tt Q}
            Example
                maxCodimensionUnstable bipartiteQuiver(2, 3)
            Text     
                We note that this value can range from $|Q_1|$, as is the case with the quiver associated 
                with the projective space, to 1 when the quiver is not tight.
            Example 
                maxCodimensionUnstable toricQuiver({{0,1},{0,1},{0,1}})
        SeeAlso
    Node
        Key
            maximalNonstableSubquivers
            [maximalNonstableSubquivers, ReturnSingletons]
            [maximalNonstableSubquivers, Format]
        Headline
            return the maximal subquivers that are semistable
        Usage
            maximalNonstableSubquivers Q
        Inputs
            Q: ToricQuiver
            ReturnSingletons => Boolean
                Whether or not to return singleton vertices subquivers
            Format => String
                format for representing the subquivers
        Outputs
            L: HashTable
                consisting of two keys: {\tt Nonsingletons} and {\tt Singletons}
        Description
            Text
                This method takes all of the possible subquivers of a given quiver {\tt Q} 
                and returns those that are not stable, and which are maximal with respect to the weight on the quiver {\tt Q}.
            Text
                Subquivers are represented by lists of arrows, except in the case of subquivers that consist of singleton vertices. 
            Example
                maximalNonstableSubquivers bipartiteQuiver (2, 3)
        SeeAlso
    Node
        Key
            maximalUnstableSubquivers
            [maximalUnstableSubquivers, ReturnSingletons]
            [maximalUnstableSubquivers, Format]
        Headline
            return the maximal subquivers that are unstable
        Usage
            maximalUnstableSubquivers Q
        Inputs
            Q: ToricQuiver
            ReturnSingletons => Boolean
                Whether or not to return singleton vertices subquivers
            Format => String
                format for representing the subquivers
        Outputs
            L: HashTable
                consisting of two keys: {\tt Nonsingletons} and {\tt Singletons}
        Description
            Text
                This method takes all of the possible subquivers of a given quiver {\tt Q} 
                and returns those that are both unstable and maximal with respect to the weight on the quiver {\tt Q}.
            Text
                Subquivers are represented by lists of arrows, except in the case of subquivers that consist of singleton vertices. 
            Example
                maximalUnstableSubquivers bipartiteQuiver (2, 3)
        SeeAlso
    Node
        Key
            mergeOnArrow
            (mergeOnArrow, ToricQuiver, ZZ, ToricQuiver, ZZ)
            (mergeOnArrow, ToricQuiver, ZZ, Matrix, ZZ)
            (mergeOnArrow, Matrix, ZZ, Matrix, ZZ)
            (mergeOnArrow, Matrix, ZZ, ToricQuiver, ZZ)
        Headline
            join two quivers together by identifying an arrow from each
        Usage
            mergeOnArrow (Q1, A1, Q2, A2)
            mergeOnArrow (Q1, A1, M2, A2)
            mergeOnArrow (M1, A1, M2, A2)
            mergeOnArrow (M1, A1, Q2, A2)
        Inputs
            A1: ZZ
            A2: ZZ
            M1: Matrix
            M2: Matrix
            Q1: ToricQuiver
            Q2: ToricQuiver
        Outputs
            : ToricQuiver
                obtained by merging the two provided quivers together at the specified arrows
        Description
            Text
                This method creates a new quiver from joining two toricQuivers together by identifying arrow $A1$ in $Q1$ with arrow $A2$ in $Q2$. 
                The input matrices must correspond to valid graphs, and the integers must correspond to arrows in each of the provided quiver objects.
            Example
                mergeOnArrow (bipartiteQuiver (2, 3), 0, bipartiteQuiver (2, 3), 0)
            Example
                mergeOnArrow (bipartiteQuiver (2, 3), 0, matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), 0)
            Example
                mergeOnArrow (matrix ({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), 0, bipartiteQuiver (2, 3), 0)

        SeeAlso
    Node
        Key
            mergeOnVertex
            (mergeOnVertex, ToricQuiver, ZZ, ToricQuiver, ZZ)
            (mergeOnVertex, ToricQuiver, ZZ, Matrix, ZZ)
            (mergeOnVertex, Matrix, ZZ, Matrix, ZZ)
            (mergeOnVertex, Matrix, ZZ, ToricQuiver, ZZ)
        Headline
            join two quivers together by identifying a vertex from each
        Usage
            mergeOnVertex (Q1, V1, Q2, V2)
            mergeOnVertex (Q1, V1, M2, V2)
            mergeOnVertex (M1, V1, M2, V2)
            mergeOnVertex (M1, V1, Q2, V2)
        Inputs
            M1: Matrix
            M2: Matrix
            Q1: ToricQuiver
            Q2: ToricQuiver
            V1: ZZ
            V2: ZZ
        Outputs
            : ToricQuiver
                obtained by merging the two provided quivers together at the specified vertices
        Description
            Text
                This method creates a new quiver from joining two toricQuivers together by identifying 
                vertex $V1$ in $Q1$ with vertex $V2$ in $Q2$. The input matrices must correspond to valid graphs,
                and the integers must correspond to vertices in each of the provided quiver objects.
            Example
                mergeOnVertex (bipartiteQuiver (2, 3), 1, bipartiteQuiver (2, 3), 0)
            Example
                mergeOnVertex (bipartiteQuiver (2, 3), 1, matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), 0)
            Example
                mergeOnVertex (matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), 1, bipartiteQuiver (2, 3), 0)
        SeeAlso
    Node
        Key
            potentialWalls
            (potentialWalls, ToricQuiver)
            (potentialWalls, Matrix)
        Headline
            lists the potential walls in the weight chamber decomposition for a given quiver
        Usage
            potentialWalls Q
            potentialWalls M
        Inputs
            Q: ToricQuiver
        Outputs
            : List
                of potential walls for he cone system associated to the toric quiver
        Description
            Text
                Every wall can be represented uniquely by a partition of the vertices 
                {\tt Q0} of {\tt Q} into two sets {\tt Qplus} and {\tt Qminus}. As a partition 
                can be expressed in terms of only one of the subsets, only one of the two sets {\tt Qplus} 
                and {\tt Qminus} is used in every case. 
                Thus we denote the wall {\tt W} by the subset of vertices {\tt Qplus} used for defining it. 

            Text
                This method is written to compute the potential walls based on the incidence matrix,
                in the case of a flow of all unit values, or based on the entire quiver if the flow is nontrivial.

            Example
                potentialWalls toricQuiver {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}}
            Example
                potentialWalls chainQuiver {1}
        SeeAlso
    Node
        Key
            primitiveArrows
        Headline
            list the primitive arrows in a quiver
        Usage
            primitiveArrows Q
        Inputs
            Q: ToricQuiver
        Outputs
            L: List
                of arrow indices corresponding to the primitive arrows in {\tt Q}
        Description
            Text
                This method returns a list of the arrows in the provided quiver that are primitive.
                An arrow {\tt a=(v0,v1)} in the quiver {\tt Q} is primitive if there exists an 
                oriented non-empty path from {\tt v0} to {\tt v1} in {\tt Q} that is distinct from {\tt a}. 
                Thus, if the arrow {\tt a} is primitive, then removing it from the quiver results in a quiver 
                that still contains a path between its endpoints {\tt v0} and {\tt v1}.
            Example
                primitiveArrows toricQuiver {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}}
        SeeAlso
    Node
        Key
            quiverIncidenceMatrix
            (quiverIncidenceMatrix, ToricQuiver)
        Headline
            return the graph incidence matrix attribute associated to the toric quiver
        Usage
            quiverIncidenceMatrix Q
        Inputs
            Q: ToricQuiver
        Outputs
            M: Matrix
                that is the incidence matrix for the graph of the toric quiver
        Description
            Text
                This method returns the incidence matrix for the quiver.
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- compute the incidence matrix of the underlying graph
                quiverIncidenceMatrix Q
        SeeAlso
            ToricQuiver
    Node
        Key
            quiverEdges
            (quiverEdges, ToricQuiver)
        Headline
            return the graph edges associated to the toric quiver
        Usage
            quiverEdges Q
        Inputs
            Q: ToricQuiver
        Outputs
            E: List
                of edges, each of which is a pair of vertices.
        Description
            Text
                This method returns the list of edges that are in the graph of the provided toric quiver.
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- return the edges of the graph underlying the quiver
                quiverEdges Q
        SeeAlso
            ToricQuiver
    Node
        Key
            quiverFlow
            (quiverFlow, ToricQuiver)
        Headline
            return the flow attribute associated to the toric quiver
        Usage
            quiverFlow Q
        Inputs
            Q: ToricQuiver
        Outputs
            F: List
                the flow for the toric quiver
        Description
            Text
                This method returns the flow of the provided toric quiver, which is a list of numeric values.
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3, Flow=> {1, 2, 3, 4, 5, 6});
                -- calculate the flow of the quiver
                quiverFlow Q
        SeeAlso
            ToricQuiver
            incInverse
    Node
        Key
            quiverVertices
            (quiverVertices, ToricQuiver)
        Headline
            return the vertices of the toric quiver
        Usage
            quiverVertices Q
        Inputs
            Q: ToricQuiver
        Outputs
            V: List
                of the vertices in the quiver
        Description
            Text
                This method returns the vertices that are in the provided toric quiver.
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- return the vertices of the underlying graph
                quiverVertices Q
        SeeAlso
            ToricQuiver
    Node
        Key
            quiverWeights
            (quiverWeights, ToricQuiver)
        Headline
            return the weight attribute associated to the toric quiver
        Usage
            quiverWeights Q
        Inputs
            Q: ToricQuiver
        Outputs
            W: List
                of the weights induced by the flow on the quiver
        Description
            Text
                This method returns the weights of the provided toric quiver.
                The weights are the image of the quiver flow under the inc map, which is defined as
                $$
                \text{inc}(\mathbf{w})(i) 
                := \sum_{a\in Q_1 \atop a^{+} = i} \mathbf{w}(a)  
                - \sum_{a\in Q_1 \atop a^{-} = i} 
                \mathbf{w}(a) \quad \text{ for all } i \in Q_0.
                $$
                where for any arrow $a\in Q_1$, we denote its head as $a^{+} \in Q_0$ and its tail as $a^{-} \in Q_0$.
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- return the weights for the given quiver
                quiverWeights Q
        SeeAlso
            ToricQuiver
            incInverse
    Node
        Key
            referenceThetas
            (referenceThetas, List)
        Headline
            return a weight for all polytopes associated to a toric quiver
        Usage
            referenceThetas QCS
        Inputs
            QCS: List
                of cones comprising the chamber decomposition for weights
        Outputs
            L: List
                of weights, where each weight is in the interior of a unique chamber of the cone system for the quiver
        Description
            Text
                This method uses the {\tt interiorVector} method for a Cone from the {\tt Polyhedra}
                package to generate a single internal point for each of the cones in the input list {\tt QCS}.
            Example
                -- create a quiver
                Q = toricQuiver {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
                -- compute the chamber decomposition into cones of the space of weights
                CS = coneSystem Q;
                -- extract a reference weight in the interior of each of the chambers of the cone system
                referenceThetas CS
        SeeAlso
            referenceWeights
            getWeights
            coneSystem
    Node
        Key
            referenceWeights
            (referenceWeights, List)
        Headline
            return a weight for all polytopes associated to a toric quiver
        Usage
            referenceWeights QCS
        Inputs
            QCS: List
                of cones comprising the chamber decomposition for weights
        Outputs
            L: List
                of weights, where each weight is in the interior of a unique chamber of the cone system for the quiver
        Description
            Text
                This method uses the {\tt interiorVector} method for a Cone from the {\tt Polyhedra}
                package to generate a single internal point for each of the cones in the input list {\tt QCS}.
            Example
                -- create a quiver
                Q = toricQuiver {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
                -- compute the chamber decomposition into cones of the space of weights
                CS = coneSystem Q;
                -- extract a reference weight in the interior of each of the chambers of the cone system
                R = referenceWeights CS
        SeeAlso
            getWeights
            coneSystem
    Node
        Key
            samePolytope
            (samePolytope, List, List, ToricQuiver)
        Headline 
            whether two weights produce the same flow polytope
        Usage
            samePolytope(Th1, Th2, TQ)
        Inputs
            Th1: List
            Th1: List
            TQ: ToricQuiver
        Outputs
            B: Boolean
                whether the two weights correspond to the same polytope
        Description
            Text
                This function returns either a boolean value of {\tt true} or else a string
                describing why the outcome cannot be determined. It requires that polytopes be 
                smooth and that the set of stable trees for both weights be nonempty.
                This is computed by calculating the flow polytope for the toric quiver with each
                of the two weights and comparing the resulting polytopes. The polytopes are considered
                equal if they are isomorphic, using the areIsomorphic method in the LatticePolytopes package.
            Example
                Q = toricQuiver({{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}});
                samePolytope({-3,2,-1,2},{-2,1,-2,3}, Q)    
        SeeAlso
            toricQuiver
            flowPolytopeVertices
            areIsomorphic
        Caveat
            This operation is computationally expensive, and may not perform well for quivers whose chamber systems are comprised of many subchambers.
    Node
        Key
            stableTrees
        Headline
            return the spanning trees that are stable
        Usage
            stableTrees(th, Q)
        Inputs
            th: List
                of weights corresponding to each vertex
            Q: ToricQuiver
        Outputs
            L: List
                of lists, each representing the arrows that comprise a spanning tree that is stable with respect to the weight th
        Description
            Text
                This routine returns the set of spanning trees of the quiver {\tt Q} that are stable with respect to the provided weight {\tt th}.
            Example
                -- create a quiver
                Q = bipartiteQuiver(2, 3);
                -- create a weight
                th = {-3,-3,2,2,2};
                -- calculate the set of spanning trees that are stable with respect to the weight th
                stableTrees(th, Q)
        SeeAlso
    Node
        Key
            subquivers
            (subquivers, Matrix)
            (subquivers, ToricQuiver)
            [subquivers, Format]
            [subquivers, AsSubquiver]
        Headline
            return all possible subquivers of a given quiver
        Usage
            subquivers M
            subquivers Q
        Inputs
            M: Matrix
            Q: ToricQuiver
            Format => String
                options include {\tt quiver}, which returns a list of quivers, and {\tt list}, 
                which returns a list of arrows for each subquiver
            AsSubquiver => Boolean
                if Format is specified as {\tt quiver}, then applying 
                {\tt AsSubquiver = true} insures that the matrix representation 
                of the subquiver is the same size as the matrix original quiver
        Outputs
            L: List
                of either quiver objects, or arrow indices
        Description
            Text 
                A subquiver is a subgraph of a quiver. Specifically, it is formed by selecting a subset of the arrows
                from the original quiver, ensuring that the tails and heads of the chosen arrows correspond to the selected vertices. 
                There are several ways to represent a subquiver:
            Text      
                @UL {
                    {"Using a list of arrow indices to indicate the selected arrows. "}, 
                    {"Selecting a subset of rows and columns from the original incidence matrix. Here, we leverage the fact that columns 
                      in the incidence matrix correspond to arrows in the quiver."},
                }@
            Text
                subquivers Q list all possible subquivers of Q.
            
            Example
                Q = chainQuiver {2};
                subquivers Q
            Example
                Q = chainQuiver {2};
                subquivers(Q, Format => "list")
            Example
                subquivers bipartiteQuiver(2, 2)
            Example
                subquivers(bipartiteQuiver(2, 2), Format => "list")
            Example
                subquivers(bipartiteQuiver(2, 2), Format => "quiver", AsSubquiver => true)
        SeeAlso
            AsSubquiver
    Node
        Key
            wallQPlus
            (wallQPlus, Wall)
        Headline
            get the partition of the set of vertices induced by a wall
        Usage
            wallQPlus (W)
        Inputs
            W: Wall
        Outputs
            QP: Sequence
                of vertices contained in one of the two sets forming the partition induced by the wall W
        Description
            Text
                Every wall can be represented uniquely by a partition of the vertices 
                {\tt Q0} of {\tt Q} into two sets {\tt Qplus} and {\tt Qminus}. 
                We denote the wall {\tt W} by the subset of vertices {\tt Qplus} used for defining it. 
            Example
                wallQPlus(first potentialWalls bipartiteQuiver(2, 3))
        SeeAlso
            wallType
    Node
        Key
            wallType
            (wallType, List, ToricQuiver)
            (wallType, List, Matrix)
            (wallType, Wall)
        Headline
            get the type of a wall for a given quiver
        Usage
            wallType (Qplus, Q)
            wallType (Qplus, M)
            wallType (W)
        Inputs
            Q: ToricQuiver
            M: Matrix
            Qplus: List
            W: Wall
        Outputs
            WT: Sequence
                having two nonnegative integer entries denoting the type of the wall
        Description
            Text
                Every wall can be represented uniquely by a partition of the vertices 
                {\tt Q0} of {\tt Q} into two sets {\tt Qplus} and {\tt Qminus}. 
                We denote the wall {\tt W} by the subset of vertices {\tt Qplus} used for defining it. 
            Text
                The type of the wall is defined as {\tt (t+,t-)} where {\tt t^+} 
                is the number of arrows starting {\tt Qplus} and ending in 
                {\tt Qminus}, and {\tt t-} is the number of arrows starting {\tt Qminus} 
                and ending in {\tt Qplus}. 

            Text
                This method is written to compute the wall type based on the incidence matrix,
                in the case of a trivial flow, or the entire quiver. It will also return the type for a given wall.

            Example
                wallType({0,2,3}, bipartiteQuiver(2, 3))
            Example
                wallType({0,2,3}, quiverIncidenceMatrix(bipartiteQuiver(2, 3)))
            Example
                wallType({1,2,3}, chainQuiver({2,3,4}))
            Example
                wallType(first potentialWalls chainQuiver({2,3,4}))
        SeeAlso
    Node
        Key
            getWeights
            (getWeights, Matrix)
            (getWeights, ToricQuiver)
        Headline
            image of the flow on the vertices
        Usage
            getWeights M
            getWeights Q
        Inputs
            M: Matrix
            Q: ToricQuiver
        Outputs
            L: List
                of integers, corresponding to the image of the flow of the provided toric quiver under the $Inc$ map
        Description
            Text
                This method returns the weight of the quiver.
                This vector is the image of its flow under the $inc$ map, which is defined as
                $$
                \text{inc}(\mathbf{w})(i) 
                := \sum_{a\in Q_1 \atop a^{+} = i} \mathbf{w}(a)  
                - \sum_{a\in Q_1 \atop a^{-} = i} 
                \mathbf{w}(a) \quad \text{ for all } i \in Q_0.
                $$
                where for any arrow $a\in Q_1$, we denote its head as $a^{+} \in Q_0$ and its tail as $a^{-} \in Q_0$.
            Example
                Q = bipartiteQuiver(2, 3, Flow => "Random");
                getWeights Q
        SeeAlso
            incInverse
-- documentation for symbols
    Node
        Key
            AsSubquiver
        Description
            Text
                This is a symbol used to determine subquiver representation. Applying
                {\tt AsSubquiver = true} insures that the matrix representation 
                of the subquiver is the same size as the matrix original quiver. 
                If false, then the subquiver is returned as a standalone quiver 
                represented by only the vertices and arrows comprising the subquiver. 
            Example
                subquivers(bipartiteQuiver(2, 2), Format => "quiver", AsSubquiver => true)
            Example
                subquivers(bipartiteQuiver(2, 2), Format => "quiver", AsSubquiver => false)
        SeeAlso
            subquivers
    Node
        Key
            Flow
        Description
            Text
                This is an optional argument that can be a string with values {\tt Canonical}, which assigns 
                a value of 1 to each edge, or {\tt Random}, which assigns random integer values. 
            Example
                -- create a toric quiver from a matrix with keyword flow
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{1,1,0,0},{0,0,1,1}}), Flow => "Canonical")
            Example
                -- create a toric quiver from a matrix with random flow
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random")
            Example
                -- create a bipartite quiver with a random flow
                Q = bipartiteQuiver(2, 3, Flow => "Random")
        SeeAlso
            toricQuiver
    Node
        Key
            Height
        Description
            Text
                This is an optional argument that can be positive integer values, which and which is 
                used in quiver constructor methods when the optional argument for flow is set to 
                random {\tt Flow => "Random"}. The {\tt Height} argument sets the maximum value for 
                the integer-valued random numhber generator so that values in the flow are chosen 
                from the interval {\tt [0, Height)}.
            Example
                -- create a toric quiver from a matrix with random flow with values in the interval [0, 10)
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random")
            Example
                -- create a toric quiver from a matrix with random flow with values in the interval [0, 100)
                Q = toricQuiver(matrix({{-1,-1,-1,-1},{0,0,1,1},{1,1,0,0}}), Flow => "Random", Height => 100)
        SeeAlso
            Flow
            toricQuiver
    Node
        Key
            ReturnSingletons
        Description
            Text
                This is an optional argument for the function {\tt maximalNonstableSubquivers},
                which allows the user to consider single vertices as subquivers. For most computations, 
                these subquivers are trivial, and are ignored. 
            Example
                maximalNonstableSubquivers(bipartiteQuiver (2, 3), ReturnSingletons => true)
            Example
                maximalNonstableSubquivers(bipartiteQuiver (2, 3), ReturnSingletons => false)
        SeeAlso
            maximalNonstableSubquivers
///
TEST ///
    testQuiverConstruction = Q -> (
        assert (isWellDefined(Q));
        AllSubquivers = subquivers(Q, Format => "list");
        EdgeSubsets = subsets(#quiverEdges(Q));
        assert (#AllSubquivers == #EdgeSubsets - 2);
    )
    Q = threeVertexQuiver {1,2,3};
    testQuiverConstruction(Q)
///
TEST ///
    Q = bipartiteQuiver(2, 2);
    assert isWellDefined Q;
    assert (allSpanningTrees Q == {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}});
///
TEST ///
    Q = bipartiteQuiver(2, 2);
    assert (not isClosedUnderArrows({1, 2}, Q));
///
TEST ///
    Q = bipartiteQuiver(2, 2);
    munsbs = maximalNonstableSubquivers(Q, ReturnSingletons => true)
    vals = flatten for key in keys(munsbs) list(munsbs#key)
    assert (vals === {{0}, {1}, {0, 1, 2}, {0, 1, 3}, {0, 2, 3}, {1, 2, 3}})
///
TEST ///
    Q = bipartiteQuiver(2, 2);
    CS = coneSystem Q;
    rts = referenceWeights CS;
    assert (#CS == #rts);
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert isWellDefined Q;
    assert (#potentialWalls(Q) == 11)
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert isTight Q
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert (getWeights Q === {-3, -3, 2, 2, 2})
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert (flowPolytopeVertices Q == {{-1, 1}, {-1, 0}, {1, -1}, {0, -1}, {1, 0}, {0, 1}})
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert (entries basisForFlowPolytope Q === {{-1, 0}, {0, -1}, {1, 1}, {1, 0}, {0, 1}, {-1, -1}})
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    th = {-5, -1, 2, 2, 2};
    F = incInverse(th, Q);
    assert not isTight(F, Q)
    assert isAcyclic makeTight(th, Q);
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert not isSemistable({1, 2}, Q)
    assert isStable({1, 2, 3, 4}, Q)
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    th = {-5, -1, 2, 2, 2};
    assert (stableTrees(th, Q) === {{0, 1, 2, 5}, {0, 1, 2, 4}, {0, 1, 2, 3}})
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert (maxCodimensionUnstable(Q) == 6)
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    assert (wallType({0,2,3}, Q) == (1, 2))
///
TEST ///
    Q = bipartiteQuiver(2, 3);
    M = maximalUnstableSubquivers(Q);
    for v in values M do (
        assert (#v == 6)
    )
///
TEST ///
    testQuiverConstruction = Q -> (
        assert (isWellDefined(Q));
        AllSubquivers = subquivers(Q, Format => "list");
        EdgeSubsets = subsets(#quiverEdges(Q));
        assert (#AllSubquivers == #EdgeSubsets - 2);
    )
    P = chainQuiver {2, 2};
    Q = chainQuiver {1, 2, 3};
    testQuiverConstruction(P)
    testQuiverConstruction(Q)
///
TEST ///
    P = chainQuiver {2, 2};
    Q = chainQuiver {1, 2, 3};
    assert (mergeOnVertex(P, 2, Q, 0) == chainQuiver {2, 2, 1, 2, 3})
    assert (mergeOnArrow(P, 3, Q, 0) == chainQuiver {2, 2, 2, 3})
///
TEST ///
    Q = toricQuiver {{0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}}
    assert isWellDefined Q;
    assert (primitiveArrows Q === {0, 3, 5})
///
TEST ///
    Q = toricQuiver {{0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}}
    assert samePolytope({-3, 2, -1, 2}, {-2, 1, -2, 3}, Q)
///
TEST ///
    Q = toricQuiver {{0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}}
    assert (#potentialWalls Q > 0);
///
end--
