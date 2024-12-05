-- -*- coding: utf-8 -*-
-- This file is in the public domain
newPackage(
    "CellularResolutions",
    Version => "1.0",
    Date => "May 17, 2023",
    Authors => {
        {Name => "Jay Yang", Email => "jayy@wustl.edu"},
        {Name => "Aleksandra Sobieska", Email => "asobieska@math.wisc.edu"}
        },
    Headline => "cellular resolutions of monomial ideals",
    AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    PackageExports => {"Polyhedra", "SimplicialComplexes", "Posets"},
    Keywords => {"Commutative Algebra"}
    )

export {--types
        "CellComplex",
        "Cell",
        --methods
        "boundary",
        "boundaryCells",
        "cells",
        "cellComplex",
        "cellComplexSphere",
        "cellComplexRPn",
        "cellComplexTorus",
        "cellLabel",
        "hullComplex",
        "isCycle",
--        "isFree",
        "isMinimal",
        "isSimplex",
        "newCell",
        "newSimplexCell",
        "maxCells",
        "relabelCellComplex",
        "scarfComplex",
        "subcomplex",
        "taylorComplex",
        --symbols (for optional parameters)
        "CellDimension",
        "InferLabels",
        "LabelRing",
        "Reduced"
        --"Prune"
        }
protect labelRing
protect label
protect cellDimension
protect CellDimension
protect InferLabels
protect LabelRing
protect Reduced
protect Prune

hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";

CellComplex = new Type of HashTable
CellComplex.synonym = "cell complex"
CellComplex.GlobalAssignHook = globalAssignFunction
CellComplex.GlobalReleaseHook = globalReleaseFunction
--Note, the mutable hash table means that equality works "Correctly"
Cell = new Type of MutableHashTable
Cell.synonym = "cell"

maxAndAllCells := (lst) -> (
    if #lst == 0 then return (new HashTable,new HashTable);
    bdfn := c -> set boundaryCells c;
    maxcells := set lst;
    bdcells := sum (maxcells/bdfn);
    allcells := maxcells + bdcells;
    maxcells = maxcells - bdcells;
    while #bdcells != 0 do (
        bdcells = sum (bdcells/bdfn);
        allcells = allcells + bdcells;
        maxcells = maxcells - bdcells;
        );
    (partition(dim,toList maxcells), partition(dim,toList allcells))
    )

--returns a hashtable of lists of cells indexed by dimension
cellsFromMaxCells := lst -> (
    pendingCells := set lst;
    finishedCells := {};
    while #pendingCells !=0 do (
        c := (elements pendingCells)#0;
        pendingCells = pendingCells + ((set ((boundary c)/first)) - finishedCells) - {c};
        finishedCells = append(finishedCells,c);
        );
    partition(dim,finishedCells)
    )

--Private constructor, creates the cache
mkCellComplex := (labelRingVal, cellsVal, maxCellsVal) -> (
    new CellComplex from {
        symbol labelRing => labelRingVal,
        symbol cells => cellsVal,
        cache => new CacheTable from (
            if maxCellsVal === null
            then {}
            else {symbol maxCells => maxCellsVal})
        }
    )

cellComplex = method(Options=>true, TypicalValue=>CellComplex)
cellComplex(Ring,List) := {} >> o -> (R,maxCells) -> (
    (realMaxCells,allCells) := maxAndAllCells maxCells;
    mkCellComplex(R, allCells, realMaxCells)
    )

cellComplex(Ring,SimplicialComplex) := {Labels=>null} >> o -> (S,C) -> (
    R := ring C;
    Cfaces := new HashTable from faces C;
    --cells indexes Cells by monomials corresponding to faces of the simplicial complex
    cells := new MutableHashTable from {};
    for i from 0 to dim C do (
        for simplex in Cfaces#i do (
            label :=
               (if class(o.Labels) === HashTable
               then (if o.Labels#?simplex
                     then o.Labels#simplex
                     else null
                   )
               else null);
            bd := if i==0
                  then {}
                  else
                      for x in gens R list (
                          if simplex%x==0 then cells#(simplex//x)
                          else continue);
            cells#simplex = if label === null then newCell bd else newCell(bd,label)
            );
        );
    cellComplex(S,values cells)
    )

maxCells = method(TypicalValue=>HashTable)
maxCells(CellComplex) := (cacheValue (symbol maxCells)) (cellComplex ->
    (
        lst := flatten values cells cellComplex;
        if #lst == 0 then return new HashTable;
        bdfn := c -> set boundaryCells c;
        maxcells := set lst;
        bdcells := sum (maxcells/bdfn);
        maxcells = maxcells - bdcells;
        while #bdcells != 0 do (
            bdcells = sum (bdcells/bdfn);
            maxcells = maxcells - bdcells;
            );
        partition(dim,toList maxcells)
        ))

--Define dimension for cell
dim(Cell) := (cell) -> cell.cellDimension

--Define dimension for cell complex
dim(CellComplex) := (cellComplex) -> max keys cellComplex.cells

--Define ring for cell complex
ring(CellComplex) := (cellComplex) -> cellComplex.labelRing


cellLabel = method()
cellLabel(Cell) := (cell) -> cell.label

--Make a cell, internal function
makeCell := (lst, l, d) -> (
    bdim := -1;
    for cell in lst do (
        if bdim < 0
        then bdim = dim cell#0
        else assert(bdim == dim cell#0)
        );
    n := max(bdim + 1,d);
    new Cell from {
        symbol cellDimension => n,
        symbol boundary => lst, -- could verify that it's a list
        symbol label => l
        }
    );

chainToVirtualTally := (lst) -> (
    if lst == {}
    then new VirtualTally from {}
    else sum(lst, (cell,deg) -> new VirtualTally from {cell => deg})
    )

boundary = method()
boundary(Cell) := List => (cell) -> cell.boundary
boundaryCells = method(TypicalValue=>List)
boundaryCells(Cell) := (cell) -> apply(boundary(cell), c -> first c)
--Boundary function, returns the boundary as a VirtualTally
boundaryTally := (cell) -> chainToVirtualTally cell.boundary


internalCycleCheck := (lst) -> ((sum(lst,l -> (
                c := l#0;
                deg := l#1;
                if deg>0
                then sum(deg,i -> boundaryTally c)
                else - sum( - deg,i -> boundaryTally c)))) ? 0) == symbol ==

--Check if a chain, represented by a list is a boundary
isCycle = method(TypicalValue=>Boolean)
isCycle(List) := {Reduced=>true} >> o -> (lst) ->
    (if o.Reduced
    then (
        p := partition(x -> dim (x#0) == 0, lst);
        zeroDimCells := if p#?true then p#true else {};
        otherCells := if p#?false then p#false else {};
        internalCycleCheck otherCells and sum(zeroDimCells,last) == 0
        )
    else internalCycleCheck lst)


--Figure out an orientation automatically
inferOrientation := (lst) -> (
    if #lst == 2 and (dim first lst) == 0 then (
        ret := {(lst#0,1),(lst#1,-1)};
        if not isCycle ret then error "The given list of cells do not form a cycle";
        return ret
        );
    boundaryChain := new VirtualTally from {};
    remainingCells := lst;
    --the "?" based comparison is a workaround for "==" not working correctly for VirtualTally==ZZ
    while (boundaryChain ? 0) != symbol == or #remainingCells!=0 list (
        if (boundaryChain ? 0) == symbol ==
        then (
            if remainingCells =!= lst then error "The orientation on the cycle is non-unique";
            nextCell := last remainingCells;
            remainingCells = drop(remainingCells,-1);
            boundaryChain = boundaryTally nextCell;
            (nextCell,1)
            )
        else (
            c := (keys boundaryChain)#0;
            nextElems := select(remainingCells,c2 -> (boundaryTally c2)#?c);
            if #nextElems==0 then error "The given list of cells do not form a cycle";
            newBoundaryComponent := boundaryTally (nextElems#0);
            remainingCells = delete(nextElems#0,remainingCells);--Inefficient
            --check sign equality
            if (boundaryChain#c)*(newBoundaryComponent#c)<0
            then (
                boundaryChain = boundaryChain + boundaryTally (nextElems#0);
                (nextElems#0,1)
                )
            else (
                boundaryChain = boundaryChain - boundaryTally (nextElems#0);
                (nextElems#0,-1)
                )
            )
        )
    )

--Convert it to a submodule of R^1 if possible
toModule := (R,x) -> (
    if instance(x,Module) then return x;
    if instance(x,Ideal) then return module x;
    if instance(x,RingElement) then return image matrix {{x}};
    if instance(x,Number) then return image matrix {{x_R}};
    error "Expected a Module, Ideal, RingElement, or Number"
    )

inferLabel := boundary -> (
    if boundary == {} then return 1;
    if instance(boundary#0,Sequence) then return boundary/first//inferLabel;
    if all(boundary/cellLabel,b -> instance(b,RingElement) or instance(b,Number))
    then boundary/cellLabel//lcm
    else (
        rings := select(boundary/cellLabel, b -> not (instance(b,RingElement) or instance(b,Number)))/ring;
        nonNumberRings := select(rings,r -> ancestor(Number,r));
        R := if nonNumberRings==={} then rings#0 else nonNumberRings#0;
        boundary/cellLabel/(x -> toModule(R,x))//intersect
        )
    )

--Attach a cell
newCell = method(Options => {CellDimension=>null}, TypicalValue=>Cell)
newCell(List,Number) :=
newCell(List,RingElement) :=
newCell(List,Module) :=
newCell(List,Ideal) := opt -> (boundary,label) -> (
    if #boundary!=0 and instance(boundary#0,Cell)
    then return newCell(inferOrientation boundary,label,CellDimension=>opt.CellDimension);
    if not isCycle boundary then error "Expected the boundary to be a cycle";
    cd := if opt.CellDimension=!=null then opt.CellDimension else 0;
    c := makeCell(boundary,label,cd);
    if opt.CellDimension=!=null and dim c > cd then error "Incorrect CellDimension optional parameter";
    c
    )
newCell(List) := opt -> cells -> newCell(cells,inferLabel cells,CellDimension=>opt.CellDimension);



isSimplexBoundary := (lst) -> (
    if #lst==0 then return true;
    bdim := dim first lst#0;
    all(lst,isSimplex @@ first) and
    all(lst,i -> dim first i == bdim) and
    (#lst == bdim+2) and
    (length lst == length unique (lst/first)) and
    (isCycle lst)
    )

isSimplex = method(TypicalValue=>Boolean);
isSimplex(Cell) := cell ->
     isSimplexBoundary boundary cell

newSimplexCell = method(TypicalValue=>Cell);
newSimplexCell(List) := (boundary) -> (
    if #boundary!=0 and instance(boundary#0,Cell)
    then return newSimplexCell inferOrientation boundary;
    if not isSimplexBoundary boundary then error "The given boundary is not a valid boundary for a simplex";
    newCell boundary
    )
newSimplexCell(List,Number) :=
newSimplexCell(List,RingElement) :=
newSimplexCell(List,Module) :=
newSimplexCell(List,Ideal) := (boundary,label) -> (
    if #boundary!=0 and instance(boundary#0,Cell)
    then return newSimplexCell(inferOrientation boundary,label);
    if not isSimplexBoundary boundary then error "The given boundary is not a valid boundary for a simplex";
    newCell(boundary,label)
    )

--Relabel function
relabelCellComplex = method(Options=>{InferLabels=>true},TypicalValue=>CellComplex);
relabelCellComplex(CellComplex,HashTable) := o -> (C,T) -> (
    dimC := dim C;
    R := ring C;
    tablecellsbydim := for i to dimC list select(keys T, c -> dim c == i);
    relabeledcells := new MutableHashTable;
    for c in cells(0,C) do relabeledcells#c = (
        if any(tablecellsbydim#0, cell -> cell === c) then newCell({},T#c)
        else newCell({},cellLabel c)
        );
    for i from 1 to dimC do (
        for c in cells(i,C) do (
            newbd := for b in boundaryCells c list relabeledcells#b;
            newlabel := if any(tablecellsbydim#i, cell -> cell === c) then T#c
            else if not o.InferLabels then cellLabel(c)
            else inferLabel(newbd);
            relabeledcells#c = newCell(newbd, newlabel);
            );
        );
    cellComplex(R, flatten values relabeledcells)
    )

RingMap ** CellComplex := (f,c) -> (
    if source f =!= ring c then error "source ring should match label ring";
    R := source f;
    S := target f;
    allCells := flatten values cells(c);
    -- ht := hashTable apply(allCells, c -> (c,f ** toModule(R,cellLabel c)));
    ht := hashTable apply(allCells, c -> (c,f(cellLabel c)));
    tempCellComplex := relabelCellComplex(c,ht);
    cellComplex(S,flatten values cells tempCellComplex)
    )

--Get list of cells
cells = method();
cells(CellComplex) := HashTable => (cellComplex) -> cellComplex.cells
cells(ZZ,CellComplex) := List => (r,cellComplex) -> (
    if cellComplex.cells#?r
    then cellComplex.cells#r
    else {}
    )

skeleton(ZZ,CellComplex) := CellComplex => (n,cellComplex) -> (
    c := new HashTable from select(pairs cellComplex.cells, (k,v) -> k<=n);
    mkCellComplex(cellComplex.labelRing,c,null)
    )

--take a hash table of RingElements/Matrices, and make a matrix, or 0
sparseBlockMap := (codomain,domain,ht) -> (
    ks := keys ht;
    if ks == {} then return map(codomain,domain,0);
    rows := max (ks/first) + 1;
    columns := max (ks/(p->p#1)) + 1;
    maybeHt := p -> (
        if ht#?p then ht#p else 0
        );
    assert(rows <= #components codomain);
    assert(columns <= #components domain);
    map(codomain,domain,matrix apply(#components codomain,i -> apply(#components domain, j -> maybeHt(i,j)))))

--Create one boundary map in the chain complex
boundaryMap(ZZ,CellComplex) := opts -> (r,cellComplex) -> (
    R := cellComplex.labelRing;
    t := r-1;
    rCells := cells(r,cellComplex);
    tCells := cells(t,cellComplex);
    --We define these tables in two steps so that the ordering of the modules in domain and codomain
    --is consistent, especially between calls to boundary with different values for "r".
    domainModules := apply(toList rCells, c-> (c,toModule(R,cellLabel c)));
    codomainModules := apply(toList tCells, c -> (c,toModule(R,cellLabel c)));
    domainModulesTable :=
        new HashTable from domainModules;
    codomainModulesTable :=
        new HashTable from codomainModules;
    domain := if domainModules == {} then R^0 else directSum(apply(domainModules,last));
    codomain := if t==-1 then R^1 else if codomainModules == {} then R^0 else directSum(apply(codomainModules,last));
    tCellsIndexed := new HashTable from toList apply(pairs(tCells),reverse);
    i := 0;
    L := flatten for F in rCells list (
        l := if t==-1
             then (0,i) => inducedMap(codomain,domainModulesTable#F)
             else for p in pairs boundaryTally F list(
                 (cell,deg) := p;
                 if dim cell < dim F - 1 then continue;
                 (tCellsIndexed#cell,i) => deg_R*inducedMap(codomainModulesTable#cell,domainModulesTable#F));
        i = i+1;
        l
        );
    sparseBlockMap(codomain,domain,new HashTable from L)
    );

complex(CellComplex) := {Reduced=>true, Prune=>true} >> o -> (cellComplex) -> (
    if not cellComplex.cache.?complex then (
        cellComplex.cache.complex =
            (complex apply(max((dim cellComplex) + 1,1), r -> boundaryMap(r,cellComplex)))[1]
        );
    ret := if not o.Reduced then (
        Ccopy := complex apply(max cellComplex.cache.complex,
                                    i -> cellComplex.cache.complex.dd_(i+1));
        Ccopy
        )
        else cellComplex.cache.complex;
    if o.Prune then (
        prune ret --how expensive is prune? should it be cached?
        )
    else ret
    );

--Get homology directly from cell complex
homology(ZZ,CellComplex) := opts -> (i,cellComplex) -> (
    homology_i complex(cellComplex)
    );

homology(CellComplex) := opts -> (cellComplex) -> (
    homology complex(cellComplex)
    );

--Get cohomology directly from cell complex
cohomology(ZZ,CellComplex) := opts -> (i,cellComplex) -> (
    cohomology_i Hom(complex(cellComplex),cellComplex.labelRing^1)
    );

----------
---Here there be polyhedra
----------
cellComplex(Ring,Polyhedron) := {Labels => null} >> o -> (R,P) -> (
    if not isCompact P then error "The given polyhedron is not compact.";
    Pdim := dim P;
    Pfaces := applyPairs(faces P, (i,lst) -> (Pdim-i,apply(lst,first)));
    verts := vertices P;
    vertexCells := apply(numColumns verts,
                         if o.Labels =!= null
                         then (n -> newCell({},o.Labels#(verts_n)))
                         else (n -> newCell({})));
    cells := new MutableHashTable;
    for i from 0 to Pdim do (
        for face in Pfaces#i do (
            bd := if i!=0
                  then for f in Pfaces#(i-1) list (if isSubset(f,face) then cells#f else continue)
                  else {};
            cells#face =
                if i==0
                then vertexCells#(face#0)
                else newCell bd;
            );
        );
    cellComplex(R,flatten values cells)
    );

cellComplex(Ring,PolyhedralComplex) := {Labels => null} >> o -> (R,P) -> (
    Pdim := dim P;
    Pfaces := applyPairs(faces P, (i,lst) -> (Pdim-i-1,apply(lst,first)));
    verts := vertices P;
    vertexCells := apply(numColumns verts,
                         if o.Labels =!= null
                         then (n -> newCell({},o.Labels#(verts_n)))
                         else (n -> newCell({})));
    cells := new MutableHashTable;
    for i from 0 to Pdim do (
        for face in Pfaces#i do (
            bd := if i!=0
                  then for f in Pfaces#(i-1) list (if isSubset(f,face) then cells#f else continue)
                  else {};
            cells#face =
                if i==0
                then vertexCells#(face#0)
                else newCell bd;
            );
        );
    cellComplex(R,flatten values cells)
   );

-------------
-- Posets
-------------

facePoset(CellComplex) := (cellComplex) -> (
    G := flatten values cells cellComplex;
    contain := (a,b) -> member(a,boundaryCells b) or a === b;-- a contained or equal b
    P := poset(G,contain);
    rel := allRelations P;
    M := transitiveClosure(G,rel);
    poset(G,rel,M)
    )

-------------
-- Minimality
-------------

--isFree = method(TypicalValue => Boolean);
--check if all the labels are free modules
isFree(CellComplex) := (cellComplex) -> (
    R := cellComplex.labelRing;
    all(flatten values cells cellComplex,c -> isFreeModule prune toModule(R,cellLabel c))
    )

isCellMinimal := (R,cell) -> (
    label := toModule(R,cellLabel cell);
    all(boundary cell, c -> toModule(R,cellLabel first c) != label)
    )

isMinimal = method(TypicalValue => Boolean)
--Check if a labeled cell complex is minimal, Note: we assume the cell complex is free (see isFree)
isMinimal(CellComplex) := (cellComplex) -> (
    R := cellComplex.labelRing;
    all(flatten values cells cellComplex,c -> isCellMinimal(R,c))
    )

subcomplex = method(TypicalValue => CellComplex, Options=>{LabelRing=>null})
subcomplex(CellComplex,RingElement) := o -> (C,m) -> (
    allCells := flatten values cells C;
    R := ring C;
    S := if o.LabelRing =!= null then o.LabelRing else coefficientRing R;
    withNewLabels := apply(allCells, c -> (c,if (m % ideal toModule(R,cellLabel c))==0 then 1_S else 0));
    nontrivialCells := select(withNewLabels, p -> (p#1) != 0);
    finalCells := new MutableHashTable;
    --it is important here that the cells in allCells are sorted by dimension at this point
    for p in nontrivialCells do (
        c := p#0;
        l := p#1;
        newBoundary := apply(boundary c, p -> (finalCells#(p#0),p#1));
        finalCells#c = newCell(newBoundary,l);
        );
    cellComplex(S,values finalCells)
    )

subcomplex(CellComplex,List) := o -> (C,d) -> (
    allCells := flatten values cells C;
    R := ring C;
    S := if o.LabelRing =!= null then o.LabelRing else coefficientRing R;
    withNewLabels := apply(allCells, c -> (c,source basis(d,toModule(R,cellLabel c),SourceRing=>S)));
    nontrivialCells := select(withNewLabels, p -> (p#1) != 0);
    finalCells := new MutableHashTable;
    --it is important here that the cells in allCells are sorted by dimension at this point
    for p in nontrivialCells do (
        c := p#0;
        l := p#1;
        newBoundary := apply(boundary c, p -> (finalCells#(p#0),p#1));
        finalCells#c = newCell(newBoundary,l);
        );
    cellComplex(R,values finalCells)
    )

subcomplex(CellComplex,ZZ) := o -> (C,d) -> subcomplex(C,{d},LabelRing=>o.LabelRing);

CellComplex _ RingElement := (C,m) -> (subcomplex(C,m))
CellComplex _ List := (C,d) -> (subcomplex(C,d))
CellComplex _ ZZ := (C,d) -> (subcomplex(C,{d}))

---------
-- Output
---------

net(Cell) := (cell) -> (
    "Cell of dimension " | (dim cell) | " with label " | (net cellLabel cell)
    )

net(CellComplex) := (cellComplex) -> (
    if hasAttribute (cellComplex, ReverseDictionary) then return getAttribute (cellComplex, ReverseDictionary);
    d := dim cellComplex;
    nTotalCells := #(flatten values cells cellComplex);
    if nTotalCells == 0
    then "empty CellComplex"
    else (
        ("CellComplex over " | (net cellComplex.labelRing) | " of dimension " | d | " with " | nTotalCells | " total cells") ||
        stack(apply(d+1,i -> net cells_i cellComplex)))
    );


------------------------
-- Common cell complexes
------------------------

cellComplexSphere = method(TypicalValue=>CellComplex);
cellComplexSphere(Ring,ZZ) := (R,n) -> (
    if n<0 then error "cellComplexSphere expects a non-negative integer";
    v := newSimplexCell {};
    if n==0 then (
        w := newSimplexCell {};
        cellComplex(R,{v,w})
        )
    else(
        c := newCell({(v,0)},CellDimension=>n);
        cellComplex (R,{c})
        )
    )

cellComplexRPn = method(TypicalValue=>CellComplex);
cellComplexRPn(Ring,ZZ) := (R,n) -> (
    if n<0 then error "cellComplexRPn expects a non-negative integer";
    t := newSimplexCell {};
    if n==0 then return cellComplex(R,{t});
    for i from 1 to n do(
        attachingDegree := if even i then 2 else 0;
        t = newCell {(t,attachingDegree)};
        );
    cellComplex(R,{t})
    )

cellComplexTorus = method(TypicalValue=>CellComplex);
cellComplexTorus(Ring,ZZ) := (R,n) -> (
    if n<0 then error "cellComplexTorus expects a non-negative integer";
    v := newSimplexCell {};
    if n==0 then return cellComplex(R,{v});
    cells := new MutableHashTable;
    for s in subsets(n) do (
        k := #s;
        cells#s = newCell apply(subsets(s,k-1), s' -> (cells#s',0))
        );
    cellComplex(R,{cells#(toList (0..(n-1)))})
    )


----------------------------
-- Specific chain complexes
----------------------------

taylorComplex = method(TypicalValue=>CellComplex);
taylorComplex(MonomialIdeal) := (I) -> (
    gensI := I_*;
    r := #gensI;
    if r == 0 then error "taylorComplex expects a non-zero monomialIdeal";
    cells := new MutableHashTable;
    for i to r-1 do cells#{i} = newSimplexCell({},gensI#i);
    for k from 2 to r do (
        for s in subsets(r,k) do (
            bd := for t in subsets(s,k-1) list cells#t;
            cells#s = newSimplexCell(bd, lcm(gensI_s));
            );
        );
    cellComplex(ring I, {cells#(toList (0..(r-1)))})
    )

scarfComplex = method(TypicalValue=>CellComplex)
scarfComplex(MonomialIdeal) := (I) -> (
    gensI := I_*;
    r := #gensI;
    if r == 0 then error "scarfComplex expects a non-zero monomialIdeal";
    cells := new MutableHashTable;
    dupLabels := new MutableHashTable;
    for i to r-1 do cells#(gensI#i) = newSimplexCell({},gensI#i);
    for k from 2 to r do (
        hasCells := false;
        for s in subsets(r,k) do (
            --boundary cell via the labels
            bdLabels := for t in subsets(s,k-1) list lcm(gensI_t);
            --compute the label of the cell
            m := lcm(gensI_s);
            --figure out if we have a duplicate label, if we do remove any cells with
            --the same label, otherwise add the cell
            if not dupLabels#?m
            then (if cells#?m
                 then (
                     dupLabels#m = true;
                     remove(cells,m))
                 else (
                     incompleteBoundary := false;
                     bd := for label in bdLabels list (
                         if cells#?label then cells#label else (incompleteBoundary = true; break;));
                     if incompleteBoundary then{
                         dupLabels#m = true;
                         continue
                         };
                     hasCells = true;
                     cells#m = newSimplexCell(bd, m)));
            );
        --if there are no cells at dimension k, there won't be any of higher dimension
        --as well as no new labels
        if not hasCells then break;
        );
    cellComplex(ring I, values cells)
    )

hullComplex = method(TypicalValue=>CellComplex);
hullComplex(MonomialIdeal) := (I) -> (
    n := numgens ring I;
    hullComplex((n+1)!+1,I)
    )
hullComplex(ZZ,MonomialIdeal) := (t,I) -> hullComplex(t/1,I)
hullComplex(QQ,MonomialIdeal) := (t,I) -> (
    gensI := I_*;
    R := ring I;
    n := numgens R;
    expvecs := flatten (gensI/exponents);
    verts := for a in expvecs list for i from 0 to (n-1) list t^(a#i);
    P := convexHull(transpose matrix verts, id_(ZZ^n));
    Pdim := dim P;
    Pfaces := new HashTable from select(pairs faces P, (k,v) -> (k <= Pdim and k > 0)); --weird inequalities bc codim
    Pfaces = applyValues(Pfaces, v -> select(v,p -> p#1 == {})); -- selecting compact faces
    Pfaces = applyValues(Pfaces, v -> apply(v,p -> p#0)); --get the vertices for the faces
    Pfaces = applyKeys(Pfaces, d -> Pdim-d); --flipping from codim to dim
    cells := new MutableHashTable;
    for v in Pfaces#0 do cells#v = newCell({},gensI#(v#0));
    for i from 1 to Pdim-1 do (
        for face in Pfaces#i do (
            bd := for f in Pfaces#(i-1) list (if isSubset(f,face) then cells#f else continue);
            cells#face = newCell bd
            );
        );
    cellComplex(R,flatten values cells)
    )

isWellDefined(Cell) := (C) -> (
    R := ring C.label;
    M := toModule(R,C.label);
    if not isSubmodule M then return false;
    --check that the boundary labels are compatible
    --boundary is a list of pairs where the first element is the cell
    if not all(C.boundary, x ->  isSubset(M,toModule(R,(x#0).label))) then return false;
    --check that the boundary is a cycle in homology
    isCycle(C.boundary)
    )

isWellDefined(CellComplex) := (C) -> (
    allCells := flatten values C.cells;
    containingModule := null;
    for cell in allCells do (
        if ring cell.label =!= C.labelRing then return false;
        if not isWellDefined cell then return false;
        M := toModule(C.labelRing,cell.label);
        if containingModule === null then containingModule = ambient M;
        if containingModule != ambient M then return false;
        );
    true
    )

----------------------------

----------------------------


load "./CellularResolutions/doc.m2"
load "./CellularResolutions/tests.m2"

end
