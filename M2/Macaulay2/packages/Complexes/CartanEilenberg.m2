--methods for constructing Cartan--Eilenberg resolutions of complexes as
--type FilteredComplex, and the associated spectral sequences

needsPackage "SpectralSequences"

--want to consider spectral sequences associated to vertical/horizontal filtrations of bicomplexes
--so we need a type bicomplex. As implemented this Type is rather barebones; I've added no well-definedness
--checks and haven't even bothered to define a string representing the hash table
Bicomplex = new Type of MutableHashTable --

--since the differentials of a bicomplex are not a morphism of bicomplexes (considering the horizontal/vertical diffs in isolation are),
--I added a type BicomplexDiff when displaying the differentials of a bicomplex
BicomplexDiff = new Type of HashTable

--also added a type bicomplex map (so things like totalization are functorial)
BicomplexMap = new Type of HashTable


Bicomplex.synonym = "bicomplex"
BicomplexDiff.synonym = "bicomplex differentials"
BicomplexMap.synonym = "map of bicomplexes"

--differentials of a double complex will be specified by
--a triple: (i,j,k) where k= 1,2. Tuples of the form
--(i,j,1) = vertical differentials
--(i,j,2) = horizontal differenials
bicomplex = method()
bicomplex HashTable := Bicomplex => maps -> (
    spots := sort keys maps;
    arrows := apply(spots, k -> maps#k);
    if #spots === 0 then
      error "expected at least one map";
    moduleList := new MutableHashTable;
    --print "got here";
    vertspots := select(spots, j -> j_2 == 2);
    horspots := select(spots, j-> j_2 == 1);
    vertmin := min apply(vertspots, j-> j_1);
    if instance(vertmin, InfiniteNumber) then vertmin = (first spots)_1+1;
    vertmax := max apply(vertspots, j-> j_1);
    if instance(vertmax, InfiniteNumber) then vertmax = (first spots)_1;
    hormin  := min apply(horspots,  j-> j_0);
    if instance(hormin, InfiniteNumber) then hormin = (first spots)_0+1;
    hormax  := max apply(horspots,  j-> j_0);
    if instance(hormax, InfiniteNumber) then hormax = (first spots)_0;
    for k in vertspots do (
        if not moduleList#?(k_0,k_1-1)
          then moduleList#(k_0, k_1-1) = target maps#k;
        moduleList#(k_0, k_1) = source maps#k;
        );
    --outlier case: if no vertical diffs
    if #(keys moduleList)==0 then (
	for k in horspots do (
        if not moduleList#?(k_0-1,k_1)
          then moduleList#(k_0-1, k_1) = target maps#k;
        moduleList#(k_0, k_1) = source maps#k;
        );
    );
    --print "got here";
    C := new Bicomplex from {
	   symbol ring => ring arrows#0,
	-- TODO: rename module to category agnostic term
           symbol module => new HashTable from moduleList,
           symbol concentration => ((hormin-1,vertmin-1), (hormax, vertmax)),
           symbol cache => new CacheTable
           };
    C.dd = diffs(C,maps);
    C
    )

ring Bicomplex := B -> B.ring
module Bicomplex := B -> B.module
concentration Bicomplex := B -> B.concentration

--this constructor just builds the differentials of a bicomplex from the above constructor
diffs = method()
diffs(Bicomplex, HashTable) := BicomplexDiff => (bicx, maps) -> (
    R := ring bicx;
    (lo,hi) := bicx.concentration;
    maps' := hashTable for k in keys maps list (
        f := maps#k;
        -- note: we use != instead of =!= in the next 2 tests,
        -- since we want to ignore any term order differences
        if (k_0,k_1) < lo or (k_0,k_1) > hi then continue else (k,f)
        );
    new BicomplexDiff from {
	symbol target => bicx,
	symbol source => bicx,
        symbol map => maps',
        symbol cache => new CacheTable
        }
    )

--pretty clear how this constructor should work: straightforward variant of existing map(Complex, Complex, Blah) code
map(Bicomplex, Bicomplex, HashTable) := BicomplexMap => opts -> (tar, src, maps) -> (
    --R := ring tar;
    --if ring src =!= R or any(values maps, f -> ring f =!= R) then
    --    error "expected source, target and maps to be over the same ring";
    (lo,hi) := src.concentration;
    maps' := hashTable for k in keys maps list (
        f := maps#k;
        -- note: we use != instead of =!= in the next 2 tests,
        -- since we want to ignore any term order differences
        if k < lo or k > hi then continue else (k,f)
        );
    new BicomplexMap from {
        symbol source => src,
        symbol target => tar,
        symbol degree => deg,
        symbol map => maps',
        symbol cache => new CacheTable
        }
    )

map(Bicomplex, Bicomplex, ZZ) := BicomplexMap => opts -> (C,D,n) -> (
    H := hashTable for i in keys D.module list i => map(C_i, D_i, 0);
    map(C,D,H)
    )

--how to access the modules of a bicomplex
Bicomplex _ Sequence := Module => (B,i) -> if B.module#?i then B.module#i else (ring B)^0
--how to access the differentials of a bicomplex
BicomplexDiff _ Sequence := Matrix => (f, i) -> (
    src := f.source;
    trg := f.target;
    if f.map#?i then f.map#i else (
	if i_2 == 1 then map( trg_(i_0-1,i_1), src_(i_0,i_1), 0) else map( trg_(i_0,i_1-1), src_(i_0,i_1), 0)))

--how to access the terms of a morphism of bicomplexes
BicomplexMap _ Sequence := Matrix => (f,i) -> (
    src := f.source;
    trg := f.target;
    if f.map#?i then f.map#i else map( trg_(i_0,i_1), src_(i_0,i_1), 0))

source BicomplexMap := Bicomplex => phi -> phi.source
target BicomplexMap := Bicomplex => phi -> phi.target

--not yet working
-*kernel BicomplexMap := Bicomplex => opts -> f -> (
    -- f : B --> C
    B := source f;
    (lo,hi) := B.concentration;
    modules = hashTable for i in keys B.module list i => kernel f_i;
    print modules;
    result := --if lo === hi then complex(modules#lo, Base => lo)
        --else (
            (inducedMaps := hashTable for i in keys B.module list i => inducedMap(B_i, modules#i);
            maps := hashTable for i in keys B.dd.map list i => (
                if i_2==1 then (B.dd_i * inducedMaps#(i_0,i_1)) // inducedMaps#(i_0-1,i_1) else (B.dd_i * inducedMaps#(i_0,i_1)) // inducedMaps#(i_0,i_1-1)
                );
            bicomplex maps);
           -- );
    --if not isCommutativeCached f and not isWellDefined result then
    --    error "expected differential on the source to induce a well-defined differential on the kernel";
    result.cache.kernel = f;
    result
    )
cokernel BicomplexMap := Complex => f -> (
    -- f : B --> C
    C := target f;
    (lo,hi) := C.concentration;
    deg := degree f;
    modules := hashTable for i from lo to hi list i => cokernel f_(i-deg);
    result := if lo === hi then complex(modules#lo, Base => lo)
        else (
            maps := hashTable for i from lo+1 to hi list i => (
                map(modules#(i-1), modules#i, matrix dd^C_i)
                );
            complex maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the target to induce a well-defined differential on the cokernel";
    result.cache.cokernel = f;
    result
    )

image BicomplexMap := Complex => f -> (
    -- f : B --> C
    B := source f;
    C := target f;
    deg := degree f;
    (lo,hi) := C.concentration;
    modules := hashTable for i from lo to hi list i => image f_(i-deg);
    result :=  if lo === hi then complex(modules#lo, Base => lo)
        else (
            maps := if isCommutativeCached f then (
                hashTable for i from lo+1 to hi list i => (
                    map(modules#(i-1), modules#i, matrix dd^B_(i-deg))
                ))
                else (
                    inducedMaps := hashTable for i from lo to hi list i => inducedMap(C_i, modules#i);
                    hashTable for i from lo+1 to hi list i => (
                        map(modules#(i-1), modules#i, (dd^C_i * inducedMaps#i) // inducedMaps#(i-1))
                        ));
            complex maps
            );
    if not isCommutativeCached f and not isWellDefined result then
        error "expected differential on the target to induce a well-defined differential on the image";
    result.cache.image = f;
    result
    )
*-

--this is the totalization functor. If the input is a bicomplex, the output in the totalization of the bicomplex
--if the input is a morphism of bicomplexes, the output is the induced morphism of totalizations
--if the input is a filtered bicomplex, then the output will be of type FilteredComplex (so we can build spectral sequences)
totalize = method();
totalize(Bicomplex) := Complex => B -> (
    R := ring B;
    (lo,hi) := concentration B;
    modules := hashTable for i from lo_0+lo_1 to hi_0+hi_1 list i => (
        directSum for j from lo_0 to hi_0 list (
            if i-j >= lo_1 and i-j <= hi_1 then
                {j,i-j} => B_(j,i-j)
            else
                continue
            )
        );
    maps := hashTable for i from lo_0+lo_1+1 to hi_0+hi_1 list i => (
        map(modules#(i-1),
            modules#i,
            matrix table(
                indices modules#(i-1),
                indices modules#i,
                (j,k) -> (
                    tar := component(modules#(i-1), j);
                    src := component(modules#i, k);
                    m := map(tar, src,
                        if k-j === {0,1} then (-1)^(k#0) * (B.dd_(k#0,k#1,2))
                        else if k-j === {1,0} then (B.dd_(k#0,k#1,1))
                        else 0);
                    m
                    ))));
    result = complex maps;
    result
    )

totalize(BicomplexMap) := ComplexMap => phi -> (
    -- f : C1 --> C2, g : D1 --> D2
    -- f**g : C1**D1 --> C2**D2
    -- (f**g)_i : sum_j(C1_j ** D1_(i-j) --> C2_(j+df) ** D2_(i-j+dg))
    src := totalize source phi;
    tar := totalize target phi;
    -- for the i-th matrix src_i --> tar_(i+df+dg)
    -- we make a table of matrices, and create a block matrix from that using "matrix" and "map"
    (lo,hi) := src.concentration;
    maps := hashTable for i from lo to hi list i => (
        if tar_i == 0 or src_i == 0 then (
            map(tar_(i), src_i, 0)
            )
        else (
            m := for q in indices tar_(i) list (
                -- so q == {k,i+df+dg-k}
                for p in indices src_i list (
                    -- so p == {j,i-j}, for various j
                    if p#0 == q#0
                    then (
                        sgn := 1; -- function of df, dg, i
                        sgn = 1;
                        sgn * phi_(p#0,p#1)
                        )
                    else map(component(tar_(i), q),
                        component(src_i, p),
                        0)
                    ));
            map(tar_(i), src_i, matrix m)
            )
        );
    result := map(tar, src, maps);
    result
    )

totalize(FilteredBicomplex) := FilteredComplex => F -> (
    bcxes := for i in keys F.inclusions list F.inclusions#i;
    filteredComplex apply(bcxes, j -> chainComplex imageInclusion totalize j)
    )

--returns the complex in the dth column of a bicomplex B
verticalComplex = method()
verticalComplex(ZZ, Bicomplex) := Complex => (d,B) -> (
    dthMapKeys := select(keys B.dd.map, j -> j_0==d and j_2==2);
    lo := min apply(dthMapKeys, j -> j_1) - 1;
    maps :=  for i in dthMapKeys list B.dd.map#i;
    complex(maps, Base => lo)
    )

--returns the complex in the dth row of a bicomplex B
horizontalComplex = method()
horizontalComplex(ZZ, Bicomplex) := Complex => (d,B) -> (
    dthMapKeys := select(keys B.dd.map, j -> j_1==d and j_2==1);
    lo := min apply(dthMapKeys, j -> j_0) - 1;
    maps :=  for i in dthMapKeys list B.dd.map#i;
    complex(maps, Base => lo)
    )

--this outputs the filtered complex induced by the horizontal filtration of a bicomplex
horizontalFiltration = method()
horizontalFiltration(Bicomplex) := FilteredBicomplex => B -> (
    (lo, hi) := concentration B;
    L := for i from lo_0 to lo_1 list dthHorizontalInclusion(i,B);
    filteredBicomplex L
    )

--helper function for the above
dthHorizontalSubbicomplex = method();
dthHorizontalSubbicomplex(ZZ, Bicomplex) := Bicomplex => (d,B) -> (
    H := hashTable for i in keys B.dd.map list i => (
	if i_0 <= d and i_2==1 then map((ring B)^0, (ring B)^0, 0)
	else if i_0 <= d and i_2==2 then map((ring B)^0, (ring B)^0, 0)
	else if i_0==d+1 and i_2==2 then map((ring B)^0, B_(i_0,i_1), 0)
	else B.dd_i
	);
    bicomplex H
    )

--helper function for above
dthHorizontalSubbicomplex = method();
dthHorizontalSubbicomplex(ZZ, Bicomplex) := Bicomplex => (d,B) -> (
    H = hashTable for i in keys B.dd.map list i => (
	if i_0 > d and i_2==1 then continue
	else if i_0 >= d+1 and i_2==2 then continue
	else if i_0==d+1 and i_2==2 then continue
	else B.dd_i
	);
    bicomplex H
    )


dthHorizontalInclusion = method();
dthHorizontalInclusion(ZZ, Bicomplex) := Bicomplex => (d,B) -> (
    Bd := dthHorizontalSubbicomplex(d, B);
    H := hashTable for i in keys Bd.module list i => id_(B_i);
    map(B, Bd, H)
    )

dthVerticalSubbicomplex = method();
dthVerticalSubbicomplex(ZZ, Bicomplex) := Bicomplex => (d,B) -> (
    H = hashTable for i in keys B.dd.map list i => (
	if i_1 > d and i_2==2 then continue
	else if i_1 >= d+1 and i_2==1 then continue
	else if i_1==d+1 and i_2==1 then continue
	else B.dd_i
	);
    bicomplex H
    )

dthVerticalInclusion = method();
dthVerticalInclusion(ZZ, Bicomplex) := Bicomplex => (d,B) -> (
    Bd := dthVerticalSubbicomplex(d, B);
    H := hashTable for i in keys Bd.module list i => id_(B_i);
    map(B, Bd, H)
    )

--new type: FilteredBicomplex. The intent is that we can totalize these objects to
--obtain filtered complexes, then use the SpectralSequences package to construct induced spectral sequences
FilteredBicomplex = new Type of MutableHashTable
FilteredBicomplex.synonym = "filtered bicomplex"

--this type is very barebones: the input is just a list of BicomplexMaps
filteredBicomplex = method()
filteredBicomplex(List) := FilteredBicomplex => L -> (
  maps := L;
  moduleList := for i from 0 to #L-1 list i =>  source L_i;
  --Z := image map(C, C,0); -- make zero subcomplex as a subcomplex of ambient complex
  maps' := hashTable for i from 0 to #maps - 1 list i => maps_i;
  C := new FilteredBicomplex from {
	   symbol ring => ring source maps#0,
	-- TODO: rename module to category agnostic term
           symbol bicomplexes => new HashTable from moduleList,
	   symbol inclusions => maps',
           symbol concentration => (0, #maps),
           symbol cache => new CacheTable
           };
   C
   )

--given a morphism of complexes, this outputs the induced inclusion
--of the image into the target
imageInclusion = method()
imageInclusion(ComplexMap) := ComplexMap => phi -> (
    D := image phi;
    canonicalMap(target D.cache.image, D)
    )

--given two complexes C and D, outputs the induced tensor bicomplex.
--the totalization of this complex is isomorphic to the tensor product
tensorBicomplex = method()
tensorBicomplex(Complex, Complex) := Bicomplex => (C,D) -> (
    (loC, hiC) := concentration C;
    (loD, hiD) := concentration D;
    L1 := new MutableList from {};
    for i from loC+1 to hiC do (
	for j from loD to hiD do (
	    L1#(#L1) = (i,j,1) => C.dd_i**D_j;
	    );
	);
    L2 := new MutableList from {};
    for i from loC to hiC do (
	for j from loD+1 to hiD do (
	    L2#(#L2) = (i,j,2) => C_i**D.dd_j;
	    );
	);
    H := hashTable ((toList L1)|(toList L2));
    bicomplex H
    )



--constructs resolution of i-cycles from resolutions of homology and boundaries
horseshoeHomologyRes = method()
horseshoeHomologyRes(Complex, ZZ) := Complex => (C,i) -> (
    iota = inducedMap(ker C.dd_i, image C.dd_(i+1));
    p = inducedMap(coker iota, target iota);
    horseshoeResolution complex {p, iota}
    )

--not yet written
-*
doubleHorseshoe = method()
doubleHorseshoe(Complex, ZZ) := Complex => (C, i) -> (
*-
