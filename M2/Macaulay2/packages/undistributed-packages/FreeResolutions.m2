newPackage(
        "FreeResolutions",
        Version => "0.1", 
        Date => "Oct 2014",
        Authors => {{Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "Experimental versions of free resolutions"
        )

export {
    -- top level resolution experiments
    "ModuleMonomial",
    "component",
    "monomial",
    "moduleMonomial",
    "Polynomial",
    "PolynomialList",
    "makeFrameFromPolynomials",
    "makeResolutionData",
    "findModuleMonomial",
    "monomialLookup",
    "nextFrame",
    "makeFrames",
    "getImage",
    "ResolutionData",
    "LeadTerm",
    "Coefficients",
    "DescendentRange",
    "getMatrix",
    -- examples
    "AGRExample"
    }

protect Frame
protect RowList
protect MonomialHashTable
protect SPairMatrix

component = method()
monomial = method()
moduleMonomial = method()

ModuleMonomial = new Type of List -- (monomial, component#)
Polynomial = new Type of MutableHashTable -- has
  -- components: LeadTerm, Monomials, Coefficients, Degree, DescendentRange
  --  "Coefficients", "Monomials", "DescendentRange" can all be missing
  -- and in fact the main algorithm's duty is to populate Coefficients, Monomials.
  -- DescendentRange: list of two integer indices into the next PolynomialList if any.
  -- Only set once the next frame has been constructed.

moduleMonomial(RingElement, ZZ) := (f, i) -> new ModuleMonomial from {f,i}
component ModuleMonomial := (m) -> m#1
monomial ModuleMonomial := (m) -> m#0
toString ModuleMonomial := (m) -> toString {m#0, m#1}
-- The following structure contains the data we need to compute one
-- matrix in the non-minimal resolution It contains also the variables
-- needed in the generation of the matrices over the base field kk.
ResolutionData = new Type of MutableHashTable

-- A list of:
--   polynomials forming a GB.
-- OR: partial information, such as the lead term only
-- Format:
--   each polynomial is a pair of lists:
--    a. list of ModuleMonomial's
--    b. the coefficients of each monomial
-- The first monomial is the lead term
-- The first coefficient is ONE (this is a MONIC GB).
PolynomialList = new Type of MutableList

leadTerm PolynomialList := (P) -> (
    for f from 0 to #P-1 list P#f . LeadTerm
    )

net PolynomialList := (P) -> (
    netList for f from 0 to #P-1 list (
        entry := P#f;
        monomstr := if not entry.?Monomials then "(no poly)"
          else (
              monoms := entry.Monomials;
              if #monoms > 5 then (toString(#monoms)|" terms")
              else monoms/toString
              );
        coeffstr := if not entry.?Coefficients then "" else (
                    coeffs := entry.Coefficients;
                    if #coeffs > 5 then ""
                    else coeffs/toString);
        desc := if entry.?DescendentRange then (
            toString (entry.DescendentRange#0)
                    | ".." 
                    | toString (entry.DescendentRange#1-1)
             ) else "--";
        {
        f,
        entry.LeadTerm#0, 
        entry.LeadTerm#1, 
        entry.Degree,
        desc,
        monomstr,
        coeffstr
        }
    ))

makeFrameFromPolynomials = method(TypicalValue=>Sequence) -- of two PolynomialList's
makeFrameFromPolynomials List := (L) -> (
    assert(#L > 0);
    R := ring L#0; -- better not be the empty list!
    result0 := new PolynomialList;
    result0#0 = new MutableHashTable from {
        LeadTerm => moduleMonomial(1_R, 0),
        Monomials => {1_R},
        Coefficients => {1_(coefficientRing R)},
        DescendentRange => {0,#L},
        Degree => 0
        };
    result := new PolynomialList;
    for i from 0 to #L - 1 do (
        ts := terms L#i;
        poly := transpose for t in ts list 
            {moduleMonomial(leadMonomial t, 0), leadCoefficient t};
        result#i = new MutableHashTable from {
            LeadTerm => moduleMonomial(leadMonomial L#i, 0),
            Monomials => poly#0,
            Coefficients => poly#1,
            Degree => first degree L#i
            }
        );
    (result0, result)
    )

nextFrame = method()
nextFrame(PolynomialList,PolynomialList) := (P0,P1) -> (
    -- returns either a PolynomialList P2 for the next level,
    -- or null, if there are no new elements at that level.
    -- Modifies P1, by adding the "DescendentRange" field to each polynomial
    --   that occur as lead term components at the next level.
    P2 := new PolynomialList;
    nextindex := 0;
    for i from 0 to #P0-1 do (
        if P0#i .? DescendentRange then (
            (lo,hi) := toSequence (P0#i . DescendentRange);
            for j from lo+1 to hi-1 do (
                firstindex := nextindex;
                -- idea here: compute the mingens of the ideal quotient
                -- (f_lo, ..., f_(j-1)) : f_j.
                -- Add each of these to the frame P2.
                thisI := monomialIdeal for ell from lo to j-1 list 
                    monomial P1#ell.LeadTerm;
                nextI := thisI : (monomial P1#j.LeadTerm);
                for m in nextI_* do (
                    P2#nextindex = new MutableHashTable from {
                        LeadTerm => moduleMonomial(m, j),
                        Degree => first degree m + P1#j.Degree
                        };
                    nextindex = nextindex+1;
                    );
                P1#j.DescendentRange = {firstindex, nextindex};
                );
            )    
        );
    P2
    )

makeFrames = method()
makeFrames(PolynomialList, PolynomialList) := (P0,P1) -> (
    result := new MutableList from {P0,P1};
    while #result#-1 > 0 do (
        lev := #result-1;
        elapsedTime result#(lev+1) = nextFrame(result#(lev-1), result#lev);
        );
    toList result
    )

makeResolutionData = method()
makeResolutionData(List) := (Ps) -> (
    new ResolutionData from {
        Frame => Ps
        }
    )
ring ResolutionData := (D) -> ring(D.Frame#0#0 . LeadTerm#0)
degrees(ZZ, ResolutionData) := (level, D) -> (
    if level >= #D.Frame then {} else (
        (toList D.Frame#level) / (x -> x.Degree) // unique // sort
    ))
setNextMatrix = method()
setNextMatrix ResolutionData := (D) -> (
    D.RowList = new MutableList;
    D.MonomialHashTable = new MutableHashTable;
    )

monomialLookup = method()
processMonomial = method()
processRow = method()
findModuleMonomial = method()

findModuleMonomial(
    PolynomialList,
    PolynomialList, 
    ModuleMonomial)  := (P0,P1,mon) -> (
    -- mon is a monomial at level P1
    -- find the (canonical) monomial at level P2 which maps to mon (if it exists)
    -- first find the locations
    (m, comp) := toSequence mon;
    if P0#comp.?DescendentRange then (
        (start,end) := toSequence(P0#comp . DescendentRange);
        for j from start to end-1 do (
            n := monomial (P1#j . LeadTerm);
            if m % n == 0 then (
                -- We got one!
                m1 := m // n;
                return moduleMonomial(m1, j);
                );
            );
        );
    null
    )

monomialLookup(ModuleMonomial, ZZ, ResolutionData) := (mon, lev, D) -> (
    -- returns: either an index or null
    -- the index will be into the matrix being constructed.
    -- mon: monomial at level 'lev' (when we are constructing lev+1)
    -- side effects:
    --   mon might be added to a hash table
    --   a Modulemonomial at the next level might be added to the rowList
    --   the internal 'nextIndex' number might be incremented.
    H := D.MonomialHashTable;
    L := D.RowList;
    if not H#?mon then (
        -- need to add mon to H
        -- need to find the corresponding row
        --   and add it to L, assuming mon is in the submodule of initial terms
        val := findModuleMonomial(D.Frame#(lev-1), D.Frame#lev, mon);
        if val === null then (
            H#mon = null;
            return null;
            );
        -- otherwise 'val' is a ModuleMonomial at level 'lev+1'.
        H#mon = #L;
        L#(#L) = val;
        );
    H#mon    
    )

getImage = method()
getImage(ModuleMonomial, ZZ, ResolutionData) := (mon, level, D) -> (
    (m, comp) := toSequence mon;
    prev := D.Frame#(level-1)#comp;
    moduleMonomial(m * prev.LeadTerm#0, prev.LeadTerm#1)
    )

processRow(ModuleMonomial, ZZ, ResolutionData) := (mon,lev,D) -> (
    (m,comp) := toSequence mon;
    thiscomp := D.Frame#(lev-1)#comp;
    (monoms,coeffs) := if thiscomp.?Monomials then (
                (for f in thiscomp.Monomials list
                    monomialLookup(moduleMonomial(m*f#0, f#1), lev-1, D),
                 thiscomp.Coefficients)
            ) else (
                ({ monomialLookup(thiscomp.LeadTerm, lev-1, D) },
                 { 1_(coefficientRing ring D) })
            );
   (monoms, coeffs)
   )

makeMatrix = method()
makeMatrix(ZZ, ZZ, ResolutionData) := (lev, deg, D) -> (
    kk := coefficientRing ring D;
    -- step 0: initialize matrix data in D
    setNextMatrix D;    
    -- step 1: loop through all "spairs" in D.Frame#lev, and call monomialLookup on them
    thesepairs := positions(D.Frame#lev, t -> t.Degree == deg);
    elapsedTime spairs := for i in thesepairs list (
            t := D.Frame#lev#i;
            processRow(t.LeadTerm, lev, D)
            );
    -- step 2: loop through all elements of RowList, do the same, until at end of liast
    r := 0;
    elapsedTime rows := while r < #D.RowList list (
        -- these will have degree 'deg'
        thisrow := processRow(D.RowList#r, lev, D);
        r = r+1;
        thisrow
        );
    -- step 3: at this point, we are ready to construct the matrices, so initialize both
    --   step 3A, optimized: sort the monomials (optional, but probably a good idea?)
    D.Matrix = mutableMatrix(coefficientRing ring D, # D.RowList, #D.RowList);
    D.SPairMatrix = mutableMatrix(coefficientRing ring D, #D.RowList, #thesepairs);
    -- step 4: construct the two matrices
    -- step 4A: construct D.Matrix
    nentries := 0;
    elapsedTime for i from 0 to #rows-1 do (
        (monoms, coeffs) := rows#i;
        for j from 0 to #monoms-1 do
            if monoms#j =!= null then (
                D.Matrix_(monoms#j,i) = coeffs#j;
                nentries = nentries+1;
                );
        );
    << "# of entries in " << numRows D.Matrix << " by " << numRows D.Matrix << " is " << nentries << endl;
    -- step 4B: construct D.SPairMatrix
    nentries = 0;
    elapsedTime for i from 0 to #spairs-1 do (
        (monoms, coeffs) := spairs#i;
        for j from 0 to #monoms-1 do
            if monoms#j =!= null then (
                D.SPairMatrix_(monoms#j,i) = coeffs#j;
                nentries = nentries + 1;
                );
        );
    << "# of entries in " << numRows D.Matrix << " by " << numColumns D.SPairMatrix << " is " << nentries << endl;    
    -- step 5: solve
    << "matrix sizes: " << numColumns D.Matrix << " and " << numColumns D.SPairMatrix << endl;
    elapsedTime X := solve(D.Matrix, D.SPairMatrix);
    -- step 6: put the polynomials back into D.Frame#lev
    --   each spair needs the original lead term.
    elapsedTime for i from 0 to #thesepairs-1 do (
        t := D.Frame#lev#(thesepairs#i);
        -- fill in t.Coefficients, t.Monomials
        t.Monomials = {t.LeadTerm};
        t.Coefficients = {1_kk};
        for j from 0 to #D.RowList-1 do (
            if X_(j,i) != 0 then (
                t.Monomials = append(t.Monomials, D.RowList#j);
                t.Coefficients = append(t.Coefficients, - X_(j,i));
                );
            );
        );
    )

getMatrix = method()
getMatrix(ZZ, ResolutionData) := (level, D) -> (
    ncols := #D.Frame#level;
    nrows := #D.Frame#(level-1);
    M := mutableMatrix(ring D, nrows, ncols);
    for c from 0 to ncols-1 do (
        t := D.Frame#level#c;
        if not t.?Monomials then 
            M_(t.LeadTerm#1, c) = t.LeadTerm#0
        else for i from 0 to #t.Monomials-1 do (
            mon := t.Monomials#i;
            coeff := t.Coefficients#i;
            M_(mon#1, c) = M_(mon#1, c) + coeff * mon#0
            );
        );
    M
    )

getMatrix(ZZ, ZZ, ZZ, ResolutionData) := (level, srcdeg, targetdeg, D) -> (
    cols := positions(D.Frame#level, t -> t.Degree == srcdeg);
    rows := positions(D.Frame#(level-1), t -> t.Degree == targetdeg);
    inv'cols := new MutableList from #cols:null;
    for c from 0 to #cols-1 do inv'cols#(cols#c) = c;
    inv'rows := new MutableHashTable;
    for r from 0 to #rows-1 do inv'rows#(rows#r) = r;
    M := mutableMatrix(ring D, #rows, #cols);
    for c from 0 to #cols-1 do (
        t := D.Frame#level#(cols#c);
        if not t.?Monomials then (
            if rows#?(t.LeadTerm#1) then 
            M_(rows#(t.LeadTerm#1), c) = t.LeadTerm#0
        ) else for i from 0 to #t.Monomials-1 do (
            mon := t.Monomials#i;
            coeff := t.Coefficients#i;
            if inv'rows#?(mon#1) then (
                cp := inv'rows#(mon#1);
                M_(cp, c) = M_(cp, c) + coeff * mon#0
            );
        ));
    M
    )

betti(ResolutionData) := opts -> (D) -> (
    new BettiTally from flatten for level from 0 to #D.Frame -1 list (
        degs := degrees(level,D);
        for d in degs list (
            nindegree := # select(D.Frame#level, t -> t.Degree == d);
            (level, {d}, d) => nindegree
            )
        )
    )



beginDocumentation()

end--

doc ///
Key
  FreeResolutions
Headline
Description
  Text
  Example
Caveat
SeeAlso
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///



-- Example 1
XXXXXXXXXXX
restart
debug needsPackage "FreeResolutions"
R = ZZ/101[a..e]
I = ideal"abc-cde,a2c-b2d,ade-cb2"
J = ideal gens gb I
(P0,P1) = makeFrameFromPolynomials J_*
Ps = makeFrames(P0,P1)

D = makeResolutionData(Ps)
makeMatrix(2,4,D)
makeMatrix(2,5,D)
makeMatrix(2,6,D)
makeMatrix(2,7,D)
makeMatrix(2,8,D)
degrees(3,D)
makeMatrix(3,7,D)
makeMatrix(3,8,D)
makeMatrix(3,9,D)
degrees(4,D)
makeMatrix(4,9,D)
degrees(5,D)
degrees(6,D)

betti D

degrees(1,D)
degrees(2,D)
M1 = getMatrix(1,D)
M2 = getMatrix(2,D)
  M22 = getMatrix(2,4,4,D)
  M22 = getMatrix(2,4,3,D)  
M1 * M2
M3 = getMatrix(3,D)
M2 * M3
M4 = getMatrix(4,D)
M3 * M4



for t in D.Frame#2 do processRow(t#LeadTerm, 2, D)
for r in D.RowList list (
    -- get the row monomials, and coeffs
    -- apply lookup, set the elemnts of the row as appropriate
    )
spairs = for i from 0 to 13 list Ps#2#i . LeadTerm
SP = for sp in spairs list getImage(sp, 2, D)
for sp in SP list monomialLookup(sp, D)

debug FreeResolutions
netList toList D.RowList
peek D.MonomialHashTable

findModuleMonomial(Ps#0,Ps#1,moduleMonomial(b^3*c^3*d,0))

findModuleMonomial(Ps#0,Ps#1,moduleMonomial(a*b^2*c,0))
findModuleMonomial(Ps#0,Ps#1,moduleMonomial(b*c*d*e,0))
findModuleMonomial(Ps#0,Ps#1,moduleMonomial(a^2*b*c,0))
findModuleMonomial(Ps#0,Ps#1,moduleMonomial(b^3*d,0))
findModuleMonomial(Ps#0,Ps#1,moduleMonomial(a^2*d*e,0))

Ps#1
Ps#2
P2 = nextFrame(P0,P1)
P3 = nextFrame(P1,P2)
P4 = nextFrame(P2,P3)
netList leadTerm P1
netList leadTerm P0
net P0
net P1
-- Example 2
YYYYYYYYYYYY
restart
debug needsPackage "FreeResolutions"
load "g16n2.m2"
J = ideal groebnerBasis(I, Strategy=>"F4");
J = ideal sort(gens J, MonomialOrder=>Descending, DegreeOrder=>Ascending);
(P0,P1) = makeFrameFromPolynomials J_*;
elapsedTime Ps = makeFrames(P0,P1);
D = makeResolutionData(Ps);
degrees(2,D)
degrees(3,D)
degrees(4,D)
-- first strand:
elapsedTime makeMatrix(2,3,D);
  rank getMatrix(2,3,3,D)
elapsedTime makeMatrix(3,4,D);
  elapsedTime rank elapsedTime getMatrix(3,4,4,D)
elapsedTime makeMatrix(4,5,D);

elapsedTime makeMatrix(2,4,D);


elapsedTime makeMatrix(3,5,D);


elapsedTime makeMatrix(4,6,D);


inJ = ideal leadTerm gens J
inJ2 = sort(gens inJ, MonomialOrder=>Descending, DegreeOrder=>Ascending)


-------------------
ZZZZZZZZZ
restart
debug needsPackage "FreeResolutions"
kk = ZZ/32003
R = kk[a..f]
I = ideal(e^2-1950*a*f-10835*b*f+19*c*f+6967*d*f+471*e*f+11482*f^2,
    d*e-4153*a*f-14463*b*f+3753*c*f-9438*d*f+1852*e*f-7402*f^2,
    c*e-13313*a*f+7574*b*f+3723*c*f+7768*d*f-2078*e*f-8028*f^2,
    b*e+1562*a*f-5172*b*f+1579*c*f+10666*d*f-14377*e*f+1206*f^2,
    a*e+13953*a*f-13529*b*f+12169*c*f+9295*d*f-3373*e*f-10190*f^2,
    d^2-4296*a*f+1019*b*f-11558*c*f+10583*d*f+14140*e*f-11542*f^2,
    c*d+12549*a*f-7879*b*f+6209*c*f-1679*d*f+12382*e*f+4322*f^2,
    b*d+4799*a*f-14761*b*f+10505*c*f+777*d*f-15307*e*f+7747*f^2,
    a*d+11450*a*f+5277*b*f+1201*c*f-2171*d*f-673*e*f-4936*f^2,
    c^2+1559*a*f-11074*b*f+6744*c*f+11458*d*f-9666*e*f+14902*f^2,
    b*c-7602*a*f-885*b*f-1455*c*f-10716*d*f+15330*e*f-8343*f^2,
    a*c-4035*a*f-11483*b*f-1225*c*f-9754*d*f-5280*e*f-7065*f^2,
    b^2+720*a*f-3277*b*f-1638*c*f-7205*d*f-2605*e*f-13781*f^2,
    a*b-12997*a*f-15389*b*f+1197*c*f+2206*d*f+4151*e*f-15246*f^2,
    a^2+8500*a*f-522*b*f+16001*c*f+11291*d*f+10250*e*f+13604*f^2)
J = ideal sort(gens gb I, MonomialOrder=>Descending, DegreeOrder=>Ascending);
(P0,P1) = makeFrameFromPolynomials J_*;
elapsedTime Ps = makeFrames(P0,P1);
D = makeResolutionData(Ps);
betti D
degrees(1,D)
degrees(2,D)
degrees(3,D)
degrees(4,D)
degrees(5,D)
degrees(6,D)
degrees(7,D) -- nothing here

-- linear strand
elapsedTime makeMatrix(2,3,D);
elapsedTime makeMatrix(3,4,D);
elapsedTime makeMatrix(4,5,D);
elapsedTime makeMatrix(5,6,D);
elapsedTime makeMatrix(6,7,D); -- 0 by 0

-- quadratic strand
elapsedTime makeMatrix(2,4,D);
elapsedTime makeMatrix(3,5,D);
elapsedTime makeMatrix(4,6,D);
elapsedTime makeMatrix(5,7,D);
elapsedTime makeMatrix(6,8,D); -- 0 by 0

getMatrix(2,4,4,D)
rank getMatrix(3,5,5,D)
rank getMatrix(4,6,6,D)
rank getMatrix(5,7,7,D)
rank getMatrix(6,8,8,D)
rank getMatrix(7,9,9,D)

elapsedTime makeMatrix(2,4,D);


getMatrix(2,3,3,D)
getMatrix(3,4,4,D)
rank getMatrix(4,5,5,D)
rank getMatrix(5,6,6,D)

getMatrix(2,4,4,D)

M1 = getMatrix(1,D)
M2 = getMatrix(2,D)
M1 * M2
M3 = getMatrix(3,D)
M2 * M3
M4 = getMatrix(4,D)
M3 * M4

