-- -*- coding: utf-8 -*-
newPackage(
	"ReactionNetworks",
    	Version => "1.0", 
    	Date => "June, 2016",
    	Authors => {
	     {Name => "Jane Doe", Email => "doe@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "Reaction networks",
	PackageImports => {"Graphs"},
	AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export {"reactionNetwork", "ReactionNetwork", "Species", "Complexes", "NullSymbol", "NullIndex", "ReactionGraph",
    "stoichiometricSubspace", 
    "steadyStateEquations", "conservationEquations", 
    "laplacian", "FullEdges", "NullEdges" --, "netComplex", "networkToHRF", "glue"
    }
exportMutable {}

-- helper functions for string parsing: better to move these and other non-exports to bottom of file?

removeWhitespace = s -> s = replace(" ", "", s)

-- returns species component of a summand

stripCoef = (s) -> (
    i := 0;
    while regex("[0-9]", s#i) =!= null do i = i+1;
    substring(i,length(s),s)
    )


-- returns coefficient component of a summand
specProportion = (s) -> (
    i := 0;
    while regex("[0-9]", s#i) =!= null do i = i+1;
    value substring(0,i,s)    
    )

-- removes any equationally redundant null symbols from reaction string in HR format
repairReaction = (r, nsym) -> (
    l := select(separateRegexp("\\+", removeWhitespace(r)), s -> s != nsym); 
    r := l#0;
    for i from 1 to length(l)-1 do (
	if match("-|>", last l#(i-1)) then r = concatenate(r,l#i) else r = concatenate(r,concatenate("+", l#i));
    );
    r
    )

-- todo: add functionality for different delimiters

ReactionNetwork = new Type of MutableHashTable

-- todo: 1) other types of input (eg. stoichiometry matrix)
-- 3) check BNF w/ control people, ask about default nullsymbol

reactionNetwork = method(TypicalValue => ReactionNetwork, Options => {NullSymbol => ""})
reactionNetwork String := String => o -> str -> reactionNetwork(separateRegexp(",", str), o)
reactionNetwork List := String => o -> rs -> (
    Rn := new ReactionNetwork from {Species => {}, Complexes => {}, ReactionGraph => digraph {}, NullSymbol => o.NullSymbol, NullIndex => -1};
    scan(rs, r -> addReaction(r,Rn));
    Rn
    )

TEST ///
restart
needs "ReactionNetworks.m2"
NN = reactionNetwork("A --> 2B, A + C --> D, D --> 0", NullSymbol => "0")
NN.Complexes
NN.NullSymbol
///

addSpecies = method()
addSpecies(String, ReactionNetwork) := (s,Rn) -> 
    if not member(s,Rn.Species) then (
	Rn.Species = Rn.Species | {s};
	Rn.Complexes = apply(Rn.Complexes, c -> c | matrix{{0}})
	)

addComplex = method()
addComplex(String, ReactionNetwork) := (c,Rn) -> (
    isNonempty := (c!= Rn.NullSymbol); 
    if isNonempty then (
	species := apply(delete("", separateRegexp("[^((A-Z)|(a-z)_?(0-9)*'?)]", c)), stripCoef);
    	for specie in species do addSpecies(specie, Rn);
	);	
    	-- v is a row vector encoding the monomial corresponding to complex c
    	v := mutableMatrix(ZZ,1,#Rn.Species);
	if isNonempty then (    	
    	    apply(separateRegexp("\\+", c), t -> (
	    	    s:=stripCoef(t);
	    	    a:=specProportion(t);
	    	    if a === null then a = 1;
	    	    i:=position(Rn.Species, s' -> s' == s);
	    	    v_(0,i) = v_(0,i) + a;	  
	    	    ));
    	    );
    v = matrix v;
    if member(v,Rn.Complexes) then position(Rn.Complexes, v' -> v' == v) 
    else (
	Rn.Complexes = Rn.Complexes | {v};
	if not isNonempty then Rn.NullIndex = #Rn.Complexes - 1;
        #Rn.Complexes - 1
	)
    )

addReaction = method()
addReaction(String, ReactionNetwork) := (r,Rn) -> (
    r = repairReaction(removeWhitespace r, Rn.NullSymbol);
    complexes := apply(separateRegexp("(-->)|(<--)|(<-->)|,", r), removeWhitespace);
    if #complexes != 2 then error "Expected two complexes.";
    i := addComplex(first complexes, Rn);
    j := addComplex(last complexes, Rn);
    Rn.ReactionGraph = addVertices(Rn.ReactionGraph, {i,j});
    delim := concatenate separateRegexp(///[A-Z]|[a-z]|[0-9]|,|_|\+| ///, r);
    if delim == "-->" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{i,j}})
    else if delim == "<--" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{j,i}})
    else if delim == "<-->" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{i,j},{j,i}})
    else error "String not in expected format";
    )


netComplex = (Rn,c) -> (
    C := flatten entries Rn.Complexes#c;
    l := apply(#Rn.Species, i -> if C#i == 0 then "" 
	else (if C#i ==1 then "" else toString C#i) | Rn.Species#i);
    l = delete("", l);
    l = between("+", l);
    l = concatenate l;
    if l == "" then l = Rn.NullSymbol;
    l
    )

-- test belows fails when this method inherits from 'merge', not sure why
glue = method()
glue (ReactionNetwork,ReactionNetwork) := (N1, N2) -> (
    assert(N1.NullSymbol == N2.NullSymbol);
    N := copy N1;
    apply(networkToHRF N2, r -> addReaction(r,N));
    N
    );
glue (List, ReactionNetwork) := (L, N) -> glue(reactionNetwork(L, NullSymbol => N.NullSymbol), N)
glue (ReactionNetwork, List) := (N, L) -> glue(N, reactionNetwork(L, NullSymbol => N.NullSymbol))


TEST ///
restart
needsPackage "ReactionNetworks"
NM = reactionNetwork("A <-- 2B, A + C <-- D, B + E --> A + C", NullSymbol => "0")
NN = reactionNetwork("A --> 2B, A + C --> D, D --> B+E", NullSymbol => "0")
-- add another example with nul symbols
needsPackage "Graphs"
glue(NM, NN)
///



-- L is a list of options of the form "A" => "B"
sub(ReactionNetwork, List) := (N, L) -> (
    T := new HashTable from L;
    N.Species = for s in N.Species list (if T#?s then T#s else s);
    N
    )


TEST ///
restart
needs "ReactionNetworks.m2"
NM = reactionNetwork "A <-- 2B, A + C <-- D, B + E --> A + C"
sub(NM, {"A" => "Y"})
N = oneSiteModificationA()
sub(N, {"S_0" => "A"})
///



networkToHRF = N -> apply(edges N.ReactionGraph, e -> netComplex(N, first e) | "-->" | 
    netComplex(N, last e))

net ReactionNetwork := N -> stack networkToHRF N 

stoichiometricSubspace = method()
stoichiometricSubspace ReactionNetwork := N -> (
    C := N.Complexes;
    reactions := apply(edges N.ReactionGraph, e -> C#(last e) - C#(first e));
    M:=reactions#0;
    for i from 1 to #reactions - 1 do M=M||reactions#i;
    mingens ker M
    )


TEST ///
restart
needsPackage "ReactionNetworks"
CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, A+C --> D"
stoichiometricSubspace CRN
netList steadyStateEquations CRN
netList flatten entries laplacian(CRN, QQ)
///

concentration = (species,N,R) -> R_(position(N.Species, s->s==species))
    
termInp = (a,inp,out,N,R) -> if member(a,inp/first) then (     
    p := position(inp/first,x->x==a);
    - last inp#p * product(inp,b->(concentration(first b,N,R))^(last b)) 
    ) else 0
termOut = (a,inp,out,N,R) -> if member(a,out/first) then (     
    p := position(out/first,x->x==a);
    last out#p * product(inp,b->(concentration(first b,N,R))^(last b)) 
    ) else 0

-- helper function for laplacian: partitions ReactionGraph edges according to those which dont contain a null complex
sepEdges = Rn -> (
    seps := new MutableHashTable from {NullEdges => {}, FullEdges => {}};
    apply(edges Rn.ReactionGraph, 
	e -> if (first e =!= Rn.NullIndex) and (last e =!= Rn.NullIndex) then seps.FullEdges = append(seps.FullEdges, e)
	else seps.NullEdges = append(seps.NullEdges,e));
    seps
	)

-- interface can be greatly improved, but this seems to work, and also handles CRNs with NullSymbols
laplacian = (Rn, FF) -> (
    -- step 1) build parameter ring
    n := #Rn.Complexes;
    s := #Rn.Species;
    seps :=  sepEdges Rn;
    kk := symbol kk; 
    K := FF[apply(seps.FullEdges, e -> kk_e)];
    -- step 2) build Y matrix
    Y := (Rn.Complexes)#0;
    for i from 2 to n do  Y = Y || (Rn.Complexes)#(i-1);
    if Rn.NullIndex >= 0 then (
	for i from 0 to n - 1 do (
	     Y = Y_{0..Rn.NullIndex-1 } | matrix(ring Y, {{0}}) | Y_{Rn.NullIndex .. s-1};
	     --tmp := 0;
	     --if member({i, Rn.NullIndex}, seps.NullEdges) then tmp = tmp +1;
	     --Y^{i} = (Y^{i})_{0..Rn.NullIndex -1} | matrix(ring Y, {{tmp}})  | (Y^{i})_{Rn.NullIndex .. s-1}; 
	     );
	);
    -- step 3 buil support monomials
    xx := symbol xx;
    R := K[apply(Rn.Species, s -> xx_(s))];
    x := vars R;
    if Rn.NullIndex >= 0 then x = x_{0..Rn.NullIndex} | {{1}} | x_{Rn.NullIndex + 1 .. numgens R -1};
    mons := apply(1 .. numrows Y, i -> product apply(flatten entries Y^{i-1}, flatten entries x, (a,b) -> b^a));
    mons = substitute(matrix {toList mons}, R);
    Y = substitute(Y, R);
    -- step four, build laplacian matrix
    rates := new MutableList from append(gens K,1);
    tmp := rates#(Rn.NullIndex-1);
    rates#(n-1) = tmp;
    rates#(Rn.NullIndex-1) = 1;
    rates = toList rates;
    L := mutableMatrix(K,n,n);
    for e in seps.FullEdges do L_(toSequence e) = kk_e;
    for e in seps.NullEdges do L_(toSequence e) = 1;
    for i from 0 to n-1 do L_(i,i) = - sum flatten entries matrix L^{i};
    L = substitute(matrix L, R);
    mons*L*Y
    )

steadyStateEquations = method()
steadyStateEquations ReactionNetwork := N -> steadyStateEquations(N,QQ)
steadyStateEquations (ReactionNetwork,Ring) := (N,FF) -> (
    -- K is the parameter ring
    kk := symbol kk; 
    rates := apply(edges N.ReactionGraph, e->kk_e);
    K := FF[rates];
    kk = gens K;
    -- C is a list of pairs (species, input_rate)
    C := apply(N.Species,a->(a,0));
    -- R is a list of reaction equations, formatted ({(specie, coefficient), ... } => {(specie, coefficient), ...}, fwdrate, bckwd rate)
    R := apply(edges N.ReactionGraph, e->(
	    (i,j) := toSequence e;
	    (
		apply(N.Species, flatten entries N.Complexes#i, (s,c)->(s,c)) =>
	    	apply(N.Species, flatten entries N.Complexes#j, (s,c)->(s,c))
		,
		kk#(position(edges N.ReactionGraph,e'->e'==e))
		,
		0
		)  
	    ));
    cc := symbol cc;
    RING := K[apply(C,i->cc_(first i))];
    cc = gens RING;
    F := for i in C list (
	(a,af) := i;
	sum(R,reaction->(
		(inp'out,k1,k2) := reaction;
		r1 := first inp'out;
		r2 := last inp'out;
		k1 * (termInp(a,r1,r2,N,RING) + termOut(a,r1,r2,N,RING)) +
		k2 * (termInp(a,r2,r1,N,RING) + termOut(a,r2,r1,N,RING))
		))  
	)
    )

TEST ///
restart
needsPackage "ReactionNetworks"
CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
F = steadyStateEquations CRN
netList F
///

-- Need to allow for parameters, either random or input by user, to translate the 
-- stoichiometric subspace
conservationEquations = method()
conservationEquations ReactionNetwork := N -> conservationEquations(N,QQ)
conservationEquations (ReactionNetwork,Ring) := (N,FF) -> (
    -- K is the parameter ring
    kk := symbol kk; 
    rates := apply(edges N.ReactionGraph, e->kk_e);
    K := FF[rates];
    kk = gens K;
    -- C is a list of pairs (species, input_rate)
    C := apply(N.Species,a->(a,0));
    cc := symbol cc;
    RING := K[apply(C,i->cc_(first i))];
    cc = gens RING;
    S := stoichiometricSubspace N;
    M := matrix{cc};
    St := flatten entries (M*S);
    St	  
    )

TEST ///
restart 
needsPackage "ReactionNetworks"
needsPackage "Graphs"
N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
CE = conservationEquations N
SSE = steadyStateEquations N
F = join (CE, SSE)
netList F
I = ideal CE 
J = ideal SSE
-- Why can't I and J be combined?  They appear to be in the same ring...
-- ideal F
///


load "ReactionNetworks/motifs-Kisun.m2"
load "ReactionNetworks/motifs-Cvetelina.m2"

-- end

beginDocumentation()
scan({
     "CrosslinkingModelCellDeath.m2",
     "OnesiteModificationA.m2",
     "OnesiteModificationB.m2",
     "OnesiteModificationC.m2",
     "OnesiteModificationD.m2",
     "TwolayerCascadeL.m2",
     "TwositeModificationE.m2",
     "TwositeModificationF.m2",
    "docCHill.m2"
    },
    motif -> load("./ReactionNetworks/"|motif) 
    )
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
uninstallPackage "ReactionNetworks"
installPackage "ReactionNetworks"
installPackage("ReactionNetworks", RemakeAllDocumentation=>true)
check "ReactionNetworks"
peek ReactionNetworks
help "OnesiteModificationA"
viewHelp "OnesiteModificationA"
examples "OnesiteModificationA"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:
