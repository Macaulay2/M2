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
export {"reactionNetwork", "ReactionNetwork", "Species", "Complexes", "ReactionGraph"}
exportMutable {}

removeWhitespace = s -> s = replace(" ", "", s)

ReactionNetwork = new Type of MutableHashTable

reactionNetwork = method()
reactionNetwork List := rs -> (
    Rn := new ReactionNetwork from {Species => {}, Complexes => {}, ReactionGraph => digraph {}};
    scan(rs, r -> addReaction(r,Rn));
    Rn
    )

addSpecies = method()
addSpecies(String, ReactionNetwork) := (s,Rn) -> 
    if not member(s,Rn.Species) then (
	Rn.Species = Rn.Species | {s};
	Rn.Complexes = apply(Rn.Complexes, c -> c | matrix{{0}})
	)
    

addComplex = method()
addComplex(String, ReactionNetwork) := (c,Rn) -> (
    species := delete("", separateRegexp("[^A-Z]", c));
    for specie in species do addSpecies(specie, Rn);
    v := mutableMatrix(ZZ,1,#Rn.Species);	
    apply(separateRegexp("\\+", c), t -> (
	    s:=concatenate separateRegexp("[0-9]", t);
	    a:=value concatenate separateRegexp("[A-Z]", t);
	    if a === null then a = 1;
	    i:=position(Rn.Species, s' -> s' == s);
	    v_(0,i) = v_(0,i) + a;
	    ));
    v = matrix v;
    if member(v,Rn.Complexes) then position(Rn.Complexes, v' -> v' == v) 
    else (
	Rn.Complexes = Rn.Complexes | {v};	
        #Rn.Complexes - 1
	)
    )

addReaction = method()
addReaction(String, ReactionNetwork) := (r,Rn) -> (
    r = removeWhitespace r;
    complexes := apply(separateRegexp("(-->)|(<--)|(<-->)|,", r), removeWhitespace);
    if #complexes != 2 then error "Expected two complexes.";
    i := addComplex(first complexes, Rn);
    j := addComplex(last complexes, Rn);
    Rn.ReactionGraph = addVertices(Rn.ReactionGraph, {i,j});
    delim := concatenate separateRegexp(///[A-Z]|[0-9]|,|\+| ///, r);
    if delim == "-->" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{i,j}})
    else if delim == "<--" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{j,i}})
    else if delim == "<-->" then Rn.ReactionGraph = addEdges'(Rn.ReactionGraph, {{i,j},{j,i}})
    else error "String not in expected format";
    )
 


netComplex = (r,c) -> (
    C := flatten entries r.Complexes#c;
    l := apply(#r.Species, i -> if C#i == 0 then "" 
	else (if C#i ==1 then "" else toString C#i) | r.Species#i);
    l = delete("", l);
    l = between("+", l);
    concatenate l
    )

net ReactionNetwork := r -> stack apply(edges r.ReactionGraph, e -> netComplex(r, first e) | "-->" | 
    netComplex(r, last e))

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
    "TwositeModificationF.m2"
    },
    motif -> load("./ReactionNetworks/"|motif) 
    )
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
installPackage "ReactionNetworks"
installPackage("ReactionNetworks", RemakeAllDocumentation=>true)
check ReactionNetworks
help "OnesiteModificationA"
viewHelp "OnesiteModificationA"
examples "OnesiteModificationA"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:
