removeWhitespace = s -> s = replace(" ", "", s)
needsPackage "Graphs"

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
    delim = concatenate separateRegexp(///[A-Z]|[0-9]|,|\+| ///, r);
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