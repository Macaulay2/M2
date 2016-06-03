removeWhitespace = s -> s = replace(" ", "", s)

-- string in "human-readable format"
string = "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"

-- process input string fo reactions, species, and complexes
string = removeWhitespace(string)
reactions = separateRegexp(",", string)
species = delete("", unique separateRegexp("[^A-Z]", string))
complexes = unique apply(separateRegexp("(-->)|(<--)|(<-->)|,", string), removeWhitespace)

help "new"

restart
needsPackage "Graphs"
ReactionNetwork = new Type from MutableHashTable

addReaction

methods digraph
reactionNetwork = method()
reactionNetwork := () -> new ReactionNetwork from {Species => {}, Complexes => {}, ReactionGraph => digraph {}}
reactionNetwork List := rs -> (
    Rn := reactionNetwork();
    scan(rs, r -> addReaction(r,Rn));
    Rn
    )

addReaction = method()
addReaction(String, ReactionNetwork) := (r,Rn) -> (
    
    )
 
 
 
-- now build the hash table
CRN = new ReactionNetwork from {
    Species => {"A","B","C","D","E"}, 
    Complexes => {matrix {{1,0,0,0,0}}, matrix {{0,2,0,0,0}}, matrix {{1,0,1,0,0}}, 
	          matrix {{0,0,0,1,0}}, matrix {{0,1,0,0,1}}},
    ReactionGraph => digraph({{0,1}, {1,0}, {2,3}, {3,2}, {4,2}, {3,4}}, EntryMode => "edges")
    }


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

help stack
CRN 
CRN.ReactionGraph


needsPackage "Graphs"
    help(digraph)

complexes

-- what I'd like to do: 
-- 1) initialize ReactionGraph as a mutable hash table whose keys comprise the list complexes,
-- 2) for each complex, scan the list reactions and append to its list of values 'edges' of the form 
-- (neighboring complex, fwd_rate, bkwd_rate)  where each rate is either a new parameter or zero, 
-- as determined by the corresponding delimiter (-->, <--, or <-->)

scan(complexes, complex -> ReactionGraph#complex = { })
keys reactionGraph

help MutableHashTable

-- not current correct, should move through heads one at at time and only select those tails which count
stripped = apply(reactions, reaction -> separateRegexp("[--]", reaction))
fwds = apply(stripped, reaction -> match(">", reaction#2))
revs = apply(stripped, reaction -> match("<", reaction#0))
heads = apply(stripped, reaction -> concatenate separateRegexp("[^A-Z..+]", reaction#0))
tails = apply(stripped, reaction -> concatenate separateRegexp("[^A-Z..+]", reaction#2))

for i from 0 to length(heads)-1 do( append(ReactionGraph#(heads#i), tails#i))
keys ReactionGraph#"A"
for i from 0 to length(heads)-1 do( append(ReactionGraph#(tails#i), {heads#i}))
heads
tails
heads#i in keys ReactionGraph
ReactionGraph#"A+C"
describe ReactionGraph

reactions
