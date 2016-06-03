removeWhitespace = s -> s = replace(" ", "", s)

-- string in "human-readable format"
string = "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"

-- process input string fo reactions, species, and complexes
string = removeWhitespace(string)
reactions = separateRegexp(",", string)
species = delete("", unique separateRegexp("[^A-Z]", string))
complexes = unique apply(separateRegexp("(-->)|(<--)|(<-->)|,", string), removeWhitespace)
complexes

-- now build the hash table
ReactionGraph = new MutableHashTable

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
