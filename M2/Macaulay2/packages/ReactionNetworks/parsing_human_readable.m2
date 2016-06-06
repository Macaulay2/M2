restart
-- errorDepth = 0 
needs "ReactionNetworks.m2"
-- string in "human-readable format"
string = "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"
reactions = separateRegexp(",", string)
CRN = reactionNetwork reactions
peek CRN 
 
-- now build the hash table
CRN = new ReactionNetwork from {
    Species => {"A","B","C","D","E"}, 
    Complexes => {matrix {{1,0,0,0,0}}, matrix {{0,2,0,0,0}}, matrix {{1,0,1,0,0}}, 
	          matrix {{0,0,0,1,0}}, matrix {{0,1,0,0,1}}},
    ReactionGraph => digraph({{0,1}, {1,0}, {2,3}, {3,2}, {4,2}, {3,4}}, EntryMode => "edges")
    }

