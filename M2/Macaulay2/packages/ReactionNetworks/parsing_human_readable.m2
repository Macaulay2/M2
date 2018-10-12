restart
-- errorDepth = 0 
needs "ReactionNetworks.m2"
needsPackage "Graphs"
-- string in "human-readable format"
string = "A_0 <--> 2b, A + C <--> D, B_6 + E_9 --> A + C, D --> B + E"
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

S = "222S_0"
CRN.Species
addSpecies(s,CRN)
c = "2S_0 + A + 55B_3 + d"
separateRegexp("", S)
delete("", separateRegexp("[^(((A-Z)|(a-z))_?(0-9)*)]", c))
s

s = toString "222S_2"
stripCoef(s)
while i=1 do print "hello";
  i=0



length(S)

addSpecies("D_3", CRN)
Rn = CRN
ts = apply(separateRegexp("\\+", c), removeWhitespace)

c
