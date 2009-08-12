-- from email of Dan, 4/30/09

a = symbol a
R = QQ[a_1 .. a_3, Weights => {{1,1,1},{0,1,1},{0,0,1}}, Degrees => {1,2,3}];
F = a_3 + a_1 * a_2
assert(leadTerm F == a_1 * a_2)
S = QQ[a_1 .. a_3, Weights => {{1,1,1},{0,1,1},{0,0,1}}, Degrees => {1,1,1}];
G = a_3 + a_1 * a_2
assert(leadTerm G == a_1 * a_2)
