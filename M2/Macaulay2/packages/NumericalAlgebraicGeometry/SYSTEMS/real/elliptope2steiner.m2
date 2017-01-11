-- exponential varieties, July 2015
restart
load "SLP-expressions.m2"
load "NAGtools.m2"
m = 4
for i from 1 to m do z_i = inputGate symbol z_i 
f = sum apply(subsets(4,3), a->product apply(a,i->z_(i+1)))
zs = apply(m, i->z_(i+1))
-- F = transpose matrix{apply(zs, z->z*diff(z,f)/f)}
F = transpose matrix{apply(zs, z->diff(z,f))}
PHS = gateHomotopy4preimage(F,zs,zs)
x0 = transpose matrix{toList(m:1_CC)}
y0 = value(F,hashTable(apply(zs,flatten entries x0,(i,j)->(i=>j))|{cache=>new CacheTable}))
dF = value(diff(matrix{zs},F),hashTable(apply(zs,flatten entries x0,(i,j)->(i=>j))|{cache=>new CacheTable}))
det dF

RR[Z_1..Z_m]
E = polySystem apply(3, i->sum apply(subsets(4,i+1), a->product apply(a,i->Z_(i+1))));
while true do (
    while true do(
    	x1 = random(RR^4,RR^1) - transpose matrix{toList(m:0.5)};
    	if all(flatten entries evaluate(E,x1), a->a>0) then (
	    y1 = transpose evaluate((jacobian E)^{2},x1);
    	    break	    
	    )
	);
    print (transpose x1=>transpose y1);
    HS = specialize(PHS,y0||y1);
    time s = first trackHomotopy(HS, {x0}, Software=>M2);
    print status s;
    -- if status s =!= Regular then break 
    )
peek s
x1 = transpose matrix s
assert areEqual(value(F,hashTable(apply(zs,flatten entries x1,(i,j)->(i=>j))|{cache=>new CacheTable})), y1)
