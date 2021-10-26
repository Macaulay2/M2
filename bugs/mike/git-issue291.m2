R = ZZ[x_1..x_3, MonomialOrder=>{RevLex}, Global =>false ]

I = ideal(-2+3*x_1^3,-1+2*x_1^2)

gI = ideal gens gb I;
assert( ( (gens I)%gI) == 0 );
ggI = ideal gens gb gI;

assert(numColumns (gens gI) == numColumns(gens ggI));
assert( leadingTermsEquivalent( gI, ggI ) ) ;
gI
ggI


-- #292
gbTrace=3
R = ZZ[x_1..x_3, MonomialOrder=>{RevLex}, Global =>false ]
I = ideal(-2+9*x_1^3,-1+3*x_1^2)
gI = ideal gens gb I; -- takes a lot of time, probably does not finish
