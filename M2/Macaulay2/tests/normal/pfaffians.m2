R = ZZ/101[a..f]
m = genericSkewMatrix(R,a,4)
assert( pfaffians(-2,m) == ideal(0_R) )
assert( pfaffians(0,m) == ideal(1_R) )
assert( pfaffians(1,m) == ideal(0_R) )
assert( pfaffians(2,m) == ideal(a,b,c,d,e,f) )
assert( pfaffians(3,m) == ideal(0_R) )
assert( pfaffians(4,m) == ideal(c*d-b*e+a*f) )


R = QQ[vars(0..9)]
M = genericSkewMatrix(R,5)

assert (pfaffians(-1,M) == 0)
assert (pfaffians(0,M) == ideal(1_R))
assert (pfaffians(1,M) == 0)
assert (gens pfaffians(2,M) == matrix{{a,b,e,c,f,h,d,g,i,j}})
assert (pfaffians(3,M) == 0)
assert (gens pfaffians(4,M) == matrix{{c*e-b*f+a*h,d*e-b*g+a*i,d*f-c*g+a*j,d*h-c*i+b*j,g*h-f*i+e*j}})
assert (pfaffians(5,M) == 0)
assert (pfaffians(6,M) == 0)

-- Now use sort of random elements
S = ZZ/32003[x,y,z]
sz = 5
lins = {
    -3406*x+2450*y-8791*z,
    -5366*x+14628*y-15656*z,
    -914*x+15059*y+12363*z,
    -9200*x-8011*y-7982*z,
    14983*x-5580*y+11247*z,
    3110*x-14469*y+4958*z,
    -1036*x+12495*y+14730*z,
    3054*x-2311*y+7682*z,
    9813*x-6378*y+1425*z,
    5424*x-14870*y+5194*z,
    718*x+11248*y-519*z,
    -7462*x+6727*y-8077*z,
    -2433*x+11840*y+7349*z,
    1670*x-1788*y+1945*z,
    7812*x-5082*y+5237*z,
    -8171*x+13702*y+695*z,
    1912*x-13298*y-13788*z,
    12425*x+14430*y+1312*z,
    6104*x+2714*y-15292*z,
    -2718*x-13687*y-12324*z,
    -2994*x-6767*y+6205*z,
    -5778*x-10737*y+6215*z,
    4851*x+2988*y+15428*z,
    -2716*x-10557*y-4168*z,
    -15643*x+9958*y-7724*z,
    6590*x-450*y+6367*z,
    -8558*x+7832*y+4351*z,
    -12205*x+4820*y+8179*z,
    -10377*x+10537*y+8637*z,
    5527*x+15107*y-3285*z,
    -8168*x-8688*y+6997*z
    }
nextlin = 0;
M = mutableMatrix(S, sz, sz)
for i from 0 to sz-1 do for j from i+1 to sz-1 do (
     F = lins#nextlin;
     nextlin = nextlin+1;
     M_(i,j) = F;
     M_(j,i) = -F;
     )
M = matrix M
ans = matrix {{-15433*x^2-6577*x*y-2071*y^2+2764*x*z+2601*y*z+2667*z^2, 
     -9279*x^2+5727*x*y+8829*y^2-13213*x*z+7920*y*z+12124*z^2, 
     3455*x^2+3495*x*y-358*y^2+6789*x*z-3541*y*z+10862*z^2, 
     -4381*x^2-9380*x*y-4986*y^2+14490*x*z+10306*y*z-13242*z^2, 
     -3043*x^2+3425*x*y-5048*y^2+4186*x*z-15712*y*z+12208*z^2}}
assert(gens pfaffians(4,M) == ans)

lins = {
    0,
    x,
    0,
    0,
    y,
    0,
    z,
    3054*x-2311*y+7682*z,
    0,
    5424*x-14870*y+5194*z,
    718*x+11248*y-519*z,
    -7462*x+6727*y-8077*z,
    -2433*x+11840*y+7349*z,
    1670*x-1788*y+1945*z,
    7812*x-5082*y+5237*z,
    -8171*x+13702*y+695*z,
    1912*x-13298*y-13788*z,
    12425*x+14430*y+1312*z,
    6104*x+2714*y-15292*z,
    -2718*x-13687*y-12324*z,
    -2994*x-6767*y+6205*z,
    -5778*x-10737*y+6215*z,
    4851*x+2988*y+15428*z,
    -2716*x-10557*y-4168*z,
    -15643*x+9958*y-7724*z,
    6590*x-450*y+6367*z,
    -8558*x+7832*y+4351*z,
    -12205*x+4820*y+8179*z,
    -10377*x+10537*y+8637*z,
    5527*x+15107*y-3285*z,
    -8168*x-8688*y+6997*z
    }
nextlin = 0;
M = mutableMatrix(S, sz, sz)
for i from 0 to sz-1 do for j from i+1 to sz-1 do (
     F = lins#nextlin;
     nextlin = nextlin+1;
     M_(i,j) = F;
     M_(j,i) = -F;
     )
M = matrix M
assert(gens pfaffians(4,M) == matrix {{-x*z, 5424*x^2-14870*x*y+5194*x*z, 5424*x*y-14870*y^2+3054*x*z+2883*y*z+7682*z^2}})

-- larger pfaffians
R = QQ[vars(0..100)]
M = genericSkewMatrix(R,9);
M8 = pfaffians(8,M);
assert (numgens M8 == 9)
assert (degrees source gens M8 == toList(9:{4}))
assert(M8_*/size//unique == {105})
assert(pfaffians(7,M) == 0)

M = genericSkewMatrix(R,12)
time M12 = pfaffians(12,M);
assert(size M12_0 == 10395)
