-- Some code for rings involving RR, CC, and poly rings over them
 RCC = CC_53[x,y,z];
 
 p = 0_(coefficientRing RCC)
 phi = map(RCC, coefficientRing RCC)
 phi p 
 assert(phi p == 0_RCC)
 assert(promote(p, RCC) == 0_RCC)
 
 RCC = CC_100[x,y,z];
 p = 0_(coefficientRing RCC)
 phi = map(RCC, coefficientRing RCC)
 phi p
 assert(phi p == 0_RCC)
 assert(promote(p, RCC) == 0_RCC)
 
 