C = ZZ/101[x,y,z,u,w];
I = ideal(x-x^2-y,z+x*y,w^2-u^2);
minPres I
I.cache.minimalPresentationMap
I.cache.minimalPresentationMapInv
