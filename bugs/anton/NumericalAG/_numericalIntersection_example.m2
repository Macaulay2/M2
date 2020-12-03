CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
V := numericalIrreducibleDecomposition I 

----------------------------------------------
-- Intersection
----------------------------------------------
w1 := last V#1
w2 := first V#2
V12 := numericalIntersection(w1,w2)
end


i30 : V12 := numericalIntersection(w1,w2)

o30 = a numerical variety with components in

o30 : NumericalVariety
