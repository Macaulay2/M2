CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
V = numericalIrreducibleDecomposition I 

----------------------------------------------
-- Intersection
----------------------------------------------
w1 = last V#1
w2 = first V#2
V = numericalIntersection(w1,w2)
end

i52 : V = numericalIntersection(w1,w2)
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/witness-set.m2:13:144:(3):[9]: error: check failed
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/witness-set.m2:13:144:(3):[9]: --entering debugger (type help to see debugger commands)
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/witness-set.m2:13:144-13:144: --source code:
check WitnessSet := o -> W -> for p in points W do if norm sub(matrix{equations W | slice W}, matrix {p})/norm p > 1000*DEFAULT.Tolerance then error "check failed" 


----- another one
w2 = witnessSet(ideal((x^2+y^2+z^2-1)*(x+y^2+z^3)),2)
(w,w') := toSequence decompose w2
numericalIntersection(w,w')

