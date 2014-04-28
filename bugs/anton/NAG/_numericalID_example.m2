CC[x,y,z]
(   sph := x^2+y^2+z^2-4; 
    f1 := sph*(y^2-x^4);
    f2 := sph*(y-1)*z;
    f3 := sph*(z-1)*z;    )
numericalIrreducibleDecomposition ideal(f1,f2,f3)
end 

ii40 : numericalIrreducibleDecomposition ideal(f1,f2,f3)
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/witness-set.m2:13:144:(3):[13]: error: check failed
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/intersection.m2:89:21:(3):[10]: --back trace--
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/intersection.m2:85:13:(3):[9]: --back trace--
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/intersection.m2:40:34:(3):[9]: --back trace--
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/decomposition.m2:19:24:(3):[6]: --back trace--
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/decomposition.m2:12:32:(3):[6]: --back trace--
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/decomposition.m2:143:81:(3):[3]: --back trace--
/nethome/aleykin3/packagesM2scratch/NumericalAlgebraicGeometry/decomposition.m2:148:5:(3):[3]: --back trace--
