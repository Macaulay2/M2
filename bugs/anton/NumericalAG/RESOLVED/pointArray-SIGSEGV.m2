needsPackage "NumericalAlgebraicGeometry"
load (currentFileDirectory|"PointArray.m2")
end


restart  -- PointArray crashes
load "pointArray-SIGSEGV.m2"  
setDefault(Software=>PHCPACK)

b = point{{.781262-.624203*ii, -.462954-.886382*ii, -.272832+.962062*ii, -.670948-.741504*ii, .9868+.161945*ii}}
A = pointArray {b} 
