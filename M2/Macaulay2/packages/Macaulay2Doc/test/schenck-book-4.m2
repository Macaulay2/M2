needsPackage "Normaliz"
assert( (A=matrix {{0, 1, 1, 0, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {1, 1, 1, 0, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}}) === map(ZZ^10,ZZ^6,{{0, 1, 1, 0, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {1, 1, 1, 0, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}}) )
assert( (normaliz(A,"normalization")) === 
     new RationalCone from {"gen" => map((ZZ)^11,(ZZ)^6,{{0, 0, 1, 1, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 1, 0, 0, 1}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 1, 1, 0, 0, 0}, {1, 1, 1, 1, 1, 1}}), "inv" => new HashTable from {"" => (1,1,1,1,1,1), "hilbert quasipolynomial denom" => 120, "hilbert basis elements" => 11, "multiplicity denom" => 1, "inhomogeneous" => false, "rank" => 6, "hilbert series num" => (1,4,11,4,1), "graded" => true, "internal index" => 1, "size triangulation" => 18, "hilbert series denom" => (1,1,1,1,1,1), "integrally closed" => false, "grading" => (1,1,1,1,1,1), "number extreme rays" => 10, "sum dets" => 21, "class group" => 1:(16), "degree 1 elements" => 10, "dim max subspace" => 0, "number support hyperplanes" => 22, "external index" => 3, "multiplicity" => 21, "grading denom" => 3, "embedding dim" => 6}}
     )
end

print generateAssertions ///
needsPackage "Normaliz"
A=matrix {{0, 1, 1, 0, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {1, 1, 1, 0, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}}
normaliz(A,1)
///
