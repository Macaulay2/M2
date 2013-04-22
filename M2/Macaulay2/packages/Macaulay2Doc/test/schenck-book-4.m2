needsPackage "Normaliz"
assert( (A=matrix {{0, 1, 1, 0, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {1, 1, 1, 0, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}}) === map(ZZ^10,ZZ^6,{{0, 1, 1, 0, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {1, 1, 1, 0, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}}) )
assert( (normaliz(A,1)) === 
     new RationalCone from {"gen" => map((ZZ)^11,(ZZ)^6,{{1, 0, 0, 1, 0, 1}, {1, 0, 0, 0, 1, 1}, {1, 1, 0, 1, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 1, 0, 0, 0},
      {0, 1, 0, 0, 1, 1}, {0, 1, 0, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 1, 1, 0, 0, 1}, {1, 1, 1, 1, 1, 1}}), "inv" => new HashTable from
      {"" => (1,1,1,1,1,1), "hilbert basis elements" => 11, "multiplicity denom" => 1, "rank" => 6, "hilbert series num" => (1,4,11,4,1), "graded" => true,
      "size triangulation" => 18, "hilbert series denom" => (1,1,1,1,1,1), "index" => 1, "number extreme rays" => 10, "grading" => (1,1,1,1,1,1), "sum dets"
      => 21, "degree 1 elements" => 10, "number support hyperplanes" => 22, "grading denom" => 3, "multiplicity" => 21}}
)

end

print generateAssertions ///
needsPackage "Normaliz"
A=matrix {{0, 1, 1, 0, 0, 1}, {0, 0, 1, 1, 1, 0}, {0, 0, 1, 1, 0, 1}, {0, 1, 0, 1, 1, 0}, {0, 1, 0, 0, 1, 1}, {1, 1, 1, 0, 0, 0}, {1, 0, 1, 0, 1, 0}, {1, 1, 0, 1, 0, 0}, {1, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 1}}
normaliz(A,1)
///
