needsPackage "Polyhedra"
assert( (M= matrix{{1,0,0},{0,1,0},{1,0,1},{0,1,1}}) === map(ZZ^4,ZZ^3,{{1, 0, 0}, {0, 1, 0}, {1, 0, 1}, {0, 1, 1}}) )
assert( net (C=posHull transpose M) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 4
 number of rays => 4"^0 ) -- toExternalString fails
assert( (rays C) === map(ZZ^3,ZZ^4,{{1, 0, 1, 0}, {0, 1, 0, 1}, {0, 0, 1, 1}}) )
assert( (fVector C) === {1,4,4,1} )
assert( net (Cv = dualCone C) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 4
 number of rays => 4"^0 ) -- toExternalString fails
assert( (rays Cv) === map(ZZ^3,ZZ^4,{{1, 0, 1, 0}, {0, 1, 1, 0}, {0, 0, -1, 1}}) )
assert( (hilbertBasis C) === {map(ZZ^3,ZZ^1,{{1}, {0}, {0}}),map(ZZ^3,ZZ^1,{{0}, {1}, {0}}),map(ZZ^3,ZZ^1,{{1}, {0}, {1}}),map(ZZ^3,ZZ^1,{{0}, {1}, {1}})} )
assert( net (C1 = posHull transpose matrix {{0,-1,0},{0,0,1},{ -1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (C2 = posHull transpose matrix {{0,-1,0},{0,0,-1},{ -1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (C3 = posHull transpose matrix {{0,-1,0},{0,0,1},{1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (C4 = posHull transpose matrix {{0,-1,0},{0,0,-1},{1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (C5 = posHull transpose matrix {{0,1,0},{0,0,1},{ -1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (C6 = posHull transpose matrix {{0,1,0},{0,0,-1},{ -1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (C7 = posHull transpose matrix {{0,1,0},{0,0,-1},{1,0,0}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B1 = posHull transpose matrix {{1,0,0},{0,0,1},{1,1,2}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B2 = posHull transpose matrix {{1,0,0},{0,1,0},{2,1,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B3 = posHull transpose matrix {{0,0,1},{0,1,0},{1,2,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B4 = posHull transpose matrix {{0,0,1},{1,1,2},{1,2,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B5 = posHull transpose matrix {{0,1,0},{2,1,1},{1,2,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B6 = posHull transpose matrix {{1,0,0},{2,1,1},{1,1,2}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B7 = posHull transpose matrix {{1,1,1},{2,1,1},{1,2,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B8 = posHull transpose matrix {{1,1,1},{1,1,2},{1,2,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (B9 = posHull transpose matrix {{1,1,1},{1,1,2},{2,1,1}}) === "{ambient dimension => 3           }
 dimension of lineality space => 0
 dimension of the cone => 3
 number of facets => 3
 number of rays => 3"^0 ) -- toExternalString fails
assert( net (F=fan{C1,C2,C3,C4,C5,C6,C7,B1,B2,B3,B4,B5,B6,B7,B8,B9}) === "{ambient dimension => 3          }
 number of generating cones => 16
 number of rays => 10
 top dimension of the cones => 3"^0 ) -- toExternalString fails
assert( (isPolytopal F) === false )

end

print generateAssertions ///
needsPackage "Polyhedra"
M= matrix{{1,0,0},{0,1,0},{1,0,1},{0,1,1}}
C=posHull transpose M
rays C
fVector C
Cv = dualCone C
rays Cv
hilbertBasis C
C1 = posHull transpose matrix {{0,-1,0},{0,0,1},{ -1,0,0}};
C2 = posHull transpose matrix {{0,-1,0},{0,0,-1},{ -1,0,0}};
C3 = posHull transpose matrix {{0,-1,0},{0,0,1},{1,0,0}};
C4 = posHull transpose matrix {{0,-1,0},{0,0,-1},{1,0,0}};
C5 = posHull transpose matrix {{0,1,0},{0,0,1},{ -1,0,0}};
C6 = posHull transpose matrix {{0,1,0},{0,0,-1},{ -1,0,0}};
C7 = posHull transpose matrix {{0,1,0},{0,0,-1},{1,0,0}};
B1 = posHull transpose matrix {{1,0,0},{0,0,1},{1,1,2}};
B2 = posHull transpose matrix {{1,0,0},{0,1,0},{2,1,1}};
B3 = posHull transpose matrix {{0,0,1},{0,1,0},{1,2,1}};
B4 = posHull transpose matrix {{0,0,1},{1,1,2},{1,2,1}};
B5 = posHull transpose matrix {{0,1,0},{2,1,1},{1,2,1}};
B6 = posHull transpose matrix {{1,0,0},{2,1,1},{1,1,2}};
B7 = posHull transpose matrix {{1,1,1},{2,1,1},{1,2,1}};
B8 = posHull transpose matrix {{1,1,1},{1,1,2},{1,2,1}};
B9 = posHull transpose matrix {{1,1,1},{1,1,2},{2,1,1}};
F=fan{C1,C2,C3,C4,C5,C6,C7,B1,B2,B3,B4,B5,B6,B7,B8,B9}
isPolytopal F
///
