needsPackage "Polyhedra"
assert( (M= matrix{{1,0,0},{0,1,0},{1,0,1},{0,1,1}}) === map((ZZ)^4,(ZZ)^3,{{1, 0, 0}, {0, 1, 0}, {1, 0, 1}, {0, 1, 1}}) );
C=posHull transpose M
assert( (rays C) === map((ZZ)^3,(ZZ)^4,{{1, 0, 1, 0}, {0, 1, 0, 1}, {0, 0, 1, 1}}) );
assert( (fVector C) === {1,4,4,1} );
Cv = dualCone C
assert( (rays Cv) === map((ZZ)^3,(ZZ)^4,{{1, 0, 0, 1}, {0, 1, 0, 1}, {0, 0, 1, -1}}) );
assert( (hilbertBasis C) === {map((ZZ)^3,(ZZ)^1,{{1}, {0}, {0}}),map((ZZ)^3,(ZZ)^1,{{0}, {1}, {0}}),map((ZZ)^3,(ZZ)^1,{{0}, {1}, {1}}),map((ZZ)^3,(ZZ)^1,{{1}, {0}, {1}})} );
C1 = posHull transpose matrix {{0,-1,0},{0,0,1},{ -1,0,0}};
assert( (rays C1) === map((ZZ)^3,(ZZ)^3,{{-1, 0, 0}, {0, -1, 0}, {0, 0, 1}}) );
assert( (linealitySpace C1) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C1) === 3 );
assert( (isPointed C1) === true );
assert( (isSimplicial C1) === true );
C2 = posHull transpose matrix {{0,-1,0},{0,0,-1},{ -1,0,0}};
assert( (rays C2) === map((ZZ)^3,(ZZ)^3,{{-1, 0, 0}, {0, -1, 0}, {0, 0, -1}}) );
assert( (linealitySpace C2) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C2) === 3 );
assert( (isPointed C2) === true );
assert( (isSimplicial C2) === true );
C3 = posHull transpose matrix {{0,-1,0},{0,0,1},{1,0,0}};
assert( (rays C3) === map((ZZ)^3,(ZZ)^3,{{1, 0, 0}, {0, -1, 0}, {0, 0, 1}}) );
assert( (linealitySpace C3) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C3) === 3 );
assert( (isPointed C3) === true );
assert( (isSimplicial C3) === true );
C4 = posHull transpose matrix {{0,-1,0},{0,0,-1},{1,0,0}};
assert( (rays C4) === map((ZZ)^3,(ZZ)^3,{{1, 0, 0}, {0, -1, 0}, {0, 0, -1}}) );
assert( (linealitySpace C4) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C4) === 3 );
assert( (isPointed C4) === true );
assert( (isSimplicial C4) === true );
C5 = posHull transpose matrix {{0,1,0},{0,0,1},{ -1,0,0}};
assert( (rays C5) === map((ZZ)^3,(ZZ)^3,{{-1, 0, 0}, {0, 1, 0}, {0, 0, 1}}) );
assert( (linealitySpace C5) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C5) === 3 );
assert( (isPointed C5) === true );
assert( (isSimplicial C5) === true );
C6 = posHull transpose matrix {{0,1,0},{0,0,-1},{ -1,0,0}};
assert( (rays C6) === map((ZZ)^3,(ZZ)^3,{{-1, 0, 0}, {0, 1, 0}, {0, 0, -1}}) );
assert( (linealitySpace C6) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C6) === 3 );
assert( (isPointed C6) === true );
assert( (isSimplicial C6) === true );
C7 = posHull transpose matrix {{0,1,0},{0,0,-1},{1,0,0}};
assert( (rays C7) === map((ZZ)^3,(ZZ)^3,{{1, 0, 0}, {0, 1, 0}, {0, 0, -1}}) );
assert( (linealitySpace C7) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim C7) === 3 );
assert( (isPointed C7) === true );
assert( (isSimplicial C7) === true );
B1 = posHull transpose matrix {{1,0,0},{0,0,1},{1,1,2}};
assert( (rays B1) === map((ZZ)^3,(ZZ)^3,{{1, 0, 1}, {0, 0, 1}, {0, 1, 2}}) );
assert( (linealitySpace B1) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B1) === 3 );
assert( (isPointed B1) === true );
assert( (isSimplicial B1) === true );
B2 = posHull transpose matrix {{1,0,0},{0,1,0},{2,1,1}};
assert( (rays B2) === map((ZZ)^3,(ZZ)^3,{{1, 0, 2}, {0, 1, 1}, {0, 0, 1}}) );
assert( (linealitySpace B2) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B2) === 3 );
assert( (isPointed B2) === true );
assert( (isSimplicial B2) === true );
B3 = posHull transpose matrix {{0,0,1},{0,1,0},{1,2,1}};
assert( (rays B3) === map((ZZ)^3,(ZZ)^3,{{0, 0, 1}, {1, 0, 2}, {0, 1, 1}}) );
assert( (linealitySpace B3) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B3) === 3 );
assert( (isPointed B3) === true );
assert( (isSimplicial B3) === true );
B4 = posHull transpose matrix {{0,0,1},{1,1,2},{1,2,1}};
assert( (rays B4) === map((ZZ)^3,(ZZ)^3,{{0, 1, 1}, {0, 2, 1}, {1, 1, 2}}) );
assert( (linealitySpace B4) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B4) === 3 );
assert( (isPointed B4) === true );
assert( (isSimplicial B4) === true );
B5 = posHull transpose matrix {{0,1,0},{2,1,1},{1,2,1}};
assert( (rays B5) === map((ZZ)^3,(ZZ)^3,{{0, 2, 1}, {1, 1, 2}, {0, 1, 1}}) );
assert( (linealitySpace B5) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B5) === 3 );
assert( (isPointed B5) === true );
assert( (isSimplicial B5) === true );
B6 = posHull transpose matrix {{1,0,0},{2,1,1},{1,1,2}};
assert( (rays B6) === map((ZZ)^3,(ZZ)^3,{{1, 2, 1}, {0, 1, 1}, {0, 1, 2}}) );
assert( (linealitySpace B6) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B6) === 3 );
assert( (isPointed B6) === true );
assert( (isSimplicial B6) === true );
B7 = posHull transpose matrix {{1,1,1},{2,1,1},{1,2,1}};
assert( (rays B7) === map((ZZ)^3,(ZZ)^3,{{1, 2, 1}, {1, 1, 2}, {1, 1, 1}}) );
assert( (linealitySpace B7) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B7) === 3 );
assert( (isPointed B7) === true );
assert( (isSimplicial B7) === true );
B8 = posHull transpose matrix {{1,1,1},{1,1,2},{1,2,1}};
assert( (rays B8) === map((ZZ)^3,(ZZ)^3,{{1, 1, 1}, {1, 2, 1}, {1, 1, 2}}) );
assert( (linealitySpace B8) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B8) === 3 );
assert( (isPointed B8) === true );
assert( (isSimplicial B8) === true );
B9 = posHull transpose matrix {{1,1,1},{1,1,2},{2,1,1}};
assert( (rays B9) === map((ZZ)^3,(ZZ)^3,{{1, 2, 1}, {1, 1, 1}, {1, 1, 2}}) );
assert( (linealitySpace B9) === map((ZZ)^3,(ZZ)^0,0) );
assert( (dim B9) === 3 );
assert( (isPointed B9) === true );
assert( (isSimplicial B9) === true );
F=fan{C1,C2,C3,C4,C5,C6,C7,B1,B2,B3,B4,B5,B6,B7,B8,B9}
assert( (isPolytopal F) === false );

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
rays C1
linealitySpace C1
dim C1
isPointed C1
isSimplicial C1
C2 = posHull transpose matrix {{0,-1,0},{0,0,-1},{ -1,0,0}};
rays C2
linealitySpace C2
dim C2
isPointed C2
isSimplicial C2
C3 = posHull transpose matrix {{0,-1,0},{0,0,1},{1,0,0}};
rays C3
linealitySpace C3
dim C3
isPointed C3
isSimplicial C3
C4 = posHull transpose matrix {{0,-1,0},{0,0,-1},{1,0,0}};
rays C4
linealitySpace C4
dim C4
isPointed C4
isSimplicial C4
C5 = posHull transpose matrix {{0,1,0},{0,0,1},{ -1,0,0}};
rays C5
linealitySpace C5
dim C5
isPointed C5
isSimplicial C5
C6 = posHull transpose matrix {{0,1,0},{0,0,-1},{ -1,0,0}};
rays C6
linealitySpace C6
dim C6
isPointed C6
isSimplicial C6
C7 = posHull transpose matrix {{0,1,0},{0,0,-1},{1,0,0}};
rays C7
linealitySpace C7
dim C7
isPointed C7
isSimplicial C7
B1 = posHull transpose matrix {{1,0,0},{0,0,1},{1,1,2}};
rays B1
linealitySpace B1
dim B1
isPointed B1
isSimplicial B1
B2 = posHull transpose matrix {{1,0,0},{0,1,0},{2,1,1}};
rays B2
linealitySpace B2
dim B2
isPointed B2
isSimplicial B2
B3 = posHull transpose matrix {{0,0,1},{0,1,0},{1,2,1}};
rays B3
linealitySpace B3
dim B3
isPointed B3
isSimplicial B3
B4 = posHull transpose matrix {{0,0,1},{1,1,2},{1,2,1}};
rays B4
linealitySpace B4
dim B4
isPointed B4
isSimplicial B4
B5 = posHull transpose matrix {{0,1,0},{2,1,1},{1,2,1}};
rays B5
linealitySpace B5
dim B5
isPointed B5
isSimplicial B5
B6 = posHull transpose matrix {{1,0,0},{2,1,1},{1,1,2}};
rays B6
linealitySpace B6
dim B6
isPointed B6
isSimplicial B6
B7 = posHull transpose matrix {{1,1,1},{2,1,1},{1,2,1}};
rays B7
linealitySpace B7
dim B7
isPointed B7
isSimplicial B7
B8 = posHull transpose matrix {{1,1,1},{1,1,2},{1,2,1}};
rays B8
linealitySpace B8
dim B8
isPointed B8
isSimplicial B8
B9 = posHull transpose matrix {{1,1,1},{1,1,2},{2,1,1}};
rays B9
linealitySpace B9
dim B9
isPointed B9
isSimplicial B9
F=fan{C1,C2,C3,C4,C5,C6,C7,B1,B2,B3,B4,B5,B6,B7,B8,B9}
isPolytopal F
///
