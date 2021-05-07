-- first test single grading, degree 1 variables, and ideals
R = ZZ/101[a..f]
m = matrix{{a*b, b^2, c^2, d*e, e^2}}

g = basis(2, cokernel m)
assert(target g === cokernel m)
assert(g -
     matrix {{a^2, a*c, a*d, a*e, a*f, b*c, b*d, b*e, 
              b*f, c*d, c*e, c*f, d^2, d*f, e*f, f^2}} == 0)

assert(image (super basis(3, image m)) ==
     image matrix {{a*e^2, b*e^2, c*e^2, e^3, e^2*f, a*d*e, b*d*e, 
              c*d*e, d^2*e, d*e^2, d*e*f, a*c^2, b*c^2, c^3, 
              c^2*d, c^2*e, c^2*f, b^3, b^2*c, b^2*d, b^2*e, 
              b^2*f, a^2*b, a*b^2, a*b*c, a*b*d, a*b*e, a*b*f}})

R = ZZ/101[symbol a..symbol d]
assert(basis(0, R^{-1,0}) == map(R^{-1,0},,{{0}, {1}}))
assert(basis(1, R^{-1,0}) == map(R^{-1,0},R^{5:-1}, {{1, 0, 0, 0, 0}, {0, a, b, c, d}}))
assert(basis(2, R^{-1,0}) == map(R^{-1,0},R^{14:-2}, {
      {a, b, c, d, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 
      {0, 0, 0, 0, a^2, a*b, a*c, a*d, b^2, b*c, b*d, c^2, c*d, d^2}}))
assert(basis(-1, R^{1,2}) == map(R^{1,2},R^{5:1}, {{1, 0, 0, 0, 0}, {0, a, b, c, d}}))

R = ZZ/101[symbol a..symbol c]
I = image matrix {{a,b}}/ image matrix {{a^2, b^2, c^2}}
assert(super basis(2,I) ==  map(super I,R^{3:-2},{{a*b, a*c, b*c}}))
assert(super basis I == map(super I,R^{-1,-2,-3,-2,-1,-2}, {{a, a*b, a*b*c, a*c, b, b*c}}))
m1 = matrix{{a,b}}
M = image m1 ++ cokernel m1
assert(super basis(2, M) == map(super M,R^{6:-2},
	  { {a^2, a*b, a*c, b^2, b*c, 0}, {0, 0, 0, 0, 0, c^2}} ))

-- test using multi degrees
R = ZZ/101[symbol a..symbol f, Degrees => {{1, 0}, {1, -1}, {1, -2}, {1, -3}, {1, -4}, {2, 0}}];
assert(basis({3,-3}, R) == matrix {{a^2*d, a*b*c, b^3, d*f}})

R = ZZ/101[symbol a..symbol c]
R1 = R/(a^2, b^2, c^3)
assert(basis R1 == matrix {{ 1, a, a*b, a*b*c, a*b*c^2, a*c, a*c^2, b, b*c, b*c^2, c, c^2}})

-- now test using single grading, but 'funny' weights
R = ZZ/101[symbol a..symbol f, Degrees => {1,3,5,7,9,11}];
assert(basis(9,R) == matrix {{a^9, a^6*b, a^4*c, a^3*b^2, a^2*d, a*b*c, b^3, e}} )

-- test using a matrix with more than one row
R = ZZ/101[symbol a..symbol f, 
         Degrees => {{1, 0}, {1, -1}, {1, -2}, {1, -3}, {1, -4}, {2, 0}}];
m = matrix {{a,b,c,0,0,0}, {0,0,0,d,e,f}};
assert(basis({3,-3},cokernel m) == map( cokernel m,R^{3 : {-3,3}}, {{d*f, 0, 0}, {0, a*b*c, b^3}} ))
assert(super basis({3,-3},image m) == map(super image m,R^{5:{-3,3}},
	  {{a^2*d, a*b*c, b^3, 0, 0}, {0, 0, 0, a^2*d, d*f}}))
M = module ideal(a*b*c-b^3, a^2*d-d*f)/ module ideal(a*b*c-b^3)
assert(super basis({3,-3},M) == map(super M,R^{{-3,3}}, {{a^2*d-d*f}}))
assert(super basis({4,-3},M) == map(super M,R^{{-4,3}},{{a^3*d-a*d*f}}))
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test testkbasis.out"
-- End:
