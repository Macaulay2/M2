restart
needsPackage "NumericalAlgebraicGeometry"
needsPackage "Bertini"
FF = ZZ/nextPrime 10000
R = FF[x_1..x_7, y_1..y_7]
I = ideal(
    y_1+y_2+y_4+y_6+y_7, 
    -x_2*y_1-x_3*y_1-x_5*y_1-x_7*y_1+x_1*y_2+x_1*y_3+x_1*y_5+x_1*y_7-y_1, 
    x_2*y_1-x_1*y_2-x_3*y_2-x_4*y_2-x_6*y_2+x_2*y_3+x_2*y_4+x_2*y_6-y_2, 
    x_3*y_1+x_3*y_2-x_1*y_3-x_2*y_3-x_4*y_3-x_5*y_3-x_7*y_3+x_3*y_4+x_3*y_5+x_3*y_7, 
    x_4*y_2+x_4*y_3-x_2*y_4-x_3*y_4-x_5*y_4-x_6*y_4+x_4*y_5+x_4*y_6-y_4, 
    x_5*y_1+x_5*y_3+x_5*y_4-x_1*y_5-x_3*y_5-x_4*y_5-x_6*y_5-x_7*y_5+x_5*y_6+x_5*y_7, 
    x_6*y_2+x_6*y_4+x_6*y_5-x_2*y_6-x_4*y_6-x_5*y_6-x_7*y_6+x_6*y_7-y_6, 
    x_1^2+y_1^2-1, 
    x_2^2+y_2^2-1, 
    x_3^2+y_3^2-1, 
    x_4^2+y_4^2-1, 
    x_5^2+y_5^2-1, 
    x_6^2+y_6^2-1, 
    x_7^2+y_7^2-1
    )

elapsedTime G = groebnerBasis(I, Strategy=>"F4");

elapsedTime bertiniPosDimSolve(I_*, Verbose => true); -- takes about an hour

NAGtrace 1
elapsedTime numericalIrreducibleDecomposition I; -- uses > 20 GB after several hours

