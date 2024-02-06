-- Mike Stillman contributed this example where the new code
-- for minimal primes runs much faster.
-- This file was added to the ExampleIdeals directory
-- at the IMA 2019 M2 workshop by Primary Decomposition group.
makeIdeal = (kk) -> (
    S = kk[x_1..x_5,y_1..y_5];
    I = ideal(y_1+y_3,
        -x_2*y_1-x_5*y_1+x_1*y_2+x_1*y_5-y_1,
        x_2*y_1-x_1*y_2-x_3*y_2+x_2*y_3,
        x_3*y_2-x_2*y_3-x_4*y_3+x_3*y_4-y_3,
        x_4*y_3-x_3*y_4-x_5*y_4+x_4*y_5,
        x_5*y_1+x_5*y_4-x_1*y_5-x_4*y_5,
        x_1^2+y_1^2-1,
        x_2^2+y_2^2-1,
        x_3^2+y_3^2-1,
        x_4^2+y_4^2-1,
        x_5^2+y_5^2-1
        )
    )
I = makeIdeal QQ
S = ring I

-- This example contains an ideal whose primary decomposition
-- runs for a long time in M2 (using the old default methods)
-- but can be decomposed quickly in Singular using GTZ.
-- It was communicated by Andrew Carroll and Jerzy Weyman to
-- Federico Galetto, and it comes from the orbit closure of
-- some quiver representation.
R=QQ[x_11,x_12,x_21,x_22,y_11,y_12,y_21,y_22,z_11,z_12,z_21,z_22]
g1 = x_11^2+x_21*x_12;
g2 = x_11*x_12+x_12*x_22;
g3 = x_11*x_21+x_21*x_22;
g4 = x_21*x_12+x_22^2;
g5 = z_11^2+z_21*z_12;
g6 = z_11*z_12+z_12*z_22;
g7 = z_11*z_21+z_21*z_22;
g8 = z_21*z_12+z_22^2;
g9 = x_11*y_11*z_11+x_12*y_21*z_11+x_11*y_12*z_21+x_12*y_22*z_21;
g10 = x_11*y_11*z_12+x_12*y_21*z_12+x_11*y_12*z_22+x_12*y_22*z_22;
g11 = x_21*y_11*z_11+x_22*y_21*z_11+x_21*y_12*z_21+x_22*y_22*z_21;
g12 = x_21*y_11*z_12+x_22*y_21*z_12+x_21*y_12*z_22+x_22*y_22*z_22;
I = ideal(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12);
