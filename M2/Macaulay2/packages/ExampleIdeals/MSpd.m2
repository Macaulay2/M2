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

