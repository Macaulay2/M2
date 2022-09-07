restart
needsPackage "SubalgebraBases";
R = QQ[x_1..x_3];
f = x_1^2 + x_2^2 + x_3^2;
A = subring({x_1+x_2+x_3, x_1*x_2 + x_1*x_3 + x_2*x_3, x_1*x_2*x_3}, GeneratorSymbol => s)
forceSB A;
f // A 

subringESP = n -> (
    R := QQ[x_0..x_(n-1)];
    esps := for k from 1 to n list sum(subsets(n, k), S -> product(S, i -> x_i));
    subring(esps, GeneratorSymbol => symbol s)
    )
n = 12
-- what makes these timings so close? initializeCompTable?
elapsedTime Sn = subringESP n; elapsedTime verifySagbi Sn
elapsedTime Sn = subringESP n; forceSB Sn; elapsedTime verifySagbi Sn
use ambient Sn
f = sum(n, i -> x_i^3); 
elapsedTime f // Sn -- how does timing compare if we subduct instead?
