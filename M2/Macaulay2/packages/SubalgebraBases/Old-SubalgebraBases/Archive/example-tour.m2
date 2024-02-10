needsPackage "SubalgebraBases";
subringESP = n -> (
    R := QQ[x_0..x_(n-1)];
    esps := for k from 1 to n list sum(subsets(n, k), S -> product(S, i -> x_i));
    subring(esps, GeneratorSymbol => symbol s)
    )
n = 8
elapsedTime Sn = subringESP n; 
forceSB Sn; elapsedTime verifySagbi Sn
f = sum(2..8, k -> sum(n, i -> x_i^k)); 
end--
restart
load "./SubalgebraBases/example-tour.m2"
elapsedTime f // Sn;
elapsedTime subduction(Sn, f, SubductionMethod => "Engine")
elapsedTime subduction(Sn, f, SubductionMethod => "Top")
--- engine > top > //
