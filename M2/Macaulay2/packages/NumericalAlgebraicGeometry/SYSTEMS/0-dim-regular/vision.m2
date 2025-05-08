needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 0 
n = 5 -- 5 points is minimal
F = CC_53
R = F[e_(1,1)..e_(3,3)]
E = transpose genericMatrix (R,3,3)
B = apply(n,i->random(F^3,F^1))
C = apply(n,i->random(F^3,F^1))
P1 = E * transpose E * E - (1/2) * trace(E * transpose E) * E
P2 = for i to n-1 list transpose B#i * E * C#i  
I = ideal P1 + ideal P2
-- numericalIrreducibleDecomposition I -- takes too long
R1 = F[drop(gens R,1)]
-* bug???
matrix{{1}}|vars R1
*-
RtoR1 = map(R1,R,matrix{{1_F}}|vars R1)
PS = polySystem RtoR1 I

if F === CC_53 then (
    -- stashed solutions
    ten'sols = point \ {{ {1.25678+.773202*ii, -2.43669-1.18573*ii, .807574-.166822*ii, -2.80276-.965475*ii, 1.42974+.443489*ii, -2.83727-.605914*ii, 1.66649+1.43463*ii, 1.69374+1.21527*ii}, SolutionStatus => Regular },{ {-1.11861+1.03509*ii, -.839097-1.38552*ii, .288841+1.22571*ii, -2.02908-.857154*ii, 1.21923-1.78306*ii,
      -2.96057-.050912*ii, 3.79458-2.01041*ii, 1.59868+4.63222*ii}, SolutionStatus => Regular },{ {-.307929+3.84135*ii, -5.10546-4.5236*ii, -1.99881-.046587*ii, -3.44626-1.23565*ii, 8.11083-2.19987*ii, -.033004-2.65553*ii, 2.36344-1.73295*ii, -.65177+8.35282*ii}, SolutionStatus => Regular },{ {1.59495+.054244*ii,
      1.20444-.592168*ii, 3.78567+1.59631*ii, -2.04716-4.24008*ii, -4.49854+3.70089*ii, -5.02978-1.00028*ii, .254582+4.85886*ii, 3.21692-2.57055*ii}, SolutionStatus => Regular },{ {-.350564+.157102*ii, -.343433+.119616*ii, -.522361+.11855*ii, .812316-.253519*ii, -.708143+.364527*ii, -.509347+.332827*ii,
      -.31393-.344132*ii, .830918-.154772*ii}, SolutionStatus => Regular },{ {-.691862-.540768*ii, -.417566+.359237*ii, -.868051-.637898*ii, .576807+.808934*ii, .156835+.346513*ii, -.464842+1.29024*ii, .988549-.934435*ii, -.0099413-.264246*ii}, SolutionStatus => Regular },{ {-4.18591-.106823*ii,
      3.15493-.633812*ii, -3.41618+1.54952*ii, 4.53757-2.56195*ii, 1.14272+1.20649*ii, 2.62605-1.21085*ii, .706372-.307508*ii, -5.36774+2.4767*ii}, SolutionStatus => Regular },{ {-.0270915-.12955*ii, -1.06035-.348027*ii, -.525361-.26785*ii, -.657316-.484579*ii, 1.26734+.664573*ii, -1.28462-.108466*ii,
      1.54955+.925889*ii, -.33974+.59602*ii}, SolutionStatus => Regular },{ {-1.6033-.256744*ii, 1.63431-.544357*ii, .772456+.357682*ii, .27222-.829672*ii, -1.90073+1.54093*ii, -1.69713+1.63451*ii, 1.71868-1.16308*ii, .640531-.099971*ii}, SolutionStatus => Regular },{ {-.098315+.871262*ii, -1.30013-.712257*ii,
      -.747723-.713907*ii, .625451+.157568*ii, .291167+.840718*ii, .512794+.54527*ii, -1.37721-.904274*ii, .813883-.287774*ii}, SolutionStatus => Regular }}
    ) else (
    sols = solveSystem PS;
    reg'sols = select(sols,s -> status s == Regular);
    ten'sols = select(reg'sols,s -> norm evaluate(PS,s) < 1e-5)
    )
scan(ten'sols, s->s.Coordinates = s.Coordinates/(c->sub(c,F))) 

-- parameter homotopy 
R = F[e_(1,1)..e_(3,3),b_(1,1)..b_(n,3),c_(1,1)..c_(n,3)]
E' = transpose genericMatrix (R,3,3)
R1 = F[drop(gens R,1)]
RtoR1 = map(R1,R,matrix{{1_F}}|vars R1)
E = RtoR1 E'
Bp = genericMatrix (R1,b_(1,1),3,n)
Cp = genericMatrix (R1,c_(1,1),3,n)
P1 = E * transpose E * E - (1/2) * trace(E * transpose E) * E
P2 = for i to n-1 list transpose Bp_{i} * E * Cp_{i}  
-- PS = polySystem (ideal P1 + ideal P2)
PS = polySystem (ideal {P1_(0,0),P1_(0,1),P1_(2,2)} + ideal P2)

debug NumericalAlgebraicGeometry
PS.NumberOfVariables = 8 -- hack!!!
-- squarePS = squareUp PS 
gPS = gateSystem(PS,drop(gens R1,8));  
PH = parametricSegmentHomotopy gPS
--PH = parametricSegmentHomotopy PS
printAsSLP(PH.GateHomotopy#"X"|matrix{{PH.GateHomotopy#"T"}},PH.GateHomotopy#"Hx")

-- start solutions
BC = matrix{flatten flatten(B/entries) | flatten flatten(C/entries)}
assert all(ten'sols,s->norm evaluate(PS,matrix s | BC) < 0.001)

NAGtrace 3
setDefault(ErrorTolerance=>1e-6)

-- run 1
setRandomSeed 1
B' = apply(n,i->random(F^3,F^1))
C' = apply(n,i->random(F^3,F^1))
BC' = matrix{flatten flatten(B'/entries) | flatten flatten(C'/entries)}
H = specialize (PH, transpose (BC|BC'))
time sols = trackHomotopy(H,ten'sols);
assert all(sols,s->norm evaluate(PS,matrix s | BC') < 0.001)
sols / (s->s.cache.NumberOfSteps)

-- run 2 (the target is close to start)
setRandomSeed 2
eps = 0.1
B'' = B+eps*apply(n,i->random(F^3,F^1))
C'' = C+eps*apply(n,i->random(F^3,F^1))
BC'' = matrix{flatten flatten(B''/entries) | flatten flatten(C''/entries)}
H2 = specialize (PH, transpose (BC|BC''))
time sols2 = trackHomotopy(H2,ten'sols);
assert all(sols2,s->norm evaluate(PS,matrix s | BC'') < 0.001)
sols2 / (s->s.cache.NumberOfSteps)
end --------------------------------------------------------------------------------------
restart
load "vision.m2"

-- RedHat

-- track took 63ms.
-- # of solveLinear calls = 2158
-- time of solveLinear calls = 7739859 ns.
-- time of evaluate calls = 46275195 ns.
-- trackHomotopyM2engine time: .0630688 sec.

-- track took 18ms.
-- # of solveLinear calls = 566
-- time of solveLinear calls = 2382026 ns.
-- time of evaluate calls = 13412694 ns.
-- trackHomotopyM2engine time: .0191059 sec.
