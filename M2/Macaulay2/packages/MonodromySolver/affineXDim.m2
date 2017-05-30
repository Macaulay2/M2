restart
needsPackage "MonodromySolver"

R=CC[a,b,c,d,e,f][A,B,C]
polys = polySystem{A*a,B*b,C*c,A*B*d+A*C*f+B*C*e,A*B*C+1}
D=2
m=numgens coefficientRing ring polys
p = point random(CC^1,CC^m)
specializeSystem(p,polys) -- why is it generating the first three equations?

{*
-- heuristic seeding
seedX = method()
-- when projection onto xs is D-dimensional
seedX (polySystem, D) = (polys,D) -> (
    R:= CC[gens ring polys];

    sys=polySystem apply(sys,i->sub(i,R))
    G = gens ring sys
    n = length G
    eqs = equations sys
    assert(n==#eqs)
    coords = apply(eqs,e->sub(random(1,CC[G#0..G#(D-1)]),R))
    subs = apply(eqs,e->sub(e,apply(G,coords,(g,c)->g=>c)))
    targetSys=squareUp polySystem subs
    N=first monodromySolve(targetSys, TargetSolutionCount=>1, GuessFamily=>true);
    p = first track(polySystem N.SpecializedSystem,targetSys,{first N.PartialSols})
    if p.SolutionStatus =!= Regular then return 1;
    else 

*}