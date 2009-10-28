loadPackage ("NumericalAlgebraicGeometry", FileName=>"../../NumericalAlgebraicGeometry.m2")
load "pieri.m2";

H = new MutableHashTable
-- G list of numerical matrices that define the start system
G = {};

launchSimpleSchubert = method()
launchSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
   -- l and m are partitions of n
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   G=apply(d, i->random(CC^(n-k),CC^n));
   solveSimpleSchubert(kn,l,m)
   )
     
solveSimpleSchubert = method(TypicalValue=>List)
solveSimpleSchubert(Sequence, List,List) := (kn,l,m)->(
   -- l and m are partitions of n
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   E := skewSchubertVariety(k,n,l,m);
   if H#?(l,m) then(
   H # (l,m)
   )
   else if d == 1 then (
      -- solve linear equation
      H#(l,m) = solveEasy det (matrix E || sub(GT#0, ring E))
   )
   else(
      L:=generateChildren m;
      start := flatten apply(L, p->(
         C := solveSimpleSchubert(kn,l,p);
         i := positionVariableChildren((k,n),l,m,p);
         apply(C,c->insert(i,0,c))
         )
      );
      S := apply(take(G,d-1), g->det( matrix E || sub(g, ring E))) | {det(precookPieri(kn,l,m))};
      T := apply(take(G,d), g->det( matrix E || sub(g, ring E))); 
      H#(l,m) = track(S,T,start)
   )
)

solveEasy = method(TypicalValue=>QQ)
solveEasy(RingElement) := (p)->(
   R:=ring p;
   var:=support p;
   b:=part(0,p);
   a:=p_(var_(0));
   {(-b)/a}
)

launchSimpleSchubert((3,7),{2,1,0},{1,1,0})