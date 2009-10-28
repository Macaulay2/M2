loadPackage ("NumericalAlgebraicGeometry", FileName=>"../../NumericalAlgebraicGeometry.m2", DebuggingMode=>true)
load "pieri.m2";

H = new MutableHashTable;
-- G list of numerical matrices that define the start system
G = {};

launchSimpleSchubert = method()
launchSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
   -- l and m are partitions of n
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   G=apply(d, i->random(QQ^(n-k),QQ^n));
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
      H#(l,m) = solveEasy det (matrix E || sub(G#0, ring E))
   )
   else(
      L:=generateChildren(kn, m);
      start := flatten apply(L, p->(
         C := solveSimpleSchubert(kn,l,p);
         i := positionVariableChildren((k,n),l,m,p);
         apply(C,c->insert(i-1,0,c))
         )
      );
      S := apply(take(G,d-1), g->det( matrix E || sub(g, ring E))) | {det(precookPieriHomotopy(kn,l,m))};
      T := apply(take(G,d), g->det( matrix E || sub(g, ring E))); 
      newR := CC[gens ring first S];
      S = S/(s->sub(s,newR));
      T = T/(t->sub(t,newR));
      print (S,T,start);
      H#(l,m) = track(S,T,start,gamma=>exp(2*pi*ii*random RR)
      --,Software=>PHCpack
      )
   )
)

solveEasy = method(TypicalValue=>QQ)
solveEasy(RingElement) := (p)->(
   R:=ring p;
   var:=support p;
--   if var=={} and p==0 then {{p}}
--   else(
   b:=part(0,p);
   a:=p_(var_(0));
   print(p,a,b);
   {{toCC lift((-b)/a, coefficientRing R)}}
--   )
)
--launchSimpleSchubert((2,4),{1,0},{1,0})
launchSimpleSchubert((3,7),{2,1,0},{1,1,0})