a={1,1,2,2,2,3,4};
b=set a;
c=tally a;
f=x->x//2;
assert( a/f === {0,0,1,1,1,1,2} );
-- */f below calls applyKeys with possible third argument depending on type of *
assert( b/f === set {0,1,2} );
assert( c/f === tally (a/f) );
  