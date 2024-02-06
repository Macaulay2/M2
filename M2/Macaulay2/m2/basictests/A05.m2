-- test map

assert := x -> if not x then error "assertion failed "

f := 5:(1,2,3)
w := 44
apply(f,(x,y,z)->(
	  w = w + 1;
	  w :=w; 
	  t :=t;
	  assert(w===null);
	  assert(t===null);
	  w = 4;
	  t = 5;
	  assert(x===1);
	  assert(y===2);
	  assert(z===3);
	  ))
assert(w === 49)

t := 44
apply(f,(x,y,z)->(
	  t = t + 1;
	  () -> ();
	  t :=t;
	  assert(t===null);
	  t = 4;
	  assert(x===1);
	  assert(y===2);
	  assert(z===3);
	  ))
assert(t === 49)

assert( apply(0,i->i) === {} )

h := ( () -> (
	       f := 77;
	       x -> f
	       )) ()

g := apply( {1,2,3,(4,5),(6,7)}, h)

assert(g === {77,77,77,77,77})

k := apply( {2,3}, x -> apply( {4,5}, i -> x ))

assert(k === {{2,2},{3,3}})

assert(apply( {2,3}, (x) -> apply( {4,5}, (i) -> x )) === {{2,2},{3,3}})

assert(apply( {2,3}, x -> apply( {4,5}, i -> i )) === {{4,5},{4,5}})

assert(apply( (2,3), (x) -> apply( {4,5}, (i) -> i )) === ({4,5},{4,5}))


di = (x) -> (
     a := 4;
     apply({1,2,3}, i -> (
	       b := 5;
	       apply(x, j -> ( j; x; a; i; b;));
	       apply(x, (j) -> ( j; x; a; i; b;));
	       apply({(2,3)}, (j,k) -> ( j; x; a; i; b;));
	       a; b; id; i;
	       )
	  );
     apply({1,2,3}, (i) -> (
	       b := 5;
	       apply(x, j -> ( j; x; a; i; b;));
	       apply(x, (j) -> ( j; x; a; i; b;));
	       a; b; id; i;
	       )
	  );
     apply({(1,2,3)}, (i,m,n) -> (
	       b := 5;
	       apply(x, j -> ( j; x; a;  i; m; n; b;));
	       apply(x, (j) -> ( j; x; a; i; m; n; b;));
	       a; b; id; i; m; n;
	       )
	  );
     a; id; x;
     )

di {5,6,7,8}

yy = (x) -> (
     a := 1;
     b := 2;
     c := 3;
     () -> ();
     )

fun = i->i;
(() -> (w := 3; apply({0},fun); w;)) ()


assert(apply(apply({33,44},i -> () -> i),f -> f()) === {33,44})

-- test scan

ii = 0
scan(5,i -> ii = ii + 1)
assert(ii === 5)

ii = 0
scan(5,(i) -> ii = ii + 1)
assert(ii === 5)

ii = 0
scan({1,2,3,4,5},i -> ii = ii + 1)
assert(ii === 5)

ii = 0
scan(5,i -> (()->(); ii = ii + 1))
assert(ii === 5)

ii = 0
scan(5,(i) -> (()->(); ii = ii + 1))
assert(ii === 5)

ii = 0
scan({1,2,3,4,5},i -> (()->(); ii = ii + 1))
assert(ii === 5)

apply(0 .. 5, i -> (
	j := j;
	assert(j === null);
	j = true;
	))

apply(5, i -> (
	j := j;
	assert(j === null);
	j = true;
	))

scan(0 .. 5, i -> (
	j := j;
	assert(j === null);
	j = true;
	))

scan(5, i -> (
	j := j;
	assert(j === null);
	j = true;
	))

assert( apply(apply(0 .. 5, i -> () -> i),f -> f()) === 0 .. 5 )
assert( apply(apply(0 .. 5, i -> f := () -> i),f -> f()) === 0 .. 5 )

assert(null === scan((),))
assert(null === scan({},))
assert(null === scan(0 .. 4,x->x))

x = new MutableList from {1,2,3}
y = apply(x,identity)
assert(class x === class y)
assert(x =!= y)
assert(isMutable x)
assert(isMutable y)
assert(hash y =!= 0)
