use stdio;
use strings;
use gmp;

n := toInteger(10)^1000;

for 1000 do x := n*n;

-- stdout << n << endl;

foo := null;
bar := null;
f():foo := foo() ;
g(i:bar):void := nothing;

j := f();
g(j);

