getn = () -> (x0 := apply(10000,i->random 2^3000);)
for i from 1 to 1000 do (<< "." << flush; getn();)

a = 2^3000
b = 10000*1000
for i from 1 to b do random a;

-- This snippet takes 136 MB virtual memory ----
a = 2^3000
b = 10000*1000
for i from 1 to b do random a;
-------------------------------------------------

-- This snippet takes 136 MB virtual memory ----
b = 10000*1000
for i from 1 to b do random (2^3000);
-------------------------------------------------

-- This snippet takes large amount of virtual memory ----
a = 0..9999;
getn = () -> (x0 := apply(a,i->random 2^3000);)
for i from 1 to 1000 do getn();
-------------------------------------------------

-- This snippet takes large amount of virtual memory ----
getn = () -> (x0 := for i from 1 to 10000 list random 2^3000;)
for i from 1 to 1000 do getn();
-------------------------------------------------
