getn = () -> (x0 := apply(10000,i->random 2^3000);)
for i from 1 to 1000 do (<< "." << flush; getn();)
