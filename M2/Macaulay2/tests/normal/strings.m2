---- concatenate
-- one arg
assert(concatenate("ABC")=="ABC");
assert(concatenate(2)=="  ");
assert(concatenate(0)=="");
assert(concatenate(-1)=="");
-- multiple args
assert(concatenate("ABC","DEF")=="ABCDEF");
assert(concatenate("ABC",2)=="ABC  ");
assert(concatenate(2,"ABC")=="  ABC");
assert(concatenate("ABC",0)=="ABC");
assert(concatenate(0,"ABC")=="ABC");
assert(concatenate("ABC",-1)=="ABC");
assert(concatenate(-1,"ABC")=="ABC");
assert(concatenate("ABC",-2^10)=="ABC");
assert(concatenate(-2^10,"ABC")=="ABC");
assert(concatenate(-2^10,2^10)==concatenate(2^10));

---- pad
assert(pad("ABC",4)=="ABC ");
assert(pad(4,"ABC")==" ABC");
assert(pad("ABC",3)=="ABC");
assert(pad(3,"ABC")=="ABC");
-- ignore the numbers if it's too short
assert(pad("ABC",2)=="ABC");
assert(pad(2,"ABC")=="ABC");
