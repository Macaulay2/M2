w
w = 2^100
w
(w,w') = (33,44)
w
w'
(w,w') = (33,   -- this is a comment
          44)
w = "abcdefghij"
w | w
w || w
2^100
2.^100
(36 + 1/8)^6
x1 = {1,a}
x2 = (2,b)
x3 = [3,c,d,e]
1 .. 6
a .. f
xx = {x1,x2,x3}
#xx
xx#0
xx#0#1
join(x1,x2,x3)
append(x3,f)
prepend(f,x3)
sum {1,2,3,4}
product {1,2,3,4}
f = (x,y) -> 1000 * x + y
f = (x,y) -> (z := 1000 * x; z + y)
f(3,7)
s = (3,7)
f s
sin 2.1
apply(1 .. 10, i -> i^3)
scan(1 .. 5, print)
apply(1 .. 10, i -> if even i then 1000*i else i)
apply(1 .. 10, i -> (if even i then return 1000*i; -i))
i = 1; while i < 50 do (print i; i = 2*i)
for i from 1 to 10 list i^3
for i from 1 to 4 do print i
for i from 2 to 100 do if not isPrime i then break i
for i from 2 to 100 when isPrime i do print i
print 2^100
(1 .. 5) / print;
<< 2^100
<< "the value is : " << 2^100
<< "A = " << 2^100 << endl << "B = " << 2^200 << endl;
"foo" << "A = " << 2^100 << endl << close
get "foo"
load "foo"
A
input "foo"
R = QQ[x,y,z]
f = (x+y)^3
"foo" << f << close;
get "foo"
toString f
"foo" << toString f << close;
get "foo"
value oo
vars R
toString vars R
toExternalString vars R
R = QQ[x,y,z]/(x^3-y)
(x+y)^4
f = new HashTable from { a=>444, Daniel=>555, {c,d}=>{1,2,3,4}}
f#Daniel
f#{c,d}
Daniel = a
f.Daniel
f#?a
f#?c
x = set{1,a,{4,5},a}
x#?a
peek x
y = tally{1,a,{4,5},a}
y#a
factor 60
# factor 60
apply(2 .. 1000, i -> # factor i)
tally oo
R.ideal
ideal R
code demark
code(symbol **, RingMap, Module)
code(ideal, QuotientRing)
denom = method();
denom QQ := x -> denominator x;
denom ZZ := x -> 1;
denom(5/3)
denom 5
