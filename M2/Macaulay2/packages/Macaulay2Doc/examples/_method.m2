f = method()
f ZZ := x -> -x;
f(ZZ,String) := (n,s) -> concatenate(n:s);
f(String,ZZ,String) := (s,n,t) -> concatenate(s," : ",toString n," : ",t);
f 44
f(5,"abcd ")
f("foo",88,"bar")
p = method(Binary => true, TypicalValue => List)
p(ZZ,ZZ) := p(List,ZZ) := (i,j) -> {i,j}
p(1,2)
p(1,2,3,4,5,6)
g = method(Dispatch => Thing);
g ZZ := i -> -i;
g Sequence := S -> reverse S;
g 44
g(3,4,5,6)
h = method(Dispatch => {Type})
h(QQ,ZZ) := (QQ,n) -> n/1;
h(RR,ZZ) := (RR,n) -> n + 0.;
h(ZZ,ZZ) := (ZZ,n) -> n;
h(ZZ,14)
h(QQ,14)
h(RR,14)
r = method(Options => {Slope => 1, Intercept => 1})
r RR := o -> x -> o.Slope * x + o.Intercept
r(5.)
r(5.,Slope=>100)
options r
methodOptions r
