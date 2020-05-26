stderr << "--testing the error messages must be done manually" << endl
end

X = new Type of BasicList
x = new X from {}
net X := x -> error "can't format this"

Y = new Type of BasicList
y = new Y from {}
net Y := x -> while true do 1				    -- loop forever

Z = new Type of X
z = new Z from {}
toString Z := z -> error "can't convert this to string, either!"

long = w -> demark(" ",100 : w)
p1 = long "ab" | ".  "
p2 = long "abc" | ".  "
p3 = long "abcd" | ".  "
high = w -> stack(20:w)
h1 = high "hi there"

d = method(Dispatch => Thing)
e = method(Dispatch => Thing, Options => {a=>null})
f = method()
g = method(Options => {a=>null})

end

d()
d(3)
d(1:3)
d(3,4)
d(3,4,a=>5)
d(3,4,5,6,7,a=>5)
e()
e(3)
e(1:3)
e(3,4)
e(3,4,a=>5)
e(3,4,5,6,7,a=>5)
f()
f(3)
f(1:3)
f(3,4)
f(3,4,a=>5)
f(3,4,5,6,7,a=>5)
g()
g(3)
g(1:3)
g(3,4)
g(3,4,a=>5)
g(3,4,5,6,7,a=>5)

d(x,y,z)
e(x,y,z)
f(x,y,z)
g(x,y,z)

d(x,p1,h1)
e(x,p1,h1)
f(x,p1,h1)
g(x,p1,h1)
