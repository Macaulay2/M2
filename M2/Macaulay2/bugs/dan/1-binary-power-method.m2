change BinaryPowerMethod it gets f^-10 as f^-1^10 instead of as f^10^-1!

    i1 : T = QQ[x]

    o1 = T

    o1 : PolynomialRing

    i2 : f = map(T,T,{x+1})

    o2 = map(T,T,{x + 1})

    o2 : RingMap T <--- T

    i3 : f.cache.inverse = foo

    o3 = foo

    o3 : Symbol

    i4 : f^-1

    o4 = foo

    o4 : Symbol

    i5 : f

    o5 = map(T,T,{x + 1})

    o5 : RingMap T <--- T

    i6 : f^10

    o6 = map(T,T,{x + 10})

    o6 : RingMap T <--- T

    i7 : f^-3
    stdio:7:2:(2):[2]: not implemented yet
