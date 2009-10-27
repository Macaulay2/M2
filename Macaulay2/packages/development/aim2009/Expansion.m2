newPackage "Expansion"
export "expand"

add = method()
multiply = method()
exponentiate = method()
expand = method()

add(Expression,Thing) := add(Thing,Expression) := plus
multiply(Expression,Thing) := multiply(Thing,Expression) := times
exponentiate(Expression,Thing) := exponentiate(Thing,Expression) := power

add(Thing,Thing) := (x,y) -> new Sum from {x,y}
multiply(Thing,Thing) := (x,y) -> new Product from {x,y}
exponentiate(Thing,Thing) := (x,n) -> new Power from {x,n}

multiply(Sum,Sum) := (x,y) -> fold(add,flatten apply(toList x,t -> apply(toList y, u -> multiply(t,u))))
multiply(Sum,Thing) := (x,y) -> fold(add,apply(toList x,t -> multiply(t,y)))
multiply(Thing,Sum) := (x,y) -> fold(add,apply(toList y,u -> multiply(x,u)))

binomialate = (n,x,y) -> fold(add,apply(n+1,i->multiply(binomial(n,i),multiply(exponentiate(x,n-i),exponentiate(y,i)))))

exponentiate(Product,Thing) := (x,n) -> fold(multiply,apply(toList x, t -> exponentiate(t,n)))
exponentiate(Thing,Sum) := (x,n) -> fold(multiply,apply(toList x, t -> exponentiate(t,n)))
exponentiate(Product,Sum) := (x,y) -> fold(multiply,flatten apply(toList x, t -> apply(toList y, u -> exponentiate(t,u))))
exponentiate(Sum,ZZ) := (x,n) -> (
     if n === 0 then 1 
     else if n < 0 then new Power from {x,n}
     else if #x === 0 then x
     else if #x === 1 then exponentiate(x#0, n)
     else fold(binomialate_n,toList x))
exponentiate(Thing,ZZ) := (x,n) -> if n === 0 then expression 1 else if n === 1 then x else new Power from {x,n}

expand Thing := identity
expand Product := x -> fold(multiply,expand \ toList x)
expand Sum := x -> fold(add,expand \ toList x)
expand Power := x -> fold(exponentiate,expand \ toList x)

end 
loadPackage "Expansion"
expand (hold x + y)^6
