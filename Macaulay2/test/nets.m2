-- nets

t = "   asdf qwer dfhh   xcvb  eryy wert"
t = t||t||t
wrap("-",10,t)

List.BeforePrint = x -> wrap("-", 80, net x)
toList ( 0 .. 100 )

-- net/string conversion

x = "asdf\nasdf\n"
assert ( toString net x === x )

x = "asdf\nasdf\nqwer"
assert ( toString net x === x )

x = "\nasdf\nqwer"
assert ( toString net x === x )

