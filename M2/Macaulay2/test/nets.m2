-- nets

t = "   asdf qwer dfhh   xcvb  eryy wert"
t = t||t||t
wrap(10,t)

List.BeforePrint = x -> wrap(80, net x)
toList ( 0 .. 100 )
