x = a .. z
x#12
y = new MutableList from x
y#12 = foo
toSequence y
