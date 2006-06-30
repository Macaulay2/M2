d = new Dictionary
sym = getGlobalSymbol(d,"foo")
d
peek d
d#"foo" === sym
d#"asfd" = sym
peek d
