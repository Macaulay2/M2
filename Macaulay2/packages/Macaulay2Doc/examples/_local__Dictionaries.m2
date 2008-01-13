f := x -> y -> z -> 11;
d := localDictionaries ((f 22) 33)
peek d
d#0#"y"
value d#0#"y"
peek localDictionaries()
