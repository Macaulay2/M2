y := 66							    -- variable locally defined
L00input = temporaryFileName()
L00input << "y = 55" << endl << close
load L00input
assert (y === 66)					    -- checking integrity of lexical scope

