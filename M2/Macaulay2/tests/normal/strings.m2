---- concatenate
-- one arg
assert(concatenate("ABC")=="ABC");
assert(concatenate(2)=="  ");
assert(concatenate(0)=="");
assert(concatenate(-1)=="");
-- multiple args
assert(concatenate("ABC","DEF")=="ABCDEF");
assert(concatenate("ABC",2)=="ABC  ");
assert(concatenate(2,"ABC")=="  ABC");
assert(concatenate("ABC",0)=="ABC");
assert(concatenate(0,"ABC")=="ABC");
assert(concatenate("ABC",-1)=="ABC");
assert(concatenate(-1,"ABC")=="ABC");
assert(concatenate("ABC",-2^10)=="ABC");
assert(concatenate(-2^10,"ABC")=="ABC");
assert(concatenate(-2^10,2^10)==concatenate(2^10));

---- pad
assert(pad("ABC",4)=="ABC ");
assert(pad(4,"ABC")==" ABC");
assert(pad("ABC",3)=="ABC");
assert(pad(3,"ABC")=="ABC");
-- ignore the numbers if it's too short
assert(pad("ABC",2)=="ABC");
assert(pad(2,"ABC")=="ABC");
-- nets
assert(pad(4,"ABC"^1)==" ABC"^1)
assert(pad("ABC"^1,4)=="ABC "^1)
assert(pad(4,"ABC\nDEF"^1)==" ABC\n DEF"^1)
assert(pad("ABC\nDEF"^1,4)=="ABC\nDEF "^1)
assert(pad(3,"ABC"^1)=="ABC"^1)
assert(pad("ABC"^1,3)=="ABC"^1)
assert(pad(2,"ABC"^1)=="ABC"^1)
assert(pad("ABC"^1,2)=="ABC"^1)

-- commentize
debug Core
assert(commentize null=="")
assert(commentize()==horizontalJoin())
assert(commentize("ABC")==" -- ABC")
assert(commentize("ABC","DEF")==" -- ABCDEF")
assert(commentize("ABC\nDEF")==" -- ABC\n -- DEF")
assert(commentize("ABC"^1)==" -- ABC"^1)
assert(commentize("ABC\nDEF"^1)==" -- ABC\n -- DEF"^1)

-- iteration
assert Equation(toList "foo", {"f", "o", "o"})
assert Equation(toSequence "foo", ("f", "o", "o"))
assert Equation(for c in "foo" list c, {"f", "o", "o"})
i = 0
scan("aaaaaaaaaa", c -> (assert Equation(c, "a"); i = i + 1))
assert Equation(i, 10)
assert Equation(apply("foo", identity), ("f", "o", "o"))
assert Equation(apply("foo", "bar", concatenate), ("fb", "oa", "or"))
assert Equation(apply("foo", ("b", "a", "r"), concatenate), ("fb", "oa", "or"))
assert Equation(apply(("f", "o", "o"), "bar", concatenate), ("fb", "oa", "or"))

-- reverse
assert Equation(reverse "Hello, world!", "!dlrow ,olleH")

-- pack
assert Equation(pack("The quick brown fox jumps over the lazy dog", 5),
    {"The q","uick ","brown"," fox ","jumps"," over"," the ","lazy ","dog"})
assert Equation(pack(5, "The quick brown fox jumps over the lazy dog"),
    {"The q","uick ","brown"," fox ","jumps"," over"," the ","lazy ","dog"})
assert Equation(pack(0, ""), pack("", 0))

-- tally
assert BinaryOperation(symbol ===, tally "Hello, world!", new Tally from {
	" " => 1, "!" => 1, "r" => 1, "d" => 1, "e" => 1, "w" => 1,
	"H" => 1, "l" => 3, "," => 1, "o" => 2})
