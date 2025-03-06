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
str = "The quick brown fox jumps over the lazy dog"
cut = {"The q","uick ","brown"," fox ","jumps"," over"," the ","lazy ","dog"}
assert Equation(pack(5, str), cut)
assert Equation(pack(str, 5), cut)
assert Equation(pack(5, ascii str), ascii \ cut)
assert Equation(pack(ascii str, 5), ascii \ cut)
assert Equation(pack(5, toSequence ascii str), ascii \ cut)
assert Equation(pack(toSequence ascii str, 5), ascii \ cut)
assert Equation(pack(0, ""), {})
assert Equation(pack("", 0), {})

-- tally
assert BinaryOperation(symbol ===, tally "Hello, world!", new Tally from {
	" " => 1, "!" => 1, "r" => 1, "d" => 1, "e" => 1, "w" => 1,
	"H" => 1, "l" => 3, "," => 1, "o" => 2})

-- escape sequences
assert Equation(ascii "\"\\\a\b\e\E\f\n\r\t\v",
    {0x22, 0x5c, 0x07, 0x08, 0x1b, 0x1b, 0x0c, 0x0a, 0x0d, 0x09, 0x0b})
assert Equation("\172\x7a\x7A\u007a\u007A", "zzzzz")
assert Equation(format(ascii(0..31) | "\"\\"),
    ///"\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\b\t\n\u000b\f\r/// |
    ///\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017/// |
    ///\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f\"\\"///)
