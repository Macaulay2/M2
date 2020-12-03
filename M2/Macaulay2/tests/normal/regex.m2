-- tests for regex
assert(regex("^(.*[^ ])? *$", " abcdef ") === {(0, 8), (0, 7)})
assert(regex("^ *([^ ].*[^ ])? *$", " abcdef ") === {(0, 8), (1, 6)})
assert(regex("^ *([^ ].*)$", " abcdef ") === {(0, 8), (1, 7)})
assert(regex(".?", "") === {(0, 0)})
assert(regex(".*/", "/aa/bb") === {(0, 4)})
assert(regex("a|b", "a") === {(0,1)})
assert(regex("^a+$", " \naaa\n ") === {(2,3)})
assert(regex("$a"," $a") === null)
assert(regex(".*", "a\nb") === {(0, 1)})
assert(regex("a+", " aaa ") === {(1,3)})
assert(regex("a+", 0, " aaa ") === {(1,3)})
assert(regex("a+", 0, 0, " aaa ") === null)
assert(regex("a+", 0, 1, " aaa ") === {(1,3)})
assert(regex("a+", 0, 100, " aaa ") === {(1,3)})
assert(regex("(a|ab)(c|bcd)(d*)", "abcd")                === {(0,4),(0,1),(1,3),(4,0)})
assert(regex("(a|ab)(c|bcd)(d*)", "abcd", POSIX => true) === {(0,4),(0,2),(2,1),(3,1)})

-- tests for replace
s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
assert(replace("(A|B|C)", "\\L$1", s) === "abcdefghijklmnopqrstuvwxyzabcDEFGHIJKLMNOPQRSTUVWXYZ")
assert(replace("(a|b|c)", "\\U$1", s) === "ABCdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
assert(replace("---", "DEF", replace("DEF", "---", s)) === s)
assert(replace("(B)", ///\\bar \1///, "ABC") === "A\\bar BC")
assert(replace("a(b+)(c+)", "\\2\\1", " abbcc abccc ") === " ccbb cccb ")
assert(replace("a(b+).(c+)", "\\2\\1", " abbcc ab\nccc ") === " cbb ab\nccc ")
assert(replace("a", "b", "-a-a-") === "-b-b-")
assert(replace("^a", "x", "a \na \naaa a") === "x \nx \nxaa a")

-- tests for separate
s = "A\nB\r\nC"
assert(separate s === {"A","B","C"})
assert(separate s === lines s)
assert(separate "\n\nA\n\nB\n\n" === {"","","A","","B","",""})
assert(separateRegexp("[,.;]", "A:B.C,D,E;F.") === {"A:B","C","D","E","F",""})
assert(separate("[,.;]", "A:B.C,D,E;F.", POSIX => true) === {"A:B","C","D","E","F",""})
assert(concatenate separate("[\t ]+", " A 	 B C   D	E  F     G", POSIX => true) === "ABCDEFG")
s = "algng xjfr kfjxse xhgfj xooi xwj kvexr anvi endj xkfi"
assert(concatenate separate(" x[A-Za-z]*", s, POSIX => true) === "algng kfjxse kvexr anvi endj")
assert(concatenate separate(" (x)[A-Za-z]*", 1, s, POSIX => true) === "algng jfr kfjxse hgfj ooi wj kvexr anvi endj kfi")
assert(demark_" " separate("[ \t]*\r?\n[ \t]*", " A\n\t  B  \r\n  \tC ", POSIX => false) === " A B C ")
-- TODO: at some point deprecate support for this
assert(separate(".", "ABC.DEF") === {"ABC", "DEF"})
assert(separate("-", "a-cd-xxx-yyy-") == {"a", "cd", "xxx", "yyy", ""})
assert(separate(".?", "ABCD") === toList(5:""))
-- zero-length separations
assert(separate(    "a",  "ahoaaabba") == {"", "ho", "", "", "bb", ""})
assert(separate( "(?=a)", "ahoaaabba") == {"aho", "a", "a", "abb", "a"}) -- positive lookahead
assert(separate( "(?!a)", "ahoaaabba") == {"a", "h", "oaaa", "b", "ba"}) -- negative lookahead
assert(separate("(?<=a)", "ahoaaabba") == {"a", "hoa", "a", "a", "bba"}) -- positive lookbehind
-- FIXME: this one doesn't seem to work, are negative lookbehinds different in boost regex?
--assert(separate("(?<!a)", "ahoaaabba") == {"ah", "o", "aaab", "b", "a"}) -- negative lookbehind

-- tests for select
assert(select("a+","aaa aaaa") === {"aaa","aaaa"})
assert(select("[[:alpha:]]+", "Dog, cat, and deer.") === {"Dog","cat","and","deer"})
assert(select("^.*$", "ABC\nDEF\r\nGHI") === {"ABC","DEF","GHI"})
assert(select("([a-zA-Z]+);", "$1", "Dog; cat, deer;") === {"Dog","deer"})
assert(select("([a-zA-Z]+);", "\\L$1", "Dog; cat, deer;") === {"dog","deer"})
s = "catfish cats dogs"
assert(select("cat(?!fish)", s, POSIX => false) === {"cat"})
assert(select("\\w+(?=s\\b)", s, POSIX => false) === {"cat", "dog"})
s = "goldfish swordfish catfish catdog"
assert(select("\\w+(?=fish)", s, POSIX => false) === {"gold","sword","cat"})
assert(select("(?<=cat)\\w+", s, POSIX => false) === {"fish","dog"})

-- tests for match
assert not match(".a",   "  \na  ")
assert     match("^a",   "  \na  ")
assert     match(".a",   "  a  ")
assert not match("^a",   "  a  ")
assert not match("a\\>", "ab")
assert not match("a\\b", "ab")
assert match("(a)",   "aa")
assert match("a+",    "aa")
assert match("a[2]",  "a2")
assert match("a[^a]", "a2")
assert match("a\\>",  "a b")
assert match("a\\>",  "a")
assert match("a\\b",  "a b")
assert match("a{2}",  "aa")
assert match("a|b",   "a b")
assert (lastMatch === {(0, 1)})
assert match({"Cat", "Dog"}, "CatDog")
assert match({"Cat", "Dog"}, "CatDog", Strategy => all)
assert match({"Cat", "Dog"}, "Catfish")
assert not match({"Cat", "Dog"}, "Catfish", Strategy => all)
assert not match("cats", "three dogs, two catfishes, and a cat")
assert     match("cat",  "three dogs, two catfishes, and a cat")
assert not match("(?<!cat)fish", "cat catfish dog", POSIX => false)
assert     match("(?<!cat)fish", "cat swordfish dog", POSIX => false)
s = "catfish cat dog"
assert match("cat(?!fish)", s, POSIX => false)
assert(substring(lastMatch#0#0, lastMatch#0#1 + 4, s) == "cat dog")
assert match("cat(?=fish)", s, POSIX => false)
assert(substring(lastMatch#0#0, lastMatch#0#1 + 4, s) == "catfish")

-- tests for regexQuote
assert match(regexQuote "foo\\", "foo\\")
assert match(regexQuote "foo^", "foo^")
assert match(regexQuote "foo$", "foo$")
assert match(regexQuote "foo.", "foo.")
assert match(regexQuote "foo|", "foo|")
assert match(regexQuote "foo?", "foo?")
assert match(regexQuote "foo*", "foo*")
assert match(regexQuote "foo+", "foo+")
assert match(regexQuote "(foo)", "(foo)")
assert match(regexQuote "[foo]", "[foo]")
assert match(regexQuote "{foo}", "{foo}")

-- the following matches would all return true without regexQuote
assert not match(regexQuote "\\w",  "foo")
assert not match(regexQuote "^foo", "foo")
assert not match(regexQuote "foo$", "foo")
assert not match(regexQuote "foo.", "fooo")
assert not match(regexQuote "f|oo", "oo")
assert not match(regexQuote "foo?", "fo")
assert not match(regexQuote "foo*", "fo")
assert not match(regexQuote "foo+", "foo")
assert not match(regexQuote "(foo)", "foo")
assert not match(regexQuote "[foo]", "foo")
assert not match(regexQuote "foo{1}", "foo")
