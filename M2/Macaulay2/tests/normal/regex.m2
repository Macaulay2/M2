assert( regex(".*/","/aa/bb") === {(0, 4)} )
assert( regex("a|b","a") === {(0,1)} )
assert( regex("^a+$"," \naaa\n ") === {(2,3)} )
assert( replace("a","b","-a-a-") === "-b-b-" )
assert( regex("$a","$a") === null )
assert( regex(".*","a\nb") === {(0, 1)} )
assert( select("a+","aaa aaaa") === {"aaa","aaaa"} )
assert( (regex("a+"," aaa ")) === {(1,3)} )
assert( (regex("a+",0," aaa ")) === {(1,3)} )
assert( (regex("a+",0,0," aaa ")) === null )
assert( (regex("a+",0,1," aaa ")) === {(1,3)} )
assert( (regex("a+",0,100," aaa ")) === {(1,3)} )
s = "lpdibdjlpwvwvpvuovhqdjcgdufxqfwixsmtknxguojtkslkizqddomtxadwnltmaqlihbujxyfkistsrtvtyqyhpqdtpqwvzayk"
assert(replace("b","B",replace("a","A",s)) === "lpdiBdjlpwvwvpvuovhqdjcgdufxqfwixsmtknxguojtkslkizqddomtxAdwnltmAqlihBujxyfkistsrtvtyqyhpqdtpqwvzAyk")
assert(replace("b","B",replace("a","A",s)) === replace("a","A",replace("b","B",s)))
assert( (replace("a(b+)(c+)","\\2\\1"," abbcc abccc ")) === " ccbb cccb " )
assert( (replace("a(b+).(c+)","\\2\\1"," abbcc ab\nccc ")) === " cbb ab\nccc " )

assert match(regexQuote ///foo\///, ///foo\///)
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
assert not match(regexQuote ///\w///, "foo")
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

assert (separateRegexp ("-","a-cd-xxx-yyy-") == {"a", "cd", "xxx", "yyy", ""})

assert (try separateRegexp(".?", "ABCD") else true)
    -- i3 : separateRegexp(".?", "ABCD")
    -- stdio:3:1:(3): error: separateRegexp: regular expression made no progress

end
print generateAssertions ///
6
///
