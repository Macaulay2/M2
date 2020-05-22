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
end
print generateAssertions ///
6
///
