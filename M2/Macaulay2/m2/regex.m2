-----------------------------------------------------------------------------
-- Enum for passing internal flags to the regex engine
-----------------------------------------------------------------------------

-- Keep this enum in sync with RegexFlags in Macaulay2/d/boost-regex.cpp
-- The values are used as a bitmask in order to simplify passing flags to
-- the various functions defined there.
RegexFlags = new HashTable from {
    "ECMAScript" =>  0,       -- ECMAScript flavor (default)
    "Extended"   => (1 << 1), -- POSIX ERE flavor
    "Literal"    => (1 << 2), -- treat the pattern text as literal

    "Icase"   => (1 << 5), -- ignore case
    "Nosubs"  => (1 << 6), -- ignore subexpressions
    "Collate" => (1 << 8), -- makes [a-b] locale sensitive

    "NoModM"   => (1 << 12), -- don't match ^ $ with newlines
    "NoModS"   => (1 << 13), -- don't match . with newlines

    "NoEscapeInLists" => (1 << 17), -- disable \ escapes in lists
    "NoBkRefs"        => (1 << 18), -- disable backreferences

    "MatchAny"           => (1 << 25), -- return any match
    "MatchNotNull"       => (1 << 26), -- match must be nonempty
    "MatchContinuous"    => (1 << 27), -- match must start at the beginning
    "MatchPrevAvail"     => (1 << 30), -- lead-1 is a valid iterator position
    "MatchNotDotNewline" => (1 << 31), -- doesn't match . with newlines
    }

regexSpecialChars = concatenate(
    "([", apply({"\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}"}, c -> "\\" | c), "])")

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

setRegexFlags = opts -> (
    if instance(opts, ZZ) then return opts;
    if opts.?POSIX and instance(opts.POSIX, Boolean) then if opts.POSIX
    then RegexFlags#"Extended"   | RegexFlags#"MatchNotDotNewline"
    else RegexFlags#"ECMAScript" | RegexFlags#"NoModS"
    else error "regex: expected true or false for option POSIX => ...")

-----------------------------------------------------------------------------
-- regex
-----------------------------------------------------------------------------

regex' = regex
regex = method(TypicalValue => List, Options => {POSIX => false})
regex(String,         String) := opts -> (re,              str) -> regex(re, 0,    length str, str, opts)
regex(String, ZZ,     String) := opts -> (re, head,        str) -> regex(re, head, length str, str, opts)
regex(String, ZZ, ZZ, String) := opts -> (re, head, range, str) -> (
    tail := length str;
    flags := setRegexFlags opts;
    if head + range >= tail then return regex'(re, head, tail, str, flags);
    -- When head + range != tail, this is backwards compatible with GNU regex in Extended POSIX flavor;
    -- however, the lookbehind feature of Perl flavor doesn't work in this case.
    flags = flags | (if head + range != tail then RegexFlags#"MatchContinuous" else 0);
    if range >= 0
    then for lead from 0 to range when head + lead <= tail do (
	ret := regex'(re, head + lead, tail, str, flags);
	if ret =!= null then return ret)
    else for lead from 0 to -range when head - lead >= 0 do (
	ret := regex'(re, head - lead, tail, str, flags);
	if ret =!= null then return ret))
protect symbol regex

-----------------------------------------------------------------------------
-- separate
-----------------------------------------------------------------------------

separate' = separate
separate = method(TypicalValue => List, Options => options regex)
separate(            String) := opts -> (       str) -> separate'("\r?\n", str, -1)
separate(String,     String) := opts -> (re,    str) -> (
    flags := if length re == 1 and match(regexSpecialChars, re) then (
	stderr << "warning: unescaped special character '" << re << "' found in call to 'separate'" << endl;
	RegexFlags#"Literal") else setRegexFlags opts;
    separate'(re, str, flags))
separate(String, ZZ, String) := opts -> (re, n, str) -> (
    (offset, tail) := (0, length str);
    while offset <= tail list (
	m := regex(re, offset, tail, str, opts);
	if m#?n
	then first (substring(str, offset, m#n#0 - offset), offset = m#n#0 + max(1, m#n#1))
	else first (substring(str, offset), offset = tail + 1)))
protect symbol separate

-- Deprecated
separateRegexp = separate

-----------------------------------------------------------------------------
-- select
-----------------------------------------------------------------------------

select(String,         String) := List => {POSIX => true} >> opts ->
    (re,       str) -> select'(re, "$&", str, setRegexFlags opts)
select(String, String, String) := List => {POSIX => true} >> opts ->
    (re, form, str) -> select'(re, form, str, setRegexFlags opts)
protect symbol select

-----------------------------------------------------------------------------
-- match
-----------------------------------------------------------------------------

lastMatch = null
match = method(TypicalValue => Boolean, Options => options regex ++ {Strategy => any})
match(List,   String) := opts -> (rs, str) -> (
    if member(opts.Strategy, {any, all}) then (opts.Strategy)(rs, re -> match(re, str, opts))
    else error concatenate("unknown quantifier for match: ", toString opts.Strategy))
match(String, String) := opts -> (re, str) ->
    null =!= (lastMatch = regex'(re, 0, length str, str, setRegexFlags opts))

-----------------------------------------------------------------------------
-- replace
-----------------------------------------------------------------------------

-- previously in methods.m2
replace' = replace
replace = method(Options => true)
replace(String, String, String) := String => options regex >>
    opts -> (re, s, r) -> replace'(re, s, r, setRegexFlags opts)
protect symbol replace

-- previously in html0.m2
toLower = s -> replace("(\\w+)", "\\L$1", s)
toUpper = s -> replace("(\\w+)", "\\U$1", s)

-----------------------------------------------------------------------------
-- regexQuote
-----------------------------------------------------------------------------

regexQuote = method(Dispatch => Thing, TypicalValue => String)
regexQuote String := s -> replace(regexSpecialChars, "\\\\$1", s)
