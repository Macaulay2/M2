-----------------------------------------------------------------------------
-- Enum for passing internal flags to the regex engine
-----------------------------------------------------------------------------

-- Keep this enum in sync with RegexFlags in Macaulay2/e/regex.cpp
RegexFlags = new HashTable from {
    "ECMAScript" => 0,        -- ECMAScript flavor (default)
    "Extended"   => (1 << 1), -- POSIX ERE flavor

    "Icase"  => (1 << 5),  -- ignore case
    "Nosubs" => (1 << 6),  -- ignore subexpressions
    "Collate" => (1 << 8), -- makes [a-b] locale sensitive

    "NoModM"   => (1 << 12), -- don't match ^ $ with newlines
    "NoModS"   => (1 << 13), -- don't match . with newlines

    "NoEscapeInLists" => (1 << 17), -- disable \ escapes in lists
    "NoBkRefs"        => (1 << 18), -- disable backreferences

    "MatchAny"           => (1 << 25), -- return any match
    "MatchContinuous"    => (1 << 27), -- match must start at the beginning
    "MatchPrevAvail"     => (1 << 30), -- lead-1 is a valid iterator position
    "MatchNotDotNewline" => (1 << 31), -- doesn't match . with newlines
    }

RegexPerl  = RegexFlags#"ECMAScript" | RegexFlags#"NoModS"
RegexPOSIX = RegexFlags#"Extended"   | RegexFlags#"MatchNotDotNewline" -- RegexFlags#"NoEscapeInLists"

-- Note: the default may be adjusted by adding the following to the user's init.m2 file:
--   Core#"private dictionary"#"defaultRegexFlags" <- RegexPerl
defaultRegexFlags = RegexPOSIX
defaultMatchFlags = RegexFlags#"Nosubs" | RegexFlags#"MatchAny"

-----------------------------------------------------------------------------
-- regex
-----------------------------------------------------------------------------

regex' := regex
regex = method(TypicalValue => List, Options => {Flags => null})
regex(String,         String) := opts -> (re, str)              -> regex(re, 0,    length str, str, opts)
regex(String, ZZ,     String) := opts -> (re, head, str)        -> regex(re, head, length str, str, opts)
regex(String, ZZ, ZZ, String) := opts -> (re, head, range, str) -> (
    tail := length str;
    flags := if opts.Flags =!= null then opts.Flags else defaultRegexFlags;
    if head + range >= tail then return regex'(re, head, tail, str, flags);
    -- When head + range != tail, this is backwards compatible with GNU regex in Extended POSIX flavor;
    -- however, the lookbehind feature of ECMAScript flavor doesn't work in this case.
    flags = flags | (if head + range != tail then RegexFlags#"MatchContinuous" else 0);
    if range >= 0
    then for lead from 0 to range when head + lead <= tail do (
	ret := regex'(re, head + lead, tail, str, flags);
	if ret =!= null then return ret)
    else for lead from 0 to -range when head - lead >= 0 do (
	ret := regex'(re, head - lead, tail, str, flags);
	if ret =!= null then return ret))
protect symbol regex

-- previously in nets.m2
separateRegexp = method(Options => options regex)
separateRegexp(String,     String) := opts -> (re,    s) -> separateRegexp(re, 0, s, opts)
separateRegexp(String, ZZ, String) := opts -> (re, n, s) -> (
    offset := 0;
    while offset <= #s
    list (
	m := regex(re,offset,s);
	if m#?n
	then first (substring(s,offset,m#n#0-offset), offset = m#n#0+m#n#1)
	else first (substring(s,offset), offset = #s + 1)))

selectRegexp = method()
selectRegexp(String,     String) := (re,    s) -> selectRegexp(re, 0, s)
selectRegexp(String, ZZ, String) := (re, n, s) -> (
    m := regex(re, s);
    if m#?n then substring(m#n#0,m#n#1,s) else error "regular expression didn't match")

-----------------------------------------------------------------------------
-- match
-----------------------------------------------------------------------------

lastMatch = null
match = method(TypicalValue => Boolean, Options => {Flags => null})
match(String, String) := opts -> (re, str) ->
    null =!= (lastMatch = regex(re, str, Flags => (
		if opts.Flags =!= null then opts.Flags
		else defaultRegexFlags | defaultMatchFlags)))
match(List, String) := opts -> (rs, str) -> any(rs, re -> match(re, str, opts))

-----------------------------------------------------------------------------
-- replace
-----------------------------------------------------------------------------

-- previously in methods.m2
replace = method(Options => options regex)
replace(String, String, String) := String => opts -> (re, s, r) ->
    regexReplace(re, s, r, if opts.Flags =!= null then opts.Flags else defaultRegexFlags)

-- previously in html0.m2
toLower = s -> replace("(\\w+)", "\\L$1", s)
toUpper = s -> replace("(\\w+)", "\\U$1", s)

-----------------------------------------------------------------------------
-- regexQuote
-----------------------------------------------------------------------------

regexQuote = method(Dispatch => Thing, TypicalValue => String)
regexQuote String := s -> (
    specialChars := {"\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}"};
    concatenate apply(characters s, c ->
	if member(c, specialChars) then "\\" | c else c))
