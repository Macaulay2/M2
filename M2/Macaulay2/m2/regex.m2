-----------------------------------------------------------------------------
-- Enum for passing internal flags to the regex engine
-----------------------------------------------------------------------------

-- Keep this enum in sync with RegexFlags in Macaulay2/e/regex.cpp
RegexFlags = new HashTable from {
    "ECMAScript" => 0,        -- ECMAScript flavor (default)
    "Extended"   => (1 << 1), -- POSIX ERE flavor
    "Literal"    => (1 << 2), -- treat the pattern text as literal

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
-- Local utilities
-----------------------------------------------------------------------------

defaultOpts := (opt, def) -> if opt =!= null then opt else def

-----------------------------------------------------------------------------
-- regex
-----------------------------------------------------------------------------

regex' := regex
regex = method(TypicalValue => List, Options => {Flags => null})
regex(String,         String) := opts -> (re,              str) -> regex(re, 0,    length str, str, opts)
regex(String, ZZ,     String) := opts -> (re, head,        str) -> regex(re, head, length str, str, opts)
regex(String, ZZ, ZZ, String) := opts -> (re, head, range, str) -> (
    tail := length str;
    flags := defaultOpts(opts.Flags, defaultRegexFlags);
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

-----------------------------------------------------------------------------
-- separate
-----------------------------------------------------------------------------

separate' = separate
separate = method(TypicalValue => List, Options => options regex)
separate(            String) := opts -> (       str) -> separate'("\r?\n", str, -1)
separate(String,     String) := opts -> (re,    str) -> separate'(re, str, defaultOpts(opts.Flags, RegexFlags#"Literal"))
separate(String, ZZ, String) := opts -> (re, n, str) -> (
    (offset, tail) := (0, length str);
    while offset <= tail list (
	m := regex(re, offset, tail, str);
	if m#?n
	then first (substring(str, offset, m#n#0 - offset), offset = m#n#0 + max(1, m#n#1))
	else first (substring(str, offset), offset = tail + 1)))
protect symbol separate

-- TODO: deprecate this
separateRegexp = method(TypicalValue => List, Options => options regex)
separateRegexp(String, String) := opts -> (re, str) -> separate'(re, str,  defaultOpts(opts.Flags, defaultRegexFlags))
separateRegexp(String, ZZ, String) := lookup(separate, String, ZZ, String)

-----------------------------------------------------------------------------
-- select
-----------------------------------------------------------------------------

select(String,         String) := List => opts -> (re,       str) -> select(re, "$&", str, opts)
select(String, String, String) := List => opts -> (re, form, str) -> (
    select'(re, form, str, defaultOpts(opts.Flags, defaultRegexFlags)))
protect symbol select

-----------------------------------------------------------------------------
-- match
-----------------------------------------------------------------------------

lastMatch = null
match = method(TypicalValue => Boolean, Options => options regex)
match(List,   String) := opts -> (rs, str) -> any(rs, re -> match(re, str, opts))
match(String, String) := opts -> (re, str) ->
    null =!= (lastMatch = regex(re, str, Flags =>
	    defaultOpts(opts.Flags, defaultRegexFlags | defaultMatchFlags)))

-----------------------------------------------------------------------------
-- replace
-----------------------------------------------------------------------------

-- previously in methods.m2
replace' := replace
replace = method(Options => options regex)
replace(String, String, String) := String => opts -> (re, s, r) ->
    replace'(re, s, r, defaultOpts(opts.Flags, defaultRegexFlags))
protect symbol replace

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
