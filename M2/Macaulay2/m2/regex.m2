-* Copyright 2020 by Mahrud Sayrafi *-

-- See RegexFlags defined in Macaulay2/d/regex.dd for a list of available flags.
-- More flags can be added there.

regexSpecialChars = concatenate(
    "([", apply({"\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}"}, c -> "\\" | c), "])")

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

setRegexFlags = opts -> (
    if instance(opts, ZZ) then return opts;
    if opts.?POSIX and instance(opts.POSIX, Boolean) then if opts.POSIX
    then RegexFlags#"extended"  - (RegexFlags#"no_bk_refs" | RegexFlags#"no_escape_in_lists")
    else RegexFlags#"ECMAScript" | RegexFlags#"no_mod_s"
    else error "regex: expected true or false for option POSIX => ...")

setMatchFlags = opts -> (
    if instance(opts, ZZ) then return opts;
    if opts.?POSIX and instance(opts.POSIX, Boolean) then if opts.POSIX
    then RegexFlags#"format_sed" | RegexFlags#"match_not_dot_newline"
    else RegexFlags#"format_perl"
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
    (regexFlags, matchFlags) := (setRegexFlags opts, setMatchFlags opts);
    if head + range >= tail then return regex'(re, head, tail, str, regexFlags, matchFlags);
    -- When head + range != tail, this is backwards compatible with GNU regex in Extended POSIX flavor;
    -- however, the lookbehind feature of Perl flavor doesn't work in this case.
    matchFlags = matchFlags | (if head + range != tail then RegexFlags#"match_continuous" else 0);
    if range >= 0
    then for lead from 0 to range when head + lead <= tail do (
	ret := regex'(re, head + lead, tail, str, regexFlags, matchFlags);
	if ret =!= null then return ret)
    else for lead from 0 to -range when head - lead >= 0 do (
	ret := regex'(re, head - lead, tail, str, regexFlags, matchFlags);
	if ret =!= null then return ret))
protect symbol regex

-----------------------------------------------------------------------------
-- separate
-----------------------------------------------------------------------------

separate' = separate
separate = method(TypicalValue => List, Options => options regex)
separate(            String) := opts -> (       str) -> separate("\r?\n", str, opts)
separate(String,     String) := opts -> (re,    str) -> (
    regexFlags := if length re == 1 and match(regexSpecialChars, re) then (
	stderr << "warning: unescaped special character '" << re << "' found (and escaped) in call to 'separate'" << endl;
	RegexFlags#"literal") else setRegexFlags opts;
    separate'(re, str, regexFlags, setMatchFlags opts))
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

select(String,         String) := List => options regex >> opts ->
    (re,       str) -> select'(re, "$&", str, setRegexFlags opts, setMatchFlags opts)
select(String, String, String) := List => options regex >> opts ->
    (re, form, str) -> select'(re, form, str, setRegexFlags opts, setMatchFlags opts)
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
    null =!= (lastMatch = regex'(re, 0, length str, str, setRegexFlags opts, setMatchFlags opts))

-----------------------------------------------------------------------------
-- replace
-----------------------------------------------------------------------------

-- previously in methods.m2
replace' = replace
replace = method(Options => true)
replace(String, String, String) := String => options regex >>
    opts -> (re, s, r) -> replace'(re, s, r, setRegexFlags opts, setMatchFlags opts)
protect symbol replace

-- previously in html0.m2
toLower = s -> replace("(\\w+)", "\\L$1", s)
toUpper = s -> replace("(\\w+)", "\\U$1", s)

-----------------------------------------------------------------------------
-- regexQuote
-----------------------------------------------------------------------------

regexQuote = method(Dispatch => Thing, TypicalValue => String)
regexQuote String := s -> replace(regexSpecialChars, "\\\\$1", s)
