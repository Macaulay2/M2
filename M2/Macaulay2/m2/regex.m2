-----------------------------------------------------------------------------
-- Enum for passing internal flags to the regex engine
-----------------------------------------------------------------------------

-- Keep this enum in sync with RawRegexFlags in Macaulay2/e/regex.cpp
RawRegexFlags = new HashTable from {
  "REGEX_FLAVOR_ECMAScript" => (1 << 0), -- ECMAScript flavor (default)
  "REGEX_FLAVOR_BASIC"      => (1 << 1), -- POSIX BRE flavor
  "REGEX_FLAVOR_EXTENDED"   => (1 << 2), -- POSIX ERE flavor

  "REGEX_SYNTAX_ICASE"    => (1 << 8),  -- ignore case
  "REGEX_SYNTAX_NOSUBS"   => (1 << 9),  -- ignore subexpressions
  "REGEX_SYNTAX_NO_MOD_M" => (1 << 14), -- don't match ^ $ with newlines
  -- Note: this one is forced in e/regex.cpp for backwards compatibility
  "REGEX_SYNTAX_NO_MOD_S" => (1 << 15), -- don't match . with newlines

  "REGEX_MATCH_ANY"        => (1 << 16), -- return any match
  "REGEX_MATCH_CONTINUOUS" => (1 << 17), -- match must start at the beginning
  }

defaultRegexFlags := RawRegexFlags#"REGEX_FLAVOR_ECMAScript" | RawRegexFlags#"REGEX_SYNTAX_NO_MOD_S";

defaultMatchFlags := defaultRegexFlags | RawRegexFlags#"REGEX_SYNTAX_NOSUBS" | RawRegexFlags#"REGEX_MATCH_ANY";

-----------------------------------------------------------------------------
-- regex
-----------------------------------------------------------------------------

rawRegex := regex
regex = method(TypicalValue => List)
regex(String, String)         := (re,             str) -> rawRegex(re,             str, defaultRegexFlags)
regex(String, ZZ, String)     := (re, head,       str) -> rawRegex(re, head,       str, defaultRegexFlags)
regex(String, ZZ, ZZ, String) := (re, head, tail, str) -> rawRegex(re, head, tail, str, defaultRegexFlags)
protect symbol regex

-- previously in nets.m2
separateRegexp = method()
separateRegexp(String,String) := (re,s) -> separateRegexp(re,0,s)
separateRegexp(String,ZZ,String) := (re,n,s) -> (
    offset := 0;
    while offset <= #s
    list (
	m := regex(re,offset,s);
	if m#?n
	then first (substring(s,offset,m#n#0-offset), offset = m#n#0+m#n#1)
	else first (substring(s,offset), offset = #s + 1)))

selectRegexp = method()
selectRegexp(String, String)     := (re,    s) -> selectRegexp(re, 0, s)
selectRegexp(String, ZZ, String) := (re, n, s) -> (
    m := regex(re, s);
    if m#?n then substring(m#n#0,m#n#1,s) else error "regular expression didn't match")

-----------------------------------------------------------------------------
-- match
-----------------------------------------------------------------------------

lastMatch = null
match = method(TypicalValue => Boolean)
match(String, String) := (re, str) -> null =!= (lastMatch = rawRegex(re, str, defaultMatchFlags))

-----------------------------------------------------------------------------
-- replace
-----------------------------------------------------------------------------

-- previously in methods.m2
replace(String, String, String) := String => regexReplace

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
