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

-----------------------------------------------------------------------------
-- match
-----------------------------------------------------------------------------

lastMatch = null
match = method(TypicalValue => Boolean)
match(String, String) := (re, str) -> null =!= (lastMatch = rawRegex(re, str, defaultMatchFlags))

-----------------------------------------------------------------------------
-- replace
-----------------------------------------------------------------------------

replace(String, String, String) := String => replaceStrings

-----------------------------------------------------------------------------
-- regexQuote
-----------------------------------------------------------------------------

regexQuote = method(Dispatch => Thing, TypicalValue => String)
regexQuote String := s -> (
    specialChars := {"\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}"};
    concatenate apply(characters s, c ->
	if member(c, specialChars) then "\\" | c else c))
