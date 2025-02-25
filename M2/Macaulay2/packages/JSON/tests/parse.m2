 -- tests from JSON Parsing Test Suite
 -- Copyright 2016 Nicolas Seriot
 -- MIT License
 -- https://github.com/nst/JSONTestSuite

 -- y_array_arraysWithSpaces.json
assert BinaryOperation(symbol ===, fromJSON "[[]   ]", {{}})

 -- y_array_empty-string.json
assert BinaryOperation(symbol ===, fromJSON "[\"\"]", {""})

 -- y_array_empty.json
assert BinaryOperation(symbol ===, fromJSON "[]", {})

 -- y_array_ending_with_newline.json
assert BinaryOperation(symbol ===, fromJSON "[\"a\"]", {"a"})

 -- y_array_false.json
assert BinaryOperation(symbol ===, fromJSON "[false]", {false})

 -- y_array_heterogeneous.json
assert BinaryOperation(symbol ===, fromJSON "[null, 1, \"1\", {}]", {nil,1,"1",new HashTable from {}})

 -- y_array_null.json
assert BinaryOperation(symbol ===, fromJSON "[null]", {nil})

 -- y_array_with_1_and_newline.json
assert BinaryOperation(symbol ===, fromJSON "[1\n]", {1})

 -- y_array_with_leading_space.json
assert BinaryOperation(symbol ===, fromJSON " [1]", {1})

 -- y_array_with_several_null.json
assert BinaryOperation(symbol ===, fromJSON "[1,null,null,null,2]", {1,nil,nil,nil,2})

 -- y_array_with_trailing_space.json
assert BinaryOperation(symbol ===, fromJSON "[2] ", {2})

 -- y_number.json
assert BinaryOperation(symbol ===, fromJSON "[123e65]", {.12300000000000001p53e68})

 -- y_number_0e+1.json
assert BinaryOperation(symbol ===, fromJSON "[0e+1]", {.0p53})

 -- y_number_0e1.json
assert BinaryOperation(symbol ===, fromJSON "[0e1]", {.0p53})

 -- y_number_after_space.json
assert BinaryOperation(symbol ===, fromJSON "[ 4]", {4})

 -- y_number_double_close_to_zero.json
assert BinaryOperation(symbol ===, fromJSON "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]\n", {-.1p53e-77})

 -- y_number_int_with_exp.json
assert BinaryOperation(symbol ===, fromJSON "[20e1]", {.2p53e3})

 -- y_number_minus_zero.json
assert BinaryOperation(symbol ===, fromJSON "[-0]", {0})

 -- y_number_negative_int.json
assert BinaryOperation(symbol ===, fromJSON "[-123]", {-123})

 -- y_number_negative_one.json
assert BinaryOperation(symbol ===, fromJSON "[-1]", {-1})

 -- y_number_negative_zero.json
assert BinaryOperation(symbol ===, fromJSON "[-0]", {0})

 -- y_number_real_capital_e.json
assert BinaryOperation(symbol ===, fromJSON "[1E22]", {.1p53e23})

 -- y_number_real_capital_e_neg_exp.json
assert BinaryOperation(symbol ===, fromJSON "[1E-2]", {.1p53e-1})

 -- y_number_real_capital_e_pos_exp.json
assert BinaryOperation(symbol ===, fromJSON "[1E+2]", {.1p53e3})

 -- y_number_real_exponent.json
assert BinaryOperation(symbol ===, fromJSON "[123e45]", {.12299999999999999p53e48})

 -- y_number_real_fraction_exponent.json
assert BinaryOperation(symbol ===, fromJSON "[123.456e78]", {.123456p53e81})

 -- y_number_real_neg_exp.json
assert BinaryOperation(symbol ===, fromJSON "[1e-2]", {.1p53e-1})

 -- y_number_real_pos_exponent.json
assert BinaryOperation(symbol ===, fromJSON "[1e+2]", {.1p53e3})

 -- y_number_simple_int.json
assert BinaryOperation(symbol ===, fromJSON "[123]", {123})

 -- y_number_simple_real.json
assert BinaryOperation(symbol ===, fromJSON "[123.456789]", {.123456789p53e3})

 -- y_object.json
assert BinaryOperation(symbol ===, fromJSON "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}", new HashTable from {"dfg" => "fgh", "asd" => "sdf"})

 -- y_object_basic.json
assert BinaryOperation(symbol ===, fromJSON "{\"asd\":\"sdf\"}", new HashTable from {"asd" => "sdf"})

 -- y_object_duplicated_key.json
assert BinaryOperation(symbol ===, fromJSON "{\"a\":\"b\",\"a\":\"c\"}", new HashTable from {"a" => "c"})

 -- y_object_duplicated_key_and_value.json
assert BinaryOperation(symbol ===, fromJSON "{\"a\":\"b\",\"a\":\"b\"}", new HashTable from {"a" => "b"})

 -- y_object_empty.json
assert BinaryOperation(symbol ===, fromJSON "{}", new HashTable from {})

 -- y_object_empty_key.json
assert BinaryOperation(symbol ===, fromJSON "{\"\":0}", new HashTable from {"" => 0})

 -- y_object_escaped_null_in_key.json
assert BinaryOperation(symbol ===, fromJSON "{\"foo\\u0000bar\": 42}", new HashTable from {"foo\0bar" => 42})

 -- y_object_extreme_numbers.json
assert BinaryOperation(symbol ===, fromJSON "{ \"min\": -1.0e+28, \"max\": 1.0e+28 }", new HashTable from {"max" => .99999999999999996p53e28, "min" => -.99999999999999996p53e28})

 -- y_object_long_strings.json
assert BinaryOperation(symbol ===, fromJSON "{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}", new HashTable from {"x" => {new HashTable from {"id" => "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}}, "id" => "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"})

 -- y_object_simple.json
assert BinaryOperation(symbol ===, fromJSON "{\"a\":[]}", new HashTable from {"a" => {}})

 -- y_object_string_unicode.json
assert BinaryOperation(symbol ===, fromJSON "{\"title\":\"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430\" }", new HashTable from {"title" => "–ü–æ–ª—Ç–æ—Ä–∞ –ó–µ–º–ª–µ–∫–æ–ø–∞"})

 -- y_object_with_newlines.json
assert BinaryOperation(symbol ===, fromJSON "{\n\"a\": \"b\"\n}", new HashTable from {"a" => "b"})

 -- y_string_1_2_3_bytes_UTF-8_sequences.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0060\\u012a\\u12AB\"]", {"`ƒ™·ä´"})

 -- y_string_accepted_surrogate_pair.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uD801\\udc37\"]", {"Ì†ÅÌ∞∑"})

 -- y_string_accepted_surrogate_pairs.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\ud83d\\ude39\\ud83d\\udc8d\"]", {"Ì†ΩÌ∏πÌ†ΩÌ≤ç"})

 -- y_string_allowed_escapes.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]", {"\"\\/\b\f\n\r\t"})

 -- y_string_backslash_and_u_escaped_zero.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\\\u0000\"]", {"\\u0000"})

 -- y_string_backslash_doublequotes.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\\"\"]", {"\""})

 -- y_string_comments.json
assert BinaryOperation(symbol ===, fromJSON "[\"a/*b*/c/*d//e\"]", {"a/*b*/c/*d//e"})

 -- y_string_double_escape_a.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\\\a\"]", {"\\a"})

 -- y_string_double_escape_n.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\\\n\"]", {"\\n"})

 -- y_string_escaped_control_character.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0012\"]", {""})

 -- y_string_escaped_noncharacter.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uFFFF\"]", {"Ôøø"})

 -- y_string_in_array.json
assert BinaryOperation(symbol ===, fromJSON "[\"asd\"]", {"asd"})

 -- y_string_in_array_with_leading_space.json
assert BinaryOperation(symbol ===, fromJSON "[ \"asd\"]", {"asd"})

 -- y_string_last_surrogates_1_and_2.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uDBFF\\uDFFF\"]", {"ÌØøÌøø"})

 -- y_string_nbsp_uescaped.json
assert BinaryOperation(symbol ===, fromJSON "[\"new\\u00A0line\"]", {"new¬†line"})

 -- y_string_nonCharacterInUTF-8_U+10FFFF.json
assert BinaryOperation(symbol ===, fromJSON "[\"Ùèøø\"]", {"Ùèøø"})

 -- y_string_nonCharacterInUTF-8_U+FFFF.json
assert BinaryOperation(symbol ===, fromJSON "[\"Ôøø\"]", {"Ôøø"})

 -- y_string_null_escape.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0000\"]", {"\0"})

 -- y_string_one-byte-utf-8.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u002c\"]", {","})

 -- y_string_pi.json
assert BinaryOperation(symbol ===, fromJSON "[\"œÄ\"]", {"œÄ"})

 -- y_string_reservedCharacterInUTF-8_U+1BFFF.json
assert BinaryOperation(symbol ===, fromJSON "[\"õøø\"]", {"õøø"})

 -- y_string_simple_ascii.json
assert BinaryOperation(symbol ===, fromJSON "[\"asd \"]", {"asd "})

 -- y_string_space.json
assert BinaryOperation(symbol ===, fromJSON "\" \"", " ")

 -- y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uD834\\uDd1e\"]", {"Ì†¥Ì¥û"})

 -- y_string_three-byte-utf-8.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0821\"]", {"‡†°"})

 -- y_string_two-byte-utf-8.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0123\"]", {"ƒ£"})

 -- y_string_u+2028_line_sep.json
assert BinaryOperation(symbol ===, fromJSON "[\"‚Ä®\"]", {"‚Ä®"})

 -- y_string_u+2029_par_sep.json
assert BinaryOperation(symbol ===, fromJSON "[\"‚Ä©\"]", {"‚Ä©"})

 -- y_string_uEscape.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0061\\u30af\\u30EA\\u30b9\"]", {"a„ÇØ„É™„Çπ"})

 -- y_string_uescaped_newline.json
assert BinaryOperation(symbol ===, fromJSON "[\"new\\u000Aline\"]", {"new\nline"})

 -- y_string_unescaped_char_delete.json
assert BinaryOperation(symbol ===, fromJSON "[\"\"]", {""})

 -- y_string_unicode.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uA66D\"]", {"Íô≠"})

 -- y_string_unicode_2.json
assert BinaryOperation(symbol ===, fromJSON "[\"‚çÇ„à¥‚çÇ\"]", {"‚çÇ„à¥‚çÇ"})

 -- y_string_unicode_escaped_double_quote.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u0022\"]", {"\""})

 -- y_string_unicode_U+1FFFE_nonchar.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uD83F\\uDFFE\"]", {"Ì†øÌøæ"})

 -- y_string_unicode_U+10FFFE_nonchar.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uDBFF\\uDFFE\"]", {"ÌØøÌøæ"})

 -- y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u200B\"]", {"‚Äã"})

 -- y_string_unicode_U+2064_invisible_plus.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u2064\"]", {"‚Å§"})

 -- y_string_unicode_U+FDD0_nonchar.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uFDD0\"]", {"Ô∑ê"})

 -- y_string_unicode_U+FFFE_nonchar.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\uFFFE\"]", {"Ôøæ"})

 -- y_string_unicodeEscapedBackslash.json
assert BinaryOperation(symbol ===, fromJSON "[\"\\u005C\"]", {"\\"})

 -- y_string_utf8.json
assert BinaryOperation(symbol ===, fromJSON "[\"‚Ç¨ùÑû\"]", {"‚Ç¨ùÑû"})

 -- y_string_with_del_character.json
assert BinaryOperation(symbol ===, fromJSON "[\"aa\"]", {"aa"})

 -- y_structure_lonely_false.json
assert BinaryOperation(symbol ===, fromJSON "false", false)

 -- y_structure_lonely_int.json
assert BinaryOperation(symbol ===, fromJSON "42", 42)

 -- y_structure_lonely_negative_real.json
assert BinaryOperation(symbol ===, fromJSON "-0.1", -.10000000000000001p53)

 -- y_structure_lonely_null.json
assert BinaryOperation(symbol ===, fromJSON "null", nil)

 -- y_structure_lonely_string.json
assert BinaryOperation(symbol ===, fromJSON "\"asd\"", "asd")

 -- y_structure_lonely_true.json
assert BinaryOperation(symbol ===, fromJSON "true", true)

 -- y_structure_string_empty.json
assert BinaryOperation(symbol ===, fromJSON "\"\"", "")

 -- y_structure_trailing_newline.json
assert BinaryOperation(symbol ===, fromJSON "[\"a\"]\n", {"a"})

 -- y_structure_true_in_array.json
assert BinaryOperation(symbol ===, fromJSON "[true]", {true})

 -- y_structure_whitespace_array.json
assert BinaryOperation(symbol ===, fromJSON " [] ", {})
