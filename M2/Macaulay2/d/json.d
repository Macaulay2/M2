use common;
use util;
use hashtables;

header "#include <jansson.h>";

json_tstar := Pointer "struct json_t *";
json_error_t := Type "struct json_error_t";

toExpr(json:json_tstar):Expr := (
    type := Ccode(int, "json_typeof(", json, ")");
    if type == Ccode(int, "JSON_OBJECT")
    then (
	h := newHashTable(hashTableClass,nothingClass);
	iter := Ccode(voidPointer, "json_object_iter(", json, ")");
	while iter != nullPointer() do (
	    storeInHashTable(h,
		-- TODO: use json_object_iter_key_len so we can have \0 in keys
		-- problem: added in jansson 2.14, which isn't available on
		-- some older systems like Ubuntu 18.04
		toExpr(Ccode(constcharstar,
			"json_object_iter_key(", iter, ")")),
		toExpr(Ccode(json_tstar,
			"json_object_iter_value(", iter, ")")));
	    iter = Ccode(voidPointer,
		"json_object_iter_next(", json, ", ", iter, ")"));
	Expr(sethash(h, false)))
    else if type == Ccode(int, "JSON_ARRAY")
    then list(new Sequence len Ccode(int, "json_array_size(", json, ")")
	at i do provide toExpr(
	    Ccode(json_tstar, "json_array_get(", json, ", ", i, ")")))
    else if type == Ccode(int, "JSON_STRING")
    then toExpr(tostringn(
	    Ccode(constcharstar, "json_string_value(", json, ")"),
	    Ccode(int, "json_string_length(", json, ")")))
    else if type == Ccode(int, "JSON_INTEGER")
    then toExpr(Ccode(long, "json_integer_value(", json, ")"))
    else if type == Ccode(int, "JSON_REAL")
    then toExpr(Ccode(double, "json_real_value(", json, ")"))
    else if type == Ccode(int, "JSON_TRUE") then True
    else if type == Ccode(int, "JSON_FALSE") then False
    else if type == Ccode(int, "JSON_NULL") then nullE
    else buildErrorPacket("unknown json type"));

jsonFlags := Ccode(int, "JSON_DECODE_ANY | JSON_ALLOW_NUL");

fromJSON(e:Expr):Expr := (
    j := Ccode(json_tstar or null, "NULL");
    jerr := Ccode(json_error_t, "(json_error_t){0}");
    when e
    is buf:stringCell do (
	j = Ccode(json_tstar or null, "json_loadb((const char *)", buf.v,
	    "->array, ", length(buf.v), ", ", jsonFlags, ", &", jerr, ")"))
    is f:file do (
	j = Ccode(json_tstar or null, "json_loadfd(", f.infd,
	    ", ", jsonFlags, ", &", jerr, ")"))
    else return WrongArg("a string or file");
    when j
    is json:json_tstar do (
	ret := toExpr(json);
	Ccode(void, "json_decref(", json, ")");
	ret)
    is null do (
	n := 55 + Ccode(int, "JSON_ERROR_TEXT_LENGTH");
	buf := newstring(n);
	Ccode(void, "snprintf((char *)", buf, "->array, ", n,
	    ", \"json error at line %d, column %d: %s\", ", jerr, ".line, ",
	    jerr, ".column, ", jerr, ".text)");
	Ccode(void, buf, "->len = strlen((char *)", buf, "->array)");
	buildErrorPacket(buf)));
setupfun("fromJSON0", fromJSON);
