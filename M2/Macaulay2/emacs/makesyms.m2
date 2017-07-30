is := X -> s -> instance(value s, X)
isAlpha := s -> match("^[[:alpha:]]+$",s)
isAlphaNumeric := s -> match("^[[:alnum:]]+$",s)

okay := method()
okay(String,Keyword) := okay(String,Symbol) := (nam,sym) -> #nam > 1 and isAlphaNumeric nam
symbols := sort join( 
     apply(join(separate(" ",version#"packages"),{"Core"}), pkgnam -> (pkgnam,symbol Core)),
     flatten apply(
     	  join(Core#"pre-installed packages", {"Core","Text","Parsing","SimpleDoc"}),
     	  pkgnam -> (
	       pkg := needsPackage pkgnam;
	       select(pairs pkg.Dictionary,okay))))
bad := select(symbols, (nam,sym) -> not okay (nam,sym))
if #bad > 0 then (
     error("symbol(s) encountered that are not alphanumeric, or are of length 0 or 1: ", concatenate between_", " (first \ bad))
     )

Function and Function := (f,g) -> s -> f s and g s

f := openOut "M2-symbols.el"
f2 := openOut "M2-symbols"

f << "(defvar M2-symbols '(" << endl

scan( select (symbols, (nam,sym) -> isAlpha nam), (nam,sym) -> (
	  f << "    " << format nam << endl;
	  f2 << nam << endl;
	  ))

f2 << close

f << "   )" << endl
f << "  " << format ///A list of the symbols available in Macaulay2, for use with dynamic completion./// << endl
f << "  )" << endl

f << ///
(defvar M2-mode-font-lock-keywords 
     (let (
	    (max-specpdl-size 1000) ; needed for passing long long lists to regexp-opt
	  )
       `(
	 ; (,"--.*" . font-lock-comment-face)
///


add := (face,words) -> if #words > 0 then (
     f
     << "         (" 
     << ///,(concat "\\<\\(" (regexp-opt '(/// << demark(" ", format\words) << ///)) "\\)\\>")///
     <<  " . " << face << ")" << endl
     )

isKeyword := is Keyword
add( "font-lock-keyword-face", first \ select(symbols, (nam,sym) -> isKeyword sym))

isType := is Type
add( "font-lock-type-face", first \ select(symbols, (nam,sym) -> isType sym))

isFunction := is Function
add( "font-lock-function-name-face", first \ select(symbols, (nam,sym) -> isFunction sym))

add( ",font-lock-constant-face", first \ select(symbols, (nam,sym) -> (
	       not isFunction sym
	       and not isType sym
	       and not isKeyword sym
	       and (sym === symbol null or value sym =!= null)
	       and isAlpha nam)))
-- f << "         (" << format "///\\(/?/?[^/]\\)*///" << " . (0 font-lock-string-face t))" << endl
-- f << "         (" << format "\"[^\"]*\"" << " . (0 font-lock-string-face t))"
f << ")))" << endl << endl

f << "(if (fboundp 'font-lock-add-keywords)
    (font-lock-add-keywords 'M2-mode M2-mode-font-lock-keywords 'set))" << endl << endl

f << "(provide 'M2-symbols)" << endl

f << close

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/emacs "
-- End:
