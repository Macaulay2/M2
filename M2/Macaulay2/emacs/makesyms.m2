okay := method()
okay(String,Keyword) := okay Thing := x -> false
okay(String,Symbol) := (nam,sym) -> not match("\\$",nam) and #nam > 1
symbols := sort join( 
     apply(join(separate(" ",version#"packages"),{"Core"}), pkgnam -> (pkgnam,symbol Core)),
     flatten apply(
     	  join(Core#"pre-installed packages", {"Core","Text","Parsing","SimpleDoc"}),
     	  pkgnam -> (
	       pkg := needsPackage pkgnam;
	       select(pairs pkg.Dictionary,okay))))
assert all(symbols, okay)
alphabet := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
is := X -> s -> instance(value s, X)
isKeyword := is Keyword
isAlpha := s -> alphabet#?((toString s)#0)

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
f << "  " << format ///A list of the symbols available in Macaulay 2, for use with dynamic completion./// << endl
f << "  )" << endl

f << ///
(defvar M2-mode-font-lock-keywords 
     (let (
	    (max-specpdl-size 1000) ; needed for passing long long lists to regexp-opt
	  )
       `(
///


add := (face,words) -> if #words > 0 then (
     f
     << "         (" 
     << ///,(concat "\\<\\(" (regexp-opt '(/// << demark(" ", format\words) << ///)) "\\)\\>")///
     <<  " . " << face << ")" << endl
     )

add( "font-lock-keyword-face", first \ select(symbols, (nam,sym) -> isKeyword sym and isAlpha nam))
add( "font-lock-type-face", first \ select(symbols, (nam,sym) -> (is Type) sym))
add( "font-lock-function-name-face", first \ select(symbols, (nam,sym) -> (is Function) sym))
add( ",font-lock-constant-face", first \ select(symbols, (nam,sym) -> (
	       not (is Function) sym
	       and not (is Type) sym
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
