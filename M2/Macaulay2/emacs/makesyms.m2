symbols := values Main.Dictionary;
alphabet := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
isKeyword := s -> not mutable s and s =!= symbol null and value s === null
isAlpha := s -> alphabet#?((toString s)#0)
is := X -> s -> instance(value s, X)
Function && Function := (f,g) -> s -> f s and g s

f := openOut "M2-symbols.el"
f2 := openOut "M2-symbols"

f << "(defvar M2-symbols '(" << endl

scan( rsort select (symbols, isAlpha), s -> (
	  f << "    " << format toString s << endl;
	  f2 << toString s << endl;
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


add := (face,words) -> (
     f
     << "         (" 
     << ///,(concat "\\<\\(" (regexp-opt '(/// << demark(" ", format\words) << ///)) "\\)\\>")///
     <<  " . " << face << ")" << endl
     )

add( "font-lock-keyword-face", toString \ select(symbols, isKeyword && isAlpha))
add( "font-lock-type-face", toString \ select(symbols, is Type))
add( "font-lock-function-name-face", toString \ select(symbols, is Function))
add( ",font-lock-constant-face", toString \ select(symbols, sym -> (
	       not (is Function) sym
	       and not (is Type) sym
	       and (sym === symbol null or value sym =!= null)
	       and isAlpha sym)))
f << "         (" << format "///\\(/?/?[^/]\\)*///" << " . (0 font-lock-string-face t))" << endl
f << "         (" << format "\"[^\"]*\"" << " . (0 font-lock-string-face t))"
f << ")))" << endl << endl

f << "(if (fboundp 'font-lock-add-keywords)
    (font-lock-add-keywords 'M2-mode M2-mode-font-lock-keywords 'set))" << endl << endl

f << "(provide 'M2-symbols)" << endl

f << close
