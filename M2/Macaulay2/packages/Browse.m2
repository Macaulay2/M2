-- -*- coding: utf-8 -*-
--		Copyright 1996 by Daniel R. Grayson
newPackage ( "Browse",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "1996",
     Version => "1.0",
     Headline => "a method for browsing and examining Macaulay2 data structures",
     Keywords => {"Miscellaneous"},
     DebuggingMode => false
     )

export {"browse"}

dictionaryPath = prepend(Core#"private dictionary",dictionaryPath)
frame' = frame
dictionaryPath = delete (Core#"private dictionary",dictionaryPath)

menu = method()

RUNME = new SelfInitializingType of BasicList
METHODS = new SelfInitializingType of BasicList

showit = (ITEMS,SAME,DEFAULT) -> (
     wid := if # ITEMS < 10 then 1 else if # ITEMS < 100 then 2 else 3;
     << columnate(
	  if printWidth != 0 then printWidth - 1 else 79,
	  apply(#ITEMS, i -> concatenate {pad(wid,toString i), ":", ITEMS#i#0})
	  ) << endl;
     while (
     	  i := value read concatenate("menu item [",toString DEFAULT,"]: ");
     	  if i === null then i = DEFAULT;
     	  not (class i === ZZ and i >= 0 and i < # ITEMS)
	  ) do (
	  << "expected integer in range 0 .. " << # ITEMS - 1 << endl; 
	  );
     -- returning SAME means repeat the current menu
     ITEMS#i#1)

menu(Thing,Thing) := (x,back) -> (
     << x << endl;
     showit(
	  {
	       ("BACK",back),
	       ("class=>" | toString class x,(class x,(x,back))),
	       ("parent=>" | toString parent x,(parent x,(x,back)))
	       },
	  (x,back),
	  0))

menu(RUNME,Thing) := (x,back) -> (x#0(); back)

menu(METHODS,Thing) := (x,back) -> (
     showit(
	  join(
	       {("BACK",back)},
	       apply(methods x#0, meth -> (
			 toString meth,
			 (RUNME {
				   () -> (
					<< "code " << toString meth << " :" << endl;
					code meth; 
					read "press return: ";)},
			      (x,back))
			 ))),
	  (x,back),
	  0))

menu(Function,Thing) := (x,back) -> (
     << x << endl;
     items := {
	  ("BACK",back),
	  ("class=>" | toString class x,(class x,(x,back))),
	  ("parent=>" | toString parent x,(parent x,(x,back))),
	  ("METHODS", (METHODS {x}, (x,back)))
	  };
     try (
	  if (locate x)#0 === "stdio" then error "";
	  items = join(items,{
		    ("CODE",(RUNME{()->(<< code x << endl;read "press return: ";)},(x,back))),
	  	    ("EDIT",(RUNME{()->edit x},(x,back)))});
	  );
     items = join(items, apply(#(frame' x), i -> (
		    "frame#" | toString i | "[" | toString class (frame' x)#i | "]",
		    ((frame' x)#i, (x,back))
		    )));
     if (try help x) =!= null
     then items = append(items,("DOC",(RUNME{()-><< help x << endl},(x,back))));
     showit(items, (x,back), 0))

menu(Symbol,Thing) := (x,back) -> (
     << x << endl;
     items := {
	  ("BACK",back),
	  ("class=>" | toString class x,(class x,(x,back))),
	  ("parent=>" | toString parent x,(parent x,(x,back)))} ;
     try (
	  if (locate x)#0 === "stdio" then error "";
	  items = append(items,
	       ("CODE",(RUNME {() -> (<< code x << endl;read "press return: ";)},(x,back)))
	       );
	  );
     items = append(items,("METHODS", (METHODS {x}, (x,back))));
     showit(items, (x,back), 0))

menu(HashTable,Thing) := (x,back) -> (
     << x << endl;
     KEYS := sort apply(pairs x, (k,v) -> {toString (k => v), k});
     showit(
	  join({
		    ("BACK",back),
		    ("class=>" | toString class x,(class x,(x,back))),
		    ("parent=>" | toString parent x,(parent x,(x,back)))
		    },
	       apply(KEYS,k -> (k#0,(x#(k#1),(x,back))))),
     	  (x,back),
	  0))

menu(BasicList,Thing) := (x,back) -> (
     << x << endl;
     showit(
	  join(apply(# x,k -> (toString x#k,(x#k,(x,back)))),
	       {
		    ("BACK",back),
		    ("class=>" | toString class x,(class x,(x,back))),
		    ("parent=>" | toString parent x,(parent x,(x,back)))
		    }),
     	  (x,back),
	  # x))

menu(Sequence,Thing) := (x,back) -> (
     << x << endl;
     showit(
	  join(apply(# x,k -> (toString x#k,(x#k,(x,back)))),
	       {
		    ("BACK",back),
		    ("class=>" | toString class x,(class x,(x,back)))
		    }),
     	  (x,back),
	  # x))

browse = x -> (
     s := (x,null);
     while null =!= (s = menu s) do ( ))

beginDocumentation()

document {
     Key => Browse,
     Headline => "a method for browsing and examining Macaulay2 data structures"
     }

document {
     Key => browse,
     Headline => "browse the contents of an object",
     TT "browse x", " -- provides an interactive mechanism that allows the user
     to explore the hash table or list ", TT "x", ".",
     PARA{},
     "A menu of numbered items is presented to the user that allow the user to
     inspect the ", TO "class", " or ", TO "parent", " of ", TT "x", ".  For a
     hash table, the keys are presented so the user can visit the 
     corresponding values, and for a list, the entries are presented so the user 
     can visit them.  One of the menu items allows the user to go back to 
     re-examine the previous item."
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Browse pre-install"
-- End:
