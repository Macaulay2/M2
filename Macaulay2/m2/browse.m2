--		Copyright 1996 by Daniel R. Grayson

back := quote back
menu := quote menu
menu = method()

RUNME := new SelfInitializingType of BasicList
RUNME.name = "RUNME"

METHODS := new SelfInitializingType of BasicList
METHODS.name = "METHODS"

showit := (ITEMS,SAME,DEFAULT) -> (
     wid := if # ITEMS < 10 then 1 else if # ITEMS < 100 then 2 else 3;
     << columnate(
	  apply(#ITEMS, i -> concatenate {pad(wid,name i), ":", ITEMS#i#0}),
	  if width stdout != 0 then width stdout - 1 else 79) << endl;
     while (
     	  i := evaluate read concatenate("menu item [",string DEFAULT,"]: ");
     	  if i === null then i = DEFAULT;
     	  not (class i === ZZ and i >= 0 and i < # ITEMS)
	  ) do (
	  << "expected integer in range 0 .. " << # ITEMS - 1 << endl; 
	  );
     -- returning SAME means repeat the current menu
     ITEMS#i#1)

menu(Thing,Thing) := (x,back) -> (
     << name x << endl;
     showit(
	  {
	       ("BACK",back),
	       ("class=>" | name class x,(class x,(x,back))),
	       ("parent=>" | name parent x,(parent x,(x,back)))
	       },
	  (x,back),
	  0))

menu(RUNME,Thing) := (x,back) -> ((x#0)(); back)

menu(METHODS,Thing) := (x,back) -> (
     showit(
	  join(
	       {("BACK",back)},
	       apply(methods x#0, meth -> (
			 name meth,
			 (RUNME {
				   () -> (
					<< "code " << name meth << " :" << endl;
					code meth; 
					read "press return: ";)},
			      (x,back))
			 ))),
	  (x,back),
	  0))

menu(Function,Thing) := (x,back) -> (
     << name x << endl;
     items := {
	  ("BACK",back),
	  ("class=>" | name class x,(class x,(x,back))),
	  ("parent=>" | name parent x,(parent x,(x,back))),
	  ("METHODS", (METHODS {x}, (x,back)))
	  };
     try (
	  if (locate x)#0 === "stdin" then error "";
	  items = join(items,{
		    ("SOURCE",(RUNME{()->(code x;read "press return: ";)},(x,back))),
	  	    ("EDIT",(RUNME{()->edit x},(x,back)))});
	  );
     try items = append(items,("ORIGINAL", (original x,(x,back))));
     if doc x =!= null
     then items = append(items,("DOC",(RUNME{()->help x},(x,back))));
     showit(items, (x,back), 0))

menu(Symbol,Thing) := (x,back) -> (
     << name x << endl;
     items := {
	  ("BACK",back),
	  ("class=>" | name class x,(class x,(x,back))),
	  ("parent=>" | name parent x,(parent x,(x,back)))} ;
     try (
	  if (locate x)#0 === "stdin" then error "";
	  items = append(items,
	       ("SOURCE",(RUNME {() -> (code x;read "press return: ";)},(x,back)))
	       );
	  );
     items = append(items,("METHODS", (METHODS {x}, (x,back))));
     showit(items, (x,back), 0))

menu(HashTable,Thing) := (x,back) -> (
     << name x << endl;
     KEYS := sort apply(keys x, k -> {name k,k});
     showit(
	  join({
		    ("BACK",back),
		    ("class=>" | name class x,(class x,(x,back))),
		    ("parent=>" | name parent x,(parent x,(x,back)))
		    },
	       apply(KEYS,k -> (k#0,(x#(k#1),(x,back))))),
     	  (x,back),
	  0))

menu(BasicList,Thing) := (x,back) -> (
     << name x << endl;
     showit(
	  join(apply(# x,k -> (name x#k,(x#k,(x,back)))),
	       {
		    ("BACK",back),
		    ("class=>" | name class x,(class x,(x,back))),
		    ("parent=>" | name parent x,(parent x,(x,back)))
		    }),
     	  (x,back),
	  # x))

menu(Sequence,Thing) := (x,back) -> (
     << name x << endl;
     showit(
	  join(apply(# x,k -> (name x#k,(x#k,(x,back)))),
	       {
		    ("BACK",back),
		    ("class=>" | name class x,(class x,(x,back)))
		    }),
     	  (x,back),
	  # x))

browse = x -> (
     s := (x,null);
     while null =!= (s = menu s) do ( ))

document { quote browse,
     TT "browse x", " -- provides an interactive mechanism which allows the user
     to explore the hash table or list ", TT "x", ".",
     PARA,
     "A menu of numbered items is presented to the user which allow the user to
     inspect the ", TO "class", " or ", TO "parent", " of ", TT "x", ".  For a
     hash table, the keys are presented so the user can visit the 
     corresponding values, and for a list, the entries are presented so the user 
     can visit them.  One of the menu items allows the user to go back to 
     re-examine the previous item."
     }

