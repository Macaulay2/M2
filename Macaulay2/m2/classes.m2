--		Copyright 1994 by Daniel R. Grayson

Net.name = quote Net
Time.name = quote Time
Handle.name = quote Handle
HashTable.name = quote HashTable
Boolean.name = quote Boolean
MutableHashTable.name = quote MutableHashTable
Function.name = quote Function
Sequence.name = quote Sequence
Error.name = quote Error
erase quote Error
Database.name = quote Database
Thing.name = quote Thing
Nothing.name = quote Nothing
Type.name = quote Type
String.name = quote String
BasicList.name = quote BasicList
List.name = quote List
MutableList.name = quote MutableList
File.name = quote File
Array.name = quote Array
Symbol.name = quote Symbol
SymbolTable.name = quote SymbolTable
ZZ.name = quote ZZ
QQ.name = quote QQ
RR.name = quote RR
Ring.name = quote Ring
Field.name = quote Field

uniform = (x) -> same apply(x,class)

document { quote uniform,
     TT "uniform x", " -- whether all elements of the list x have the same class."
     }
document { quote newClass,
     TT "newClass(N,m)", " -- makes a copy of m with N as the new class", BR,
     TT "newClass(N,M,m)", " -- makes a copy of m with N as class and M as parent",
     PARA,
     "If m is a list, then BasicList should be an ancestor of N.  If m is 
     a hash table, then ", TT "HashTable", " should be an ancestor of N.",
     PARA,
     "If m is mutable, and instances of class N are also mutable, then
     copying is not required, and is not done.",
     PARA,
     SEEALSO ( "copy", "elements" )
     }

document { quote MutableList,
     TT "MutableList", " -- the class of all mutable Lists.",
     PARA,
     "Normally the entries in a mutable hash table are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     PARA,
     EXAMPLE "s = new MutableList from {a,b,c};",
     EXAMPLE "s#2 = 1234;",
     EXAMPLE "s",
     EXAMPLE "peek s",
     SEEALSO ("lists, arrays, and sequences", "BasicList")
     }

document { quote lookup,
     TT "lookup(M,A)", "     -- provides the binary method named ", TT "M", " for class ", TT "A", ".
     The first place to look is ", TT "A#M", ".  The search proceeds with
     the parent of ", TT "A", ", and so on.",
     BR,
     NOINDENT, TT "lookup(M,A,B)", "   -- provides the binary method named ", TT "M", " for ", TT "(A,B)", ".
     The first place to look is ", TT "Y#(M,A,B)", " where ", TT "Y", " is the younger
     of ", TT "A", " and ", TT "B", ".  The search proceeds next with the parent of ", TT "B", ", and so on.",
     BR,
     NOINDENT, TT "lookup(M,A,B,C)", " -- provides the ternary method named ", TT "M", " for ", TT "(A,B,C)", ".
     The first place to look is ", TT "Y#(M,A,B,C)", " where ", TT "Y", " is the youngest
     of ", TT "A", ", ", TT "B", ", and ", TT "C", ".  The search proceeds with the parent of ", TT "C", ", and so on.",
     PARA,
     "If no method is found, then ", TT "null", " is returned.",
     PARA,
     SEEALSO ("classes", "installMethod")
     }

document { quote installMethod,
     TT "installMethod", " -- a function for installing methods.",
     PARA,
     "Most users will use a different way of installing methods.",
     BR,NOINDENT,
     TT "installMethod(M,A,f)", "     -- installs a function ", TT "f", " as a unary method for
     the class ", TT "A", " under the name ", TT "M", ".  This is the same as ", "M A := f", " if ", TT "M", " 
     is a function.  As currently implemented, this is also the same as ", TT "A#M = f", ".",
     BR,
     NOINDENT, TT "installMethod(M,A,B,f)", "   -- installs a function ", TT "f", " as a binary method for
     classes ", TT "A", " and ", TT "B", " under the name ", TT "M", ".  This is the same as ", TT "M(A,B) := f", " if ", TT "M", " is a
     function, or the same as ", TT "A M B := f", " if ", TT "M", " is a binary operator. As currently
     implemented, this is also the same as ", TT "Y#(M,A,B) = f", ", where ", TT "Y", " is 
     the younger of ", TT "A", " and ", TT "B", ".",
     BR,
     NOINDENT, TT "installMethod(M,A,B,C,f)", " -- installs a function ", TT "f", " as a ternary method 
     for classes ", TT "A", ", ", TT "B", ", and ", TT "C", " under the name ", TT "M", ".  This is the same as ", TT "M(A,B,C) := f", " if ", TT "f", "
     is a function.  As currently implemented, this is also the same as
     ", TT "Y#(M,A,B,C) = f", ", where ", TT "Y", " is the youngest of ", TT "A", ", ", TT "B", ", and ", TT "C", ".",
     PARA,
     "In all case, ", TT "f", " may also be a list ", TT "{D,f,doc...}", ", where ", TT "D", " is the class of the value
     returned by the function ", TT "f", ", and ", TT "{doc...}", " is documentation.",
     SEEALSO( "lookup",  "new", "classes")
     }
 
document { "new",
     TT "new A of b from c", " -- make a hash table of class AA and parent b initialized from c.", BR,
     NOINDENT,
     TT "new A of b", "        -- make a hash table of class AA and parent b.", BR,
     NOINDENT,
     TT "new A from c", "      -- make a hash table or list of class AA initialized from c.", BR,
     NOINDENT,
     TT "new A", "             -- makes a hash table or list n of class AA.", BR,
     PARA,
     HR,
     NOINDENT,
     TT "new A of b from c", " -- make a hash table n of class AA and parent b initialized from c.",
     PARA,
     "One may use this to model the mathematical notion
     that x is an element of A and a subset of b.
     Here A and b are Objects, and c is any expression.
     Let b be an instance of B, c be an instance of C, and let
     AA be any ancestor of A.  Then use",
     PRE "          new AA of B from C := (A,b,c) -> ... ",
     "to install the corresponding optional creation routine -- the
     value it returns will be converted so its class is A and its
     parent is b; this will involve copying unless the returned value 
     is mutable and objects of class A are mutable.",
     PARA,
     "If no installation routine has been installed, then c should be
     a hash table or a list, and it will be converted directly.",
     HR,
     NOINDENT,
     TT "new A of b", "        -- make a hash table of class A and parent b.",
     PARA,
     "Same as above, except c is missing.  Use ",
     PRE "          new AA of B := (A,b) -> ... ",
     "to install the initialization routine.",
     HR,
     NOINDENT,
     TT "new A from c", "      -- make a hash table or list n of class A initialized from c.",
     PARA,
     "The same as above except b is missing.  Use ",
     PRE "          new AA from C := (A,c) -> ... ",
     "to install the corresponding initialization routine.",
     PARA,
     "Since no parent b has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     HR,
     NOINDENT,
     TT "new A", "             -- make a hash table or list n of class A.",
     PARA,
     "Same as above, except b and c are missing.
     Use ", TT "new AA := A -> ... ", " to install the initialization routine.",
     PARA,
     "Since no parent b has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     PARA,
     "The symbols ", TO "NewMethod", ", ", TO "NewOfMethod", ", ", TO "NewFromMethod", ",
     and ", TO "NewOfFromMethod", " are used for installation of the initialization
     routines.",
     SEEALSO "classes"
     }

document { quote NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote Thing,
     TT "Thing", " -- the class of all things.",
     PARA,
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  The class of all things is ", TO "Thing", ".",
     PARA,
     "The basic types of things are:", 
     MENU {
          TO "BasicList",
          TO "Boolean",
          SHIELD TO "Database",
          TO "File",
          TO "Function",
          SHIELD TO "Handle",
          TO "HashTable",
          TO "Net",
          TO "Nothing",
          TO "QQ",
          TO "RR",
          TO "Sequence",
          TO "String",
          TO "Symbol",
          TO "Thing",
          TO "ZZ"
	  },
     "Operations on things:",
     MENU {
	  TO "comparison",
	  TO "assignment"
	  }
     }

document { quote Nothing,
     TT "Nothing", " -- the empty class.",
     PARA,
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }
