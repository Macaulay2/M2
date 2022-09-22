--		Copyright 1994 by Daniel R. Grayson
-- dictionary entries
use ctype;
use tokens;
use varstrin;

export wordEOF := dummyWord; -- replaced later
export wordEOC := dummyWord; -- replaced later
export (o:file) << (w:Word) : file := o << w.name;
export WordListCell := { word:Word, next:WordList };
export WordList := WordListCell or null;
export hashTable := (
     new array(WordList) 
     len 7313			-- just a convenient prime number
     do provide null()
     );
export makeUniqueWord(s:string,p:parseinfo):Word := (
     h := hash(s);
     hashCode := h%length(hashTable);
     hashList := hashTable.hashCode;
     while true do
     when hashList
     is null do break
     is hashCell:WordListCell do (
     	  if hashCell.word.name === s
	  then return hashCell.word;
	  hashList = hashCell.next;
	  );
     newWord := Word(s,TCid,h,p);
     hashTable.hashCode = WordListCell(newWord,hashTable.hashCode);
     newWord);

export NewlineW := Word("-*dummy word for newline*-",TCnone,0,newParseinfo());	    	  -- filled in by keywords.d
export equal(t:ParseTree,w:Word):bool := (
     when t is u:Token do u.word == w else false
     );
export (o:file) << (token:Token) : file := o << present(token.word.name);
threadLocal tokenbuf := newvarstring(100);
export LexNode := { 
     ch:char,		     	  	-- the char
     word:(null or Word), 	   	-- the word, if ch can end it
     next:(null or LexNode),		-- where to go if ch doesn't match the input
     further:(null or LexNode)		-- where to go to match the next char
     };
export baseLexNode := LexNode( char(0), NULL, NULL, NULL );
(o:file) << (node:LexNode) : file := (
     o << "'" << present(node.ch) << "'" ;
     when node.word
     is null do o
     is word:Word do o << " -> \"" << present(word.name) << "\"";
     when node.further
     is null do o
     is n:LexNode do o << " [" << n << "]";
     when node.next
     is null do o
     is next:LexNode do o << " " << next;
     o);
export dumpNodes():void := stdIO << "[" << baseLexNode << "]" << endl;
advance(node:LexNode,ch:int):(null or LexNode) := (
     if ch == EOF || ch == ERROR then return NULL;
     t := node.further;
     while true do (
	  when t
	  is null do return t
	  is node:LexNode do (
	       if int(node.ch) == ch
	       then return t
	       else t = node.next
	       )
	  )
     );
install(node:LexNode,ch:int):LexNode := (
     when advance(node,ch)
     is null do (
	  t := LexNode(char(ch),NULL,node.further,NULL);
	  node.further = t;
	  t)
     is u:LexNode do u
     );
export install(name:string,word:Word):Word := (
     node := baseLexNode;
     foreach ch in name do node = install(node,int(ch));     
     node.word = word;
     word);
recognize(file:PosFile):(null or Word) := (
     i := 0;
     state := baseLexNode;
     last := (null or Word)(NULL);
     while true do (
	  when advance(state,peek(file,i))
	  is null do break
	  is node:LexNode do (state = node; i=i+1;);
	  when state.word
	  is null do nothing
	  is word:Word do (last = word;)
	  );
     when last
     is null do (
	  p := position(file);
	  getc(file);
	  printErrorMessage(p,"invalid character" );
	  (null or Word)(NULL))
     is word:Word do ( 
	  for length(word.name) do getc(file); 
	  (null or Word)(word)));
-- the next char from o will be " or ' - we return the string it delimits
-- with the delimiters
getstringslashes(o:PosFile):(null or Word) := (		    -- /// ... ///
     line := o.line;
     column := o.column;
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     pos := position(o);
     hadnewline := false;
     tokenbuf << '\"';					    -- "
     while true do (
	  ch := getc(o);
	  if ch == ERROR then (
	       if !test(interruptedFlag)
	       then printErrorMessage(o.filename,line,column,"ERROR in string /// ... /// beginning here: " + o.file.errorMessage);
	       empty(tokenbuf);
	       return NULL;
	       );
	  if ch == EOF then (
	       printErrorMessage(o.filename,line,column,"EOF in string /// ... /// beginning here");
	       empty(tokenbuf);
	       return NULL;
	       );
	  -- this allows us to get 3,4,5,... slashes within the string by typing 4,6,8,... slashes
	  -- and to get 1,2,3,... slashes at the end of the string by typing 5,7,9,... slashes
	  while ch == int('/')
	  && peek(o,0) == int('/') 
	  && peek(o,1) == int('/')
	  && peek(o,2) == int('/') do (
	       tokenbuf << '/';
	       getc(o);
	       getc(o);
	       );
	  if ch == int('/')
	  && peek(o,0) == int('/') 
	  && peek(o,1) == int('/') then break;
	  if ch == int('\"') || ch == int('\\') then tokenbuf << '\\';
     	  tokenbuf << char(ch);
	  if ch == int('\n') && hadnewline && isatty(o) then return NULL; -- user gets out with an extra NEWLINE
	  hadnewline = ch == int('\n')
	  );
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     tokenbuf << '\"';
     s := takestring(tokenbuf);
     Word(s,TCstring,0,parseWORD));

isbindigit(c:int):bool := c == int('0') || c == int('1');
isoctdigit(c:int):bool := c >= int('0') && c <= int('7');
ishexdigit(c:int):bool := (
     c >= int('0') && c <= int('9') ||
     c >= int('a') && c <= int('f') ||
     c >= int('A') && c <= int('F')
     );

getstring(o:PosFile):(null or Word) := (
     line := o.line;
     column := o.column;
     delimiter := getc(o);
     hadnewline := false;
     escaped := false;
     tokenbuf << char(delimiter);
     hexcoming := 0;
     unicode := 0;
     while true do (
	  ch := getc(o);
	  if ch == ERROR then (
	       if !test(interruptedFlag)
	       then printErrorMessage(o.filename,line,column,
		    (if o.file.eof 
			 then "reading beyond EOF in string beginning here: "
			 else "ERROR in string beginning here: ")
		    + o.file.errorMessage);
	       empty(tokenbuf);
	       return NULL;
	       );
	  if ch == EOF then (
	       printErrorMessage(o.filename,line,column,"EOF in string beginning here");
	       empty(tokenbuf);
	       return NULL;
	       );
     	  tokenbuf << char(ch);
	  if hexcoming > 0 then (
	       if ishexdigit(ch) then (
		    hexcoming = hexcoming - 1;
		    )
	       else (
		    printErrorMessage(o.filename,line,column,"expected hex digit in unicode sequence here");
		    empty(tokenbuf);
		    while true do (ch2 := getc(o); if ch2 == EOF || ch2 == ERROR || ch2 == int('\n') then return NULL;);
		    )
	       )
	  else if escaped
	  then (
	       if char(ch) == '"' 				    -- "
	       || char(ch) == 'r'
	       || char(ch) == 'b'
	       || char(ch) == 'n'
	       || char(ch) == 't'
	       || char(ch) == 'f'
	       || char(ch) == '\\'
	       || (char(ch) == 'u' && (hexcoming = 4; true)) -- allow unicode entry this way : "\u53f7"
	       || int('0') <= ch && ch < int('8')
	       then escaped = false
	       else (
		    empty(tokenbuf);
		    printErrorMessage(o.filename,line,column,"unknown escape sequence: \\" + char(ch));
		    while true do (ch2 := getc(o); if ch2 == EOF || ch2 == ERROR || ch2 == int('\n') then return NULL;);
		    );
	       )
	  else if ch == delimiter then break
	  else if ch == int('\\') then escaped = true;
	  if ch == int('\n') && hadnewline && isatty(o) then return NULL;	-- user gets out with an extra NEWLINE
	  hadnewline = ch == int('\n');
	  );
     s := takestring(tokenbuf);
     Word(s,TCstring,0,parseWORD));
swline := ushort(0);
swcolumn := ushort(0);
skipwhite(file:PosFile):int := (
     -- skip white space
     -- '\n' is not white space
     -- white space include comments
     -- return EOF, for unterminated block comment
     --        ERROR, if interrupted
     --        0, otherwise
     while true do (
	  swline = file.line;
	  swcolumn = file.column;
	  c := peek(file);
	  d := 0;
	  if c == ERROR then return ERROR
	  else if iswhite(c) then ( getc(file); )
	  else if c == int('\n') then return 0
	  else if (
	       d = peek(file,1);
	       if d == ERROR then return ERROR;
	       c == int('-') && d == int('-')
	       ) then (
 	       -- comment: -- ...
     	       getc(file); getc(file);
     	       until (
		    c = peek(file); if c == ERROR then return c; 
		    c == int('\n') || c == EOF
		    ) do getc(file);
	       )
	  else if (
	       c == int('#') && file.line == ushort(1) && file.column == ushort(0) && 
	       d == int('!') 
	       ) then (
	       -- comment on line 1:  #! ...
     	       getc(file); getc(file);
     	       until (
		    c = peek(file); 
		    if c == ERROR then return c; 
		    c == int('\n') || c == EOF
		    ) do getc(file);
	       )
	  else if c == int('-') && peek(file,1) == int('*') then (
	       -- block comment: -* ... *-
	       getc(file); getc(file);
	       hadnewline := false;
	       until (
		    c = peek(file);
		    if c == ERROR || c == EOF then return c;
		    if c == int('\n') then (
			 if hadnewline && isatty(file) then (
			      getc(file);
			      return ERROR; -- user gets out with an extra NEWLINE
			      );
			 hadnewline = true;
			 )
		    else hadnewline = false;
		    c == int('*') && (
			 getc(file);
			 c = peek(file);
		    	 if c == ERROR || c == EOF then return c; 
		    	 c == int('-')			    -- -
		    	 && (
			      getc(file);
			      true ) ) )
	       do getc(file))
	  else return 0));

-- this errorToken means there was a parsing error or an error reading the file!
export errorToken := Token(Word("-*error token*-",TCnone,0,newParseinfo()),
     dummyPosition.filename,
     dummyPosition.line,
     dummyPosition.column,
     dummyPosition.loadDepth,
     globalDictionary,			    -- should replace this by dummyDictionary, I think
     dummySymbol,false);

gettoken1(file:PosFile,sawNewline:bool):Token := (
     -- warning : tokenbuf is static
     while true do (
	  rc := skipwhite(file);
	  if rc == ERROR then return errorToken;
	  if rc == EOF  then (
	       printErrorMessage(file.filename,swline,swcolumn,"EOF in block comment -* ... *- beginning here");
	       -- empty(tokenbuf);
	       -- while true do (ch2 := getc(file); if ch2 == EOF || ch2 == ERROR || ch2 == int('\n') then break;);
     	       return errorToken;
	       );
	  if rc == DEPRECATED then (
	       printErrorMessage(file.filename,swline,swcolumn,"encountered disabled block comment syntax {* ... *} beginning here");
     	       return errorToken;
	       );
	  line := file.line;
	  column := file.column;
	  ch := peek(file);
     	  if iseof(ch) then return Token(wordEOF,file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline)
     	  else if iserror(ch) then return errorToken
	  else if ch == int('\n') then (
	       getc(file);
	       return Token(
		    if file.file.fulllines then wordEOC else NewlineW,
		    file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if isalpha(ch) && ch != int('\'') then (
	       tokenbuf << char(getc(file));
	       while isalnum(peek(file)) do tokenbuf << char(getc(file));
	       return Token(makeUniqueWord(takestring(tokenbuf),parseWORD),file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if isdigit(ch) || ch==int('.') && isdigit(peek(file,1)) then (
	       typecode := TCint;
	       decimal := true;
	       if ch == int('0') then (
		    tokenbuf << char(getc(file));
		    c := peek(file);
		    if (c == int('b') || c == int('B')) &&
			isbindigit(peek(file,1)) then (
			 decimal = false;
			 tokenbuf << char(getc(file));
			 while isbindigit(peek(file)) do
			      tokenbuf << char(getc(file)))
		    else if (c == int('o') || c == int('O')) &&
			     isoctdigit(peek(file,1)) then (
			 decimal = false;
			 tokenbuf << char(getc(file));
			 while isoctdigit(peek(file)) do
			      tokenbuf << char(getc(file)))
		    else if (c == int('x') || c == int('X')) &&
			     ishexdigit(peek(file,1)) then (
			 decimal = false;
			 tokenbuf << char(getc(file));
			 while ishexdigit(peek(file)) do
			      tokenbuf << char(getc(file)))
		    else while isdigit(peek(file)) do
			      tokenbuf << char(getc(file))
		   )
	       else while isdigit(peek(file)) do
		    tokenbuf << char(getc(file));
	       c := peek(file);
	       if decimal && (c == int('.') && peek(file,1) != int('.') || c == int('p') || c == int('e')) || c == int('E')
	       then (
		    typecode = TCRR;
		    if c == int('.') then (
		    	 tokenbuf << char(getc(file));
     	       	    	 while isdigit(peek(file)) do tokenbuf << char(getc(file));
			 );
	       	    c = peek(file);
		    if c == int('p') then (
			 tokenbuf << char(getc(file));
			 if isdigit(char(peek(file))) 	    -- EOF on peek is rare and doesn't get converted to a digit, anyway
			 then (
			      tokenbuf << char(getc(file));
			      while isdigit(peek(file)) do tokenbuf << char(getc(file)))
			 else (
			      printErrorMessage(position(file),"precision missing in floating point constant");
			      empty(tokenbuf);
			      return errorToken;
			      )
			 );
	       	    c = peek(file);
		    if c == int('e') || c == int('E') then (
			 tokenbuf << char(getc(file));
			 if ( peek(file) == int('-') && isdigit(peek(file,1)) ||
			      peek(file) == int('+') && isdigit(peek(file,1)) ||
			      isdigit(peek(file)) )
			 then (
			      tokenbuf << char(getc(file));
			      while isdigit(peek(file)) do tokenbuf << char(getc(file));
			      )
			 else (
			      printErrorMessage(position(file),"exponent missing in floating point constant");
			      empty(tokenbuf);
			      return errorToken;
			      )
			 );
	       	    c = peek(file);
	       	    if int('.') == c then printWarningMessage(position(file),"character '"+char(c)+"' immediately following floating point number");
		    );
	       c = peek(file);
	       if isalpha(c) then printWarningMessage(position(file),"character '"+char(c)+"' immediately following number");
	       s := takestring(tokenbuf);
	       return Token(Word(s,typecode,0, parseWORD),file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline)) 
	  else if ch == int('/') && peek(file,1) == int('/') && peek(file,2) == int('/') then (
	       when getstringslashes(file)
	       is null do (
		    empty(tokenbuf);
		    return errorToken
		    )
	       is word:Word do return Token(word,file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if isquote(ch) then (
	       when getstring(file)
	       is null do (
		    empty(tokenbuf);
		    return errorToken
		    )
	       is word:Word do return Token(word,file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if ch == 226 then (
	       tokenbuf << char(getc(file));
	       tokenbuf << char(getc(file));
	       tokenbuf << char(getc(file));
	       return Token(makeUniqueWord(takestring(tokenbuf),parseWORD),file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else (
	       when recognize(file)
	       is null do (
		    empty(tokenbuf);
		    return errorToken
		    )
	       is word:Word do return Token(word,file.filename, line, column, loadDepth,globalDictionary,dummySymbol,sawNewline))));
export gettoken(file:PosFile,obeylines:bool):Token := (
     sawNewline := false;
     while true do (
	  w := gettoken1(file,sawNewline);
	  if w.word != NewlineW then return w;
	  if obeylines then return w;
	  sawNewline = true;
	  ));

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d lex.o "
-- End:
