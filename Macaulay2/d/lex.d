--		Copyright 1994 by Daniel R. Grayson
-- dictionary entries
use C;
use system;
use ctype;
use gmp;
use nets;
use tokens;
use strings;
use stdio;
use stdiop;
use varstrin;
use err;

export wordEOF := dummyWord; -- replaced later
export wordEOC := dummyWord; -- replaced later
export (o:file) << (w:Word) : file := o << w.name;
export WordList := null or WordListCell;
export WordListCell := { word:Word, next:WordList };
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

export NewlineW := Word("{*dummy word for newline*}",TCnone,0,newParseinfo());	    	  -- filled in by keywords.d
export equal(t:ParseTree,w:Word):bool := (
     when t is u:Token do u.word == w else false
     );
export (o:file) << (token:Token) : file := o << present(token.word.name);
tokenbuf := newvarstring(100);
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
export dumpNodes():void := stdout << "[" << baseLexNode << "]" << endl;
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
	  printErrorMessage(file.pos,"invalid character" );
	  getc(file);
	  (null or Word)(NULL))
     is word:Word do ( 
	  for length(word.name) do getc(file); 
	  (null or Word)(word)));
-- the next char from o will be " or ' - we return the string it delimits
-- with the delimiters
getstringslashes(o:PosFile):(null or Word) := (
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     pos := copy(o.pos);
     hadnewline := false;
     tokenbuf << '\"';
     while true do (
	  ch := getc(o);
	  if ch == EOF
	  || ch == ERROR				    -- is that the right thing to do?
	  then (
	       empty(tokenbuf);
	       printErrorMessage(pos,"EOF or ERROR in string or character constant beginning here");
	       return NULL;
	       );
	  if (
	       ch == int('/')
	       && peek(o,0) == int('/') 
	       && peek(o,1) == int('/')
	       ) then break;
	  if ch == int('\"') || ch == int('\\') then tokenbuf << '\\';
     	  tokenbuf << char(ch);
	  if isnewline(ch) && hadnewline && isatty(o) then (
	       return NULL;	  -- user gets out with an extra NEWLINE
	       );
	  hadnewline = isnewline(ch)
	  );
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     tokenbuf << '\"';
     s := takestring(tokenbuf);
     Word(s,TCstring,0,parseWORD));

ishexdigit(c:int):bool := (
     c >= int('0') && c <= int('9') ||
     c >= int('a') && c <= int('f') ||
     c >= int('A') && c <= int('F')
     );

getstring(o:PosFile):(null or Word) := (
     line := o.pos.line;
     column := o.pos.column;
     delimiter := getc(o);
     hadnewline := false;
     escaped := false;
     tokenbuf << char(delimiter);
     hexcoming := 0;
     unicode := 0;
     while true do (
	  ch := getc(o);
	  if ch == EOF 
	  || ch == ERROR				    -- is that the right thing to do?
	  then (
	       printErrorMessage(o.pos.filename,line,column,"EOF or ERROR in string or character constant beginning here");
	       empty(tokenbuf);
	       return NULL;
	       );
     	  tokenbuf << char(ch);
	  if hexcoming > 0 then (
	       if ishexdigit(ch) then (
		    hexcoming = hexcoming - 1;
		    )
	       else (
		    printErrorMessage(o.pos.filename,line,column,"expected hex digit in unicode sequence here");
		    empty(tokenbuf);
		    while true do (ch2 := getc(o); if ch2 == EOF || ch2 == ERROR || ch2 == int('\n') then return NULL;);
		    )
	       )
	  else if escaped
	  then (
	       if char(ch) == '"' 				    -- "
	       || char(ch) == 'r'
	       || char(ch) == 'n'
	       || char(ch) == 't'
	       || char(ch) == 'f'
	       || char(ch) == '\\'
	       || (char(ch) == 'u' && (hexcoming = 4; true)) -- allow unicode entry this way : "\u53f7"
	       || int('0') <= ch && ch < int('8')
	       then escaped = false
	       else (
		    empty(tokenbuf);
		    printErrorMessage(o.pos.filename,line,column,"unknown escape sequence: \\" + char(ch));
		    while true do (ch2 := getc(o); if ch2 == EOF || ch2 == ERROR || ch2 == int('\n') then return NULL;);
		    );
	       )
	  else if ch == delimiter then break
	  else if ch == int('\\') then escaped = true;
	  if isnewline(ch) && hadnewline && isatty(o) then (
	       return NULL;	  -- user gets out with an extra NEWLINE
	       );
	  hadnewline = isnewline(ch);
	  );
     s := takestring(tokenbuf);
     Word(s,TCstring,0,parseWORD));
ismore(file:PosFile):bool := ( c := peek(file); c != EOF && c != ERROR );
skipwhite(file:PosFile):int := (			    -- return -1 for unterminated block comment
     while true do (
	  c := peek(file);
	  if iswhite(c) then (
	       getc(file);
	       )
	  else if c == int('-') && peek(file,1) == int('-') then (
 	       -- comment: -- ...
     	       getc(file); getc(file);
     	       until peek(file) == int('\n') || !ismore(file) do getc(file)
	       )
	  else if c == int('#') && file.pos.line == ushort(1) && file.pos.column == ushort(0) && peek(file,1) == int('!') then (
	       -- comment on line 1:  #! ...
     	       getc(file); getc(file);
     	       until peek(file) == int('\n') || !ismore(file) do getc(file)
	       )
	  else if c == int('{') && peek(file,1) == int('*') then (
	       -- block comment: {* ... *}
	       getc(file); getc(file);
     	       fulllines := file.file.fulllines;
	       until peek(file) == int('*') && peek(file,1) == int('}')
	       do (
		    if fulllines && peek(file) == int('\n') then return -1;
		    if !ismore(file) then return -1;
		    getc(file)
		    );
	       getc(file); getc(file);
	       )
	  else return 0));

-- this errorToken means there was a parsing error or an error reading the file!
export errorToken := Token(Word("{*error token*}",TCnone,0,newParseinfo()),
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
	  if rc != 0  then (
	       printErrorMessage(file.pos,"unterminated block comment {* ... *}");
	       empty(tokenbuf);
	       while true do (ch2 := getc(file); if ch2 == EOF || ch2 == ERROR || ch2 == int('\n') then break;);
     	       return errorToken;
	       );
	  line := file.pos.line;
	  column := file.pos.column;
	  ch := peek(file);
     	  if iseof(ch) then return Token(wordEOF,file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline)
     	  else if iserror(ch) then return errorToken
	  else if isnewline(ch) then (
	       getc(file);
	       return Token(
		    if file.file.fulllines then wordEOC else NewlineW,
		    file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if isalpha(ch) && ch != int('\'') then (
	       tokenbuf << char(getc(file));
	       while isalnum(peek(file)) do tokenbuf << char(getc(file));
	       return Token(makeUniqueWord(takestring(tokenbuf),parseWORD),file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if isdigit(ch) || ch==int('.') && isdigit(peek(file,1)) then (
	       typecode := TCint;
	       while isdigit(peek(file)) do (
		    tokenbuf << char(getc(file))
		    );
	       if peek(file)==int('.') && peek(file,1)!=int('.') then (
		    typecode = TCdouble;
		    tokenbuf << char(getc(file));
		    while isdigit(peek(file)) do (
			 tokenbuf << char(getc(file))
			 );
		    if peek(file)==int('.') then (
			 printErrorMessage(file.pos,"'.' follows floating point constant");
			 while peek(file)==int('.') || isdigit(peek(file)) 
			 do getc(file);
			 );
		    );
	       s := takestring(tokenbuf);
	       return Token(Word(s,typecode,0, parseWORD),file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline)) 
	  else if ch == int('/') && peek(file,1) == int('/') && peek(file,2) == int('/') then (
	       when getstringslashes(file)
	       is null do (
		    empty(tokenbuf);
		    return errorToken
		    )
	       is word:Word do return Token(word,file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else if isquote(ch) then (
	       when getstring(file)
	       is null do (
		    empty(tokenbuf);
		    return errorToken
		    )
	       is word:Word do return Token(word,file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline))
	  else (
	       when recognize(file)
	       is null do (
		    empty(tokenbuf);
		    return errorToken
		    )
	       is word:Word do return Token(word,file.pos.filename, line, column, file.pos.loadDepth,globalDictionary,dummySymbol,sawNewline))));
export gettoken(file:PosFile,obeylines:bool):Token := (
     sawNewline := false;
     while true do (
	  w := gettoken1(file,sawNewline);
	  if w.word != NewlineW then return w;
	  if obeylines then return w;
	  if int(w.column) == 0 && isatty(file) then return errorToken; -- user gets out with an extra NEWLINE
	  sawNewline = true;
	  ));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
