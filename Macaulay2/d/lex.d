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

export wordEOF := Word("-end of file-",TCnone,0,parseEOF);
export (o:file) << (w:Word) : file := o << w.name;
export WordList := null or WordListCell;
export WordListCell := { word:Word, next:WordList };
export hashTable := (
     new array(WordList) 
     len 7313			-- just a convenient prime number
     do provide(null())
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
	  then return(hashCell.word);
	  hashList = hashCell.next;
	  );
     newWord := Word(s,TCid,h,p);
     hashTable.hashCode = WordListCell(newWord,hashTable.hashCode);
     newWord);

export semicolonW := dummyWord;	    	  -- filled in by keywords.d
export newlineW := dummyWord;	    	  -- filled in by keywords.d
export equal(t:ParseTree,w:Word):bool := (
     when t is u:Token do u.word == w else false
     );
export (o:file) << (token:Token) : file := o << present(token.word.name);
tokenbuf := newvarstring(100);
export Node := { 
     ch:char,		     	  	-- the char
     word:(null or Word), 	   	-- the word, if ch can end it
     next:(null or Node),		-- where to go if ch doesn't match the input
     further:(null or Node)		-- where to go to match the next char
     };
base := Node( char(0), NULL, NULL, NULL );
(o:file) << (node:Node) : file := (
     o << "'" << present(node.ch) << "'" ;
     when node.word
     is null do o
     is word:Word do o << " -> \"" << present(word.name) << "\"";
     when node.further
     is null do o
     is n:Node do o << " [" << n << "]";
     when node.next
     is null do o
     is next:Node do o << " " << next;
     o);
export dumpNodes():void := stdout << "[" << base << "]" << endl;
advance(node:Node,ch:int):(null or Node) := (
     if ch == EOF || ch == ERROR then return(NULL);
     t := node.further;
     while true do (
	  when t
	  is null do return(t)
	  is node:Node do (
	       if int(node.ch) == ch
	       then return(t)
	       else t = node.next
	       )
	  )
     );
install(node:Node,ch:int):Node := (
     when advance(node,ch)
     is null do (
	  t := Node(char(ch),NULL,node.further,NULL);
	  node.further = t;
	  t)
     is u:Node do u
     );
export install(name:string,word:Word):Word := (
     node := base;
     foreach ch in name do node = install(node,int(ch));     
     node.word = word;
     word);
recognize(file:PosFile):(null or Word) := (
     i := 0;
     state := base;
     last := (null or Word)(NULL);
     while true do (
	  when advance(state,peek(file,i))
	  is null do break
	  is node:Node do (state = node; i=i+1;);
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
	       return(NULL);
	       );
	  if (
	       ch == int('/')
	       && peek(o,0) == int('/') 
	       && peek(o,1) == int('/')
	       ) then break;
	  if ch == int('\"') || ch == int('\\') then tokenbuf << '\\';
     	  tokenbuf << char(ch);
	  if isnewline(ch) && hadnewline && isatty(o) then (
	       return(NULL);	  -- user gets out with an extra NEWLINE
	       );
	  hadnewline = isnewline(ch)
	  );
     getc(o);		  -- pass '/'
     getc(o);		  -- pass '/'
     tokenbuf << '\"';
     s := takestring(tokenbuf);
     Word(s,TCstring,0,parseWORD));
getstring(o:PosFile):(null or Word) := (
     pos := copy(o.pos);
     delimiter := getc(o);
     hadnewline := false;
     escaped := false;
     tokenbuf << char(delimiter);
     while true do (
	  ch := getc(o);
	  if ch == EOF 
	  || ch == ERROR				    -- is that the right thing to do?
	  then (
	       empty(tokenbuf);
	       printErrorMessage(pos,"EOF or ERROR in string or character constant beginning here");
	       return(NULL);
	       );
     	  tokenbuf << char(ch);
	  if escaped 
	  then escaped = false
	  else if ch == delimiter then break
	  else if ch == int('\\') then escaped = true;
	  if isnewline(ch) && hadnewline && isatty(o) then (
	       return(NULL);	  -- user gets out with an extra NEWLINE
	       );
	  hadnewline = isnewline(ch);
	  );
     s := takestring(tokenbuf);
     Word(s,TCstring,0,parseWORD));
lookingatcomment(file:PosFile):bool := (
     peek(file,0) == int('-') && peek(file,1) == int('-')
     );
skipwhite(file:PosFile):void := (
     while true do (
	  if lookingatcomment(file)
	  then while (
	       !isnewline(peek(file)) 
	       && (
		    c := peek(file);
		    c != EOF && c != ERROR
		    )
	       ) do getc(file)
	  else 
	  if iswhite(peek(file)) then (getc(file); while iswhite(peek(file)) do getc(file))
	  else break;
	  );
     );

-- this errorToken means there was a parsing error or an error reading the file!
export errorToken := Token(dummyWord,dummyPosition,globalScope,dummySymbol,false);

gettoken1(file:PosFile,sawNewline:bool):Token := (
     -- warning : tokenbuf is static
     while true do (
	  skipwhite(file);
	  pos := copy(file.pos);
	  ch := peek(file);
     	  if iseof(ch) then return(Token(wordEOF,pos,globalScope,dummySymbol,sawNewline))
     	  else if iserror(ch) then return(errorToken)
	  else if isnewline(ch) then (
	       getc(file);
	       return(Token(newlineW,pos,globalScope,dummySymbol,sawNewline)))
	  else if isalpha(ch) && ch != int('\'') then (
	       tokenbuf << char(getc(file));
	       while isalnum(peek(file)) do tokenbuf << char(getc(file));
	       return(Token(
			 makeUniqueWord(takestring(tokenbuf),parseWORD),
			 pos,globalScope,dummySymbol,sawNewline)))
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
	       return(Token(
			 Word(s,typecode,0, parseWORD),
			 pos,globalScope,dummySymbol,sawNewline))) 
	  else if ch == int('/') && peek(file,1) == int('/') && peek(file,2) == int('/') then (
	       when getstringslashes(file)
	       is null do (
		    empty(tokenbuf);
		    return(errorToken)
		    )
	       is word:Word do return(Token(word,pos,globalScope,dummySymbol,sawNewline)))
	  else if isquote(ch) then (
	       when getstring(file)
	       is null do (
		    empty(tokenbuf);
		    return(errorToken)
		    )
	       is word:Word do return(Token(word,pos,globalScope,dummySymbol,sawNewline)))
	  else (
	       when recognize(file)
	       is null do (
		    empty(tokenbuf);
		    return(errorToken)
		    )
	       is word:Word do return(Token(word,pos,globalScope,dummySymbol,sawNewline)))));
export gettoken(file:PosFile,obeylines:bool):Token := (
     sawNewline := false;
     while true do (
	  w := gettoken1(file,sawNewline);
	  if w.word == newlineW
	  then (
	       sawNewline = true;
	       if obeylines then return(w);
	       if int(w.position.column) == 0
	       && isatty(file) then return(errorToken);
	       -- user gets out with an extra NEWLINE
	       )
	  else return(w);
	  ));
