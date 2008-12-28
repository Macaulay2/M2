/*		Copyright 1993 by Daniel R. Grayson		*/

   /* declarations */

%{
#include "scc.h"
#define YYSTYPE node
node parservalue;
static void yyerror(char *);
static int yylex(void);
#define end_of_input 0
%}
%token NUMBER IDENTIFIER STRINGCONST SELF OP SUPER
%left ';'
%left ')' 
%left whiledo PROVIDE RETURN
%left ifte
%left infix1 prefix1
%left IMPORT EXPORT
%right COLON
%left infix2 prefix2
%left infix3 prefix3
%right infix3r
%left OROR
%left ANDAND
%left OR
%left KINDOF
%left infix4 prefix4
%left infix5 prefix5
%left infix6 prefix6 '-'
%left infix7 prefix7 
%left SPACE
%left infix8
%left infix9 prefix9
%left infix10 prefix10 '('
%left IF THEN ELSE WHEN IS FOR FOREACH WHILE DO UNTIL PACKAGE USE SIGNATURE AT IN BY FROM TO FUNCTION NEW LEN BREAK
%start wrappedprogram

%% /* rules */

wrappedprogram
     : program {
	  if (cur.wrapit) {
	       $$ = list(3,package_K,
	       	    positionof(String(newsuffixbase(BaseName(cur.filename),""))),
	       	    cons(block_K,$1));
	       }
	  parservalue = $$;
	  }
     ;

program 
     : exprlist 	      	   	       { $$ = $1; }
     | exprlistsemi 	      	   	       { $$ = $1; }
     | exprlist error 	      	{ $$ = $1; yyerrok; yyclearin; }
     | exprlistsemi error      	{ $$ = $1; yyerrok; yyclearin; }
     | /*empty*/     	       	    	       { $$ = NULL; }
     ;
exprlistsemi
     : reverseexprlistsemi     	    	       { $$ = reverse($1); }
     ;
exprlist 
     : reverseexprlist     	    	       { $$ = reverse($1); }
     ;
reverseexprlistsemi
     : reverseexprlist ';'     	    	       { $$ = $1; }
     | ';'     	    	      	   	       { $$ = NULL; }
     | error ';'     	       	    	       { $$ = NULL; yyerrok; }
     | expr ';'	    	      	   	       { $$ = cons($1,NULL); }
     | reverseexprlistsemi ';' 	    	       { $$ = $1; }
     ;
reverseexprlist
     : reverseexprlistsemi expr	    	       { $$ = cons($2,$1); }
     ;
arglistornull
     : /* empty */     	    	      	       { $$ = NULL; }
     | expr	     	       	    	       { $$ = list(1,$1); }
     | arglist
     ;
arglist
     : expr ',' expr   	       	    	       { $$ = list(2,$1,$3); }
     | expr ',' arglist	    	      	       { $$ = cons($1,$3); }
     ;
typecasen
     : WHEN expr IS expr DO expr  %prec whiledo {
	  $$=list(3,list(2,$4,$6),$2,$1);
	  }
     | typecasen IS expr DO expr  %prec whiledo { 
	  $$ = cons(list(2,$3,$5),$1); 
	  }
     ;
expr : typecasen	      	   %prec whiledo { $$ = reverse($1); }
     | typecasen ELSE expr         %prec whiledo { $$ = reverse(cons(list(1,$3),$1)); }
     | '{' arglistornull '}'	               { $$ = cons(object_K,$2); }
     | expr OROR expr	       	    	       { $$ = list(3,oror_K,$1,$3); }
     | expr ANDAND expr	       	    	       { $$ = list(3,andand_K,$1,$3); }
     | IF expr THEN expr ELSE expr %prec ifte  { $$ = list(4,$1,$2,$4,$6); }
     | IF expr THEN expr           %prec ifte  { $$ = list(3,$1,$2,$4); }
     | PROVIDE expr	     	       	       { $$ = list(2,$1,$2); }
     | KINDOF expr	     	       	       { $$ = list(2,$1,$2); }
     | expr OR expr	     	       	       {
	  if (iscons($1) && equal(CAR($1),or_S)) {
	       $$ = join($1,list(1,$3));
	       }
	  else {
	       $$ = list(3,$2,$1,$3);
	       }
	  }
     | BREAK	      	   	     	       { $$ = list(1,$1); }
     | UNTIL expr DO expr       %prec whiledo  { $$ = list(3,$1,$2,$4); }
     | WHILE expr DO expr       %prec whiledo  { $$ = list(3,$1,$2,$4); }
     | FOREACH expr IN expr DO expr   %prec whiledo { 
	  $$ = list(3,$1,list(2,$2,$4),$6); }
     | FOREACH expr AT expr IN expr DO expr   %prec whiledo { 
	  $$ = list(3,$1,list(3,$4,$2,$6),$8); }
     | FOREACH expr AT expr IN expr BY expr DO expr %prec whiledo { 
	  $$ = list(3,$1,list(4,$4,$2,$6,$8),$10); }
     | FOREACH expr IN expr BY expr DO expr %prec whiledo { 
	  $$ = list(3,$1,list(4,NULL,$2,$4,$6),$8); }
     | FOR expr DO expr %prec whiledo { $$ = list(3,$1,list(1,$2),$4); }
     | FOR expr TO expr DO expr %prec whiledo {
	  $$ = list(3,$1,list(2,$2,$4),$6); }
     | FOR expr FROM expr TO expr DO expr %prec whiledo {
	  $$ = list(3,$1,list(3,$2,$4,$6),$8); }
     | FOR expr FROM expr TO expr BY expr DO expr %prec whiledo {
	  $$ = list(3,$1,list(4,$2,$4,$6,$8),$10); }
     | expr COLON expr	      	   	       { $$ = list(3,$2,$1,$3); }
     | FUNCTION '(' arglistornull ')' COLON expr { $$ = list(3,$1,$3,$6); }
     | NEW expr LEN expr DO expr     	%prec whiledo       { 
	  $$ = list(3,$1, list(2,$2,$4), $6); }
     | NEW expr LEN expr AT expr DO expr     	%prec whiledo       { 
	  $$ = list(3,$1, list(3,$2,$4,$6), $8); }
     | expr infix10 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix9 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix8 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix7 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix6 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix5 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix4 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix3 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix3r expr    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix2 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix1 expr	    	      	       { $$ = list(3,$2,$1,$3); }
     | expr infix10 '(' arglist ')'    	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix9 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix8 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix7 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix6 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix5 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix4 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix3 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix2 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | expr infix1 '(' arglist ')'     	       { $$ = join( list(2,$2,$1), $4); }
     | '(' arglist ')' infix3r expr    	       { $$ = cons( $2, join($1,list(1,$4))); }
     | IMPORT expr 	     	       	       { $$ = list(2,$1,$2); }
     | EXPORT expr 	     	       	       { $$ = list(2,$1,$2); }
     | expr '-' expr	      	               { $$ = list(3,$2,$1,$3); }
     | '-' expr %prec infix7	      	       { $$ = list(2,$1,$2); }
     | expr SPACE expr	   	       	       { $$ = list(3,space_S,$1,$2); }
     | prefix10 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix9 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix7 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix6 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix5 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix4 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix3 expr 	      	   	       { $$ = list(2,$1,$2); }
     | prefix2 expr 	      	   	       { $$ = list(2,$1,$2); }
     | RETURN expr	     	       	       { $$ = cons($1,cons($2,NULL)); }
     | RETURN '(' ')'	     	       	       { $$ = cons($1,NULL); }
     | RETURN	     	       	       	       { $$ = cons($1,NULL); }
     | expr '(' arglistornull ')'              { $$ = cons($1,$3); }
     | '(' exprlist ')'	     	       	       { 
	  $$ = cons(blockn_K,$2); 
	  setpos($$,pos2($1));
	  }
     | '(' exprlistsemi ')'   	       	       { 
	  $$ = cons(block_K,$2); 
	  setpos($$,pos2($1));
	  }
     | '(' expr ')'	     	       	       { $$ = $2; }
     | USE IDENTIFIER     	   	       { $$ = list(2,$1,$2); }
     | PACKAGE IDENTIFIER '('  ')'     { 
	  $$ = list(3,$1,$2,list(1,block_K));
	  }
     | PACKAGE IDENTIFIER '(' expr ')'     { 
	  $$ = list(3,$1,$2,list(2,block_K,$4));
	  }
     | PACKAGE IDENTIFIER '(' exprlist ')'     { 
	  $$ = list(3,$1,$2,cons(block_K,$4));
	  }
     | PACKAGE IDENTIFIER '(' exprlistsemi ')'     { 
	  $$ = list(3,$1,$2,cons(block_K,$4));
	  }
     | SIGNATURE IDENTIFIER '('  ')'     { 
	  $$ = list(3,$1,$2,list(1,block_K));
	  }
     | SIGNATURE IDENTIFIER '(' expr ')'     { 
	  $$ = list(3,$1,$2,list(2,block_K,$4));
	  }
     | SIGNATURE IDENTIFIER '(' exprlist ')'     { 
	  $$ = list(3,$1,$2,cons(block_K,$4));
	  }
     | SIGNATURE IDENTIFIER '(' exprlistsemi ')'     { 
	  $$ = list(3,$1,$2,cons(block_K,$4));
	  }
     | STRINGCONST
     | NUMBER
     | IDENTIFIER
     ;
     
%% /* programs */

static void yyerror(char *s){
     error(s);
     }

struct TOKENTREE {
     struct TOKENTREE *next[128];
     short token[128];
     } *tokentree = NULL;

struct TOKENTREE *newtokentreenode(){
     int j;
     struct TOKENTREE *p = new(struct TOKENTREE);
     for (j=0; j<128; j++) {
	  p->next[j]=NULL;
	  p->token[j]=0;
	  }
     return p;
     }

void registerop(char *s, int token){
     struct TOKENTREE *p;
     if (tokentree == NULL) tokentree = newtokentreenode();
     p = tokentree;
     while (TRUE) {
	  int c = (*s++) & 0x7f;
	  if (*s == 0) {
	       p->token[c] = token;
	       return;
	       }
	  if (p->next[c] == NULL) p->next[c] = newtokentreenode();
	  p = p->next[c];
	  }
     }

static int token;

int tokenlength(char *s, int len) {
     struct TOKENTREE *p = tokentree;
     int n = 0, m = 0;
     while (TRUE) {
	  int c;
	  if (len == 0) break;
	  if (p == NULL) break;
	  c = 0x7f & *s, len--, s++, m++;
	  if (p->token[c] != 0) {
	       n = m;
	       token = p->token[c];
	       }
	  p = p->next[c];
	  }
     return n;
     }

void registerkeyword(char *s,int tok){
     UniqueString(s)->body.string.token = tok;
     }

void yyinit() {
     registerkeyword("do",DO);
     registerkeyword("new",NEW);
     registerkeyword("len",LEN);
     registerkeyword("until",UNTIL);
     registerkeyword("while",WHILE);
     registerkeyword("is",IS);
     registerkeyword("or",OR);
     registerkeyword("kindof",KINDOF);
     registerkeyword("when",WHEN);
     registerkeyword("foreach",FOREACH);
     registerkeyword("function",FUNCTION);
     registerkeyword("for",FOR);
     registerkeyword("at",AT);
     registerkeyword("in",IN);
     registerkeyword("by",BY);
     registerkeyword("from",FROM);
     registerkeyword("to",TO);
     registerkeyword("if",IF);
     registerkeyword("op",OP);
     registerkeyword("package",PACKAGE);
     registerkeyword("signature",SIGNATURE);
     registerkeyword("use",USE);
     registerkeyword("export",EXPORT);
     registerkeyword("import",IMPORT);
     registerkeyword("then",THEN);
     registerkeyword("else",ELSE);
     registerkeyword("break",BREAK);
     registerkeyword("provide",PROVIDE);
     registerkeyword("return",RETURN);
     registerop("::=",infix1);
     registerop(":=",infix1);
     registerop("=",infix2);
     registerop(":",COLON);
     registerop("<<",infix3);
     registerop(">>",infix3r);
     registerop("||",OROR);
     registerop("&&",ANDAND);
     registerop("!",prefix4);
     registerop("<",infix5);
     registerop(">",infix5);
     registerop("<=",infix5);
     registerop(">=",infix5);
     registerop("==",infix5);
     registerop("!=",infix5);
     registerop("===",infix5);
     registerop("=!=",infix5);
     registerop("|",infix3);
     registerop("^^",infix3);	/* exclusive or */
     registerop("~",prefix5);
     registerop("&",infix4);
     registerop("+",infix6);
     registerop(".",infix10);
     registerop("-",SELF);
     registerop("{",SELF);
     registerop("}",SELF);
     registerop(",",SELF);
     registerop(";",SELF);
     registerop("(",SELF);
     registerop(")",SELF);
     registerop("*",infix7);
     registerop("/",infix7);
     registerop("//",infix7);
     registerop("%",infix7);
     registerop("^",infix8);
     }

static int yylex() {
     node n;
     int i;
     yylval = NULL;
     top:
     while (cur.text < cur.eot) {
	  char c = *cur.text;
	  if (c=='\n' || c=='\t' || c=='\r' || c=='\f' || c==' ') {
	       advance();
	       }
	  else break;
	  }
     if (cur.text == cur.eot) return end_of_input;
     if (cur.text < cur.eot-1 && cur.text[0]=='-' && cur.text[1]=='-') {
	  advance();
	  while (*cur.text != '\n' && cur.text < cur.eot) advance();
	  goto top;
	  }
     i = tokenlength(cur.text,cur.eot-cur.text);
#if 0
     if (i==1 && *cur.text == '.' && cur.text<cur.eot && isdigit(cur.text[1])) i=0;
#endif
     yylval = positionof(NULL);
     if (i > 0) {
	  yylval->body.position.contents = UniqueStringN(cur.text,i);
	  cur.column += i;
	  if (token == SELF) {
	       char c = *cur.text;
	       assert(i==1);
	       cur.text += i;
	       if (yydebug) fprintf(stderr,"Got token %c\n",c);
	       return c;
	       }
	  else {
	       cur.text += i;
	       if (yydebug) {
#ifdef YYBISON
	       	    fprintf(stderr,"Got %s operator: ", 
			 yytname[YYTRANSLATE(token)]);
	       	    fflush(stderr);
	       	    pp(yylval);
	       	    fflush(stdout);
#else
#endif
	       	    }
	       return token;
	       }
	  }
     n = gettoken();
     yylval->body.position.contents = n;
     if (yydebug) {
	  fprintf(stderr,"Got token ");
	  fflush(stderr);
	  pp(yylval);
	  fflush(stdout);
	  }
     switch ( n -> tag ) {
	  case char_const_tag :
	  case int_const_tag :
	  case double_const_tag : return NUMBER;
	  case string_const_tag : return STRINGCONST;
	  case string_tag : {
	       if (n->body.string.token != 0) {
		    if (n->body.string.token == OP) {
			 node s;
			 cur.text += i;
			 cur.column += i;
			 i=0;
			 while (cur.text < cur.eot && !iswhite(cur.text[i])
			      && cur.text[i] != ','
			      && cur.text[i] != ';'
			      && cur.text[i] != ')'
			      && cur.text[i] != '(') i++;
			 s = UniqueStringN(cur.text,i);
			 cur.text += i;
			 cur.column += i;
			 if (yydebug) {
			      fprintf(stderr,"Got token %s\n",
				   s->body.string.contents);
			      }
			 yylval->body.position.contents = s;
			 return IDENTIFIER;
			 }
		    return n->body.string.token;
		    }
	       return IDENTIFIER;
	       }
	  case position_tag :
	  case type_tag :
	  case symbol_tag :
	  case cons_tag : assert(FALSE); return 0;
	  }
     }


/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
