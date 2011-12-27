/*		Copyright 1993 by Daniel R. Grayson		*/

   /* declarations */

%{
#include "scc.h"
#define YYSTYPE node
node parservalue;
static void yyerror(char *);
static int yylex(void);
#define end_of_input 0
#define NU NULL
%}
%token NUMBER INTEGER IDENTIFIER STRINGCONST SELF OP SUPER
%left ';'
%left ')' 
%left whiledo PROVIDE RETURN
%left ifte
%left infix1 prefix1
%right infix1r
%left EXPORT
%right COLON
%left infix2 prefix2
%right infix2r
%left infix3 prefix3
%right infix3r
%left OROR
%left ANDAND
%left OR
%left infix4 prefix4
%right infix4r
%left infix5 prefix5
%right infix5r
%left infix6 prefix6 '-'
%right infix6r
%left infix7 prefix7 
%right infix7r
%left SPACE
%left infix8 prefix8
%right infix8r
%left infix9 prefix9
%right infix9r
%left infix10 prefix10 '('
%right infix10r
%left IF THEN ELSE WHEN IS FOR FOREACH WHILE DO PACKAGE OPERATORLEFT OPERATORRIGHT OPERATORPREFIX HEADER USE STRINGOP SIGNATURE AT IN BY FROM TO FUNCTION NEW LEN BREAK BRACEPLUS
%start wrappedprogram

%% /* rules */

wrappedprogram
     : program {
	  if (cur.wrapit) {
	       $$ = list(3,package_K,
	       	    positionof(UniqueString(newsuffixbase(BaseName(cur.filename),""))),
	       	    cons(block__K,$1));
	       }
	  parservalue = $$;
	  }
     ;

program 
     : exprlist 	      	   	                            { $$ = $1; }
     | exprlistsemi 	      	   	                            { $$ = $1; }
     | exprlist error 	      		                            { $$ = $1; yyerrok; yyclearin; }
     | exprlistsemi error      		                            { $$ = $1; yyerrok; yyclearin; }
     | /*empty*/     	       	    	                            { $$ = NU; }
     ;
exprlistsemi
     : reverseexprlistsemi     	    	                            { $$ = reverse($1); }
     ;
exprlist 
     : reverseexprlist     	    	                            { $$ = reverse($1); }
     ;
reverseexprlistsemi
     : reverseexprlist ';'     	    	                            { $$ = $1; }
     | ';'     	    	      	   	                            { $$ = NU; }
     | error ';'     	       	    	                            { $$ = NU; yyerrok; }
     | expr ';'	    	      	   	                            { $$ = cons($1,NU); }
     | reverseexprlistsemi ';' 	    	                            { $$ = $1; }
     ;
reverseexprlist
     : reverseexprlistsemi expr	    	                            { $$ = cons($2,$1); }
     ;
arglistornull
     : /* empty */     	    	      	                            { $$ = NU; }
     | expr	     	       	    	                            { $$ = list(1,$1); }
     | arglist
     ;
arglist
     : expr ',' expr   	       	    	                            { $$ = list(2,$1,$3); }
     | expr ',' arglist	    	      	                            { $$ = cons($1,$3); }
     ;
typecasen
     : WHEN expr IS expr DO expr  %prec whiledo                     { $$ = list(3,list(2,$4,$6),$2,$1); }
     | typecasen IS expr DO expr  %prec whiledo                     { $$ = cons(list(2,$3,$5),$1); }
     ;
expr : typecasen	      	 %prec whiledo                      { $$ = reverse($1); }
     | typecasen ELSE expr       %prec whiledo                      { $$ = reverse(cons(list(1,$3),$1)); }
     | '{' arglistornull '}'	                                    { $$ = cons(object__K,$2); }
     | BRACEPLUS arglistornull '}'	                            { $$ = cons(tagged_object_K,$2); }
     | expr OROR expr	       	    	                            { $$ = list(3,oror_K,$1,$3); }
     | expr ANDAND expr	       	    	                            { $$ = list(3,andand_K,$1,$3); }
     | IF expr THEN expr ELSE expr %prec ifte                       { $$ = list(4,$1,$2,$4,$6); }
     | IF expr THEN expr           %prec ifte                       { $$ = list(3,$1,$2,$4); }
     | PROVIDE expr	     	       	                            { $$ = list(2,$1,$2); }
     | expr OR expr	     	       	       {
	  if (iscons($1) && equal(CAR($1),or_S))
	       $$ = join($1,list(1,$3));
	  else $$ = list(3,$2,$1,$3);
	  }
     | BREAK	      	   	     	                            { $$ = list(1,$1); }
     | WHILE expr DO expr       %prec whiledo                       { $$ = list(3,$1,$2,$4); }
     | FOREACH expr IN expr DO expr   %prec whiledo 
					                            { $$ = list(3,$1,list(2,$2,$4),$6); }
     | FOREACH expr AT expr IN expr DO expr   %prec whiledo         { $$ = list(3,$1,list(3,$4,$2,$6),$8); }
     | FOREACH expr AT expr IN expr BY expr DO expr %prec whiledo   { $$ = list(3,$1,list(4,$4,$2,$6,$8),$10); }
     | FOREACH expr IN expr BY expr DO expr %prec whiledo           { $$ = list(3,$1,list(4,NU,$2,$4,$6),$8); }
     | FOR expr DO expr %prec whiledo 	                            { $$ = list(3,$1,list(1,$2),$4); }
     | FOR expr TO expr DO expr %prec whiledo                       { $$ = list(3,$1,list(2,$2,$4),$6); }
     | FOR expr FROM expr TO expr DO expr %prec whiledo             { $$ = list(3,$1,list(3,$2,$4,$6),$8); }
     | FOR expr FROM expr TO expr BY expr DO expr %prec whiledo     { $$ = list(3,$1,list(4,$2,$4,$6,$8),$10); }
     | expr COLON expr	      	   	                            { $$ = list(3,$2,$1,$3); }
     | FUNCTION '(' arglistornull ')' COLON expr                    { $$ = list(3,$1,$3,$6); }
     | NEW expr LEN expr AT expr DO expr       %prec whiledo        { $$ = list(5,$1,$2,$4,$6,$8); }
     | NEW expr LEN expr         DO expr       %prec whiledo        { $$ = list(5,$1,$2,$4,NU,$6); }
     | NEW expr          AT expr DO expr       %prec whiledo        { $$ = list(5,$1,$2,NU,$4,$6); }
     | NEW expr                  DO expr       %prec whiledo        { $$ = list(5,$1,$2,NU,NU,$4); }
     | expr infix10 expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix9 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix8 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix7 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix6 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix5 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix4 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix3 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix2 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix1 expr	    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix10r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix9r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix8r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix7r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix6r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix5r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix4r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix3r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix2r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     | expr infix1r expr    	      	                            { $$ = list(3,$2,$1,$3); }
     /*
       The next several rules allow definitions like this:
         (x:T) << (y:U,z:W) : S := e
       and corresponding usage like this:
         x << (y,z)
     */
     | expr infix10 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix9 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix8 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix7 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix6 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix5 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix4 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix3 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix2 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix1 '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix10r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix9r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix8r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix7r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix6r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix5r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix4r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix3r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix2r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | expr infix1r '(' arglist ')'	                            { $$ = cons($2,cons($1,$4)); }
     | EXPORT expr 	     	       	                            { $$ = list(2,$1,$2); }
     | expr '-' expr	      	                                    { $$ = list(3,$2,$1,$3); }
     | '-' expr %prec infix7	      	                            { $$ = list(2,$1,$2); }
     | prefix10 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix9 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix8 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix7 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix6 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix5 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix4 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix3 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix2 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | prefix1 expr 	      	   	                            { $$ = list(2,$1,$2); }
     | RETURN expr	     	       	                            { $$ = cons($1,cons($2,NU)); }
     | RETURN '(' ')'	     	       	                            { $$ = cons($1,NU); }
     | RETURN	     	       	       	                            { $$ = cons($1,NU); }
     | expr '(' arglistornull ')'                                   { $$ = cons($1,$3); }
     | '(' exprlist ')'	     	       	                            { $$ = cons(blockn__K,$2); setpos($$,pos2($1)); }
     | '(' exprlistsemi ')'   	       	                            { $$ = cons(block__K,$2); setpos($$,pos2($1)); }
     | '(' expr ')'	     	       	                            { $$ = $2; }
     | USE IDENTIFIER     	   	                            { $$ = list(2,$1,$2); }
     | STRINGOP STRINGCONST     	                            { $$ = list(2,$1,$2); }
     | HEADER STRINGCONST     	   	                            { $$ = list(2,$1,$2); }
     | OPERATORLEFT INTEGER STRINGCONST	                            { $$ = leftOperator(list(3,$1,$2,$3)); }
     | OPERATORRIGHT INTEGER STRINGCONST                            { $$ = rightOperator(list(3,$1,$2,$3)); }
     | OPERATORPREFIX INTEGER STRINGCONST                           { $$ = prefixOperator(list(3,$1,$2,$3)); }
     | PACKAGE IDENTIFIER '('  ')'     	                            { $$ = list(3,$1,$2,list(1,block__K)); }
     | PACKAGE IDENTIFIER '(' expr ')'                              { $$ = list(3,$1,$2,list(2,block__K,$4)); }
     | PACKAGE IDENTIFIER '(' exprlist ')'                          { $$ = list(3,$1,$2,cons(block__K,$4)); }
     | PACKAGE IDENTIFIER '(' exprlistsemi ')'                      { $$ = list(3,$1,$2,cons(block__K,$4)); }
     | SIGNATURE IDENTIFIER '('  ')'                                { $$ = list(3,$1,$2,list(1,block__K)); }
     | SIGNATURE IDENTIFIER '(' expr ')'                            { $$ = list(3,$1,$2,list(2,block__K,$4)); }
     | SIGNATURE IDENTIFIER '(' exprlist ')'                        { $$ = list(3,$1,$2,cons(block__K,$4)); }
     | SIGNATURE IDENTIFIER '(' exprlistsemi ')'                    { $$ = list(3,$1,$2,cons(block__K,$4)); }
     | STRINGCONST
     | NUMBER
     | INTEGER
     | IDENTIFIER
     ;
     
%% /* programs */

static void yyerror(char *s){
     error(s);
     }

#define TSIZE 128

struct TOKENTREE {
     struct TOKENTREE *next[TSIZE];
     short token[TSIZE];
     } *tokentree = NULL;

int registerop(char *s, int token){
     struct TOKENTREE *p;
     if (tokentree == NULL) tokentree = newoftype(struct TOKENTREE);
     p = tokentree;
     while (TRUE) {
	  int c = (*s++) & 0x7f;
	  if (*s == 0) {
	       if (p->token[c] != 0) return ERROR;
	       p->token[c] = token;
	       return 0;
	       }
	  if (p->next[c] == NULL) p->next[c] = newoftype(struct TOKENTREE);
	  p = p->next[c];
	  }
     }

int setopleft(int priority, char *str) {
  int token;
  switch (priority) {
  case 1: token = infix1; break;
  case 2: token = infix2; break;
  case 3: token = infix3; break;
  case 4: token = infix4; break;
  case 5: token = infix5; break;
  case 6: token = infix6; break;
  case 7: token = infix7; break;
  case 8: token = infix8; break;
  case 9: token = infix9; break;
  case 10: token = infix10; break;
  default: return ERROR;
  }
  return registerop(str,token);
}

int setopprefix(int priority, char *str) {
  int token;
  switch (priority) {
  case 1: token = prefix1; break;
  case 2: token = prefix2; break;
  case 3: token = prefix3; break;
  case 4: token = prefix4; break;
  case 5: token = prefix5; break;
  case 6: token = prefix6; break;
  case 7: token = prefix7; break;
  case 8: token = prefix8; break;
  case 9: token = prefix9; break;
  case 10: token = prefix10; break;
  default: return ERROR;
  }
  return registerop(str,token);
}

int setopright(int priority, char *str) {
  int token;
  switch (priority) {
  case 1: token = infix1r; break;
  case 2: token = infix2r; break;
  case 3: token = infix3r; break;
  case 4: token = infix4r; break;
  case 5: token = infix5r; break;
  case 6: token = infix6r; break;
  case 7: token = infix7r; break;
  case 8: token = infix8r; break;
  case 9: token = infix9r; break;
  case 10: token = infix10r; break;
  default: return ERROR;
  }
  return registerop(str,token);
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
     UniqueString(s)->body.unique_string.token = tok;
     }

void yyinit() {
     registerkeyword("do",DO);
     registerkeyword("new",NEW);
     registerkeyword("len",LEN);
     registerkeyword("until",WHILE);
     registerkeyword("while",WHILE);
     registerkeyword("is",IS);
     registerkeyword("or",OR);
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
     registerkeyword("Pointer",STRINGOP);
     registerkeyword("atomicPointer",STRINGOP);
     registerkeyword("Type",STRINGOP);
     registerkeyword("atomicType",STRINGOP);
     registerkeyword("arithmeticType",STRINGOP);
     registerkeyword("integerType",STRINGOP);
     registerkeyword("header",HEADER);
     registerkeyword("declarations",HEADER);
     registerkeyword("leftOperator",OPERATORLEFT);
     registerkeyword("rightOperator",OPERATORRIGHT);
     registerkeyword("prefixOperator",OPERATORPREFIX);
     registerkeyword("threadLocal",EXPORT);
     registerkeyword("constant",EXPORT);
     registerkeyword("export",EXPORT);
     registerkeyword("import",EXPORT);
     registerkeyword("then",THEN);
     registerkeyword("else",ELSE);
     registerkeyword("break",BREAK);
     registerkeyword("provide",PROVIDE);
     registerkeyword("return",RETURN);
     registerop(":",COLON);
     registerop("||",OROR);
     registerop("&&",ANDAND);
     registerop("{+",BRACEPLUS);
     registerop("-",SELF);
     registerop("{",SELF);
     registerop("}",SELF);
     registerop(",",SELF);
     registerop(";",SELF);
     registerop("(",SELF);
     registerop(")",SELF);
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
	       	    d_pp(yylval);
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
	  d_pp(yylval);
	  fflush(stdout);
	  }
     switch ( n -> tag ) {
          case int_const_tag : return INTEGER;
	  case char_const_tag :
	  case double_const_tag : return NUMBER;
	  case string_const_tag : return STRINGCONST;
	  case unique_string_tag : {
	       if (n->body.unique_string.token != 0) {
		    if (n->body.unique_string.token == OP) {
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
			 if (yydebug) fprintf(stderr,"Got token %s\n",tostring(s));
			 yylval->body.position.contents = s;
			 return IDENTIFIER;
			 }
		    return n->body.unique_string.token;
		    }
	       return IDENTIFIER;
	       }
	  default: assert(FALSE); return 0;
	  }
     }


/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
