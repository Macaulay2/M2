%{
#include <unistd.h>
#include "getmem.h"
#include "grammar.h"
#include "grammar.tab.h"
unsigned lastcount;
#define BOL lineno++, column = 1
#define COUNT column += lastcount = yyleng
#define COUNTN do { \
		  int i; \
		  lastcount = yyleng; \
		  for(i=0; i<yyleng; i++) if (yytext[i]=='\n') BOL; else column++; \
		  } while (0)
static void yyunput(int, char *) __attribute__ ((unused));
char *filename = "", *dirname="", *rootname = "";
int lineno = 1, column = 1, tagline, tagcol;
int yywrap(void) { lineno = 1, column = 1; return 1; }
%}

ID       [a-zA-Z][a-zA-Z0-9]*
INT	 [0-9]+
STRING   "\""(\\.|[^"\\])*"\""
WORD	 [^ \t\r\n\"=>]+
NEWLINE	 \r\n|\r|\n

%x string tag

%%

{NEWLINE}			BOL; lastcount = 0;
"<"				tagline = lineno; tagcol = column; COUNT; BEGIN(tag); return '<';
[^<\n\r]+			COUNT;

<tag>background			COUNT; yylval = strperm(yytext); return BACKGROUND;
<tag>href			COUNT; yylval = strperm(yytext); return HREF;
<tag>src			COUNT; yylval = strperm(yytext); return SRC;
<tag>{STRING}			COUNTN; yylval = strperm(yytext); return WORD;
<tag>{WORD}			COUNTN; yylval = strperm(yytext); return WORD;
<tag>\"				BEGIN(string);
<tag>=				COUNT; return *yytext;
<tag>[ \t]*			COUNT;
<tag>{NEWLINE}			BOL; lastcount = 0;
<tag>">"			COUNT; BEGIN(INITIAL); return '>';
<tag><<EOF>>			{
				fprintf(stderr,"%s:%d:%d: unterminated tag starting here\n", 
					filename, tagline, tagcol );
				lastcount = 0; return 0;
				}

<string>[^\\\"]+
<string>\"
<string>\\.
<string>\\
<string><<EOF>>		{ fprintf(stderr,"%s:%d:%d: unterminated string starting here\n", filename, lineno, column);
		  	  lastcount = 0; return 0; }

<<EOF>>				lastcount = 0; return 0;
