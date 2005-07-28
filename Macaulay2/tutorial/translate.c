/* Translate a tutorial to a documentation node
 * for Macaulay 2
 *
 * Any line starting with '--' is considered part of the
 * text, which is placed using TEX mode.
 * Any blank line is ignored
 * Lines not beginning with '--' are considered code
 * and are placed into an EXAMPLE block.
 *
 * This program reads from its standard input, and writes
 * to its standard output.
 */

#include <ctype.h>
#include <stdio.h>

#define LEN 10000

enum { BLANK_LINE, TEX, EXAMPLE, PARA, BLOCK, HEADING, PRE };

unsigned char line[LEN];

static char *present(unsigned char *s) {
     static char buf[5*LEN];
     char *p = buf;
     for(;*s != 0;s++) {
	  char i = *s;
	  switch(i) {
	       case '\n' :
	       case '\r' :
	       case '\t' : {
		    *p++ = i;
	       	    break;
		    }
	       case '\\' :
	       case '"' : {
		    *p++ = '\\';
		    *p++ = i;
		    break;
		    }
	       default : {
	  	    if (32 <= i && i < 127) {
	       		 *p++ = i;
	       		 }
		    else {
		    	 *p++ = '\\';
		    	 *p++ = (char)('0' + i/64), i %= 64;
		    	 *p++ = (char)('0' + i/8), i %= 8;
		    	 *p++ = (char)('0' + i);
		    	 break;
			 }
		    }
	       }
	  }
     *p = 0;
     return buf;
     }

int line_type(char **s)
{
  int i;
  int is_blank = 1;
  *s = (char *)line;
  for (i=0; i<LEN; i++)
    {
      if (line[i] == '\0') break;
      else if (!isspace(line[i]))
	is_blank = 0;
      else if (line[i] == '\n')
	{
	  line[i] = '\0';
	  break;
	}
    }
  if (is_blank) return BLANK_LINE;
  if (line[0] == '-' && line[1] == '-')
    {
      if (line[2] == '\0') return PARA;
      if (line[2] == '$') return BLOCK;
      if (line[2] == '-') return HEADING;
      if (line[2] == 'B') return BLANK_LINE;
      if (line[2] == 'P') return PRE;
      (*s) += 2;
      return TEX;
    }
  return EXAMPLE;
}

int main(int argc, char **argv)
{
  char *s;
  int typ = BLANK_LINE;
  int block = 0;
  int head = 0;
  int state = EXAMPLE;
  fprintf(stdout, "-- this file produced by 'translate', do not edit\n");
  if (argc > 1)
    {
      return 1;
    }
  fgets((char *)line,LEN,stdin);
  line_type(&s);
  fprintf(stdout, "document { Key => \"%s", line+3);
  while (fgets((char *)line,LEN,stdin))
    {
      typ = line_type(&s);
      if (typ == BLANK_LINE) continue;

      if (state == TEX && typ != TEX)
	fprintf(stdout, "\",\n");
      else if (state == EXAMPLE && typ != EXAMPLE)
	{
	  fprintf(stdout, "\",\n");
	  block = 0;
	}
      else if (state == EXAMPLE && typ == EXAMPLE)
	{
	  if (!block) fprintf(stdout, "\",\n");
	}
      else
	fprintf(stdout, "\n");

      switch (typ) {
      case HEADING:
	head = !head;
	state = HEADING;
	break;
      case BLOCK:
	block = 1;
	fprintf(stdout, "EXAMPLE \"");
	state = EXAMPLE;
	break;
      case PRE:
	block = 1;
	fprintf(stdout, "PRE \"");
	state = EXAMPLE;
	break;
      case PARA:
	state = PARA;
	fprintf(stdout, "PARA,");
	break;
      case TEX:
	if (head)
	  {
	    fprintf(stdout, "TEX \"\n\\\\par{");
	    fprintf(stdout, "%s}\\\\par\n\",", present((unsigned char*)s));
	  }
	else
	  {
	    if (state != TEX)
	      fprintf(stdout,"TEX \"");
	    state = TEX;
	    fprintf(stdout, "%s", present((unsigned char*)s));
	  }
	break;
      case EXAMPLE:
	if (block == 0)
	  fprintf(stdout, "EXAMPLE \"");
	else if (block > 1)
	  fprintf(stdout, "\n");
	state = EXAMPLE;
	if (block > 0) block++;
	fprintf(stdout, "%s", present((unsigned char *)s));
	break;
      }
    }
  fprintf(stdout, "\"\n}\n");
  return 0;
}
