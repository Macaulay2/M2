/*		Copyright 1993 by Daniel R. Grayson		*/

extern int yyparse(void);
extern node parservalue;
extern int yydebug;
extern void yyinit(void);
int setopleft(int priority, char *str);
int setopright(int priority, char *str);
int setopprefix(int priority, char *str);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
