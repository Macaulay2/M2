/*		Copyright 1993 by Daniel R. Grayson		*/

struct POS *pos2(node);
void cprintlist(node);
void cprinttypes(void);
void pprintl(node);
void cprint(node);
void dprinttype(node e);
void cprintsemi(node);
void pput(char *);
void put(char *);
void pprint(node);
void printsymbol(node);
void pp(node);
char *tostring(node);
void dprint(node);
void put_unescape(char *s);
void locn(node);
void printpos();
void dprinttype(node);
void dprintcons(node,node);
void dprint(node);
void dprintlist(node);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
