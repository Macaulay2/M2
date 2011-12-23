/*		Copyright 1993 by Daniel R. Grayson		*/

extern const char *uniquify(const char *);
extern const char *totoken(const char *);
extern void exportit(node, scope);
extern const char *prefixify(node, const char *);
extern void checkfordeferredsymbols();
extern void reinternsymbol(node, scope);
extern int sequence(node);
extern node complete_symbol_list;
extern void internsymbol(node, scope);
extern void printsymboltable();
extern void printstringlist();
extern node String(const char *);
extern node UniqueString(const char *);
extern node UniqueStringN(const char *s, unsigned int len);
extern void init_dictionary(scope v);
extern void setcprintvalue(node, node);
extern node lookupword(node);
extern node newtmp(node, scope, bool);
extern node newstmp(node, scope, bool);
extern node newsymbol(node, node, scope, int);

extern node type__T, keyword_T, int_T, double_T,
       bool_T, char_T, symbol_T, package_T;
extern node void_T, exits_T, returns_T, bad_or_undefined_T, null_T, deferred__T;

#define f(x,y) extern node y ## _S, y ## _K;
#define g(y)   extern node y ## _S, y ## _K;
#include "keywords.h"
#undef f
#undef g

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/














