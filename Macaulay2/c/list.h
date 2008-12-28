/*		Copyright 1993 by Daniel R. Grayson		*/

bool member(node, node);
node car(node x);
node cdr(node x);
node cons(node x, node y);
int length(node x);
node last(node l);
node tailend(node l);
node nth(node x, unsigned int n);
node list(int n,...);
node join(node list1, node list2);
node pop_1(node *listcell);
node allbutone(node e);
node reverse(node x);
bool equal(node, node);
bool occursin(node, node);
bool member(node, node);
int memberindex(node, node);
node substitute(node, node, node);

#define pop(x) pop_1(&(x))

#define push(list,y) (list = cons(y,list))

#define CAR(X) (assert((X)!=NULL && iscons(X)), (X)->body.cons.car)
#define CDR(X) (assert((X)!=NULL && iscons(X)), (X)->body.cons.cdr)
#define CDDR(X) CDR(CDR(X))
#define CDAR(X) CDR(CAR(X))
#define CDDDR(X) CDR(CDR(CDR(X)))
#define CADADR(X) CAR(CDR(CAR(CDR(X))))
#define CADR(N) CAR(CDR(N))
#define CADDR(N) CAR(CDR(CDR(N)))
#define CADDDR(N) CAR(CDR(CDR(CDR(N))))
#define CDADR(X) CDR(CAR(CDR(X)))
#define CAAR(X) CAR(CAR(X))
#define CAADR(X) CAR(CAR(CDR(X)))
#define CADAR(X) CAR(CDR(CAR(X)))

extern node cddr(node);
extern node cdar(node);
extern node cdddr(node);
extern node cadadr(node);
extern node cadr(node);
extern node caddr(node);
extern node caaddr(node);
extern node cadddr(node);
extern node cdadr(node);
extern node caar(node);
extern node caadr(node);
extern node cadar(node);


/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
