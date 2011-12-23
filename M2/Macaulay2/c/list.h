/*		Copyright 1993 by Daniel R. Grayson		*/
#ifndef _LIST_H_
#define _LIST_H_
/***
	Returns (x,y) where y is a list or null.
 ***/
node cons(node x, node y);
/***
	Returns the length of the list by traversing the list.
***/
int length(node x);
/***
	Returns CAR((tailend))
 ***/
node last(node l);
/***
	Returns the node in the list such that CDR==NULL.
	This is equivalent to the last node in the list.
 ***/
node tailend(node l);
/***
	@return The nth element of list x
***/
node nth(node x, unsigned int n);
/***
	Create a list from function parameters.
	@param n The number of parameters.
***/
node list(int n,...);
/***
	@return list1 + list2 with + as concatenation.
 ***/
node join(node list1, node list2);
/***
	Pop the front of the list.
	@param listcell List to pop from by reference.
	@return The node at the front of the list.
***/
node pop_1(node *listcell);
/***
	Return all but the last element of the list by recursion.
	@param e A list.
	@return A list or NULL.
***/
node allbutone(node e);
/***
	Reverse list x.
	May return NULL if x is NULL, not NULL otherwise.
 ***/
node reverse(node x);
/***
	Test for node equality.
	@return True if equal, false otherwise.
***/
bool equal(node, node);
/***
	???
***/
bool occursin(node a, node x);
/***
	@return True if node e is a member of list l, false otherwise.
***/
bool member(node e, node l);
/***
	@return The index in list l of element e starting with 1. Returns 0 if not contained.
 ***/
int memberindex(node e, node l);
/***
	???
 ***/
node substitute(node fromlist, node tolist, node subject);

#define pop(x) pop_1(&(x))
/***
    list = (y,list).
    Note that this forms lists with the list part as the CDR part of the tuple.
***/
#define push(list,y) (list = cons(y,list))
/***
	Return the car position oif the node (the first part of the tuple).
***/
static inline node& car(node x)
{
	assert((x)!=NULL && iscons(x)); 
	return x->body.cons.car;
}
/***
	Return the cdr position oif the node (the second part of the tuple).
***/
static inline node& cdr(node x)
{
	assert(islist(x->body.cons.cdr));
	return x->body.cons.cdr;
}
static inline node& CAR(node x) { return car(x); }
static inline node& CDR(node x) { return cdr(x); }
/***
    @return CAR(CAR(CDR(X)))
***/
static inline node& caadr(node x)
{
	x = CDR(x);
	x = CAR(x);
	return CAR(x);
}
/***
    @return CAR(CAR(X))
***/
static inline node& caar(node x) 
{
	x = CAR(x);
	return CAR(x);
}
/***
    @return CAR(CDR(CAR(CDR(X))))
***/
static inline node& cadadr(node x) 
{
	x = CDR(x);
	x = CAR(x);
	x = CDR(x);
	return CAR(x);
}
/***
    @return CAR(CDR(CAR(X)))
***/
static inline node& cadar(node x) 
{
	x = CAR(x);
	x = CDR(x);
	return CAR(x);
}
/***
    @return CAR(CDR(CDR(CDR(X))))
***/
static inline node& cadddr(node x) 
{
	x = CDR(x);
	x = CDR(x);
	x = CDR(x);
	return CAR(x);
}
/***
    @return CAR(CDR(CDR(X)))
***/
static inline node& caddr(node x) 
{
	x = CDR(x);
	x = CDR(x);
    return CAR(x);
}
/***
    @return CAR(CAR(CDR(CDR(X))))
***/
static inline node& caaddr(node x) 
{
	x = CDR(x);
	x = CDR(x);
	x = CAR(x);
	return CAR(x);
}
/***
    @return CAR(CDR(X))
***/
static inline node& cadr(node x) 
{
	x = CDR(x);
	return CAR(x);
}
/***
    @return CDR(CAR(CDR(X)))
***/
static inline node& cdadr(node x) 
{
	x = CDR(x);
	x = CAR(x);
	return CDR(x);
}
/***
    @return CDR(CAR(X))
***/
static inline node& cdar(node x) 
{
	x = CAR(x);
	return CDR(x);
}
/***
    @return CDR(CDR(CDR(X)))
***/
static inline node& cdddr(node x) 
{
	x = CDR(x);
	x = CDR(x);
    return CDR(x);
}
/***
    @return CDR(CDR(X))
***/
static inline node& cddr(node x) 
{
	x = CDR(x);
	return CDR(x);
}

static inline node& CDDR(node x) { return cddr(x); }
static inline node& CDAR(node x) { return cdar(x); }
static inline node& CDDDR(node x) { return cdddr(x); }
static inline node& CADADR(node x) { return cadadr(x); }
static inline node& CADR(node x) { return cadr(x); }
static inline node& CADDR(node x) { return caddr(x); }
static inline node& CADDDR(node x) { return cadddr(x); }
static inline node& CDADR(node x) { return cdadr(x); }
static inline node& CAAR(node x) { return caar(x); }
static inline node& CAADR(node x) { return caadr(x); }
static inline node& CADAR(node x) { return cadar(x); }

#endif
/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
