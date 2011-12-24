/*		Copyright 1993 by Daniel R. Grayson		*/

/***
	Get the current position in the D source file of the given node.
	This ignore the position where a symbol was defined, and insists on seeing the position where it
	is being used.
	It also ignores define commands.
	@param n Node, cons, not null.
	@return Position node or null.
***/
struct POS *pos2(node);
/***
	Print list in c to current output file.
	@param e cons node, may be null.
 ***/
void cprintlist(node);
/***
	Print c types/function declarations to the current output file.
	This is used to generate the cfile-exports.h file.
***/
void cprinttypes(void);
/***
	Pprint cons list.
	@param n Cons node to print, may be null.
***/
void pprintl(node n);
/***
	This function will print to the c file the output equivalent of the node.
	This is used to generate the c/cpp files and headers.
	@param e The node to print, not null.
 ***/
void cprint(node e);
/***
	Print type to current d output file.
	@param e Type node, not null.
 ***/
void dprinttype(node e);
/***
	Print c statement(s) to the current output file.
	@param e Node.  May be cons or not.  
 ***/
void cprintsemi(node e);
/***
	Put characters directly to current output file.
	@param s Null terminated character string, not null.
***/
void pput(const char *);
/***
	Put characters to output file, adjusting c indentation level if needed.
	@param s Null terminated character string, not null.
 ***/
void put(const char *);
/***
	Used to print information about the node to the stdout.
	Appears to be mainly used for debugging.
	@node p Node, may be null.
***/
void pprint(node e);
/***
	Prints symbol to current c output file.
	A symbol reference may need extra handling, so take care of that here as opposed.
	Example: A thread local variable on a system that does not support __thread needs to be a function call.
	@param p Symbol node, not null.
***/
void printsymbol(node);
/***
	Print the node, followed by newline.
	@param n Node to print, may be null.
***/
void pp(node n);
/***
	Attempt to create a string from the node in a sane way.
	@param e Node to create a string from, not null.
	@return Character string.
***/
const char *tostring(node);
/***
	Top level print function to print the node to the current d output file.
	@node e, Node to print, may be null.
***/
void dprint(node e);
/***
	Print characters that may be escaped to the current output file.
	Used primarily to output c code directly entered by the user.
	@param s Null terminated character string, not null.
 ***/
void put_unescape(const char *s);
/***
	Set the current position to the position of n.
	If n is null, current position becomes n.
	@param n Current node.
***/
void locn(node);
/***
   This function will print to the c file the current location in the d file where this code was generated.
   This is used by GDB for debugging.
***/
void printpos();
void dprinttype(node);
/***
	This function will print to the d file the output equivalent of the list g with type f.
	Note that the way this is used is s is cons node, f = CAR(s), g = CDR(s).
	@Param f The type node, not null.
	@param g The list node, not null.
***/
void dprintcons(node,node);
/***
	Print to the d file the output equivalent of node e.
	This is used to generate .sig files
***/
void dprint(node);
/***
	Print list to current d output file.
	@param e Cons list, may be null.
***/
void dprintlist(node);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
