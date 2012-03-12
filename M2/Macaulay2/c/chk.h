/*		Copyright 1993 by Daniel R. Grayson		*/

node lookupfunction(node fun, node argtypes);
bool inside_defun(scope);
node chkprogram(node e);
node chktype(node,scope);
node chk(node, scope);
node chklist(node e, scope v);
void init_chk(void);
extern node headerstrings;
extern node declarationsstrings;
void printtypecodes();
node leftOperator(node);
node rightOperator(node);
node prefixOperator(node);
/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
