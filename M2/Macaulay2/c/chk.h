/*		Copyright 1993 by Daniel R. Grayson		*/

node lookupfunction(node fun, node argtypes);
bool inside_defun(env);
node chkprogram(node e);
node chktype(node,env);
node chk(node, env);
node chklist(node e, env v);
void init_chk(void);
extern const struct ENV empty_env;
