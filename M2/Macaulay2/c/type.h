/*		Copyright 1993 by Daniel R. Grayson		*/

bool atomic(node);
node typedefinition(node);
node ormemberindex(node, node);
node ormembertype(node, node);
void forwardtype(node,node);
node typeforward(node);
node typedeftail(node t);
node ExpandType(node t, node *f);
bool checkargtypes(node, node);
node chktype2(node e,env v);
node integer(int n);
void totypesRec(node e);
bool isfunctiontype(node);
node functionargtypes(node);

bool isstruct(node);
bool isobject(node);
node IntegerN(char*, unsigned int);
node newtype(node , node , bool );
void interntype(node );
extern node *typelist;
extern int numtypes;
node enpos(node , struct POS *);
node repos(node ,node );
node SETPOS(node , struct POS *);
node setpos(node , struct POS *);
node positionof(node n);
struct POS *pos(node n);
node unpos(node e);
node elementtype(node arraytype);
node membertype(node structtype, node membername);
node type(node e);
node realtype(node e);
node chktypelist(node e,env v);
node newtype(node value, node name, bool basic_type);
void printtypelist();
node thetype(int i);
bool isfunctiontype(node e);
bool isarraytype(node e);
bool isobjecttype(node e);
bool isobjecttypeexpr(node e);
bool isarraytypeexpr(node e);
bool isortype(node e);
bool iscompositeortype(node e);
node functionargtypes(node t);
node functionrettype(node t);
node totype(node e);
node chktype(node e,env v);
bool subtype(node s, node t);
bool checkargtypes(node argtypes, node funargtypes);
node returntype(node fun);
bool couldbenull(node t);
bool reservable(node e);
bool reservable(node e);
node nextfun(node fun);
bool isbasictype(node e);
bool isarithmetictype(node e);
bool isobject(node e);
node basictype(node n);
node UniqueString(char *s);

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
