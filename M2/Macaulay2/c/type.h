/*		Copyright 1993 by Daniel R. Grayson		*/

bool is_atomic_memory(node type);
node arrayElementLength(node arraytype);
bool israwtype(node e);
bool pointer_to_atomic_memory(node type);
node typedefinition(node);
node ormemberindex(node, node);
node ormembertype(node, node);
bool forwardtype(node,node);
bool ispointertypeexpr(node e);
bool istaggedobjecttypeexpr(node e);
bool isdeferredtype(node e);
node typeforward(node);
node typedeftail(node t);
node ExpandType(node t, node *f);
bool checkargtypes(node, node);
node chktype2(node e,scope v);
node integer(int n);
void totypesRec(node e);
bool isfunctiontype(node);
node functionargtypes(node);

bool isstruct(node);
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
node arrayElementType(node arraytype);
node membertype(node structtype, node membername);
node type(node e);
node chktypelist(node e,scope v);
node newtype(node value, node name, bool basic_type);
void printtypelist();
node thetype(int i);
bool isfunctiontype(node e);
bool isarraytype(node e);
bool istaggedarraytype(node e);
bool isobjecttype(node e);
bool isobjecttypeexpr(node e);
bool isarraytypeexpr(node e);
bool istaggedarraytypeexpr(node e);
bool isortype(node e);
bool ispointertype(node e);
bool istaggedtype(node e);
bool iscompositeortype(node e);
node functionargtypes(node t);
node functionrettype(node t);
node totype(node e);
node chktype(node e,scope v);
bool subtype(node s, node t);
bool checkargtypes(node argtypes, node funargtypes);
node returntype(node fun);
bool couldbenull(node t);
node nextfun(node fun);
bool isbasictype(node e);
bool isarithmetictype(node);
node basictype(node);
bool israwpointertypeexpr(node);
bool israwtypeexpr(node);
bool israwpointertype(node);
/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
