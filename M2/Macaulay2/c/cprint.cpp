/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"
#include <sstream>

//are we currently printing a declaration?
//used only for when in pthread mode to tell symbols not to print workaround for symbol 
int threadLocalDeclarationFlag=0;

/***
	Print escaped string constant.
	@param p String_Const node, not null.
***/
static void d_printstringconst(node p){
     const char *s = tostring(p);
     putchar('"');
     while (*s) {
	  switch(*s) {
	       case '\n': d_put("\\n"); break;
	       case '"' : d_put("\"" ); break;
	       case '\r': d_put("\\r"); break;
	       case '\f': d_put("\\f"); break;
	       case '\t': d_put("\\t"); break;
	       default: putchar(*s);
	       }
	  s++;
	  }
     putchar('"');
     }

/***
	Write string constant to stringstream.
	@param p String_Const node, not null.
	@param ss Stringstream to write to.
***/
static void printstringconst(node p, std::stringstream& ss ){
     const char *s = tostring(p);
	 ss << '"';
     while (*s) 
	 {
		 switch(*s) 
		 {
		 case '\n':
			 ss << "\\n";
			 break;
		 case '"' :
			 ss << "\"" ;
			 break;
		 case '\r':
			 ss << "\\r";
			 break;
		 case '\f':
			 ss << "\\f";
			 break;
		 case '\t':
			 ss << "\\t";
			 break;
		 default:
			 ss << *s;
	       }
		 s++;
	 }
     ss << '"';
}


const char *tostring(node e){
     if (e == NULL) return "<<NULL>>";
     switch(e->tag){
          case cons_tag: d_pp(e);return "<<cons>>";
	  case position_tag: return tostring(e->body.position.contents);
	  case unique_string_tag: return e->body.unique_string.characters;
	  case string_tag: return e->body.string.characters;
	  case int_const_tag: return e->body.int_const.contents;
          case double_const_tag: return e->body.double_const.contents;
          case type_tag: return tostring(e->body.type.name);
	  case symbol_tag: return tostring(e->body.symbol.name);
     	  case char_const_tag: return strperm(intToString(e->body.char_const.contents));
	  case string_const_tag: return e->body.string_const.characters;
	  default: assert(FALSE); return "<<unrecognized node type>>";
	  }
     }
/***
    This function will output special delimited names if the d keyword is actually a c++ reserved keyword. 
	@param p A symbol node, not null.
	@param ss Stringstream to write to.
    @return 1 if this function found a reserved symbol, 0 otherwise.
 ***/
static int makeSymbolSafe(node p, std::stringstream& ss)
{
  if(0==strcmp(p->body.symbol.Cname,"class"))
	  ss << "scc_reserved_class";
  else if(0==strcmp(p->body.symbol.Cname,"mutable"))
	  ss << "scc_reserved_mutable";
  else if(0==strcmp(p->body.symbol.Cname,"not"))
	  ss << "scc_reserved_not";
  else if(0==strcmp(p->body.symbol.Cname,"new"))
	  ss << "scc_reserved_new";
  else
    return 0;
  return 1;
}
void generateC(node e, std::stringstream& ss);
void generateCtypevar(node t, node v, std::stringstream& ss);
/***
	Generate a basic C or C++ symbol.
	@param p A symbol node, not null.
	@param stringstream Stringstream to write to
***/
static void generateSymbolBasic(node p, std::stringstream& ss)
{
	assert(p != NULL);
	assert(p->tag == symbol_tag);
	if (p->body.symbol.cprintvalue) {
		generateC(p->body.symbol.cprintvalue, ss);
	}
	else if (p->body.symbol.Cname != NULL) {
		/**
		   This case corresponds to emitting a basic symbol.
		   Note that we have a few special cases in here because class, mutable, and not are not keywords in d but they are in c++.
		   While these cases will compile fine in C, they will cause c++ compiler errors if we do not special case and catch them.
		   For backwards compatability we do this here.  If we did it by making them d keywords, we would break existing d code.
		**/
		if(!makeSymbolSafe(p,ss))
			ss << p->body.symbol.Cname;
	}
	else {
		ss << tostring(p);
	}	
}
/***
	Get symbol name.
	@param p Symbol node, not null.
***/
const char* getsymbolbasicname(node p){
	assert(p != NULL);
	assert(p->tag == symbol_tag);
	if (p->body.symbol.Cname != NULL) {
		return (p->body.symbol.Cname);
	}
	else {
		return (tostring(p));
	}
}
/***
	Generate a string containing the c code for the symbol.
	@param p A symbol node, not null.
	@param stringstream Stringstream to write to
***/
void generateSymbol(node p, std::stringstream& ss)
{
	if(pthreadThreadLocal && !threadLocalDeclarationFlag && p->body.symbol.flags & threadLocal_F)
	{
	    //THIS IS AN INT EXAMPLE
	    //	    *((int*)TS_Get_Local(M2_gbTrace_id)) = 0;
		ss << "(*((";
	    node ltype = type(p);
		generateC(ltype,ss);
		ss << "*)" << "TS_Get_Local(";
	    generateSymbolBasic(p,ss);
		ss << "_id)))";
	}
	else
		generateSymbolBasic(p,ss);
}
void d_printsymbol(node p){
	std::stringstream ss;
	generateSymbol(p,ss);
	d_put(ss.str().c_str());
}

void d_pput(const char *s){
	while (*s) {
		putchar(*s);
		if (*s == '\n') ;
		s++;
	}
}
/***
	Current c indentation level.
***/
int clevel=0;
/***
	Put correct number of tabs/spaces for current c indentation level.
***/
static void cindent(){
	int cols = 2 * clevel;
	while (cols >= 8) {
		cols -= 8;
		d_pput("\t");
	}
	while (cols >= 1) {
		cols -= 1;
		d_pput(" ");
	}
}     

void d_put(const char *s){
     if (0 == strcmp(s,"{")) clevel++;
     if (0 == strcmp(s,"}")) clevel--;
     while (*s) {
	  putchar(*s);
	  if (*s == '\n') cindent();
	  s++;
	  }
     }

static struct POS *curpos = NULL;

struct POS *pos2(node n) {
     struct POS *p;
     while (iscons(n)) {
     	  if (CAR(n) == declare__S) return NULL;
	  if (n->body.cons.pos.filename != NULL) return &n->body.cons.pos;
	  p = pos2(CAR(n));
	  if (p != NULL) return p;
	  n = CDR(n);
	  }
     return (
	  ispos(n) 
	  ? &n->body.position.pos 
	  : NULL
	  );
     }
void locn(node n){
     if (n == NULL) {
	  curpos = NULL;
	  return;
	  }
     struct POS *p = pos2(n);
     if (p != NULL) 
	  curpos = p;
     }

void d_printpos(){
     if (curpos != NULL && !noline) {
	  printf("# line %d \"%s\"",curpos->lineno,curpos->filename);
	  d_put("\n");
	  }
     }
void printpos(std::stringstream& ss){
	if (curpos != NULL && !noline) {
		ss << "# line " << curpos->lineno << "\"" << curpos->filename << "\"";
		ss << "\n";
	}
}

void pprint(node p){
	if (p==NULL) {
		d_pput("()");
		return;
	}
	switch (p->tag) {
	case cons_tag: {
		if (p->body.cons.pos.filename != NULL) {
			printf("@%d:%d ", p->body.cons.pos.lineno, p->body.cons.pos.column);
		}
		d_pput("(");
		while (p != NULL) {
		    pprint(CAR(p));
		    p = CDR(p);
		    if (p == NULL) break;
		    d_pput(" ");
		}
		d_pput(")");
		break;
	}
	case type_tag: {
		printf("{");
		while (TRUE) {
			if (p->body.type.name != NULL) {
				printf("TYPE[%d]:name => ",p->body.type.seqno);
				pprint(p->body.type.name);
			}
			else if (p->body.type.definition != NULL) {
				printf("TYPE[%d]:definition => ",p->body.type.seqno);
				pprint(p->body.type.definition);
			}
			else {
				printf("TYPE[%d]", p->body.type.seqno);
			}
			if (p->body.type.flags) {
				printf(" flags<");
				if (p->body.type.flags & deferred_F) printf(" deferred");
				if (p->body.type.flags & should_be_pointer_F) printf(" should_be_pointer");
				if (p->body.type.flags & should_be_tagged_F) printf(" should_be_tagged");
				if (p->body.type.flags & identified_F) printf(" identified");
				printf(" >");
			}
			if (p->body.type.forward == NULL) break;
			p = p->body.type.forward;
			printf(" ===> "); /* forwarding */
		}
		printf("}");
		break;
	}
	case position_tag: {
		if (p->body.cons.pos.filename != NULL) {
			printf("@%d:%d ", p->body.position.pos.lineno, p->body.position.pos.column);
		}
		pprint(p->body.position.contents);
		break;
	}
	case unique_string_tag: case string_tag: {
		d_pput(tostring(p));
		break;
	}
	case int_const_tag: {
		d_pput(p->body.int_const.contents);
		break;
	}
	case char_const_tag: {
		d_pput(intToString(p->body.char_const.contents));
		break;
	}
	case double_const_tag: {
		d_pput(p->body.double_const.contents);
		break;
	}
	case string_const_tag: {
		d_printstringconst(p);
		break;
	}
	case symbol_tag: {
		d_printsymbol(p);
		break;
	}
	}
}

void d_pp(node n){
     pprint(n);
     d_pput("\n");
     }

void pprintl(node n){
     while (n != NULL) d_pp(CAR(n)), n = CDR(n);
     }

static bool isTypedefImported(const char* t)
{
	if(t)
	{
		std::string tstr(t);
		for(std::set<std::string>::iterator it = importPrefixes.begin(); it!=importPrefixes.end(); ++it)
		{
			if(tstr.find(*it)==0)
			{
				//check for strict equality. we don't want strings to match strings1_
				if(it->length()<=tstr.length()-2 && tstr.at(it->length())=='_')
				{
					return true;
				}
			}
		}
	}
	return false;
}
/***
	@param stringstream Stringstream to write to.
***/
void generateCList(node e, std::stringstream& ss)
{
	ss << "(";
	if (e != NULL) while (TRUE) {
			generateC(CAR(e),ss);
			e = CDR(e);
			if (e == NULL) break;
			ss << ",";
		}
	ss << ")";
}

void cprintlist(node e){
	std::stringstream ss;
	generateCList(e,ss);
	d_put(ss.str().c_str());
}
/***
	@param stringstream Stringstream to write to.
 ***/
void generateCSemi(node e,std::stringstream& ss){
	while (e != NULL) {
		node f = CAR(e);
		node g = iscons(f) ? CAR(f) : NULL;
		locn(f);
		printpos(ss);
		generateC(f,ss);
		std::string curstring = ss.str();
		if (g==block__K || g==define__S ) 
		{
		}
		else if(curstring[curstring.length()-1]=='\n')
		{
			
		}
		else if(curstring[curstring.length()-1]==' ')
		{
			
		}
		else
		{
			ss << ";\n";
		}
		e = CDR(e);
	}
}

void generateUniqueM2StructName(node t, std::stringstream& ss)
{
	std::stringstream ss2;
	bool tagged = istaggedarraytype(t);
	node m = typedeftail(t);
	node typ = CAR(m);
	node len;
	if (length(m)==2)
		len = CADR(m);
	else
		len = NULL;
	ss2 << "SCC_M2_" << tagged << "_";
	if (length(m) == 1)
		ss2 << "int_len_";
	generateC(typ,ss2);
	ss2 << "_array";
	if (len!=NULL) 
		generateC(len,ss2);
	else 
		ss2 << "1";
	std::string unsafe = ss2.str();
	for(int i = 0; i < unsafe.length(); ++i)
	{
		if(unsafe[i]==' ')
			ss << "_";
		else if(unsafe[i]=='*')
			ss << "STAR";
		else
			ss << unsafe[i];
	}
}
/***
	Generate C struct header for an object type to string.
	@param t Type, not null.
	@param baseclass True if a baseclass definition should be emitted.  This is to enable C++ new/delete compatability.
	@param ss Stringstream to write to.
***/
static void generateCStructTag(node t, bool baseclass, std::stringstream& ss)
{ 
	assert(istype(t));
	ss << "struct ";
	if (t->body.type.name) {
		generateC(t->body.type.name,ss);
		ss << "_struct";
	}
	else
	{
		generateUniqueM2StructName(t,ss);
	}
	if (baseclass) ss << " BASECLASS";
}
/***
	Generate an array object definition.
	@param t Type node, that is an array, not null.
	@param ss Stringstream to write to.
	@param exportFile True if this is being called during the process of generating an exports file, false otherwise.
***/
static void generateCArrayDef(node t, std::stringstream& ss, bool exportFile = false){
	bool tagged = istaggedarraytype(t);
	node m = typedeftail(t);
	node typ = CAR(m);
	node len;
	std::stringstream ss2;
	std::string structTag;
	generateCStructTag(t,FALSE,ss2);
	structTag = ss2.str();
	std::string subStructTag = structTag.substr(7);
	if(exportFile && isTypedefImported(subStructTag.c_str()))
	{
		return;
	}
	else if(exportFile && t->body.type.name==NULL)
	{
		ss << "#ifndef DEF_";
		generateUniqueM2StructName(t,ss);
		ss << "\n#define DEF_";
		generateUniqueM2StructName(t,ss);
		ss << "\n";
	}
	ss << structTag.c_str();
	if (length(m)==2)
		len = CADR(m);
	else
		len = NULL;
	ss << " {";			/* the extra space prevents further indentation! */
	if (tagged) {
		ss << "unsigned short type_;";
	}
	if (length(m) == 1)
		ss << "int len;";
	generateC(typ,ss);
	ss << " array[";
	if (len!=NULL) 
		generateC(len,ss);
	else 
		ss << "1";
	ss << "];};\n";
	if(exportFile && t->body.type.name==NULL)
	{
		ss << "#endif\n";
	}

}
/***
	Generate an object (struct) definition.
	@param t Type node, not null.
	@param ss Stringstream to write to.
	@param exportFile True if this is being called during the process of generating an exports file, false otherwise.
***/
static void generateCObjectDef(node t, std::stringstream& ss, bool exportFile = false){
	node m;
	bool had_a_member = FALSE;
	std::stringstream ss2;
	generateCStructTag(t,true,ss2);
	std::string structTag = ss2.str();
	std::string subStructTag = structTag.substr(7);
	if(exportFile && isTypedefImported(subStructTag.c_str()))
	{
		return;
	}
	ss << structTag;
	ss << " {";
	if (istaggedobjecttype(t)) {
		ss << "unsigned int type_;";
		had_a_member = TRUE;
	}
	for (m=typedeftail(t); m != NULL; m = CDR(m)) {
		if (CADAR(m) == void_T) continue;
		generateCtypevar(CADAR(m),CAAR(m),ss);
		ss << ";";
		had_a_member = TRUE;
	}
	if (!had_a_member) {
		ss << "char _;";
	}
	ss << "};\n";
}
/***
	Generate c type output.
	@param t Type node.
	@param withstar Make pointer type (append *).
	@param usename If true, use name of type instead of cname.
	@param ss Stringstream to write to.
***/
static void generateCType(node t, bool withstar, bool usename, std::stringstream& ss)
{
	assert(istype(t));
	if (t == bad_or_undefined_T) {
		if (debug) warning("undefined type");
		ss << "<<undefined type>> ";
		assert(FALSE);
	}
	t = typeforward(t);
	if (t->body.type.Cname != NULL) {
		assert(!(israwpointertype(t) && !withstar));
		ss << t->body.type.Cname;
	}
	else if (t->body.type.name != NULL && withstar && usename) {
		generateC(t->body.type.name,ss);
	}
	else if (isortype(t)) {
		if (t->body.type.flags & composite_F)
			ss << "struct tagged_union *";
		else {
			node tt = typedeftail(t);
			while (tt != NULL) {
				if (car(tt) != null_T) {
					std::string tmp;
					generateCType(car(tt),TRUE,TRUE,ss);
				}
				tt = cdr(tt);
			}
		}
	}
	else if (isarraytype(t) || istaggedarraytype(t) || isobjecttype(t) || istaggedobjecttype(t)) {
		generateCStructTag(t,FALSE,ss);
		if (withstar) ss << " *";
	}
	else if (t->body.type.definition != NULL) {
		generateC(t->body.type.definition,ss);
	}
	else assert(FALSE);
	return;
}
/***
    This function generates the sizeof type t to the stringstream.
    @param t Type, not null, to print size of.
    @param arrayLength Length of array.  NULL if not array type, otherwise node that describes array length.
	@param ss Stringstream to write to.
***/
static void generateCsomesizeof(node t, node arraylen, std::stringstream& ss){
	assert(istype(t));
    ss << "sizeof(";
	if (israwpointertype(t)) {
		ss << "*((";
		generateCType(t,TRUE,FALSE,ss);
		ss << ")0)";
	}
	else generateCType(t,FALSE,FALSE,ss);
	ss << ")";
	if (isarraytype(t)||istaggedarraytype(t)) {
		node m = typedeftail(t);
		if (length(m) == 1) {
			/* if m has length 2, the second element is the declared length, already checked, so we don't have to add anything here */
			node typ = CAR(m);
			assert(arraylen != NULL);
			ss << " + (";
			generateC(arraylen,ss);
			ss << " - 1)*sizeof(";
			generateC(typ,ss);
			ss << ")";
		}
	}
}
/***
	Given node s of type previously determined getmem__S, print the c code to the stringstream.
	Generates malloc calls.
	CAR(s) = assignment target
	If CADR(s) exists, sizeof(CADR(s)), else sizeof(type(CAR(s))))
	@param g Node s (Note: not CAR since g needs a list).
	@param ss Stringstream to write to.
***/
static void generateCgetmem(node g,std::stringstream& ss){
	node s = CAR(g);
	node t = type(s);
	generateC(s,ss);
	ss << " = ";
	ss << "(";
	generateC(t,ss);
	ss << ") ";
	ss << (pointer_to_atomic_memory(t) ? "GC_MALLOC_ATOMIC" : "GC_MALLOC");
	ss << "(";
	generateCsomesizeof(t, length(g)==2 ? CADR(g) : NULL,ss);
	ss << ")";
}
/***
	Generate typedefs to the stringstream.
	@param t Type node, not null.
	@param ss Stringstream to write to.
	@param exportFile True if this is being called during the process of generating an exports file, false otherwise.
***/
static void generateCtypedef(node t, std::stringstream& ss, bool exportFile = false) {
	/* define the structure/union tag */
	assert(istype(t));
	if (t->body.type.name != NULL) {
		if (isfunctiontype(t)) {
			ss << "typedef ";
			generateC(functionrettype(t),ss);
			ss << " ";
			ss << "(*";
			generateC(t->body.type.name,ss);
			ss << ")";
			generateCList(functionargtypes(t),ss);
		}
		else {
			std::stringstream ss2;
			generateC(t->body.type.name,ss2);
			if(exportFile && isTypedefImported(ss2.str().c_str()))
			{
				return;
			}			
			ss << "typedef ";
			generateCType(t,true,false,ss);
			ss << " ";
			ss << ss2.str();
		}
		assert(issym(t->body.type.name));
		assert(isstr(t->body.type.name->body.symbol.name));
		ss << ";\n";
	}
}
/***
	@param ss Stringstream to write to.
 ***/
static void generateCDefine(node t,bool definitions, std::stringstream& ss, bool exportFile=false) {
	node typ = type(t);
	int flags = t->body.symbol.flags;
	assert( typ != void_T );
	if(exportFile)
	{
		std::string name;
		std::stringstream ss2;
		generateC(t,ss2);
		name = ss2.str();
		if(isTypedefImported(name.c_str()))
		{
			return;
		}

	}
	t = unpos(t);
	assert(issym(t));
	/* assert( 0 != strcmp("node0",tostring(t)) ); */
	if (definitions && (flags & import_F)) return;
	if (definitions && (flags & macro_variable_F)) return;
	if (flags & (export_F|import_F)) {
		if (!definitions) 
			ss << "extern ";
	}
	else if ( 
			 (
			  (flags & global_F)
			  || (
				  isfunctiontype(typ)
				  &&
				  (flags & constant_F)
				  )
			  )
			 &&
			 !(flags & visible_F)
			  ) {
		ss << "static ";
	}
	//this flag entirely exists because thread locals emit semicolons. 
	//we don't want double semicolons if we are exporting files.
	//its not obvious why we do this.
	bool needsemi = true;
	if (flags & threadLocal_F)
	{
		ss << "int ";
		threadLocalDeclarationFlag = 1;
		generateC(t,ss);
		threadLocalDeclarationFlag = 0;	      
		ss << "_id;\n";
		needsemi=false;
		//	       put("__thread ");
	}
	if (flags & const_F) ss << "const ";
	if (isfunctiontype(typ)) {
		if (flags & macro_function_F) return;
		generateC(functionrettype(typ),ss);
		ss << " ";
		if (flags & constant_F) {
			generateC(t,ss);
		}
		else {
			ss << "(*";
			generateC(t,ss);
			ss << ")";
		}
		generateCList(functionargtypes(typ),ss);
	}
	else if(!(flags & threadLocal_F)){
		generateC(typ,ss);
		ss << " ";
		generateC(t,ss);
	}
	if (flags & constructor_F) ss << " __attribute__ ((constructor))";
	if (flags & destructor_F) ss << " __attribute__ ((destructor))";
	if (exportFile && needsemi) ss << ";\n";
}
/***
	Given node s of type previously determined return_S, print the c code to the current output file.
	@param g Node s.  (Note that g is a list, so no CAR).
	@param ss Stringstream to write to.
***/
static void generateCreturn(node s, std::stringstream& ss){
	if (length(s)==0) {
	    ss << "return";
	}
	else {
		ss << "return ";
		generateC(CAR(s),ss);
	}
}

/***
	Given node s of type previously determined assign__S, generate the c code to the stringstream.
	@param s CAR(s)
	@param t CADR(s)
	@param ss Stringstream to write to.
 ***/
static void generateCassign(node s, node t, std::stringstream& ss){
	std::stringstream ss2;
	generateC(type(s),ss2);
	/*note we need to cast because of incompatabilities between sequence numbers/differences between c & c++.
	  This right now seems to be the only case where this actually raises a problem.
	  Essentially this makes sure that if we are returning some sort of array that is identical to a M2_arrayint,
	  it actually returns an M2_arrayint and not something similar.  
	**/
	if(ss2.str().find("(*)")==std::string::npos)
	{
		generateC(s,ss);
		ss << " = ";
		if(t->tag==cons_tag)
		{
			node a = CAR(t);
			node b = CDR(t);
			if(a==funcall__S)
			{
				node c = CAR(b);
				node typec = type(c);
				std::stringstream ss3;
				generateC(typec,ss3);
				std::string ftypestr = ss3.str();
				std::string rettype = ftypestr.substr(0,ftypestr.find(' '));
				if(rettype == "M2_arrayint")
					ss << "(" << rettype << ")";
			}
		}
	}
	else
	{
		generateC(s,ss);
		ss << " = ";
	}
	generateC(t,ss);
     }
/***
	@param ss Stringstream to write to.
***/
void generateCtypevar(node t, node v, std::stringstream& ss){
     if (isfunctiontype(t)) {
	  node r = functionrettype(t);
	  node a = functionargtypes(t);
	  generateC(r,ss);
	  ss << " (*";
	  generateC(v,ss);
	  ss << ")";
	  generateCList(a,ss);
	  }
     else {
		 generateC(t,ss);
		 ss << " ";
		 generateC(v,ss);
	  }
     }

/***
	Given node s of type previously determined define__S, generate the c code to the stringstream.
	This generates a function definition.
	@param g Node s.  (Note no CAR since the function needs a list).
	@param ss Stringstream to write to.
***/
static void generateCdefun(node g, std::stringstream& ss) {
     node fun = CAAR(g);
     node args = CDAR(g);
     node funtype = type(fun);
     node rettype = functionrettype(funtype);
     assert(issym(fun));
     locn(g);
     /* printpos(); */
     if (!(fun->body.symbol.flags & export_F)
	  &&
	  !(fun->body.symbol.flags & visible_F)
	  ) 
		 ss << "static ";
     generateC(rettype,ss);
     ss << " ";
     generateC(fun,ss);
     ss << "(";
     if (args != NULL)
		 while (TRUE) {
			 node w = CAR(args);
			 generateCtypevar(type(w),w,ss);
			 args = CDR(args);
			 if (args == NULL) break;
			 ss << ",";
		 }
     ss << ")";
     ss << "{";
     ss << "\n";
     generateCSemi(CDDR(g),ss);
     printpos(ss);
     ss << "}";
     ss << "\n";
     }
/***
	Given node s of type previously determined goto__S, generate c code to the stringstream.
	@param l CAR(s)
	@param ss Stringstream to write to.
 ***/
static void generateCgoto(node l,std::stringstream& ss){
	ss << "goto ";
	generateC(l,ss);
}
/***
	Given node s of type previously determined if_S, generate the c code to the stringstream.
	@param g Node s.  (Note no CAR since if needs a list).
	@param ss Stringstream to write to.
***/
static void generateCif(node g, std::stringstream& ss){
	ss << "if (";
	generateC(CAR(g),ss);
	ss << ") ";
	generateC(CADR(g),ss);
     if (length(g) == 3) {
		 ss << "; else ";
		 generateC(CADDR(g),ss);
	  }
     }
/***
	Given node s of type previously determined label__S, generate the c code to the stringstream.
	@param l CAR(s)
	@param ss Stringstream to write to.
***/
static void generateClabel(node l, std::stringstream& ss){
	generateC(l,ss);
	ss << ":";
}
/***
	Given node s of type previously determined isnull__S, generate the c code to the stringstream.
	@param e CAR(s)
	@param ss Stringstream to write to.
 ***/
static void generateCisnull(node e, std::stringstream& ss){
	ss << "NULL == ";
	generateC(e,ss);
     }
/***
	Given node s of type previously determined cast__S, generate the c code to the stringstream.
	@param typ CAR(s)
	@param e CADR(s)
	@param ss Stringstream to write to.
***/
static void generateCcast(node typ, node e, std::stringstream& ss){
    ss << "((";
	generateC(typ,ss);
	ss << ")";
	generateC(e,ss);
	ss << ")";
     }
/***
	Given node s of type previously determined array_check, generate the c code to the stringstream.
	@param indx CAR(s)
	@param len CADR(s)
	@param ss Stringstream to write to.
***/
static void generateCarraycheck(node indx, node len, std::stringstream& ss){
     struct POS *p;
     if (!arraychks) return;
     locn(len);
     locn(indx);
     p = pos(indx);
     printpos(ss);
     ss << "if (";
     generateC(indx,ss);
	 ss << " < 0 || ";
     generateC(indx,ss);
	 ss << " >= ";
     generateC(len,ss);
     ss << ") fatalarrayindex(";
     generateC(indx,ss);
     ss << ",";
     generateC(len,ss);
     ss << ",";
     if (p == NULL) {
     	 ss << "__FILE__,__LINE__,-1";
	  }
     else {
		 ss << "\"" << p->filename << "\"," << p->lineno << "," << p->column;
	  }
     ss << ")";
     }
/***
	Given node s of type previously determined array_len_check_S, generate the c code to the stringstream.
	@param len CAR(g)
	@param ss Stringstream to write to.
***/
static void generateCarraylencheck(node len, std::stringstream& ss){
     struct POS *p;
     if (!arraychks) return;
     p = pos(len);
     locn(len);
     printpos(ss);
     ss << "if (0 > ";
     generateC(len,ss);
     ss << ") fatalarraylen(";
     generateC(len,ss);
     ss << ",";
     if (p == NULL) {
		 ss << "__FILE__,__LINE__,-1";
	  }
     else {
		 ss << "\"" << p->filename << "\"," << p->lineno << "," << p->column;
	  }
	 ss << ")";
     }
/***
	Given node s of type previously determined array_take_S, generate the c code to the stringstream.
	@param array CAR(s)
	@param indx CADR(s)
	@param ss Stringstream to write to.
***/
static void generateCarraytake(node arr, node indx, std::stringstream& ss){
	generateC(arr,ss);
    ss << "->array[";
	generateC(indx,ss);
    ss << "]";
}     
/***
	Given node s of type previously determined take__S, generate the c code to the stringstream.
	@param f CAR(s)
	@param e CADR(s)
	@param ss Stringstream to write to.
***/
static void generateCtake(node f, node e, std::stringstream& ss){
	generateC(f,ss);
	ss << "->";
	generateC(e,ss);
}
/***
	Given node s of type previously determined prefix__S, generate the c code to the stringstream.
	@pram fun CAR(s)
	@param x CADR(s)
	@param ss Stringstream to write to.
***/
static void generateCprefix(node fun, node x, std::stringstream& ss){
	ss << "(";
	generateC(fun,ss);
	ss << " ";
	generateC(x,ss);
	ss << ")";
}
/***
	Given node s of type previously determined postfix_S, generate the c code to the stringstream.
	@param fun CAR(s)
	@param x CADR(s)
	@param ss Stringstream to write to.
***/
static void generateCpostfix(node fun, node x, std::stringstream& ss){
	ss << "(";
	generateC(x,ss);
	ss << " ";
	generateC(fun,ss);
	ss << ")";
}
/***
	Given node s of type previously determined infix__S, generate the c code to the stringstream.
	@param fun CAR(s)
	@param x CADR(s)
	@param y CADDR(s)
	@param ss Stringstream to write to.
 ***/
static void generateCinfix(node fun, node x, node y, std::stringstream& ss){
	ss << "(";
	generateC(x,ss);
    ss << " ";
	generateC(fun,ss);
    ss << " ";
	generateC(y,ss);
    ss << ")";
}
/***
	Given node s of type previously determined funcall__S, generate the c code to the stringstream.
	@param fun CAR(s)
	@param args CDR(s)
	@param ss Stringstream to write to.
 ***/
static void generateCfuncall(node fun, node args, std::stringstream& ss){
     node g = args;
     generateC(fun,ss);
     ss << "(";
     if (g!=NULL)
		 while (TRUE) 
		 {
			 generateC(CAR(g),ss);
			 g = CDR(g);
			 if (g == NULL) break;
			 ss << ", ";
		 }
     ss << ")";
     }

void d_put_unescape(const char *s) {
	while (*s) {
		if (*s == '\\') {
			s++;
			switch (*s) {
			case 'n': putchar('\n'); break;
			case '"' : putchar('"'); break;
			case 'b': putchar('\b'); break;
			case 't': putchar('\t'); break;
			case 'f': putchar('\f'); break;
			case 'r': putchar('\r'); break;
			case  0 : return       ;	/* shouldn't happen */
			default : putchar(*s)  ; break;
			}
		}
		else putchar(*s);
		s++;
	}
}
void put_unescape(const char *s, std::stringstream& ss) {
	while (*s) {
		if (*s == '\\') {
			s++;
			switch (*s) {
			case 'n': ss << '\n'; break;
			case '"' : ss << '"'; break;
			case 'b': ss << '\b'; break;
			case 't': ss << '\t'; break;
			case 'f': ss << '\f'; break;
			case 'r': ss << '\r'; break;
			case  0 : return       ;	/* shouldn't happen */
			default : ss << *s  ; break;
			}
		}
		else ss << *s;
		s++;
	}
}
/***
	Given node s of type previously determined Ccode_S, generate the c code to the stringstream.
	@param s Node previously determined to be Ccode_S.
	@param ss Stringstream to write to.
***/
static void generateCcode(node s, std::stringstream& ss){
	while (s != NULL) {
		node a = CAR(s);
		if (a->tag == string_const_tag) put_unescape(tostring(a),ss);
		else generateC(a,ss);
		s = CDR(s);
	}
}
/***
	Print the split cons node to the c/cpp output equivalent to the stringstream.
	@param f Given cons e, CAR(e)
	@param g Given cons e, CDR(e)
	@param ss Stringstream to write to.
***/
static void generateCCons(node f, node g, std::stringstream& ss) {
	if (ispos(f)) f = f->body.position.contents;
	if (f == getmem__S) generateCgetmem(g,ss);
	else if (f == array_len_check_S) generateCarraylencheck(CAR(g),ss);
	else if (f == array_check_S) generateCarraycheck(CAR(g),CADR(g),ss);
	else if (f == array_take_S) generateCarraytake(CAR(g),CADR(g),ss);
	else if (f == assign__S) generateCassign(CAR(g),CADR(g),ss);
	else if (f == cast__S) generateCcast(CAR(g),CADR(g),ss);
	else if (f == declare__S) {
		generateCDefine(CAR(g),TRUE,ss);
	}
	else if (f == define__S) generateCdefun(g,ss);
	else if (f == equalequal__S) generateCinfix(f,CAR(g),CADR(g),ss);
	else if (f == unequal_S) generateCinfix(f,CAR(g),CADR(g),ss);
	else if (f == funcall__S) generateCfuncall(CAR(g),CDR(g),ss);
	else if (f == goto__S) generateCgoto(CAR(g),ss);
	else if (f == if_S) generateCif(g,ss);
	else if (f == infix__S) generateCinfix(CAR(g),CADR(g),CADDR(g),ss);
	else if (f == isnull__S) generateCisnull(CAR(g),ss);
	else if (f == label__S) generateClabel(CAR(g),ss);
	else if (f == prefix__S) generateCprefix(CAR(g),CADR(g),ss);
	else if (f == postfix__S) generateCpostfix(CAR(g),CADR(g),ss);
	else if (f == return_S) generateCreturn(g,ss);
	else if (f == Ccode_S) generateCcode(CDR(g),ss);
	else if (f == take__S) generateCtake(CAR(g),CADR(g),ss);
	else if (isbasictype(f)) {
		ss << "((";
		generateC(f,ss);
		ss << ")";
		generateC(CAR(g),ss);
		ss << ")";
	}
	else {
		assert(FALSE);
	}
}
void generateC(node e, std::stringstream& ss)
{
	assert(e != NULL);
	switch(e->tag) {
	case type_tag:
		{
			generateCType(e,TRUE,TRUE,ss);
			return;
		}
	case double_const_tag: 
		{
			ss << e->body.double_const.contents; 
			return;
		}
	case int_const_tag: 
		{
			ss << e->body.int_const.contents; 
			return;
		}
	case char_const_tag: 
		{
			ss << intToString(e->body.char_const.contents); 
			return;
		}
	case unique_string_tag:
	case string_tag:
		{
			ss << tostring(e);
			return;
		}
	case string_const_tag: 
		{
			printstringconst(e,ss); 
			return;
		}
	case symbol_tag:
		{ 
			generateSymbol(e,ss);
			return;
		}
	case position_tag:
		{
			generateC(e->body.position.contents,ss); 
			return;
		}
	case cons_tag: 
		{
			node f = CAR(e);
			if (f == function_S) {
				generateC(CADDR(e),ss);
				ss << " (*)";
				generateCList(CADR(e),ss);
			}
			else {
				if (e->body.cons.pos.filename)
					curpos = &e->body.cons.pos;
				generateCCons(f,CDR(e),ss); 
			}
			return;
		}
	default:
		assert(false);
	}
}
/***
	Print the given node to the c/cpp output equivalent in the curent output file.
	@param e The node to print, not null.
***/
void cprint(node e) {
	assert(e != NULL);\
	std::stringstream ss;
	generateC(e,ss);
	printf("%s",ss.str().c_str());
}
void cprintIncludeList()
{
	printf("\n/* Includes */\n");
	for(std::set<std::string>::iterator it = headerFilesNeeded.begin(); it!=headerFilesNeeded.end(); ++it)
	{
		printf("#include \"%s\"\n",it->c_str());
	}
	printf("/*End Includes */\n");
}

/***
	A first type is largely an organizational definition used to put "simple" typedefs first in cprinttypes.
	@return True if a first type, false otherwise.
 ***/
static bool firsttype(node t) {
	return israwpointertypeexpr(t) || israwtypeexpr(t) || isobjecttype(t) || istaggedobjecttype(t) || isortype(t) || isarraytype(t) || istaggedarraytype(t);
}

void cprinttypes(){
     int i;
     node n;
     d_put("/* typedefs */\n");
     for (i=0; i<numtypes; i++) {
		 node t = typelist[i];
		 if (firsttype(t)) {
			 std::stringstream ss;
			 generateCtypedef(t,ss,true);
			 printf("%s",ss.str().c_str());
		 }
	 }
     d_put("/* array types */\n");
     for (i=0; i<numtypes; i++) {
		 node t = typelist[i];
		 if (isarraytype(t)||istaggedarraytype(t))
		 {
			 std::stringstream ss;
			 generateCArrayDef(t,ss,true);
			 printf("%s",ss.str().c_str());
		 }
	 }
     d_put("/* struct types */\n");
     for (i=0; i<numtypes; i++) {
		 node t = typelist[i];
		 if (isobjecttype(t) || istaggedobjecttype(t))
		 {
			 std::stringstream ss;
			 generateCObjectDef(t,ss,true);
			 printf("%s",ss.str().c_str());
		 }
	 }
     d_put("/* basic types */\n");
     for (i=0; i<numtypes; i++) {
		 node t = typelist[i];
		 if (!(firsttype(t) || isbasictype(t))) {
			 std::stringstream ss;
			 generateCtypedef(t,ss,true); /* if things are too circular this won't work! */
			 printf("%s",ss.str().c_str());
		 }
	 }
     d_put("/* functions */\n");
     for (n=complete_symbol_list; n!=NULL; n=CDR(n)) {
		 node s = CAR(n);
		 node t = type(s);
		 /* assert( 0 != strcmp("node0",tostring(s)) ); */
		 if (
			 (isfunctiontype(t)
			  && (s->body.symbol.flags & constant_F)
			  && (s->body.symbol.flags & visible_F)
			  && !(s->body.symbol.flags & macro_function_F))
			 ||
			 ((s->body.symbol.flags & (export_F | import_F))
			  && t != type__T
			  && t != void_T)
			 ) {
			 std::stringstream ss;
			 generateCDefine(s,FALSE,ss,true);
			 printf("%s",ss.str().c_str());
		 }
	 }
}     
/***
	Current indentation level in the d file.
***/
static int signest = 0;
/***
	Print signature indentation to the d file.
***/
static void sigindent(){
     int i;
     for (i=0; i<signest; i++) d_put("     ");
     }
void dprinttype(node e){
     assert(istype(e));
     if (e->body.type.name != NULL) {
     	  dprint(e->body.type.name);
	  }
     else {
	  dprint(e->body.type.definition);
	  }
     }
void dprintlist(node e){
	if (e != NULL) while (TRUE) {
			dprint(CAR(e));
			e = CDR(e);
			if (e == NULL) break;
			d_put(",");
		}
}
/***
	Print the signature for the given node.
	@param g Are there subtle restrictions on this node?
 ***/
static void dprintsig(node g){
     signest++;
     d_put("signature ");
     dprint(CAR(g)); g = CDR(g);
     d_put(" (\n");
     while (g != NULL) {
     	  sigindent();
     	  dprint(CAR(g)); g = CDR(g);
	  d_put(";\n");
	  }
     signest--;
     sigindent();
     d_put(")");
     }
/***
	???
	@param f Are there subtle restrictions on this node?
 ***/
static void dprintop(node f){
     if (isstr(f) && !validtoken(tostring(f))) d_put("op");
     dprint(f);
     }
/***
	???
***/
static void dprintinfix(node op, node args){
     int i;
     d_put("(");
     for (i=1; ; ) {
	  dprint(nth(args,i));
	  i++;
	  if (i > length(args)) break;
	  d_put(" ");
	  dprint(op);
	  d_put(" ");
	  }
     d_put(")");
     }
/***
	Print to the d file the output equivalent of function node e.
***/
static void dprintfunction(node g){
     d_put("function(");
     dprintlist(car(g));
     d_put("):");
     dprint(cadr(g));
     }
void dprintcons(node f, node g) {
     f = unpos(f);
     if (f == colon__S) { dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == threadLocal_S) { d_put("threadLocal "); dprint(CAR(g)); }
     else if (f == const_S) { d_put("const "); dprint(CAR(g)); }
     else if (f == export_S) { d_put("import "); dprint(CAR(g)); }
     else if (f == import_S) { d_put("import "); dprint(CAR(g)); }
     else if (f == use_S) { d_put("use "); dprint(CAR(g)); }
     else if (f == Pointer_S) { d_put("Pointer "); dprint(CAR(g)); }
     else if (f == atomicPointer_S) { d_put("atomicPointer "); dprint(CAR(g)); }
     else if (f == Type_S) { d_put("Type "); dprint(CAR(g)); }
     else if (f == atomicType_S) { d_put("atomicType "); dprint(CAR(g)); }
     else if (f == arithmeticType_S) { d_put("arithmeticType "); dprint(CAR(g)); }
     else if (f == integerType_S) { d_put("integerType "); dprint(CAR(g)); }
     else if (f == header_S) { d_put("header "); dprint(CAR(g)); }
     else if ( f == leftOperator_S || f == rightOperator_S || f == prefixOperator_S 
			   ) { d_put(tostring(f)); d_put(" "); dprint(CAR(g)); d_put(" "); dprint(CADR(g)); }
     else if (f == declarations_S) { d_put("declarations "); dprint(CAR(g)); }
     else if (f == colonequal__S) {dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == coloncolonequal__S) {dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == signature_S) dprintsig(g);
     else if (f == or_S) dprintinfix(f,g);
     else if (f == or_K) dprintinfix(f,g);
     else if (f == function_S) dprintfunction(g);
     else if (f == object__K) { d_put("{ "); dprintlist(g); d_put(" }"); }
     else if (f == tagged_object_K) { d_put("{+"); dprintlist(g); d_put(" }"); }
     else if (f == while_S) { d_put ("while("); dprint(car(g)); d_put(") do ("); dprint(cadr(g)); d_put(")"); }
     else { dprintop(f); d_put("("); dprintlist(g); d_put(")"); }
     }

void dprint(node e){
     if (e == NULL) d_put("NULL");
     switch(e->tag){
	  case type_tag: dprinttype(e); return;
	  case double_const_tag: d_put(e->body.double_const.contents); return;
	  case int_const_tag: d_put(e->body.int_const.contents); return;
	  case char_const_tag: d_put(intToString(e->body.char_const.contents)); return;
          case unique_string_tag: case string_tag: d_put(tostring(e)); return;
	  case string_const_tag: d_printstringconst(e); return;
	  case symbol_tag: dprint(e->body.symbol.name); return;
	  case position_tag: cprint(e->body.position.contents); return;
	  case cons_tag: dprintcons(CAR(e),CDR(e)); return;
          default: assert(FALSE);
	  }
     }

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
