#ifndef _INTERP_H_
#define _INTERP_H_
#if defined(__cplusplus)
#include <string>
#include "supervisorinterface.h"
typedef struct M2_string_struct * M2_string;
typedef struct M2_arrayint_struct * M2_arrayint;
typedef M2_arrayint M2_arrayintOrNull;
typedef struct M2_stringCell_struct * M2_stringCell;
typedef struct M2_ArrayString_struct * M2_ArrayString;
typedef M2_ArrayString M2_ArrayStringOrNull;
typedef char * M2_charstar;
typedef unsigned char * M2_ucharstar;
typedef char ** M2_charstarstar;
typedef const char * M2_constcharstar;
typedef const unsigned char * M2_constucharstar;
typedef const char ** M2_constcharstarstar;
typedef char * M2_charstarOrNull;
typedef const char * M2_constcharstarOrNull;
typedef const unsigned char * M2_constucharstarOrNull;
typedef const char ** M2_constcharstarstarOrNull;

typedef struct parse_Frame_struct * parse_Frame;
typedef struct parse_Error_struct * parse_Error;
typedef struct parse_TokenFile_struct * parse_TokenFile;
typedef struct parse_Dictionary_struct * parse_Dictionary;
typedef struct parse_DictionaryClosure_struct * parse_DictionaryClosure;
typedef struct varstrin_varstring_struct * varstrin_varstring;
typedef struct tagged_union * parse_Expr;
typedef varstrin_varstring errio_BasicFile;
//exports
extern "C" {
	extern void interp_process();
	extern parse_Expr interp_value(parse_Expr e);
	extern parse_Expr interp_readeval(parse_TokenFile file,char returnLastvalue,char returnIfError);
	extern parse_Expr interp_readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError);
	extern parse_Expr interp_readeval4(parse_TokenFile file,char printout,parse_Dictionary dictionary,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError);
}

class M2CPP_Interperter;
/***
	Singleton for M2CPP Interperter.
	This really should not be a singleton, but while we transition code it will be.
***/
extern M2CPP_Interperter M2CPP_InterperterSingleton;

class M2CPP_Interperter
{
public:
	parse_Frame* localFrame();
	char* stopIfError();
	int& debugLevel();
	errio_BasicFile* M2_stderr();
	struct atomic_field* interruptedFlag();
	char* interruptPending();
	/***
		Get the current M2CPP Interperter in some well defined way.
		Currently returns the global interperter.
		Eventually will be instance local.
	***/
	static M2CPP_Interperter* gsp() { return &M2CPP_InterperterSingleton; }
	
	void interp_process();
	/***
		Attempt to execute the given expression.  
		@param e SymbolClosure, CodeClosure,stringCell, not null.
		@return Expr, possibly an error.
	***/
	parse_Expr value(parse_Expr e);
	/***
		???
		@param returnLastvalue ???
		@param returnIfError ???
		@param file File to read & evaluate.
		@return Expr, possibly an error.
	***/
	parse_Expr readeval(parse_TokenFile file,char returnLastvalue,char returnIfError);
	parse_Expr readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError);
	parse_Expr readeval4(parse_TokenFile file,char printout,parse_Dictionary dictionary,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError);
protected:
	/***
		From an error, deduce the correct exit code and attempt to exit.
		@param err An error, not null.  
	***/
	void exit(parse_Error err);
	/***
		Create a new file with line numbers for tokenizing the given string.
		@return Not null.
	***/
	parse_TokenFile stringTokenFile(M2_string name,M2_string contents);
};
/***
   Create a new M2 object of type T and return it as type R
***/
template<class R, class T> inline R M2CPP_NewObject(int typecode)
{
	R r = reinterpret_cast<R>(GC_MALLOC(sizeof(T)));
	r->type_ = typecode;
	return r;
}
/***
   Create a new M2 string with the given name.
   @param string Null terminated string.  This does not perform bounds checking.
***/
extern M2_string M2CPP_NewString(const char* string);
/***
   Create a new constant M2 string with the given name.
   This is allowed to pool strings or perform other optimizations assuming constantness.
   @param string Null terminated string.  This does not perform bounds checking.
***/
extern M2_string M2CPP_NewConstString(const char* string);
/***
   Create a new M2 string with the given name.
   @param string STL string.
***/
extern M2_string M2CPP_NewString(const std::string& str);
#endif

#endif
