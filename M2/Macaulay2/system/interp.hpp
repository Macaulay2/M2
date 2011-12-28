#ifndef _INTERP_H_
#define _INTERP_H_
#if defined(__cplusplus)
#include <string>
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


/**
   Create a new M2 string with the given name.
   @param string Null terminated string.  This does not perform bounds checking.
**/
extern M2_string M2CPP_NewString(const char* string);
/**
   Create a new constant M2 string with the given name.
   This is allowed to pool strings or perform other optimizations assuming constantness.
   @param string Null terminated string.  This does not perform bounds checking.
**/
extern M2_string M2CPP_NewConstString(const char* string);
/**
   Create a new M2 string with the given name.
   @param string STL string.
**/
extern M2_string M2CPP_NewString(const std::string& str);
#endif

#endif
