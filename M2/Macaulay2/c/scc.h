/*		Copyright 1993 by Daniel R. Grayson		*/

#include "M2/config.h"
#include "compat.h"

#define EQUAL 0

#define new(type) (type *)getmem(sizeof(type))
#define newarray(type,n) (type *) getmem((n) * sizeof(type))
#define numberof(x) (sizeof(x)/sizeof(x[0]))
#define forarray(i,x) for(i=0; i<numberof(x); i++)
#define forlist(p) for(;p;p=p->next)
#define ckarray(x,i) (assert(i >= 0),assert(i < numberof(x)))
#ifndef roundup
#define roundup(n,d) ((((n)+(d)-1)/(d))*(d))
#endif

struct POS { char *filename; short lineno, column; } ;
typedef struct NODE *node;
typedef struct SCOPE *scope;
extern scope global_scope;
typedef node (*chkfun) (node,scope);

#define clear_memory(v) memset(v,0,sizeof(*v))

struct NODE {
     enum TAG {
	  cons_tag, position_tag, unique_string_tag, string_tag, int_const_tag,
	  double_const_tag, type_tag, char_const_tag,
	  string_const_tag, symbol_tag } tag;
     union BODY {
     	  struct INT_CONST { char *contents; } int_const;
	  struct CHAR_CONST { char contents; } char_const;
	  struct DOUBLE_CONST { char *contents;} double_const;
	  struct CONS { 
	       node car, cdr; 
	       struct POS pos;
	       } cons;
	  struct POSITION { 
	       node contents;
	       struct POS pos;
 	       } position;
	  struct UNIQUE_STRING {
	       char *characters; 
	       node symbol_list;
	       int flags;
#define str_keyword_F 0x1
	       unsigned short seqno;
	       unsigned short hash;
	       unsigned short token;
	       } unique_string;
	  struct STRING {
	       char *characters;
	       } string;
	  struct STRING_CONST {
	       char *characters;
	       } string_const;
     	  struct SYMBOL {
	       node name;
	       node package;
	       node defining_statement;
	       node type;
	       node value;
	       node cprintvalue;
	       struct POS pos;
	       chkfun check;
	       char *Cname;
	       unsigned short seqno;
	       node args, body, export_list;
	       int flags;
#define tmp_F 	   	 0x1
#define readonly_F 	 0x2
#define symbol_F         0x4
#define keyword_F        0x8
#define constant_F       0x10
#define defined_F        0x20
#define macro_function_F 0x80
#define export_F         0x100
#define import_F         0x200
#define global_F         0x400
#define intern_F     	 0x800
#define literal_F        0x1000
#define visible_F        0x2000
#define signature_F	 0x4000
#define nouniquify_F     0x8000
#define constructor_F    0x10000
#define destructor_F     0x20000
#define threadLocal_F    0x40000
#define const_F          0x80000
#define macro_variable_F 0x100000
#define errmsg_given_F   0x200000
#define package_active_F 0x400000
#define none_F	   	0
	       } symbol;
	  struct TYPE {
	       node name;
     	       node forward;
	       node definition;
     	       node commons;  	/* for an or-type, the typedeftail held in common
				   by the non-null components of the or */
	       char *Cname;
	       short seqno;
	       int runtime_type_code;
     	       int flags;
#define deferred_F 1		/* a type whose definition is not known yet */
#define should_be_pointer_F 2
#define should_be_tagged_F 4
#define identified_F 8		/* identified by the algorithm in totypesRec() */
#define raw_pointer_type_F 0x10
#define raw_atomic_pointer_type_F 0x20
#define raw_type_F 0x40
#define raw_atomic_type_F 0x80
#define arithmetic_type_F 0x100
#define basic_type_F 0x200
#define integer_type_F 0x400
#define composite_F 0x800	/*an or type with at least 2 nonnull members*/
	       } type;
	  } body;
     };

struct SCOPE {
     scope previous;		/* scope containing this one */
     node current_package;
     node symbols;
     node decls;		/* declarations of variables; held in reverse order */
     node tmpdecls;		/* declarations of temporary variables; held in reverse order */
     node before;		/* statements to execute before the expression is used; held in reverse order */
     node after;		/* statements to execute after the expression returned is used; held in order */
     node finals;		/* statements to execute at the end of the scope; held in order */
     node signature;		/* declarations for the signature file (*.sig); held in reverse order */
     node rettype;		/* whether it's a defun */
     node thread_inits;		/* statements to execute when a thread is started; held in reverse order */
     bool defun;
     bool disable_breaks;
     bool loop;
     node break_loop_label;
     bool break_loop_label_used;
     node continue_loop_label;
     node new_array_index_tmpvar;
     node new_array_tmpvar;
     node new_array_len;
     node new_array_element_type;
     node new_array_break_label;
     bool new_array_value_provided;
     int deferred_definitions_active;
     node previous_deferred_definition;
     };
void fatalpos(node, char *,...);
node totype(node);
void cprinttypevar(node, node);
bool isarraytype(node);
bool istaggedarraytype(node);
bool isobjecttype(node);
bool istaggedobjecttype(node);
bool isortype(node);
node functionrettype(node);
void unwind(node *);

#define newnode(bodyPART,tag) newnode1( \
     sizeof(struct NODE) - sizeof(union BODY) + sizeof(struct bodyPART), \
     tag )
	  
#define apply(f,p)  ((p)=f(p))
#define isint(p)    (p!=NULL && (p)->tag == int_const_tag)
#define issym(p)    (p!=NULL && (p)->tag == symbol_tag)
#define ispos(p)    (p!=NULL && (p)->tag == position_tag)
#define isstr(p)    (p!=NULL && (p)->tag == unique_string_tag)
#define isstrconst(p)    (p!=NULL && (p)->tag == string_const_tag)
#define istype(p)   (p!=NULL && (p)->tag == type_tag)
#define isstrpos(p) (ispos(p) && isstr((p)->body.position.contents))
#define isword(p)   (isstrpos(p) ||  isstr(p))
#define issympos(p) (ispos(p) && issym((p)->body.position.contents))
#define iscons(p)   (p!=NULL && (p)->tag == cons_tag)
#define islist(p)   ((p)==NULL || (p)->tag == cons_tag)

#include "readfile.h"
#include "dictionary.h"
#include "error.h"
#include "list.h"
#include "cprint.h"
#include "chk.h"
#include "type.h"
#include "scc1.h"
#include "grammar.h"
#include "debugging.h"

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
