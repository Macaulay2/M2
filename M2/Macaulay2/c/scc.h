/*		Copyright 1993 by Daniel R. Grayson		*/

#include "M2/config.h"
#include "compat.h"
#define put(s) fputs(s,stdout)

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
typedef struct ENV *env;
typedef node (*chkfun) (node,env);

#define ZERO_MEM(v) memset(v,0,sizeof(*v))

struct NODE {
     enum TAG { cons_tag, position_tag, string_tag, int_const_tag,
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
	  struct STRING {
	       char *contents; 
	       node symbol_list;
	       unsigned short seqno;
	       unsigned short hash;
	       unsigned short token;
	       } string;
	  struct STRING_CONST {char *contents;} string_const;
     	  struct SYMBOL {
	       node name;
	       node package;
	       node type;
	       node value;
	       node cprintvalue;
	       struct POS pos;
	       chkfun chk;
	       char *Cname;
	       unsigned short seqno;
	       node args, body, export_list;
	       int flags;
#define tmp_F 	   	1
#define readonly_F 	2
#define symbol_F        4
#define keyword_F       8
#define constant_F      0x10
#define initialized_F   0x20
#define macro_F         0x80
#define export_F        0x100
#define import_F        0x200
#define global_F        0x400
#define intern_F     	0x800
#define literal_F       0x1000
#define visible_F       0x2000
#define signature_F	0x4000
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
     	       int flags;
#define deferred_F 1
	       bool basic_type;
	       bool arithmetic_type;
	       bool integer_type;
	       bool composite;	/*an or type with at least 2 nonnull members*/
	       } type;
	  } body;
     };

struct ENV {
     node symbols;
     node decls;		/* held in reverse order */
     node tmpdecls;		/* held in reverse order */
     node before;		/* held in reverse order */
     node after;		/* held in order */
     node finals;		/* held in order */
     env previous;
     bool defun;
     bool disable_breaks;
     node rettype;		/* if it's a defun */
     bool loop;
     node break_loop_label;
     bool break_loop_label_used;
     node continue_loop_label;
     node make_index;
     node make_array;
     node make_array_len;
     node make_element_type;
     node make_break_label;
     bool make_used;
     node current_package;
     node signature;		/* kept in reverse order */
     int partial_definitions_active;
     node previous_partial_definition;
     };
void fatalpos(node, char *,...);
node totype(node);
void cprinttypevar(node, node);
bool isarraytype(node);
bool isobjecttype(node);
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
#define isstr(p)    (p!=NULL && (p)->tag == string_tag)
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

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
