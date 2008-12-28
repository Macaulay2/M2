/*		Copyright 1993 by Daniel R. Grayson		*/

extern node cleaners;
extern char *uniquify(char *);
extern char *totoken(char *);
extern void exportit(node, env);
extern char *prefixify(node, char *);
extern void checkfordeferredsymbols();
extern node String(char *);
extern void reinternsymbol(node, env);
extern int sequence(node);
extern node complete_symbol_list;
extern void internsymbol(node, env);
extern void printsymboltable();
extern void printstringlist();
extern node UniqueStringN(char *s, unsigned int len);

extern void init_dictionary(env v);
extern void setcprintvalue(node, node);
extern node lookupword(node, env);
extern node newtmp(node, env, bool);
extern node newstmp(node, env, bool);
extern chkfun getchkfun(node);
extern node newsymbol(node, node, env, int);

extern node type_T, keyword_T, int_T, double_T, long_T, short_T, 
       bool_T, char_T, symbol_T, package_T;
extern node float_T, uint_T, ulong_T, ushort_T, uchar_T, pointer_T;
extern node void_T, returned_T, undefined_T, null_T, deferred_T;

extern node getmem_S, sizeof_S, function_K, define_S, define_recursive_types_K,
   nop_K, return_K, return_S, if_S, ptr_S, brace_list_S, dot_S, memcpy_S,
   chked_K, blank_S, function_S, package_K, export_K, use_K, import_K,
   export_S, import_S, use_S, signature_K, signature_S, array_len_check_S,
   refs__S, len__S, define_destroy_S, take_S, array_check_S, lt_S, gt_S,
   part_S, object_type_S, nothing_K, new_K, provide_K, plus_S, break_K,
   array_take_S, length_K, funcall_S, infix_S, prefix_S, type__S, array__S,
   len__S, setd_K, if_K, foreach_K, le_S, plusplus_S, object_S, clean_S, kindof_K, 
   postfix_S, minus_S, for_K, ge_S, symbol_K, andand_K, oror_K,
   true_K, false_K, self_K, sizeof_K, sizeof_S, comma_S,
   Ccode_K, Ccode_S, dot_K, colon_K, colonequal_K, coloncolonequal_K, colon_S, colonequal_S,
   not_S, isnull_S, equal_S, when_K, until_K, while_K, space_S, or_S,
   label_S, keyword_K, release_S, releasec_S, reservec_S, reservenc_S,
   reserve_S, reserven_S, defun_S, equal_K, unequal_K, unequal_S, minusminus_S,
   declare_K, undefined_K, bad_K, void_K, returned_K, returnedThing_K, type_K, null_K, blockn_K, assign_S,
   cast_S, block1_K, or_K, object_K, array_K, block_K, float_K, package_S, open_fd_K,
   ushort_K, uint_K, ulong_K, uchar_K, oror_S, andand_S, pointer_K,
   str_S, double_K, int_K, goto_S, long_K, short_K, char_K, bool_K, tmp_S, zero, one;


/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
