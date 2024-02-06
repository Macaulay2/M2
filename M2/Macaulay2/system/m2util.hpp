#ifndef _system_m2util_h_
#define _system_m2util_h_

#include "mutexclass.hpp"

#include <M2/gc-include.h>

extern M2_string M2_tostring_3(const char *);
struct parse_Sequence_struct {unsigned short type_;int len;parse_Expr array[1];};

typedef struct parse_Sequence_struct * parse_Sequence;

extern parse_Sequence expr_emptySequence;

parse_Sequence allocSequence(int len)
{
  return (parse_Sequence)GC_MALLOC(sizeof(parse_Sequence_struct)+sizeof(parse_Expr)*(len-1));
}

#endif
