/* Copyright 1998-2000 by Daniel R. Grayson

   This file is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

#include <stdio.h>

#define try(x) switch (sizeof(x)) { \
  case 1: int8 = #x; break; \
  case 2: int16 = #x; break; \
  case 4: int32 = #x; break; \
  case 8: int64 = #x; break; \
  }

int main() {
  static char *int8, *int16, *int32, *int64;
  int x = 1;
  try(short);
#ifndef _WIN32
  try(long long);
#endif
  try(long);
  try(char);
  try(int);
  if (int8) printf("typedef %s int8;\n",int8), printf("typedef unsigned %s uint8;\n",int8);
  if (int16) printf("typedef %s int16;\n",int16), printf("typedef unsigned %s uint16;\n",int16);
  if (int32) printf("typedef %s int32;\n",int32), printf("typedef unsigned %s uint32;\n",int32);
  if (int64) printf("typedef %s int64;\n",int64), printf("typedef unsigned %s uint64;\n",int64);
  switch (sizeof(void *)) {
  case 1: printf("typedef int8 intp;\n"); break;
  case 2: printf("typedef int16 intp;\n"); break;
  case 4: printf("typedef int32 intp;\n"); break;
  case 8: printf("typedef int64 intp;\n"); break;
  }
  printf("#define LittleEndFirst %d\n", *(char *)&x == 1);
  printf("#define SIZEOF_POINTER %d\n", sizeof(char *));
  return 0;
}
