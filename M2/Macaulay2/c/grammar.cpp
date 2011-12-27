
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */


#include "scc.h"
#define YYSTYPE node
node parservalue;
static void yyerror(char *);
static int yylex(void);
#define end_of_input 0
#define NU NULL



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     NUMBER = 258,
     INTEGER = 259,
     IDENTIFIER = 260,
     STRINGCONST = 261,
     SELF = 262,
     OP = 263,
     SUPER = 264,
     RETURN = 265,
     PROVIDE = 266,
     whiledo = 267,
     ifte = 268,
     prefix1 = 269,
     infix1 = 270,
     infix1r = 271,
     EXPORT = 272,
     COLON = 273,
     prefix2 = 274,
     infix2 = 275,
     infix2r = 276,
     prefix3 = 277,
     infix3 = 278,
     infix3r = 279,
     OROR = 280,
     ANDAND = 281,
     OR = 282,
     prefix4 = 283,
     infix4 = 284,
     infix4r = 285,
     prefix5 = 286,
     infix5 = 287,
     infix5r = 288,
     prefix6 = 289,
     infix6 = 290,
     infix6r = 291,
     prefix7 = 292,
     infix7 = 293,
     infix7r = 294,
     SPACE = 295,
     prefix8 = 296,
     infix8 = 297,
     infix8r = 298,
     prefix9 = 299,
     infix9 = 300,
     infix9r = 301,
     prefix10 = 302,
     infix10 = 303,
     infix10r = 304,
     BRACEPLUS = 305,
     BREAK = 306,
     LEN = 307,
     NEW = 308,
     FUNCTION = 309,
     TO = 310,
     FROM = 311,
     BY = 312,
     IN = 313,
     AT = 314,
     SIGNATURE = 315,
     STRINGOP = 316,
     USE = 317,
     HEADER = 318,
     OPERATORPREFIX = 319,
     OPERATORRIGHT = 320,
     OPERATORLEFT = 321,
     PACKAGE = 322,
     DO = 323,
     WHILE = 324,
     FOREACH = 325,
     FOR = 326,
     IS = 327,
     WHEN = 328,
     ELSE = 329,
     THEN = 330,
     IF = 331
   };
#endif
/* Tokens.  */
#define NUMBER 258
#define INTEGER 259
#define IDENTIFIER 260
#define STRINGCONST 261
#define SELF 262
#define OP 263
#define SUPER 264
#define RETURN 265
#define PROVIDE 266
#define whiledo 267
#define ifte 268
#define prefix1 269
#define infix1 270
#define infix1r 271
#define EXPORT 272
#define COLON 273
#define prefix2 274
#define infix2 275
#define infix2r 276
#define prefix3 277
#define infix3 278
#define infix3r 279
#define OROR 280
#define ANDAND 281
#define OR 282
#define prefix4 283
#define infix4 284
#define infix4r 285
#define prefix5 286
#define infix5 287
#define infix5r 288
#define prefix6 289
#define infix6 290
#define infix6r 291
#define prefix7 292
#define infix7 293
#define infix7r 294
#define SPACE 295
#define prefix8 296
#define infix8 297
#define infix8r 298
#define prefix9 299
#define infix9 300
#define infix9r 301
#define prefix10 302
#define infix10 303
#define infix10r 304
#define BRACEPLUS 305
#define BREAK 306
#define LEN 307
#define NEW 308
#define FUNCTION 309
#define TO 310
#define FROM 311
#define BY 312
#define IN 313
#define AT 314
#define SIGNATURE 315
#define STRINGOP 316
#define USE 317
#define HEADER 318
#define OPERATORPREFIX 319
#define OPERATORRIGHT 320
#define OPERATORLEFT 321
#define PACKAGE 322
#define DO 323
#define WHILE 324
#define FOREACH 325
#define FOR 326
#define IS 327
#define WHEN 328
#define ELSE 329
#define THEN 330
#define IF 331




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */



#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  86
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3788

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  84
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  11
/* YYNRULES -- Number of rules.  */
#define YYNRULES  126
/* YYNRULES -- Number of states.  */
#define YYNSTATES  303

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   331

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      50,    11,     2,     2,    81,    36,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    10,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    82,     2,    83,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,     9,    12,    15,    16,    18,
      20,    23,    25,    28,    31,    34,    37,    38,    40,    42,
      46,    50,    57,    63,    65,    69,    73,    77,    81,    85,
      92,    97,   100,   104,   106,   111,   118,   127,   138,   147,
     152,   159,   168,   179,   183,   190,   199,   206,   213,   218,
     222,   226,   230,   234,   238,   242,   246,   250,   254,   258,
     262,   266,   270,   274,   278,   282,   286,   290,   294,   298,
     304,   310,   316,   322,   328,   334,   340,   346,   352,   358,
     364,   370,   376,   382,   388,   394,   400,   406,   412,   418,
     421,   425,   428,   431,   434,   437,   440,   443,   446,   449,
     452,   455,   458,   461,   465,   467,   472,   476,   480,   484,
     487,   490,   493,   497,   501,   505,   510,   516,   522,   528,
     533,   539,   545,   551,   553,   555,   557
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      85,     0,    -1,    86,    -1,    88,    -1,    87,    -1,    88,
       1,    -1,    87,     1,    -1,    -1,    89,    -1,    90,    -1,
      90,    10,    -1,    10,    -1,     1,    10,    -1,    94,    10,
      -1,    89,    10,    -1,    89,    94,    -1,    -1,    94,    -1,
      92,    -1,    94,    81,    94,    -1,    94,    81,    92,    -1,
      77,    94,    76,    94,    72,    94,    -1,    93,    76,    94,
      72,    94,    -1,    93,    -1,    93,    78,    94,    -1,    82,
      91,    83,    -1,    54,    91,    83,    -1,    94,    27,    94,
      -1,    94,    28,    94,    -1,    80,    94,    79,    94,    78,
      94,    -1,    80,    94,    79,    94,    -1,    13,    94,    -1,
      94,    29,    94,    -1,    55,    -1,    73,    94,    72,    94,
      -1,    74,    94,    62,    94,    72,    94,    -1,    74,    94,
      63,    94,    62,    94,    72,    94,    -1,    74,    94,    63,
      94,    62,    94,    61,    94,    72,    94,    -1,    74,    94,
      62,    94,    61,    94,    72,    94,    -1,    75,    94,    72,
      94,    -1,    75,    94,    59,    94,    72,    94,    -1,    75,
      94,    60,    94,    59,    94,    72,    94,    -1,    75,    94,
      60,    94,    59,    94,    61,    94,    72,    94,    -1,    94,
      20,    94,    -1,    58,    50,    91,    11,    20,    94,    -1,
      57,    94,    56,    94,    63,    94,    72,    94,    -1,    57,
      94,    56,    94,    72,    94,    -1,    57,    94,    63,    94,
      72,    94,    -1,    57,    94,    72,    94,    -1,    94,    52,
      94,    -1,    94,    48,    94,    -1,    94,    45,    94,    -1,
      94,    41,    94,    -1,    94,    38,    94,    -1,    94,    34,
      94,    -1,    94,    31,    94,    -1,    94,    25,    94,    -1,
      94,    22,    94,    -1,    94,    17,    94,    -1,    94,    53,
      94,    -1,    94,    49,    94,    -1,    94,    46,    94,    -1,
      94,    42,    94,    -1,    94,    39,    94,    -1,    94,    35,
      94,    -1,    94,    32,    94,    -1,    94,    26,    94,    -1,
      94,    23,    94,    -1,    94,    18,    94,    -1,    94,    52,
      50,    92,    11,    -1,    94,    48,    50,    92,    11,    -1,
      94,    45,    50,    92,    11,    -1,    94,    41,    50,    92,
      11,    -1,    94,    38,    50,    92,    11,    -1,    94,    34,
      50,    92,    11,    -1,    94,    31,    50,    92,    11,    -1,
      94,    25,    50,    92,    11,    -1,    94,    22,    50,    92,
      11,    -1,    94,    17,    50,    92,    11,    -1,    94,    53,
      50,    92,    11,    -1,    94,    49,    50,    92,    11,    -1,
      94,    46,    50,    92,    11,    -1,    94,    42,    50,    92,
      11,    -1,    94,    39,    50,    92,    11,    -1,    94,    35,
      50,    92,    11,    -1,    94,    32,    50,    92,    11,    -1,
      94,    26,    50,    92,    11,    -1,    94,    23,    50,    92,
      11,    -1,    94,    18,    50,    92,    11,    -1,    19,    94,
      -1,    94,    36,    94,    -1,    36,    94,    -1,    51,    94,
      -1,    47,    94,    -1,    44,    94,    -1,    40,    94,    -1,
      37,    94,    -1,    33,    94,    -1,    30,    94,    -1,    24,
      94,    -1,    21,    94,    -1,    16,    94,    -1,    12,    94,
      -1,    12,    50,    11,    -1,    12,    -1,    94,    50,    91,
      11,    -1,    50,    88,    11,    -1,    50,    87,    11,    -1,
      50,    94,    11,    -1,    66,     5,    -1,    65,     6,    -1,
      67,     6,    -1,    70,     4,     6,    -1,    69,     4,     6,
      -1,    68,     4,     6,    -1,    71,     5,    50,    11,    -1,
      71,     5,    50,    94,    11,    -1,    71,     5,    50,    88,
      11,    -1,    71,     5,    50,    87,    11,    -1,    64,     5,
      50,    11,    -1,    64,     5,    50,    94,    11,    -1,    64,
       5,    50,    88,    11,    -1,    64,     5,    50,    87,    11,
      -1,     6,    -1,     3,    -1,     4,    -1,     5,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    51,    51,    62,    63,    64,    65,    66,    69,    72,
      75,    76,    77,    78,    79,    82,    85,    86,    87,    90,
      91,    94,    95,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   111,   112,   113,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NUMBER", "INTEGER", "IDENTIFIER",
  "STRINGCONST", "SELF", "OP", "SUPER", "';'", "')'", "RETURN", "PROVIDE",
  "whiledo", "ifte", "prefix1", "infix1", "infix1r", "EXPORT", "COLON",
  "prefix2", "infix2", "infix2r", "prefix3", "infix3", "infix3r", "OROR",
  "ANDAND", "OR", "prefix4", "infix4", "infix4r", "prefix5", "infix5",
  "infix5r", "'-'", "prefix6", "infix6", "infix6r", "prefix7", "infix7",
  "infix7r", "SPACE", "prefix8", "infix8", "infix8r", "prefix9", "infix9",
  "infix9r", "'('", "prefix10", "infix10", "infix10r", "BRACEPLUS",
  "BREAK", "LEN", "NEW", "FUNCTION", "TO", "FROM", "BY", "IN", "AT",
  "SIGNATURE", "STRINGOP", "USE", "HEADER", "OPERATORPREFIX",
  "OPERATORRIGHT", "OPERATORLEFT", "PACKAGE", "DO", "WHILE", "FOREACH",
  "FOR", "IS", "WHEN", "ELSE", "THEN", "IF", "','", "'{'", "'}'",
  "$accept", "wrappedprogram", "program", "exprlistsemi", "exprlist",
  "reverseexprlistsemi", "reverseexprlist", "arglistornull", "arglist",
  "typecasen", "expr", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
      59,    41,   265,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,   278,   279,   280,   281,   282,
     283,   284,   285,   286,   287,   288,    45,   289,   290,   291,
     292,   293,   294,   295,   296,   297,   298,   299,   300,   301,
      40,   302,   303,   304,   305,   306,   307,   308,   309,   310,
     311,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,   324,   325,   326,   327,   328,   329,   330,
     331,    44,   123,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    84,    85,    86,    86,    86,    86,    86,    87,    88,
      89,    89,    89,    89,    89,    90,    91,    91,    91,    92,
      92,    93,    93,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     1,     2,     2,     0,     1,     1,
       2,     1,     2,     2,     2,     2,     0,     1,     1,     3,
       3,     6,     5,     1,     3,     3,     3,     3,     3,     6,
       4,     2,     3,     1,     4,     6,     8,    10,     8,     4,
       6,     8,    10,     3,     6,     8,     6,     6,     4,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     5,
       5,     5,     5,     5,     5,     5,     5,     5,     5,     5,
       5,     5,     5,     5,     5,     5,     5,     5,     5,     2,
       3,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     1,     4,     3,     3,     3,     2,
       2,     2,     3,     3,     3,     4,     5,     5,     5,     4,
       5,     5,     5,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,   124,   125,   126,   123,    11,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    16,    33,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    16,
       0,     2,     0,     0,     8,     9,    23,     0,    12,     0,
     102,    31,   101,    89,   100,    99,    98,    97,    91,    96,
      95,    94,    93,     0,     0,     0,    92,     0,    18,    17,
       0,    16,     0,   110,   109,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     1,     6,     5,    14,
      15,    10,     0,     0,    13,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    16,     0,
       0,   103,   107,   106,   108,    26,     0,     0,     0,     0,
       0,     0,   114,   113,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    25,     0,    24,     0,    58,     0,
      68,    43,     0,    57,     0,    67,     0,    56,     0,    66,
      27,    28,    32,     0,    55,     0,    65,     0,    54,     0,
      64,    90,     0,    53,     0,    63,     0,    52,     0,    62,
       0,    51,     0,    61,     0,    50,     0,    60,     0,     0,
      49,     0,    59,    20,    19,     0,     0,    48,     0,   119,
       0,     0,     0,   115,     0,     0,     0,    34,     0,     0,
       0,     0,    39,     0,    30,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,     0,   122,   121,   120,   118,   117,   116,     0,     0,
       0,     0,     0,     0,     0,    22,    78,    88,    77,    87,
      76,    86,    75,    85,    74,    84,    73,    83,    72,    82,
      71,    81,    70,    80,    69,    79,     0,    46,    47,    44,
       0,    35,     0,    40,     0,    21,    29,     0,     0,     0,
       0,     0,     0,    45,    38,     0,    36,     0,    41,     0,
       0,    37,    42
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    40,    41,    63,    64,    44,    45,    67,    68,    46,
     217
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -51
static const yytype_int16 yypact[] =
{
     301,    -6,   -51,   -51,   -51,   -51,   -51,   781,   861,   861,
     861,   861,   861,   861,   861,   861,   861,   861,   861,   861,
     621,   861,   861,   -51,   861,   -23,    35,    36,    38,    39,
      37,    42,    43,    49,   861,   861,   861,   861,   861,   861,
      48,   -51,    29,    31,   701,    47,   -50,  3542,   -51,   381,
    3617,  3617,  3653,  3687,   232,    24,   294,   374,    33,   454,
      33,    98,   -47,    50,    53,  3417,    14,   -15,   -51,  2565,
    2718,   861,    34,   -51,   -51,   -51,    81,    82,    83,    40,
    2973,  3324,  2760,  2679,  2602,     8,   -51,   -51,   -51,   -51,
    3617,   -51,   861,   861,   -51,   941,  1021,   861,  1101,  1181,
    1261,  1341,   861,   861,   861,  1421,  1501,  1581,  1661,   861,
    1741,  1821,  1901,  1981,  2061,  2141,  2221,  2301,   861,  2381,
    2461,   -51,   -51,   -51,   -51,   -51,   861,   861,   861,   861,
     110,   461,   -51,   -51,   -51,   541,   861,   861,   861,   861,
     861,   861,   861,   861,   -51,  3012,  3617,   621,  3653,   621,
    3653,  3687,   621,   232,   621,   232,   621,    24,   621,    24,
     178,  3712,  3735,   621,   294,   621,   294,   621,   374,   621,
     374,   454,   621,   454,   621,   454,   621,    33,   621,    33,
     621,    98,   621,    98,   621,   -47,   621,   -47,   111,   621,
      14,   621,    14,   -51,  2565,  2808,  3051,  3617,    74,   -51,
     112,   113,  3461,   -51,   114,   119,  3505,  3617,  2847,  3371,
    3090,  3579,  3617,  3129,  2641,   861,   121,   151,   122,   123,
     134,   138,   141,   142,   144,   146,   148,   152,   153,   154,
     155,   156,   159,   161,   164,   -51,   170,   173,   861,   861,
     861,   861,   -51,   -51,   -51,   -51,   -51,   -51,   861,   861,
     861,   861,   861,   861,   861,  3617,   -51,   -51,   -51,   -51,
     -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,
     -51,   -51,   -51,   -51,   -51,   -51,  3168,  3617,  3617,  3687,
    3207,  3617,  2889,  3617,  2931,  3617,  3617,   861,   861,   861,
     861,   861,   861,  3617,  3617,  3246,  3617,  3285,  3617,   861,
     861,  3617,  3617
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -51,   -51,   -51,    23,    25,   -51,   -51,   -38,  2395,   -51,
       0
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -8
static const yytype_int16 yytable[] =
{
      47,    85,   117,   118,    48,   119,   120,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      65,    66,    69,    42,    70,    43,    92,    71,    93,    -4,
      87,    -3,    88,   130,    80,    81,    82,    83,    84,    69,
      72,    76,    73,    74,    90,    75,    77,    78,    86,    65,
     101,   102,   103,   104,    79,   105,   106,    91,   107,   108,
     109,   122,   110,   111,   123,   112,   113,   120,   125,   114,
     115,    69,   116,   117,   118,   113,   119,   120,   114,   115,
     188,   116,   117,   118,   131,   119,   120,   132,   133,   134,
     135,   144,   145,   146,   241,   148,   150,   151,   153,   155,
     157,   159,   160,   161,   162,   164,   166,   168,   170,   171,
     173,   175,   177,   179,   181,   183,   185,   187,    69,   190,
     192,   198,   235,   242,   243,   245,   194,   195,   196,   197,
     246,   202,   256,   257,   258,   206,   207,   208,   209,   210,
     211,   212,   213,   214,   115,   259,   116,   117,   118,   260,
     119,   120,   261,   262,   200,   263,   201,   264,   204,   265,
     205,    94,   124,   266,   267,   268,   269,   270,    95,    96,
     271,    97,   272,    98,    99,   273,   100,   101,   102,   103,
     104,   274,   105,   106,   275,   107,   108,   109,     0,   110,
     111,     0,   112,   113,     0,     0,   114,   115,     0,   116,
     117,   118,     0,   119,   120,     0,   103,   104,     0,   105,
     106,     0,   107,   108,   109,   255,   110,   111,     0,   112,
     113,     0,     0,   114,   115,     0,   116,   117,   118,     0,
     119,   120,   126,     0,     0,     0,     0,     0,   276,   277,
     278,   279,     0,     0,     0,     0,     0,     0,   280,   281,
     282,   283,   284,   285,   286,    99,     0,   100,   101,   102,
     103,   104,     0,   105,   106,     0,   107,   108,   109,     0,
     110,   111,     0,   112,   113,     0,     0,   114,   115,     0,
     116,   117,   118,     0,   119,   120,     0,   293,   294,   295,
     296,   297,   298,     0,     0,     0,     0,     0,     0,   301,
     302,    -7,     1,     0,     2,     3,     4,     5,     0,     0,
       0,     6,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,   106,     0,   107,   108,
     109,    13,   110,   111,    14,   112,   113,    15,    16,   114,
     115,    17,   116,   117,   118,    18,   119,   120,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     1,    39,     2,     3,     4,     5,     0,     0,
       0,     6,   121,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,   108,
     109,    13,   110,   111,    14,   112,   113,    15,    16,   114,
     115,    17,   116,   117,   118,    18,   119,   120,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     1,    39,     2,     3,     4,     5,     0,     0,
       0,     6,   199,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,   111,    14,   112,   113,    15,    16,   114,
     115,    17,   116,   117,   118,    18,   119,   120,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     1,    39,     2,     3,     4,     5,     0,     0,
       0,     6,   203,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     1,    39,     2,     3,     4,     5,     0,     0,
       0,     6,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,    89,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,    49,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,    20,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   147,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   149,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   152,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   154,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   156,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   158,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   163,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   165,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   167,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   169,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   172,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   174,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   176,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   178,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   180,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   182,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   184,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   186,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   189,    21,     0,     0,    22,    23,     0,    24,    25,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,     0,    39,     2,     3,     4,     5,     0,     0,
       0,     0,     0,     7,     8,     0,     0,     9,     0,     0,
      10,     0,    11,     0,     0,    12,     0,     0,     0,     0,
       0,    13,     0,     0,    14,     0,     0,    15,    16,     0,
       0,    17,     0,     0,     0,    18,     0,     0,    19,     0,
       0,   191,    21,     0,     0,    22,    23,     0,    24,    25,
       0,   193,     0,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,     0,    34,    35,    36,     0,    37,     0,
       0,    38,   216,    39,   218,     0,     0,   219,     0,   220,
       0,   221,     0,   222,     0,     0,     0,     0,   223,     0,
     224,     0,   225,     0,   226,     0,     0,   227,     0,   228,
       0,   229,     0,   230,     0,   231,     0,   232,     0,   233,
       0,   234,    95,    96,   236,    97,   237,    98,    99,     0,
     100,   101,   102,   103,   104,     0,   105,   106,     0,   107,
     108,   109,     0,   110,   111,     0,   112,   113,     0,     0,
     114,   115,     0,   116,   117,   118,     0,   119,   120,    95,
      96,     0,    97,     0,    98,    99,     0,   100,   101,   102,
     103,   104,     0,   105,   106,     0,   107,   108,   109,     0,
     110,   111,     0,   112,   113,     0,   126,   114,   115,     0,
     116,   117,   118,     0,   119,   120,     0,     0,    95,    96,
       0,    97,     0,    98,    99,     0,   100,   101,   102,   103,
     104,     0,   105,   106,     0,   107,   108,   109,     0,   110,
     111,   143,   112,   113,     0,     0,   114,   115,     0,   116,
     117,   118,     0,   119,   120,     0,    95,    96,     0,    97,
       0,    98,    99,     0,   100,   101,   102,   103,   104,     0,
     105,   106,     0,   107,   108,   109,     0,   110,   111,   254,
     112,   113,     0,     0,   114,   115,     0,   116,   117,   118,
       0,   119,   120,     0,     0,    95,    96,     0,    97,     0,
      98,    99,     0,   100,   101,   102,   103,   104,     0,   105,
     106,     0,   107,   108,   109,   142,   110,   111,     0,   112,
     113,     0,     0,   114,   115,     0,   116,   117,   118,     0,
     119,   120,     0,     0,   127,     0,     0,    95,    96,     0,
      97,   128,    98,    99,     0,   100,   101,   102,   103,   104,
     129,   105,   106,     0,   107,   108,   109,     0,   110,   111,
       0,   112,   113,     0,     0,   114,   115,     0,   116,   117,
     118,     0,   119,   120,     0,     0,     0,     0,     0,   139,
     140,     0,     0,     0,     0,    95,    96,     0,    97,     0,
      98,    99,   141,   100,   101,   102,   103,   104,     0,   105,
     106,     0,   107,   108,   109,     0,   110,   111,     0,   112,
     113,     0,     0,   114,   115,     0,   116,   117,   118,     0,
     119,   120,     0,     0,    95,    96,     0,    97,     0,    98,
      99,   238,   100,   101,   102,   103,   104,     0,   105,   106,
     239,   107,   108,   109,     0,   110,   111,     0,   112,   113,
       0,     0,   114,   115,     0,   116,   117,   118,     0,   119,
     120,     0,     0,     0,     0,     0,    95,    96,   248,    97,
       0,    98,    99,     0,   100,   101,   102,   103,   104,   249,
     105,   106,     0,   107,   108,   109,     0,   110,   111,     0,
     112,   113,     0,     0,   114,   115,     0,   116,   117,   118,
       0,   119,   120,     0,     0,     0,     0,     0,    95,    96,
     289,    97,     0,    98,    99,     0,   100,   101,   102,   103,
     104,   290,   105,   106,     0,   107,   108,   109,     0,   110,
     111,     0,   112,   113,     0,     0,   114,   115,     0,   116,
     117,   118,     0,   119,   120,     0,     0,     0,     0,     0,
      95,    96,   291,    97,     0,    98,    99,     0,   100,   101,
     102,   103,   104,   292,   105,   106,     0,   107,   108,   109,
       0,   110,   111,     0,   112,   113,     0,     0,   114,   115,
       0,   116,   117,   118,     0,   119,   120,     0,     0,    95,
      96,     0,    97,     0,    98,    99,     0,   100,   101,   102,
     103,   104,     0,   105,   106,   136,   107,   108,   109,     0,
     110,   111,     0,   112,   113,     0,     0,   114,   115,     0,
     116,   117,   118,     0,   119,   120,     0,     0,    95,    96,
       0,    97,     0,    98,    99,     0,   100,   101,   102,   103,
     104,     0,   105,   106,   215,   107,   108,   109,     0,   110,
     111,     0,   112,   113,     0,     0,   114,   115,     0,   116,
     117,   118,     0,   119,   120,     0,     0,    95,    96,     0,
      97,     0,    98,    99,     0,   100,   101,   102,   103,   104,
       0,   105,   106,   240,   107,   108,   109,     0,   110,   111,
       0,   112,   113,     0,     0,   114,   115,     0,   116,   117,
     118,     0,   119,   120,     0,     0,    95,    96,     0,    97,
       0,    98,    99,     0,   100,   101,   102,   103,   104,     0,
     105,   106,   251,   107,   108,   109,     0,   110,   111,     0,
     112,   113,     0,     0,   114,   115,     0,   116,   117,   118,
       0,   119,   120,     0,     0,    95,    96,     0,    97,     0,
      98,    99,     0,   100,   101,   102,   103,   104,     0,   105,
     106,   253,   107,   108,   109,     0,   110,   111,     0,   112,
     113,     0,     0,   114,   115,     0,   116,   117,   118,     0,
     119,   120,     0,     0,    95,    96,     0,    97,     0,    98,
      99,     0,   100,   101,   102,   103,   104,     0,   105,   106,
     287,   107,   108,   109,     0,   110,   111,     0,   112,   113,
       0,     0,   114,   115,     0,   116,   117,   118,     0,   119,
     120,     0,     0,    95,    96,     0,    97,     0,    98,    99,
       0,   100,   101,   102,   103,   104,     0,   105,   106,   288,
     107,   108,   109,     0,   110,   111,     0,   112,   113,     0,
       0,   114,   115,     0,   116,   117,   118,     0,   119,   120,
       0,     0,    95,    96,     0,    97,     0,    98,    99,     0,
     100,   101,   102,   103,   104,     0,   105,   106,   299,   107,
     108,   109,     0,   110,   111,     0,   112,   113,     0,     0,
     114,   115,     0,   116,   117,   118,     0,   119,   120,     0,
       0,    95,    96,     0,    97,     0,    98,    99,     0,   100,
     101,   102,   103,   104,     0,   105,   106,   300,   107,   108,
     109,     0,   110,   111,     0,   112,   113,     0,     0,   114,
     115,     0,   116,   117,   118,     0,   119,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   137,   138,    95,    96,
       0,    97,     0,    98,    99,     0,   100,   101,   102,   103,
     104,     0,   105,   106,     0,   107,   108,   109,     0,   110,
     111,     0,   112,   113,     0,     0,   114,   115,     0,   116,
     117,   118,     0,   119,   120,     0,     0,    94,   124,     0,
       0,     0,     0,   250,    95,    96,     0,    97,     0,    98,
      99,     0,   100,   101,   102,   103,   104,     0,   105,   106,
       0,   107,   108,   109,     0,   110,   111,     0,   112,   113,
       0,     0,   114,   115,     0,   116,   117,   118,     0,   119,
     120,    94,   244,     0,     0,     0,     0,     0,    95,    96,
       0,    97,     0,    98,    99,     0,   100,   101,   102,   103,
     104,     0,   105,   106,     0,   107,   108,   109,     0,   110,
     111,     0,   112,   113,     0,     0,   114,   115,     0,   116,
     117,   118,     0,   119,   120,    94,   247,     0,     0,     0,
       0,     0,    95,    96,     0,    97,     0,    98,    99,     0,
     100,   101,   102,   103,   104,     0,   105,   106,     0,   107,
     108,   109,     0,   110,   111,     0,   112,   113,     0,     0,
     114,   115,    94,   116,   117,   118,     0,   119,   120,    95,
      96,     0,    97,     0,    98,    99,     0,   100,   101,   102,
     103,   104,     0,   105,   106,     0,   107,   108,   109,     0,
     110,   111,     0,   112,   113,     0,     0,   114,   115,     0,
     116,   117,   118,     0,   119,   120,    95,    96,     0,    97,
       0,    98,    99,     0,   100,   101,   102,   103,   104,     0,
     105,   106,     0,   107,   108,   109,     0,   110,   111,     0,
     112,   113,     0,     0,   114,   115,     0,   116,   117,   118,
       0,   119,   120,     0,    95,    96,     0,    97,   252,    98,
      99,     0,   100,   101,   102,   103,   104,     0,   105,   106,
       0,   107,   108,   109,     0,   110,   111,     0,   112,   113,
       0,     0,   114,   115,     0,   116,   117,   118,     0,   119,
     120,    96,     0,    97,     0,    98,    99,     0,   100,   101,
     102,   103,   104,     0,   105,   106,     0,   107,   108,   109,
       0,   110,   111,     0,   112,   113,     0,     0,   114,   115,
       0,   116,   117,   118,     0,   119,   120,    97,     0,    98,
      99,     0,   100,   101,   102,   103,   104,     0,   105,   106,
       0,   107,   108,   109,     0,   110,   111,     0,   112,   113,
       0,     0,   114,   115,     0,   116,   117,   118,     0,   119,
     120,   104,     0,   105,   106,     0,   107,   108,   109,     0,
     110,   111,     0,   112,   113,     0,     0,   114,   115,     0,
     116,   117,   118,     0,   119,   120,   105,   106,     0,   107,
     108,   109,     0,   110,   111,     0,   112,   113,     0,     0,
     114,   115,     0,   116,   117,   118,     0,   119,   120
};

static const yytype_int16 yycheck[] =
{
       0,    39,    49,    50,    10,    52,    53,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,     0,    24,     0,    76,    50,    78,     0,
       1,     0,     1,    71,    34,    35,    36,    37,    38,    39,
       5,     4,     6,     5,    44,     6,     4,     4,     0,    49,
      26,    27,    28,    29,     5,    31,    32,    10,    34,    35,
      36,    11,    38,    39,    11,    41,    42,    53,    83,    45,
      46,    71,    48,    49,    50,    42,    52,    53,    45,    46,
     118,    48,    49,    50,    50,    52,    53,     6,     6,     6,
      50,    83,    92,    93,    20,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    11,    11,    11,    11,    11,   126,   127,   128,   129,
      11,   131,    11,    11,    11,   135,   136,   137,   138,   139,
     140,   141,   142,   143,    46,    11,    48,    49,    50,    11,
      52,    53,    11,    11,   131,    11,   131,    11,   135,    11,
     135,    10,    11,    11,    11,    11,    11,    11,    17,    18,
      11,    20,    11,    22,    23,    11,    25,    26,    27,    28,
      29,    11,    31,    32,    11,    34,    35,    36,    -1,    38,
      39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    -1,    52,    53,    -1,    28,    29,    -1,    31,
      32,    -1,    34,    35,    36,   215,    38,    39,    -1,    41,
      42,    -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,
      52,    53,    81,    -1,    -1,    -1,    -1,    -1,   238,   239,
     240,   241,    -1,    -1,    -1,    -1,    -1,    -1,   248,   249,
     250,   251,   252,   253,   254,    23,    -1,    25,    26,    27,
      28,    29,    -1,    31,    32,    -1,    34,    35,    36,    -1,
      38,    39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,
      48,    49,    50,    -1,    52,    53,    -1,   287,   288,   289,
     290,   291,   292,    -1,    -1,    -1,    -1,    -1,    -1,   299,
     300,     0,     1,    -1,     3,     4,     5,     6,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    32,    -1,    34,    35,
      36,    30,    38,    39,    33,    41,    42,    36,    37,    45,
      46,    40,    48,    49,    50,    44,    52,    53,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,     1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    10,    11,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    35,
      36,    30,    38,    39,    33,    41,    42,    36,    37,    45,
      46,    40,    48,    49,    50,    44,    52,    53,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,     1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    10,    11,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    39,    33,    41,    42,    36,    37,    45,
      46,    40,    48,    49,    50,    44,    52,    53,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,     1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    10,    11,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,     1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,    -1,    82,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    12,    13,    -1,    -1,    16,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    -1,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    -1,    57,    58,
      -1,   126,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    73,    74,    75,    -1,    77,    -1,
      -1,    80,   147,    82,   149,    -1,    -1,   152,    -1,   154,
      -1,   156,    -1,   158,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,   167,    -1,   169,    -1,    -1,   172,    -1,   174,
      -1,   176,    -1,   178,    -1,   180,    -1,   182,    -1,   184,
      -1,   186,    17,    18,   189,    20,   191,    22,    23,    -1,
      25,    26,    27,    28,    29,    -1,    31,    32,    -1,    34,
      35,    36,    -1,    38,    39,    -1,    41,    42,    -1,    -1,
      45,    46,    -1,    48,    49,    50,    -1,    52,    53,    17,
      18,    -1,    20,    -1,    22,    23,    -1,    25,    26,    27,
      28,    29,    -1,    31,    32,    -1,    34,    35,    36,    -1,
      38,    39,    -1,    41,    42,    -1,    81,    45,    46,    -1,
      48,    49,    50,    -1,    52,    53,    -1,    -1,    17,    18,
      -1,    20,    -1,    22,    23,    -1,    25,    26,    27,    28,
      29,    -1,    31,    32,    -1,    34,    35,    36,    -1,    38,
      39,    79,    41,    42,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    -1,    52,    53,    -1,    17,    18,    -1,    20,
      -1,    22,    23,    -1,    25,    26,    27,    28,    29,    -1,
      31,    32,    -1,    34,    35,    36,    -1,    38,    39,    78,
      41,    42,    -1,    -1,    45,    46,    -1,    48,    49,    50,
      -1,    52,    53,    -1,    -1,    17,    18,    -1,    20,    -1,
      22,    23,    -1,    25,    26,    27,    28,    29,    -1,    31,
      32,    -1,    34,    35,    36,    76,    38,    39,    -1,    41,
      42,    -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,
      52,    53,    -1,    -1,    56,    -1,    -1,    17,    18,    -1,
      20,    63,    22,    23,    -1,    25,    26,    27,    28,    29,
      72,    31,    32,    -1,    34,    35,    36,    -1,    38,    39,
      -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,    49,
      50,    -1,    52,    53,    -1,    -1,    -1,    -1,    -1,    59,
      60,    -1,    -1,    -1,    -1,    17,    18,    -1,    20,    -1,
      22,    23,    72,    25,    26,    27,    28,    29,    -1,    31,
      32,    -1,    34,    35,    36,    -1,    38,    39,    -1,    41,
      42,    -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,
      52,    53,    -1,    -1,    17,    18,    -1,    20,    -1,    22,
      23,    63,    25,    26,    27,    28,    29,    -1,    31,    32,
      72,    34,    35,    36,    -1,    38,    39,    -1,    41,    42,
      -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,    52,
      53,    -1,    -1,    -1,    -1,    -1,    17,    18,    61,    20,
      -1,    22,    23,    -1,    25,    26,    27,    28,    29,    72,
      31,    32,    -1,    34,    35,    36,    -1,    38,    39,    -1,
      41,    42,    -1,    -1,    45,    46,    -1,    48,    49,    50,
      -1,    52,    53,    -1,    -1,    -1,    -1,    -1,    17,    18,
      61,    20,    -1,    22,    23,    -1,    25,    26,    27,    28,
      29,    72,    31,    32,    -1,    34,    35,    36,    -1,    38,
      39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    -1,    52,    53,    -1,    -1,    -1,    -1,    -1,
      17,    18,    61,    20,    -1,    22,    23,    -1,    25,    26,
      27,    28,    29,    72,    31,    32,    -1,    34,    35,    36,
      -1,    38,    39,    -1,    41,    42,    -1,    -1,    45,    46,
      -1,    48,    49,    50,    -1,    52,    53,    -1,    -1,    17,
      18,    -1,    20,    -1,    22,    23,    -1,    25,    26,    27,
      28,    29,    -1,    31,    32,    72,    34,    35,    36,    -1,
      38,    39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,
      48,    49,    50,    -1,    52,    53,    -1,    -1,    17,    18,
      -1,    20,    -1,    22,    23,    -1,    25,    26,    27,    28,
      29,    -1,    31,    32,    72,    34,    35,    36,    -1,    38,
      39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    -1,    52,    53,    -1,    -1,    17,    18,    -1,
      20,    -1,    22,    23,    -1,    25,    26,    27,    28,    29,
      -1,    31,    32,    72,    34,    35,    36,    -1,    38,    39,
      -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,    49,
      50,    -1,    52,    53,    -1,    -1,    17,    18,    -1,    20,
      -1,    22,    23,    -1,    25,    26,    27,    28,    29,    -1,
      31,    32,    72,    34,    35,    36,    -1,    38,    39,    -1,
      41,    42,    -1,    -1,    45,    46,    -1,    48,    49,    50,
      -1,    52,    53,    -1,    -1,    17,    18,    -1,    20,    -1,
      22,    23,    -1,    25,    26,    27,    28,    29,    -1,    31,
      32,    72,    34,    35,    36,    -1,    38,    39,    -1,    41,
      42,    -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,
      52,    53,    -1,    -1,    17,    18,    -1,    20,    -1,    22,
      23,    -1,    25,    26,    27,    28,    29,    -1,    31,    32,
      72,    34,    35,    36,    -1,    38,    39,    -1,    41,    42,
      -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,    52,
      53,    -1,    -1,    17,    18,    -1,    20,    -1,    22,    23,
      -1,    25,    26,    27,    28,    29,    -1,    31,    32,    72,
      34,    35,    36,    -1,    38,    39,    -1,    41,    42,    -1,
      -1,    45,    46,    -1,    48,    49,    50,    -1,    52,    53,
      -1,    -1,    17,    18,    -1,    20,    -1,    22,    23,    -1,
      25,    26,    27,    28,    29,    -1,    31,    32,    72,    34,
      35,    36,    -1,    38,    39,    -1,    41,    42,    -1,    -1,
      45,    46,    -1,    48,    49,    50,    -1,    52,    53,    -1,
      -1,    17,    18,    -1,    20,    -1,    22,    23,    -1,    25,
      26,    27,    28,    29,    -1,    31,    32,    72,    34,    35,
      36,    -1,    38,    39,    -1,    41,    42,    -1,    -1,    45,
      46,    -1,    48,    49,    50,    -1,    52,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    62,    63,    17,    18,
      -1,    20,    -1,    22,    23,    -1,    25,    26,    27,    28,
      29,    -1,    31,    32,    -1,    34,    35,    36,    -1,    38,
      39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    -1,    52,    53,    -1,    -1,    10,    11,    -1,
      -1,    -1,    -1,    62,    17,    18,    -1,    20,    -1,    22,
      23,    -1,    25,    26,    27,    28,    29,    -1,    31,    32,
      -1,    34,    35,    36,    -1,    38,    39,    -1,    41,    42,
      -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,    52,
      53,    10,    11,    -1,    -1,    -1,    -1,    -1,    17,    18,
      -1,    20,    -1,    22,    23,    -1,    25,    26,    27,    28,
      29,    -1,    31,    32,    -1,    34,    35,    36,    -1,    38,
      39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    -1,    52,    53,    10,    11,    -1,    -1,    -1,
      -1,    -1,    17,    18,    -1,    20,    -1,    22,    23,    -1,
      25,    26,    27,    28,    29,    -1,    31,    32,    -1,    34,
      35,    36,    -1,    38,    39,    -1,    41,    42,    -1,    -1,
      45,    46,    10,    48,    49,    50,    -1,    52,    53,    17,
      18,    -1,    20,    -1,    22,    23,    -1,    25,    26,    27,
      28,    29,    -1,    31,    32,    -1,    34,    35,    36,    -1,
      38,    39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,
      48,    49,    50,    -1,    52,    53,    17,    18,    -1,    20,
      -1,    22,    23,    -1,    25,    26,    27,    28,    29,    -1,
      31,    32,    -1,    34,    35,    36,    -1,    38,    39,    -1,
      41,    42,    -1,    -1,    45,    46,    -1,    48,    49,    50,
      -1,    52,    53,    -1,    17,    18,    -1,    20,    59,    22,
      23,    -1,    25,    26,    27,    28,    29,    -1,    31,    32,
      -1,    34,    35,    36,    -1,    38,    39,    -1,    41,    42,
      -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,    52,
      53,    18,    -1,    20,    -1,    22,    23,    -1,    25,    26,
      27,    28,    29,    -1,    31,    32,    -1,    34,    35,    36,
      -1,    38,    39,    -1,    41,    42,    -1,    -1,    45,    46,
      -1,    48,    49,    50,    -1,    52,    53,    20,    -1,    22,
      23,    -1,    25,    26,    27,    28,    29,    -1,    31,    32,
      -1,    34,    35,    36,    -1,    38,    39,    -1,    41,    42,
      -1,    -1,    45,    46,    -1,    48,    49,    50,    -1,    52,
      53,    29,    -1,    31,    32,    -1,    34,    35,    36,    -1,
      38,    39,    -1,    41,    42,    -1,    -1,    45,    46,    -1,
      48,    49,    50,    -1,    52,    53,    31,    32,    -1,    34,
      35,    36,    -1,    38,    39,    -1,    41,    42,    -1,    -1,
      45,    46,    -1,    48,    49,    50,    -1,    52,    53
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,     6,    10,    12,    13,    16,
      19,    21,    24,    30,    33,    36,    37,    40,    44,    47,
      50,    51,    54,    55,    57,    58,    64,    65,    66,    67,
      68,    69,    70,    71,    73,    74,    75,    77,    80,    82,
      85,    86,    87,    88,    89,    90,    93,    94,    10,    50,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    87,    88,    94,    94,    91,    92,    94,
      94,    50,     5,     6,     5,     6,     4,     4,     4,     5,
      94,    94,    94,    94,    94,    91,     0,     1,     1,    10,
      94,    10,    76,    78,    10,    17,    18,    20,    22,    23,
      25,    26,    27,    28,    29,    31,    32,    34,    35,    36,
      38,    39,    41,    42,    45,    46,    48,    49,    50,    52,
      53,    11,    11,    11,    11,    83,    81,    56,    63,    72,
      91,    50,     6,     6,     6,    50,    72,    62,    63,    59,
      60,    72,    76,    79,    83,    94,    94,    50,    94,    50,
      94,    94,    50,    94,    50,    94,    50,    94,    50,    94,
      94,    94,    94,    50,    94,    50,    94,    50,    94,    50,
      94,    94,    50,    94,    50,    94,    50,    94,    50,    94,
      50,    94,    50,    94,    50,    94,    50,    94,    91,    50,
      94,    50,    94,    92,    94,    94,    94,    94,    11,    11,
      87,    88,    94,    11,    87,    88,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    72,    92,    94,    92,    92,
      92,    92,    92,    92,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    92,    11,    92,    92,    63,    72,
      72,    20,    11,    11,    11,    11,    11,    11,    61,    72,
      62,    72,    59,    72,    78,    94,    11,    11,    11,    11,
      11,    11,    11,    11,    11,    11,    11,    11,    11,    11,
      11,    11,    11,    11,    11,    11,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    72,    72,    61,
      72,    61,    72,    94,    94,    94,    94,    94,    94,    72,
      72,    94,    94
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

    {
	  if (cur.wrapit) {
	       (yyval) = list(3,package_K,
	       	    positionof(UniqueString(newsuffixbase(BaseName(cur.filename),""))),
	       	    cons(block__K,(yyvsp[(1) - (1)])));
	       }
	  parservalue = (yyval);
	  }
    break;

  case 3:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 4:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 5:

    { (yyval) = (yyvsp[(1) - (2)]); yyerrok; yyclearin; }
    break;

  case 6:

    { (yyval) = (yyvsp[(1) - (2)]); yyerrok; yyclearin; }
    break;

  case 7:

    { (yyval) = NU; }
    break;

  case 8:

    { (yyval) = reverse((yyvsp[(1) - (1)])); }
    break;

  case 9:

    { (yyval) = reverse((yyvsp[(1) - (1)])); }
    break;

  case 10:

    { (yyval) = (yyvsp[(1) - (2)]); }
    break;

  case 11:

    { (yyval) = NU; }
    break;

  case 12:

    { (yyval) = NU; yyerrok; }
    break;

  case 13:

    { (yyval) = cons((yyvsp[(1) - (2)]),NU); }
    break;

  case 14:

    { (yyval) = (yyvsp[(1) - (2)]); }
    break;

  case 15:

    { (yyval) = cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])); }
    break;

  case 16:

    { (yyval) = NU; }
    break;

  case 17:

    { (yyval) = list(1,(yyvsp[(1) - (1)])); }
    break;

  case 19:

    { (yyval) = list(2,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 20:

    { (yyval) = cons((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 21:

    { (yyval) = list(3,list(2,(yyvsp[(4) - (6)]),(yyvsp[(6) - (6)])),(yyvsp[(2) - (6)]),(yyvsp[(1) - (6)])); }
    break;

  case 22:

    { (yyval) = cons(list(2,(yyvsp[(3) - (5)]),(yyvsp[(5) - (5)])),(yyvsp[(1) - (5)])); }
    break;

  case 23:

    { (yyval) = reverse((yyvsp[(1) - (1)])); }
    break;

  case 24:

    { (yyval) = reverse(cons(list(1,(yyvsp[(3) - (3)])),(yyvsp[(1) - (3)]))); }
    break;

  case 25:

    { (yyval) = cons(object__K,(yyvsp[(2) - (3)])); }
    break;

  case 26:

    { (yyval) = cons(tagged_object_K,(yyvsp[(2) - (3)])); }
    break;

  case 27:

    { (yyval) = list(3,oror_K,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 28:

    { (yyval) = list(3,andand_K,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 29:

    { (yyval) = list(4,(yyvsp[(1) - (6)]),(yyvsp[(2) - (6)]),(yyvsp[(4) - (6)]),(yyvsp[(6) - (6)])); }
    break;

  case 30:

    { (yyval) = list(3,(yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])); }
    break;

  case 31:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 32:

    {
	  if (iscons((yyvsp[(1) - (3)])) && equal(CAR((yyvsp[(1) - (3)])),or_S))
	       (yyval) = join((yyvsp[(1) - (3)]),list(1,(yyvsp[(3) - (3)])));
	  else (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));
	  }
    break;

  case 33:

    { (yyval) = list(1,(yyvsp[(1) - (1)])); }
    break;

  case 34:

    { (yyval) = list(3,(yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])); }
    break;

  case 35:

    { (yyval) = list(3,(yyvsp[(1) - (6)]),list(2,(yyvsp[(2) - (6)]),(yyvsp[(4) - (6)])),(yyvsp[(6) - (6)])); }
    break;

  case 36:

    { (yyval) = list(3,(yyvsp[(1) - (8)]),list(3,(yyvsp[(4) - (8)]),(yyvsp[(2) - (8)]),(yyvsp[(6) - (8)])),(yyvsp[(8) - (8)])); }
    break;

  case 37:

    { (yyval) = list(3,(yyvsp[(1) - (10)]),list(4,(yyvsp[(4) - (10)]),(yyvsp[(2) - (10)]),(yyvsp[(6) - (10)]),(yyvsp[(8) - (10)])),(yyvsp[(10) - (10)])); }
    break;

  case 38:

    { (yyval) = list(3,(yyvsp[(1) - (8)]),list(4,NU,(yyvsp[(2) - (8)]),(yyvsp[(4) - (8)]),(yyvsp[(6) - (8)])),(yyvsp[(8) - (8)])); }
    break;

  case 39:

    { (yyval) = list(3,(yyvsp[(1) - (4)]),list(1,(yyvsp[(2) - (4)])),(yyvsp[(4) - (4)])); }
    break;

  case 40:

    { (yyval) = list(3,(yyvsp[(1) - (6)]),list(2,(yyvsp[(2) - (6)]),(yyvsp[(4) - (6)])),(yyvsp[(6) - (6)])); }
    break;

  case 41:

    { (yyval) = list(3,(yyvsp[(1) - (8)]),list(3,(yyvsp[(2) - (8)]),(yyvsp[(4) - (8)]),(yyvsp[(6) - (8)])),(yyvsp[(8) - (8)])); }
    break;

  case 42:

    { (yyval) = list(3,(yyvsp[(1) - (10)]),list(4,(yyvsp[(2) - (10)]),(yyvsp[(4) - (10)]),(yyvsp[(6) - (10)]),(yyvsp[(8) - (10)])),(yyvsp[(10) - (10)])); }
    break;

  case 43:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 44:

    { (yyval) = list(3,(yyvsp[(1) - (6)]),(yyvsp[(3) - (6)]),(yyvsp[(6) - (6)])); }
    break;

  case 45:

    { (yyval) = list(5,(yyvsp[(1) - (8)]),(yyvsp[(2) - (8)]),(yyvsp[(4) - (8)]),(yyvsp[(6) - (8)]),(yyvsp[(8) - (8)])); }
    break;

  case 46:

    { (yyval) = list(5,(yyvsp[(1) - (6)]),(yyvsp[(2) - (6)]),(yyvsp[(4) - (6)]),NU,(yyvsp[(6) - (6)])); }
    break;

  case 47:

    { (yyval) = list(5,(yyvsp[(1) - (6)]),(yyvsp[(2) - (6)]),NU,(yyvsp[(4) - (6)]),(yyvsp[(6) - (6)])); }
    break;

  case 48:

    { (yyval) = list(5,(yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),NU,NU,(yyvsp[(4) - (4)])); }
    break;

  case 49:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 50:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 51:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 52:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 53:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 54:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 55:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 56:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 57:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 58:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 59:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 60:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 61:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 62:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 63:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 64:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 65:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 66:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 67:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 68:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 69:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 70:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 71:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 72:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 73:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 74:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 75:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 76:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 77:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 78:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 79:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 80:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 81:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 82:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 83:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 84:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 85:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 86:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 87:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 88:

    { (yyval) = cons((yyvsp[(2) - (5)]),cons((yyvsp[(1) - (5)]),(yyvsp[(4) - (5)]))); }
    break;

  case 89:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 90:

    { (yyval) = list(3,(yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 91:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 92:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 93:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 94:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 95:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 96:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 97:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 98:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 99:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 100:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 101:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 102:

    { (yyval) = cons((yyvsp[(1) - (2)]),cons((yyvsp[(2) - (2)]),NU)); }
    break;

  case 103:

    { (yyval) = cons((yyvsp[(1) - (3)]),NU); }
    break;

  case 104:

    { (yyval) = cons((yyvsp[(1) - (1)]),NU); }
    break;

  case 105:

    { (yyval) = cons((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)])); }
    break;

  case 106:

    { (yyval) = cons(blockn__K,(yyvsp[(2) - (3)])); setpos((yyval),pos2((yyvsp[(1) - (3)]))); }
    break;

  case 107:

    { (yyval) = cons(block__K,(yyvsp[(2) - (3)])); setpos((yyval),pos2((yyvsp[(1) - (3)]))); }
    break;

  case 108:

    { (yyval) = (yyvsp[(2) - (3)]); }
    break;

  case 109:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 110:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 111:

    { (yyval) = list(2,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 112:

    { (yyval) = leftOperator(list(3,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]))); }
    break;

  case 113:

    { (yyval) = rightOperator(list(3,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]))); }
    break;

  case 114:

    { (yyval) = prefixOperator(list(3,(yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]))); }
    break;

  case 115:

    { (yyval) = list(3,(yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),list(1,block__K)); }
    break;

  case 116:

    { (yyval) = list(3,(yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),list(2,block__K,(yyvsp[(4) - (5)]))); }
    break;

  case 117:

    { (yyval) = list(3,(yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),cons(block__K,(yyvsp[(4) - (5)]))); }
    break;

  case 118:

    { (yyval) = list(3,(yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),cons(block__K,(yyvsp[(4) - (5)]))); }
    break;

  case 119:

    { (yyval) = list(3,(yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),list(1,block__K)); }
    break;

  case 120:

    { (yyval) = list(3,(yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),list(2,block__K,(yyvsp[(4) - (5)]))); }
    break;

  case 121:

    { (yyval) = list(3,(yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),cons(block__K,(yyvsp[(4) - (5)]))); }
    break;

  case 122:

    { (yyval) = list(3,(yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),cons(block__K,(yyvsp[(4) - (5)]))); }
    break;



      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



 /* programs */

static void yyerror(char *s){
     error(s);
     }

#define TSIZE 128

struct TOKENTREE {
     struct TOKENTREE *next[TSIZE];
     short token[TSIZE];
     } *tokentree = NULL;

int registerop(char *s, int token){
     struct TOKENTREE *p;
     if (tokentree == NULL) tokentree = newoftype(struct TOKENTREE);
     p = tokentree;
     while (TRUE) {
	  int c = (*s++) & 0x7f;
	  if (*s == 0) {
	       if (p->token[c] != 0) return ERROR;
	       p->token[c] = token;
	       return 0;
	       }
	  if (p->next[c] == NULL) p->next[c] = newoftype(struct TOKENTREE);
	  p = p->next[c];
	  }
     }

int setopleft(int priority, char *str) {
  int token;
  switch (priority) {
  case 1: token = infix1; break;
  case 2: token = infix2; break;
  case 3: token = infix3; break;
  case 4: token = infix4; break;
  case 5: token = infix5; break;
  case 6: token = infix6; break;
  case 7: token = infix7; break;
  case 8: token = infix8; break;
  case 9: token = infix9; break;
  case 10: token = infix10; break;
  default: return ERROR;
  }
  return registerop(str,token);
}

int setopprefix(int priority, char *str) {
  int token;
  switch (priority) {
  case 1: token = prefix1; break;
  case 2: token = prefix2; break;
  case 3: token = prefix3; break;
  case 4: token = prefix4; break;
  case 5: token = prefix5; break;
  case 6: token = prefix6; break;
  case 7: token = prefix7; break;
  case 8: token = prefix8; break;
  case 9: token = prefix9; break;
  case 10: token = prefix10; break;
  default: return ERROR;
  }
  return registerop(str,token);
}

int setopright(int priority, char *str) {
  int token;
  switch (priority) {
  case 1: token = infix1r; break;
  case 2: token = infix2r; break;
  case 3: token = infix3r; break;
  case 4: token = infix4r; break;
  case 5: token = infix5r; break;
  case 6: token = infix6r; break;
  case 7: token = infix7r; break;
  case 8: token = infix8r; break;
  case 9: token = infix9r; break;
  case 10: token = infix10r; break;
  default: return ERROR;
  }
  return registerop(str,token);
}

static int token;

int tokenlength(char *s, int len) {
     struct TOKENTREE *p = tokentree;
     int n = 0, m = 0;
     while (TRUE) {
	  int c;
	  if (len == 0) break;
	  if (p == NULL) break;
	  c = 0x7f & *s, len--, s++, m++;
	  if (p->token[c] != 0) {
	       n = m;
	       token = p->token[c];
	       }
	  p = p->next[c];
	  }
     return n;
     }

void registerkeyword(char *s,int tok){
     UniqueString(s)->body.unique_string.token = tok;
     }

void yyinit() {
     registerkeyword("do",DO);
     registerkeyword("new",NEW);
     registerkeyword("len",LEN);
     registerkeyword("until",WHILE);
     registerkeyword("while",WHILE);
     registerkeyword("is",IS);
     registerkeyword("or",OR);
     registerkeyword("when",WHEN);
     registerkeyword("foreach",FOREACH);
     registerkeyword("function",FUNCTION);
     registerkeyword("for",FOR);
     registerkeyword("at",AT);
     registerkeyword("in",IN);
     registerkeyword("by",BY);
     registerkeyword("from",FROM);
     registerkeyword("to",TO);
     registerkeyword("if",IF);
     registerkeyword("op",OP);
     registerkeyword("package",PACKAGE);
     registerkeyword("signature",SIGNATURE);
     registerkeyword("use",USE);
     registerkeyword("Pointer",STRINGOP);
     registerkeyword("atomicPointer",STRINGOP);
     registerkeyword("Type",STRINGOP);
     registerkeyword("atomicType",STRINGOP);
     registerkeyword("arithmeticType",STRINGOP);
     registerkeyword("integerType",STRINGOP);
     registerkeyword("header",HEADER);
     registerkeyword("declarations",HEADER);
     registerkeyword("leftOperator",OPERATORLEFT);
     registerkeyword("rightOperator",OPERATORRIGHT);
     registerkeyword("prefixOperator",OPERATORPREFIX);
     registerkeyword("threadLocal",EXPORT);
     registerkeyword("constant",EXPORT);
     registerkeyword("export",EXPORT);
     registerkeyword("import",EXPORT);
     registerkeyword("then",THEN);
     registerkeyword("else",ELSE);
     registerkeyword("break",BREAK);
     registerkeyword("provide",PROVIDE);
     registerkeyword("return",RETURN);
     registerop(":",COLON);
     registerop("||",OROR);
     registerop("&&",ANDAND);
     registerop("{+",BRACEPLUS);
     registerop("-",SELF);
     registerop("{",SELF);
     registerop("}",SELF);
     registerop(",",SELF);
     registerop(";",SELF);
     registerop("(",SELF);
     registerop(")",SELF);
     }

static int yylex() {
     node n;
     int i;
     yylval = NULL;
     top:
     while (cur.text < cur.eot) {
	  char c = *cur.text;
	  if (c=='\n' || c=='\t' || c=='\r' || c=='\f' || c==' ') {
	       advance();
	       }
	  else break;
	  }
     if (cur.text == cur.eot) return end_of_input;
     if (cur.text < cur.eot-1 && cur.text[0]=='-' && cur.text[1]=='-') {
	  advance();
	  while (*cur.text != '\n' && cur.text < cur.eot) advance();
	  goto top;
	  }
     i = tokenlength(cur.text,cur.eot-cur.text);
#if 0
     if (i==1 && *cur.text == '.' && cur.text<cur.eot && isdigit(cur.text[1])) i=0;
#endif
     yylval = positionof(NULL);
     if (i > 0) {
	  yylval->body.position.contents = UniqueStringN(cur.text,i);
	  cur.column += i;
	  if (token == SELF) {
	       char c = *cur.text;
	       assert(i==1);
	       cur.text += i;
	       if (yydebug) fprintf(stderr,"Got token %c\n",c);
	       return c;
	       }
	  else {
	       cur.text += i;
	       if (yydebug) {
#ifdef YYBISON
	       	    fprintf(stderr,"Got %s operator: ", 
			 yytname[YYTRANSLATE(token)]);
	       	    fflush(stderr);
	       	    d_pp(yylval);
	       	    fflush(stdout);
#else
#endif
	       	    }
	       return token;
	       }
	  }
     n = gettoken();
     yylval->body.position.contents = n;
     if (yydebug) {
	  fprintf(stderr,"Got token ");
	  fflush(stderr);
	  d_pp(yylval);
	  fflush(stdout);
	  }
     switch ( n -> tag ) {
          case int_const_tag : return INTEGER;
	  case char_const_tag :
	  case double_const_tag : return NUMBER;
	  case string_const_tag : return STRINGCONST;
	  case unique_string_tag : {
	       if (n->body.unique_string.token != 0) {
		    if (n->body.unique_string.token == OP) {
			 node s;
			 cur.text += i;
			 cur.column += i;
			 i=0;
			 while (cur.text < cur.eot && !iswhite(cur.text[i])
			      && cur.text[i] != ','
			      && cur.text[i] != ';'
			      && cur.text[i] != ')'
			      && cur.text[i] != '(') i++;
			 s = UniqueStringN(cur.text,i);
			 cur.text += i;
			 cur.column += i;
			 if (yydebug) fprintf(stderr,"Got token %s\n",tostring(s));
			 yylval->body.position.contents = s;
			 return IDENTIFIER;
			 }
		    return n->body.unique_string.token;
		    }
	       return IDENTIFIER;
	       }
	  default: assert(FALSE); return 0;
	  }
     }


/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/

