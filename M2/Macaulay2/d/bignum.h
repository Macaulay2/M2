typedef struct {int len_;char array_[1];} *string;
typedef unsigned int uint;
typedef char bool;

/* we assume 2*sizeof(int) <= sizeof(doubledigit) */

#ifdef __GNUC__
typedef unsigned long digit;
typedef long signeddigit;
typedef unsigned long long doubledigit;	
typedef long long signeddoubledigit;
#else
typedef unsigned short digit;
typedef short signeddigit;
typedef unsigned long doubledigit;
typedef long signeddoubledigit;
#endif

#define MAXDIGIT (digit)(-1)

#define BITS(type) (8*sizeof(type))
typedef struct INTEGER {bool negative; short len; digit body[0];} *integer;

int integer_sizeof(integer);

integer integer_plus(integer,integer);
int integer_plus_space(integer,integer);
void integer_plus_d(integer,integer,integer);
integer integer_plus_integer_int(integer,int);
integer integer_plus_int_integer(int,integer);

integer integer_diff(integer,integer);
int integer_diff_space(integer,integer);
void integer_diff_d(integer,integer,integer);
integer integer_diff_integer_int(integer,int);
integer integer_diff_int_integer(int,integer);

integer integer_minus(integer);
int integer_minus_space(integer);
void integer_minus_d(integer, integer);

integer integer_times(integer,integer);
int integer_times_space(integer,integer);
void integer_times_d(integer,integer,integer);
integer integer_times_integer_int(integer,int);
integer integer_times_int_integer(int,integer);

integer integer_quotient(integer,integer);

integer integer_demainder(integer,integer);

integer integer_power(integer,integer);
integer integer_gcd(integer,integer);
integer integer_abs(integer);
integer integer_toInteger(int);
integer integer_shiftleft(integer,int);
integer integer_shiftright(integer,int);

bool integer_isInt(integer);
int integer_toInt(integer);
integer integer_Floor(double);
integer integer_Round(double);
double integer_toDouble(integer);

bool integer_equal(integer,integer);
bool integer_equal_integer_int(integer,int);
bool integer_equal_int_integer(int,integer);

bool integer_less(integer,integer);
bool integer_greater(integer,integer);
bool integer_less_equal(integer,integer);
bool integer_greater_equal(integer,integer);

bool integer_less_integer_int(integer,int);
bool integer_greater_integer_int(integer,int);
bool integer_less_equal_integer_int(integer,int);
bool integer_greater_equal_integer_int(integer,int);

bool integer_less_int_integer(int,integer);
bool integer_greater_int_integer(int,integer);
bool integer_less_equal_int_integer(int,integer);
bool integer_greater_equal_int_integer(int,integer);

integer integer_logical_and(integer,integer);
integer integer_logical_or(integer,integer);
integer integer_logical_xor(integer,integer);

bool integer_less_integer_double(integer,double);
bool integer_greater_integer_double(integer,double);
bool integer_less_equal_integer_double(integer,double);
bool integer_greater_equal_integer_double(integer,double);

bool integer_less_double_integer(double,integer);
bool integer_greater_double_integer(double,integer);
bool integer_less_equal_double_integer(double,integer);
bool integer_greater_equal_double_integer(double,integer);

int integer_numbits(integer);
string integer_tostring(integer);
