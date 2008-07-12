newPackage "MYSQL" -- the name of this package conflicts with the name of the type 
export { tryit }
tryit = () -> (
     mysqlRealConnect("localhost","dan","foobar","",3306,""),
     mysqlRealConnect("localhost","dan","foobar","",0,"/var/run/mysqld/mysqld.sock"),
     mysqlRealConnect("localhost","dan","foobar","",0,"")
     )
<< "-- try tryit()" << endl
end




There are two ways for a client to process result sets. One way is to retrieve
the entire result set all at once by calling mysql_store_result(). This
function acquires from the server all the rows returned by the query and stores
them in the client. The second way is for the client to initiate a row-by-row
result set retrieval by calling mysql_use_result(). This function initializes
the retrieval, but does not actually get any rows from the server.

In both cases, you access rows by calling mysql_fetch_row(). With
mysql_store_result(), mysql_fetch_row() accesses rows that have previously been
fetched from the server. With mysql_use_result(), mysql_fetch_row() actually
retrieves the row from the server. Information about the size of the data in
each row is available by calling mysql_fetch_lengths().

After you are done with a result set, call mysql_free_result() to free the
memory used for it.

 MYSQL_RES *mysql_list_dbs(MYSQL *mysql, const char *wild)

 MYSQL_RES *mysql_store_result(MYSQL *mysql)
 MYSQL_RES *mysql_use_result(MYSQL *mysql)
 MYSQL_ROW mysql_fetch_row(MYSQL_RES *result)
 void mysql_free_result(MYSQL_RES *result)

 unsigned int mysql_field_count(MYSQL *mysql)
 unsigned long *mysql_fetch_lengths(MYSQL_RES *result)
 MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *result)
 MYSQL_FIELD *mysql_fetch_fields(MYSQL_RES *result)

    typedef char **MYSQL_ROW;		/* return data as array of strings */

    typedef struct st_mysql_field {
      char *name;                 /* Name of column */
      char *org_name;             /* Original column name, if an alias */
      char *table;                /* Table of column if column was a field */
      char *org_table;            /* Org table name, if table was an alias */
      char *db;                   /* Database for table */
      char *catalog;	      /* Catalog for table */
      char *def;                  /* Default value (set by mysql_list_fields) */
      unsigned long length;       /* Width of column (create length) */
      unsigned long max_length;   /* Max width for selected set */
      unsigned int name_length;
      unsigned int org_name_length;
      unsigned int table_length;
      unsigned int org_table_length;
      unsigned int db_length;
      unsigned int catalog_length;
      unsigned int def_length;
      unsigned int flags;         /* Div flags */
      unsigned int decimals;      /* Number of decimals in field */
      unsigned int charsetnr;     /* Character set */
      enum enum_field_types type; /* Type of field. See mysql_com.h for types */
    } MYSQL_FIELD;

    enum enum_field_types { MYSQL_TYPE_DECIMAL, MYSQL_TYPE_TINY, MYSQL_TYPE_SHORT, MYSQL_TYPE_LONG, MYSQL_TYPE_FLOAT,
	 MYSQL_TYPE_DOUBLE, MYSQL_TYPE_NULL, MYSQL_TYPE_TIMESTAMP, MYSQL_TYPE_LONGLONG, MYSQL_TYPE_INT24,
	 MYSQL_TYPE_DATE, MYSQL_TYPE_TIME, MYSQL_TYPE_DATETIME, MYSQL_TYPE_YEAR, MYSQL_TYPE_NEWDATE, MYSQL_TYPE_VARCHAR,
	 MYSQL_TYPE_BIT, MYSQL_TYPE_NEWDECIMAL, MYSQL_TYPE_ENUM, MYSQL_TYPE_SET, MYSQL_TYPE_TINY_BLOB,
	 MYSQL_TYPE_MEDIUM_BLOB, MYSQL_TYPE_LONG_BLOB, MYSQL_TYPE_BLOB, MYSQL_TYPE_VAR_STRING, MYSQL_TYPE_STRING,
	 MYSQL_TYPE_GEOMETRY };

    typedef struct st_mysql_res {
      my_ulonglong row_count;
      MYSQL_FIELD	*fields;
      MYSQL_DATA	*data;
      MYSQL_ROWS	*data_cursor;
      unsigned long *lengths;		/* column lengths of current row */
      MYSQL		*handle;		/* for unbuffered reads */
      MEM_ROOT	field_alloc;
      unsigned int	field_count, current_field;
      MYSQL_ROW	row;			/* If unbuffered read */
      MYSQL_ROW	current_row;		/* buffer to current row */
      my_bool	eof;			/* Used by mysql_fetch_row */
      /* mysql_stmt_close() had to cancel this result */
      my_bool       unbuffered_fetch_cancelled;  
      const struct st_mysql_methods *methods;
    } MYSQL_RES;
