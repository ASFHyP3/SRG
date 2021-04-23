// A SQLite interface for Fortran 90

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sqlite3.h>
#include <string.h>

#define CMDSIZE 2000
#define STRSIZE 30


static int callback(void *NotUsed, int argc, char **argv, char **azColName){
  // Dummy function that could be used to print sql query results
  NotUsed=0;
  int i;
  for(i=0; i<argc; i++){
    //printf("%s = %s\n", azColName[i], argv[i] ? argv[i]: "NULL");
  }
  //printf("\n");
  return 0;
}

////////////////////////////////////// 
//   STRING FUNCTIONS
/////////////////////////////////////
void  ucase(char * str){
  // convert string to upper case
  int ch, i;
  for(i=0;i < strlen(str); i++){
    ch = toupper(str[i]);
    str[i] = ch;
  }
} 


////////////////////////////////////// 
//   OPEN/CLOSE ADD/REMOVE TABLES 
//////////////////////////////////////

int open_dbc_(char *filename, long long *fortran_dbh) {
    sqlite3 *db;
    int rc;    

    //    ucase(filename); //convert to upper case
    rc = sqlite3_open(filename, &db); //open

    *fortran_dbh = (long long) db;

    if (rc != SQLITE_OK) {
        printf("open_database: failed to open %s, rc=%d\n", filename, rc);
        return 0;
    }
    return 1;
}


int close_dbc_(sqlite3 **fortran_dbh) {
    sqlite3 *db;
    db = *fortran_dbh;
    sqlite3_close(db);
    return 1;
}


int add_tblc_(sqlite3 **fortran_dbh,char *tblname){
  // Initialize the given database with table (tblname) containing
  //   values (t1key INTEGER PRIMARY KEY,name TEXT,value TEXT,units text,type text,comment text)
  int rc;
  char sql[CMDSIZE];
  char *zErrMsg = 0;
  sqlite3 *db;  
    
  db = *fortran_dbh;
  
  //  ucase(tblname);//convert to upper case
  
  // create sql string
  snprintf(sql,CMDSIZE,"create table %s (t1key INTEGER PRIMARY KEY,name TEXT unique,value TEXT,units text,type text,comments text)",tblname);

  // create a table
  rc = sqlite3_exec(db, sql, callback, 0, &zErrMsg);
  sqlite3_free(zErrMsg);

  return rc;
}

int del_tblc_(sqlite3 **fortran_dbh,char *tblname){
  // Remove a table
  int rc;
  char sql[CMDSIZE];
  char *zErrMsg = 0;
  sqlite3 *db;
 
  db = *fortran_dbh;

  //  ucase(tblname);   //  convert to upper case

  // create sql string
  snprintf(sql,CMDSIZE,"drop table %s",tblname);

  // create a table
  rc = sqlite3_exec(db, sql, callback, 0, &zErrMsg);
  sqlite3_free(zErrMsg);

  return rc;
}

//////////////////////////////////////
//   FUNCTIONS TO ADD/DELETE PARAMETERS 
//////////////////////////////////////

int add_paramc_(sqlite3 **fortran_dbh,char *tblname,char *name){
  // Add a parameter (name only) to the database
  int rc;
  char sql[CMDSIZE];
  char *zErrMsg = 0;
  sqlite3 *db;
  db = *fortran_dbh;
  
  //  ucase(tblname);  //force to upper case
  //  ucase(name);
  
    
  // for sql command
  snprintf(sql,CMDSIZE,"insert into %s (name) values ('%s')",tblname,name);
 
  rc = sqlite3_exec(db, sql, callback, 0, &zErrMsg);
  sqlite3_free(zErrMsg);
  return rc;
}

int del_paramc_(sqlite3 **fortran_dbh,char *tblname,char *name){
  // Delete parameter from the database
  int rc;
  char sql[CMDSIZE];
  char *zErrMsg = 0;
  sqlite3 *db;
  db = *fortran_dbh;

  //  ucase(tblname); //foce to upper case
  //  ucase(name);

  // create sql string
  snprintf(sql,CMDSIZE,"delete from %s where name = '%s'",tblname,name);

  // delete parameter
  rc = sqlite3_exec(db, sql, callback, 0, &zErrMsg);
  sqlite3_free(zErrMsg);
  return rc;
}

//////////////////////////////////////
//   FUNCTIONS TO EDIT PARAMETERS 
//////////////////////////////////////
int edit_paramcc_(sqlite3 **fortran_dbh,char *tblname,char *name,
		char *value,char *units,char *comment,char* type){
  // Edit existing float parameter
  int rc;
  char sql[CMDSIZE];
  char *zErrMsg = 0;
  sqlite3 *db;
  db = *fortran_dbh;

  // convert to upper case
  //  ucase(tblname);
  //  ucase(name);
  //  ucase(units);
  //  ucase(value);
  //  ucase(type);
    
  
  // create sql command
  snprintf(sql,CMDSIZE,"update %s set value='%s',units='%s',comments='%s',type='%s' where name='%s'",
	   tblname,value,units,comment,type,name);
  

  // update parameter
  rc = sqlite3_exec(db, sql, callback, 0, &zErrMsg);
  sqlite3_free(zErrMsg);
  return rc;
}


////////////////////////////////////// 
//   GET PARAMETERS 
//////////////////////////////////////
int get_paramfc_(sqlite3 **fortran_dbh,char *tblname,char *name,float *value,
	       char *units,char *type,int* units_len,int* type_len){
  // Get a float parameter from the database
  //   updates the input strings (value,units,type) with the corresponding values
  int rc;
  char sql[CMDSIZE];
  sqlite3_stmt* stmt = NULL;
  sqlite3 *db;
  char units_tmp[*units_len];
  char type_tmp[*type_len];
    
 
  db = *fortran_dbh;
  //  ucase(tblname);
  //  ucase(name);

  // create sql string  
  snprintf(sql,CMDSIZE,"select value,units,type from %s where name='%s'",tblname,name);
  rc = sqlite3_prepare(db,sql,-1,&stmt,NULL); //prepare sql statement
  
  // get parameter
  while(sqlite3_step(stmt) == SQLITE_ROW){
    *value = sqlite3_column_double(stmt, 0);   // get value
    snprintf(units_tmp,*units_len+1,"%s",sqlite3_column_text(stmt,1));    //get units
    snprintf(type_tmp,*type_len+1,"%s",sqlite3_column_text(stmt,2));    //get type
  }
  sqlite3_finalize(stmt); 

  //copy to output string
  snprintf(units,*units_len+1,"%s",units_tmp);
  snprintf(type,*type_len+1,"%s",type_tmp);
  *units_len = (int) strlen(units_tmp);
  *type_len = (int) strlen(type_tmp);
  
  return rc;
}

int get_paramdc_(sqlite3 **fortran_dbh,char *tblname,char *name,double *value,
	       char *units,char *type,int* units_len,int* type_len){
  // Get a float parameter from the database
  //   updates the input strings (value,units,type) with the corresponding values
  int rc;
  char sql[CMDSIZE];
  sqlite3_stmt* stmt = NULL;
  sqlite3 *db;
  char units_tmp[*units_len];
  char type_tmp[*type_len];
    
 
  db = *fortran_dbh;
  //  ucase(tblname);
  //  ucase(name);

  // create sql string  
  snprintf(sql,CMDSIZE,"select value,units,type from %s where name='%s'",tblname,name);
  rc = sqlite3_prepare(db,sql,-1,&stmt,NULL); //prepare sql statement
  
  // get parameter
  while(sqlite3_step(stmt) == SQLITE_ROW){
    *value = sqlite3_column_double(stmt, 0);   // get value
    snprintf(units_tmp,*units_len+1,"%s",sqlite3_column_text(stmt,1));    //get units
    snprintf(type_tmp,*type_len+1,"%s",sqlite3_column_text(stmt,2));    //get type
  }
  sqlite3_finalize(stmt); 

  //copy to output string
  snprintf(units,*units_len+1,"%s",units_tmp);
  snprintf(type,*type_len+1,"%s",type_tmp);
  *units_len = (int) strlen(units_tmp);
  *type_len = (int) strlen(type_tmp);
  
  return rc;
}

int get_paramic_(sqlite3 **fortran_dbh,char *tblname,char *name,int *value,
	       char *units,char *type,int* units_len,int* type_len){
  // Get a float parameter from the database
  //   updates the input strings (value,units,type) with the corresponding values
  int rc;
  char sql[CMDSIZE];
  sqlite3_stmt* stmt = NULL;
  sqlite3 *db;
  char units_tmp[*units_len];
  char type_tmp[*type_len];
    
 
  db = *fortran_dbh;
  //  ucase(tblname);
  //  ucase(name);

  // create sql string  
  snprintf(sql,CMDSIZE,"select value,units,type from %s where name='%s'",tblname,name);
  rc = sqlite3_prepare(db,sql,-1,&stmt,NULL); //prepare sql statement
  
  // get parameter
  while(sqlite3_step(stmt) == SQLITE_ROW){
    *value = sqlite3_column_int(stmt, 0);   // get value
    snprintf(units_tmp,*units_len+1,"%s",sqlite3_column_text(stmt,1));    //get units
    snprintf(type_tmp,*type_len+1,"%s",sqlite3_column_text(stmt,2));    //get type
  }
  sqlite3_finalize(stmt); 

  //copy to output string
  snprintf(units,*units_len+1,"%s",units_tmp);
  snprintf(type,*type_len+1,"%s",type_tmp);
  *units_len = (int) strlen(units_tmp);
  *type_len = (int) strlen(type_tmp);
  
  return rc;
}

int get_paramcc_(sqlite3 **fortran_dbh,char *tblname,char *name,char *value,
		 char *units,char *type,int* val_len,int* units_len,int* type_len){
  // Get a float parameter from the database
  //   updates the input strings (value,units,type) with the corresponding values
  int rc;
  char sql[CMDSIZE];
  sqlite3_stmt* stmt = NULL;
  sqlite3 *db;
  char units_tmp[*units_len];
  char type_tmp[*type_len];
  char val_tmp[*val_len];


  db = *fortran_dbh;
  //  ucase(tblname);
  //  ucase(name);

  // create sql string  
  snprintf(sql,CMDSIZE,"select value,units,type from %s where name='%s'",tblname,name);
  rc = sqlite3_prepare(db,sql,-1,&stmt,NULL); //prepare sql statement
  // get parameter
  while(sqlite3_step(stmt) == SQLITE_ROW){
    snprintf(val_tmp,*val_len+1,"%s",sqlite3_column_text(stmt,0)); // get value
    snprintf(units_tmp,*units_len+1,"%s",sqlite3_column_text(stmt,1));    //get units
    snprintf(type_tmp,*type_len+1,"%s",sqlite3_column_text(stmt,2));    //get type
  }
  sqlite3_finalize(stmt);
  //copy to output strin
  snprintf(units,*units_len+1,"%s",units_tmp);
  snprintf(type,*type_len+1,"%s",type_tmp);
  snprintf(value,*val_len+1,"%s",val_tmp);
  *val_len = (int) strlen(val_tmp);
  *units_len = (int) strlen(units_tmp);
  *type_len = (int) strlen(type_tmp);
  
  return rc;
}
