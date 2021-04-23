#!/usr/bin/env python3
# Module for SQLite functions
# Filename : sql_mod.py
#from pysqlite2 import dbapi2 as sqlite3
#import sqlite3 #for python 2.5+
import sys
ver=sys.version_info
if ver[1] == 4:
    import sqlite as sqlite3

if ver[1] > 4:
    import sqlite3

import string

# create a table
def add_tbl(c,tblname):
    # create a table
#    tblname = string.strip(tblname).upper()
    tblname = string.strip(tblname)
    try:
        c.execute("create table "+tblname+" (t1key INTEGER PRIMARY KEY,\n\
        name TEXT unique,value TEXT,units text,type text,comments text);")
    except sqlite3.Error:
        pass

def rm_tbl(c,tblname):
    # remove a table
    tblname = string.strip(tblname)
    try:
        c.execute("drop table "+tblname)
    except sqlite3.Error:
        print "Warning: Table "+tblname+" does not exist"
        pass
def add_param(c,tblname,pname):
    #add a parameter to the table
    name = string.strip(pname) #trim spaces, upper case
    
    try:
        c.execute("insert into "+tblname+" (name) values ('"+name+"')")
    except:
        pass

def del_param(c,tblname,name):
    #add a parameter to the table
    name = string.strip(name) #trim spaces, upper case
    c.execute("delete from "+tblname+" where name='"+name+"'")

def edit_param(c,tblname,name,value,units,type,comment):
    #add a parameter to the table
    name = string.strip(name) #trim spaces, upper case
    value = string.strip(str(value))
    units = string.strip(units)
    type = string.strip(type)
    comment = string.strip(comment)
    try:
        c.execute("update "+tblname+" set value='"+value+"',units='"+units+"',type='"\
                  +type+"',comments='"+comment+"' where name='"+name+"'")
    except:
        print "In function EDIT_PARAM: "+name+" does not exist"

def get_param(c,tblname,name):
    #get values from the table (returned as a list)
    # output = [name,value,units,type,comment]
    name = string.strip(name) #trim spaces, upper case
    try:
        c.execute("select name,value,units,type,comments from "+tblname+\
        " where name='"+name+"'")
        row = c.fetchone()
        return [row[0],row[1],row[2],row[3],row[4]]
    except sqlite3.Error, e:
        print "Parameter '"+name+"' not found",e.args[0]
        
def valuef(c,tblname,name):
    # return float value of a parameter in database table
    name = string.strip(name) #trim spaces, upper case
    try:
        c.execute("select name,value,units,type,comments from "+tblname+\
        " where name='"+name+"'")
        row = c.fetchone()
        return float(row[1])
    except:
        print "Parameter '"+name+"' not found"

def valuei(c,tblname,name):
    # return float value of a parameter in database table
    name = string.strip(name) #trim spaces, upper case
    try:
        c.execute("select name,value,units,type,comments from "+tblname+\
        " where name='"+name+"'")
        row = c.fetchone()
        return int(row[1])
    except:
        print "Parameter '"+name+"' not found"

def valuec(c,tblname,name):
    # return float value of a parameter in database table
    name = string.strip(name) #trim spaces, upper case
    try:
        c.execute("select name,value,units,type,comments from "+tblname+\
        " where name='"+name+"'")
        row = c.fetchone()
        return row[1]
    except:
        print "Parameter '"+name+"' not found"


 



