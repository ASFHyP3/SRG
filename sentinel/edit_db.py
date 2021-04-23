#!/usr/bin/env python3
#from pysqlite2 import dbapi2 as sqlite3
import sys
ver=sys.version_info

    


    

#   #if running python 2.5+
import sys, os


# check correct number of inputs
if len(sys.argv)!=2 and len(sys.argv)!=5:
    print "Usage: edit_db.py database_name <tablename> <parameter> <value>"
    sys.exit()

dbname = sys.argv[1]  #command line input for database name

# does the file exist?
if not(os.path.exists(dbname)):
    print "ERROR: Database '"+dbname+"' does not exist"
    sys.exit()

# open database
con = sqlite3.connect(dbname)

# create a cursor
c = con.cursor()

#  for 5 parameter version, assign from command line
if len(sys.argv)==5:
    ct=sys.argv[2]
    param=sys.argv[3]
    value=sys.argv[4]
    sql_mod.edit_param(c,ct,param,value,'-','-','edited parameter')
    # save the changes
    con.commit()
    # close cursor
    c.close()
    # close connection
    con.close()
    sys.exit()

# for two argument version, prompt for additional args

# get all tables in database
c.execute("select * from sqlite_primary where type='table'")
mstlist = c.fetchall()
tlist = []
print " "
print "%-i %-s %-s" % tuple([len(mstlist), " tables in the database ",dbname])
print " "
for i in range(0,len(mstlist),1):
    tlist.append(mstlist[i][1])
    print "     ",i+1,":",tlist[i]

print " "
num = raw_input("Select a database to edit (enter number 1-"+str(len(tlist))+", 0 to exit): ")

if int(num) == 0:
    sys.exit()

# print the contents of each list
i = int(num) - 1
#for i in range(len(tlist)):
ct = tlist[i]  #current table

print 'Table : ',ct

param = raw_input("Parameter to edit: ")
value = raw_input("Enter new parameter value: ")

sql_mod.edit_param(c,ct,param,value,'-','-','edited parameter')

# save the changes
con.commit()

# close cursor
c.close()

# close connection
con.close()
