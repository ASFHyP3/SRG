#!/usr/bin/python3

def testfunc(arg: str):
    print ('In testfunc: ',arg)

import os
print (os.getcwd())

command = 'python --version'
ret = os.system(command)

testfunc('Hello world.')

