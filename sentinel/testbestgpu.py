#!/usr/bin/env python3

import sys
import os
import string
import time
import subprocess

#  grab the least used gpu
proc = subprocess.Popen("$PROC_HOME/bestgpu.sh",stdout=subprocess.PIPE, shell=True)
(bestGPU,err)=proc.communicate()
bestGPU=str(int(bestGPU.strip()))
print ('GPU number: ',bestGPU)
os.environ['CUDA_DEVICE_ORDER'] = 'PCI_BUS_ID'
os.environ['CUDA_VISIBLE_DEVICES'] = bestGPU


# and start
command = '$PROC_HOME/test/gpu/hello'
print (command)
ret = os.system(command)
