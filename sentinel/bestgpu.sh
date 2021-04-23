#!/usr/bin/env bash

# Number of free GPUs on a machine
#export n_gpus=`/usr/bin/lspci | grep -i "nvidia" | wc -l`

# where is lspci located?
test -f /sbin/lspci && export n_gpus=`/sbin/lspci | grep -i "nvidia" | wc -l`
#test -f /sbin/lspci && echo "/sbin/lspci"
test -f /usr/bin/lspci  && export n_gpus=`/usr/bin/lspci | grep -i "nvidia" | wc -l`
#test -f /usr/bin/lspci  && echo "/usr/bin/lspci"

# Return -1 if there are no GPUs on the machine
if [ $n_gpus -eq 0 ]; then
  echo "-1"
  exit -1
fi

f_gpu=`nvidia-smi.exe | sed -e '1,/Processes/d' \
  | tail -n+3 | head -n-1 | awk '{print $2}'\
  | awk -v ng=$n_gpus 'BEGIN{for (n=0;n<ng;++n){g[n] = 1}} {delete g[$1];} END{for (i in g) print i}' \
  | tail -n 1`

# return -1 if no free GPU was found
if [ `echo $f_gpu | grep -v '^$' | wc -l` -eq 0 ]; then
  echo "-1"
  exit -1
else
  echo $f_gpu
fi
