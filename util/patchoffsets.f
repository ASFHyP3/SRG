c  patchoffsets - propagate offsets

      print *,'enter first patch number'
      read(*,*)mp

      print *,'enter first patch offsets'
      read(*,*)a,b,c,d
      print *,'enter delta offsets'
      read(*,*)da,db,dc,dd

      w=-(mp-1)*da+a
      x=-(mp-1)*db+b
      y=-(mp-1)*dc+c
      z=-(mp-1)*dd+d

      print *,'patch 1 offsets: ',w,x,y,z
      end
