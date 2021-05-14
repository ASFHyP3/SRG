!c  offsetcalc - calculate offsets for roi given two patches at zero offset

      character*60 str
      real v(4),vv(4),delta(4),y(4)

      print *,'Enter a patch number and the 4 offset values: '
      read(*,*)ip1,v
      print *,'Enter second patch number and the 4 offset values: '
      read(*,*)ip2,vv

!c  get variation first
      patchdelta=ip2-ip1
      delta=(vv-v)/patchdelta

!c  project back to patch 1
      x=(1-ip1)/patchdelta
      y=(vv-v)*x+v

      print *,'x= ',x
      print *,'patch 1 values: ',y
      print *,'delta values:   ',delta

      end

