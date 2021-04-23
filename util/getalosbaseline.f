c  getalosbaseline - get alos baseline params from sandwell's baseline program

      character*60 str

      if(iargc().lt.8)then
         print *,'usage: getalosbaselines bhstart bvstart bhend bvend rawdatalines firstroilinein numroilinesin numroilinesout'
         stop
      end if
      call getarg(1,str)
      read(str,*)bhst
      call getarg(2,str)
      read(str,*)bvst
      call getarg(3,str)
      read(str,*)bhend
      call getarg(4,str)
      read(str,*)bvend
      call getarg(5,str)
      read(str,*)linesraw
      call getarg(6,str)
      read(str,*)lineroistart
      call getarg(7,str)
      read(str,*)lineroinum
      call getarg(8,str)
      read(str,*)lineroiout

      deltabh=(bhend-bhst)/linesraw
      deltabv=(bvend-bvst)/linesraw

      bh=lineroistart*deltabh+bhst
      bv=lineroistart*deltabv+bvst

      bhdot=deltabh*lineroinum/lineroiout
      bvdot=deltabv*lineroinum/lineroiout

      print *,'bh bv phi0 bhdot bvdot',bh,bv,0.,bhdot,bvdot

      end



