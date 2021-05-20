subroutine resample2grid(rgoff,azoff,rgm,burstdata,demwidth,samplesPerBurst,linesPerBurst, &
     overlap,firstValid,lastValid,wvl,topbot,outdata)

  ! topbot flag:  =1 then bottom of burst, =2 then top of burst
  implicit none
  real*8 :: rgoff(demwidth),azoff(demwidth),rgm(demwidth)
  complex*8 :: burstdata(samplesPerBurst,linesPerburst),outdata(demwidth),complex1,complex2
  integer :: i,intr,inta,overlap,linesPerBurst,samplesPerBurst,demwidth,topbot,firstValid,lastValid
  real*8 :: pi,wvl,phase,fracr,fraca

  pi=4.d0*datan2(1.d0,1.d0)
  outdata=cmplx(0.,0.)
  do i=1,demwidth
     if(rgoff(i).ge.1.and.rgoff(i).le.samplesPerBurst-1)then
        if(topbot.eq.1)azoff(i)=azoff(i)-(linesPerBurst-overlap)
        if(azoff(i).ge.1.and.azoff(i).le.overlap-1)then
           intr=rgoff(i)
           fracr=rgoff(i)-intr
           inta=azoff(i)
           fraca=azoff(i)-inta
           complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
           complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
           !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
           if(topbot.eq.1)then
              if(inta.lt.lastValid)then ! skip if data not valid
                 outdata(i)=complex1*(1-fraca)+complex2*fraca
                 !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                 ! remove range propagation phase
                 phase=4.d0*pi/wvl*rgm(i)
                 outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                 !print *,inta,outdata(i),burstdata(intr,inta)
              end if
           else
              if(inta.gt.firstValid-1)then ! skip if data not valid
                 outdata(i)=complex1*(1-fraca)+complex2*fraca
                 !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                 ! remove range propagation phase
                 phase=4.d0*pi/wvl*rgm(i)
                 outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                 !print *,inta,outdata(i),burstdata(intr,inta)
              end if
           end if 
        end if
     end if
  end do

  return
end subroutine resample2grid
