subroutine resample2grid_burst(rgoff,azoff,rgm,burstdata,demwidth,samplesPerBurst,linesPerBurst, &
     firstValid,lastValid,wvl,outdata)

  ! topbot flag:  =1 then bottom of burst, =2 then top of burst
  implicit none
  real*8 :: rgoff(demwidth),azoff(demwidth),rgm(demwidth)
  complex*8 :: burstdata(samplesPerBurst,linesPerburst),outdata(demwidth),complex1,complex2
  integer :: i,intr,inta,linesPerBurst,samplesPerBurst,demwidth,firstValid,lastValid
  real*8 :: pi,wvl,phase,fracr,fraca

  pi=4.d0*datan2(1.d0,1.d0)
  outdata=cmplx(0.,0.)
  do i=1,demwidth
     if(rgoff(i).ge.1.and.rgoff(i).le.samplesPerBurst-1)then
        if(azoff(i).ge.firstValid+2.and.azoff(i).le.lastValid-3)then
           intr=rgoff(i)
           fracr=rgoff(i)-intr
           inta=azoff(i)
           fraca=azoff(i)-inta
           complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
           complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
           !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
           outdata(i)=complex1*(1-fraca)+complex2*fraca
           ! remove range propagation phase
           phase=4.d0*pi/wvl*rgm(i)
           outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
           !print *,inta,outdata(i),burstdata(intr,inta)
        end if
     end if
  end do

  return
end subroutine resample2grid_burst
