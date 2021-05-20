subroutine resample2grid_burst_reramp(rgoff,azoff,rgm,burstdata,derampphase,demwidth,samplesPerBurst,linesPerBurst, &
     firstValid,lastValid,firstValidSample,lastValidSample,wvl,outdata)

  ! topbot flag:  =1 then bottom of burst, =2 then top of burst
  implicit none
  real*8 :: rgoff(demwidth),azoff(demwidth),rgm(demwidth)
  complex*8 :: burstdata(samplesPerBurst,linesPerburst),outdata(demwidth),complex1,complex2
  real*8 :: derampphase(samplesPerBurst,linesPerBurst),reramp1,reramp2,reramp
  integer :: i,intr,inta,linesPerBurst,samplesPerBurst,demwidth,firstValid,lastValid,pad
  integer :: firstValidSample, lastValidSample
  real*8 :: pi,wvl,phase,fracr,fraca

  pi=4.d0*datan2(1.d0,1.d0)
  outdata=cmplx(0.,0.)
  do i=1,demwidth
     if(rgoff(i).ge.1+firstValidSample.and.rgoff(i).le.lastValidSample-1)then
        if(azoff(i).ge.firstValid+2.and.azoff(i).le.lastValid-3)then
           intr=rgoff(i)
           fracr=rgoff(i)-intr
           inta=azoff(i)
           fraca=azoff(i)-inta
           complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
           complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
           reramp1=derampphase(intr,inta)*(1-fracr)+derampphase(intr+1,inta)*fracr
           reramp2=derampphase(intr,inta+1)*(1-fracr)+derampphase(intr+1,inta+1)*fracr
           !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
           outdata(i)=complex1*(1-fraca)+complex2*fraca
           reramp=reramp1*(1-fraca)+reramp2*fraca
           ! remove range propagation phase
           phase=4.d0*pi/wvl*rgm(i)-reramp
           outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
           !print *,inta,outdata(i),burstdata(intr,inta)
        end if
     end if
  end do

  return
end subroutine resample2grid_burst_reramp
