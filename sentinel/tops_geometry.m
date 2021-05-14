%  some tops geometry for processing
clear all; close all;

prf=1685;
nburst=1410;
tburst=nburst/prf;
v=6780.;
range=900000;
wvl=0.0566;
d=10;
nburst=tburst*prf;
steerrate=6826;
angrate=steerrate/2/v*wvl;  %  radians per second
angratedeg=angrate*180/pi(); % degrees per second

time=linspace(0,nburst/prf-1/prf,nburst)-nburst/2/prf;

fd=time*steerrate;
z=fd*(wvl*range/2/v+v/steerrate);

plotyy(time,fd,time,z);
title('fd (left) z (right)');

timefromfd=fd/steerrate;
pulse=timefromfd*prf+nburst/2;

figure;
plot(fd,pulse);
title('pulsenumber (center) function of fd');

%  illumination time
% range*wvl/d=timeillum*steerate*(wvl*range/2/v+v/steerrate);
timeillum=range*wvl/d/steerrate/(wvl*range/2/v+v/steerrate)
pulsesaperture=timeillum*prf