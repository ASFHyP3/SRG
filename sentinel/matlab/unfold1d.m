%  unfold burst 1 d simulation
clear all; close all;

prforig=1685;
oversample=5;
procoversample=3;
prf=prforig*oversample;
v=6780.;
range=900000;
wvl=0.0566;
d=10;
tburst=1410/prforig;
nburst=tburst*prf;
steerrate=6826;
angrate=steerrate/2/v*wvl;  %  radians per second
angratedeg=angrate*180/pi(); % degrees per second

%  offset position on ground by antenna distance
%X=range*wvl/d*[-1.5 -1.0 -0.5 0 0.5 1.0 1.5];
X1=0.5*range*wvl/d;
X=X1*[-1.0  0  1.25 ];
A=[.3 1 .1];

frate=-2*v*v/wvl/range;
fd=2*v/wvl/range*X;
t=X/v;
tant=X1/v;
xant=tant*v;
angant=xant/range;

%  generate the echoes sampled at prf
time=linspace(0,nburst/prf-1/prf,nburst)-nburst/2/prf;
freqsteer=time*steerrate;
xsteer=freqsteer*range*wvl/2/v;
timesteer=xsteer/v;

timeratio=-steerrate*range*wvl/2/v/v;

%  create multiple target echos from pulse phases
boresight=angrate*time;
x=time*v;
echophase=complex(zeros(1,nburst),zeros(1,nburst));
figure;
for i=1:length(X)
    phase=zeros(1,nburst);
    angle=zeros(1,nburst);
    echotarget=complex(zeros(1,nburst),zeros(1,nburst));
    for j=1:nburst
        angle(j)=(x(j)-X(i))/range;
        if abs(angle(j)-boresight(j))<angant/2
            phase(j)=-sqrt(range^2+(x(j)-X(i))^2)*4*pi()/wvl;
            echotarget(j)=complex(cos(phase(j)),sin(phase(j)));
        end
    end
    echophase=echophase+echotarget*A(i);
    if(i==1)plot(abs(echotarget)*0.8,'r');hold on;end;
    if(i==ceil(length(X)/2))plot(abs(echotarget)*0.9,'g');hold on;end;
    if(i==length(X))plot(abs(echotarget),'b');hold on;end;
    %clear phase echotarget;
end
title('echo envelopes from phases');

figure;
xx=[1:nburst];
plotyy(xx,abs(echophase),xx,abs(fft(echophase)));
title('raw echoes and spectrum from pulse phases');

%  focussed sar on raw echo
timeextended=linspace(0,nburst*procoversample/prf-1/prf,nburst*procoversample)-nburst*procoversample/2/prf;
echophase(nburst+1:nburst*procoversample)=complex(0.,0.);

phase=pi()*frate*timeextended.^2;
ref=complex(cos(phase),sin(phase));
image=ifft(fft(echophase).*conj(fft(ref)));
figure;
plot(20*log10(abs(fft(ref))));
title('reference function spectrum');

figure;
plot(20.*log10(abs(image)));
title('image from raw echo');

%  let's try to unfold them
%  replicate spectrum
spec=fft(echophase);
npts=length(echophase);
overspec=complex(zeros(1,npts*3),zeros(1,npts*3));
overspec(1:npts/2)=spec(1:npts/2);
overspec(npts*3-npts/2+1:npts*3)=spec(npts/2+1:npts);
overspec(npts+1:npts+npts/2)=spec(1:npts/2);
overspec(npts/2+1:npts)=spec(npts/2+1:npts);
overspec(npts*3-npts+1:npts*3-npts/2)=spec(1:npts/2);
overspec(npts*3-npts-npts/2+1:npts*3-npts)=spec(npts/2+1:npts);
over=ifft(overspec);

figure;
xx=[1:npts*3];
plotyy(xx,abs(overspec),xx,abs(over));
title('oversampled spectrum and inverse transform');

%  deramp
totaltime=tburst*procoversample;
npts=length(over);
timederamp=linspace(0,totaltime-totaltime/npts,npts)-totaltime/2;
phasederamp=pi()*steerrate*timederamp.^2;
cpxphase=complex(cos(phasederamp),sin(phasederamp));
over=over.*conj(cpxphase);
derampedspec=fft(over);
figure;
xx=[1:npts];
plotyy(xx,abs(over),xx,abs(derampedspec));
title('deramped timedomain and transform');

%  filter and reramp
filtspec=derampedspec(1:npts/3);
filt=ifft(filtspec);
figure;
xx=[1:npts/3];
plotyy(xx,abs(filtspec),xx,abs(filt));
title('filtered spectrum and inverse transform');

phasereramp=pi()*steerrate*timeextended.^2;
rerampphase=complex(cos(phasereramp),sin(phasereramp));
reramp=filt.*rerampphase;
figure;
xx=[1:nburst*3];
plot(xx,abs(reramp));
title('reramped signal');

% and convolve
phaseref=pi()*frate*timeextended.^2;
ref=complex(cos(phaseref),sin(phaseref));
image=ifft(fft(reramp).*conj(fft(ref)));
figure;
plot(20.*log10(abs(image)));
title('dealised image from raw echo');





% fd0=0;
% fdminus1=-2*v/wvl/range*X1;
% fdplus1=2*v/wvl/range*X1;
% t0=0;
% tminus1=-X1/v;
% tplus1=X1/v;
% tant=0.4*X1/v;
% %  create multiple target echos from chirp params
% echo=complex(zeros(1,nburst),zeros(1,nburst));
% figure;
% for i=1:1%length(X)
%     phase=pi()*frate*(time-t(i)/timeratio).^2-2*pi()*fd(i)*time;
%     echotarget=complex(cos(phase),sin(phase));
%     echotarget(abs(timesteer-t(i))>tant)=complex(0.,0.);
%     echo=echo+echotarget;
%     if(i==1)plot(abs(echotarget)*0.8,'r');hold on;end;
%     if(i==ceil(length(X)/2))plot(abs(echotarget)*0.9,'g');hold on;end;
%     if(i==length(X))plot(abs(echotarget),'b');hold on;end;
%     clear phase echotarget;
% end
% title('echo envelopes from chirp params');
% 
% 
% % boresight, unambiguous
% phase=pi()*frate*time.^2;
% echo0=complex(cos(phase),sin(phase));
% echo0(abs(timesteer)>tant)=complex(0.,0.);
% 
% % minus1 echo
% phase=pi()*frate*(time-tminus1/timeratio).^2-2*pi()*fdminus1*time;
% echominus1=complex(cos(phase),sin(phase));
% echominus1(abs(timesteer-tminus1)>tant)=complex(0.,0.);
% 
% % plus1 echo
% phase=pi()*frate*(time-tplus1/timeratio).^2-2*pi()*fdplus1*time;
% echoplus1=complex(cos(phase),sin(phase));
% echoplus1(abs(timesteer-tplus1)>tant)=complex(0.,0.);
% 
% figure; 
% plot(abs(echo0),'r');
% hold on;
% plot(abs(echominus1),'g');
% plot(abs(echoplus1),'b');

% % echo=echo0+echoplus1+echominus1;
% figure;
% xx=[1:nburst];
% plotyy(xx,abs(echo),xx,abs(fft(echo)));
% title('raw echo and its transform from chirp params');

