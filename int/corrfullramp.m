% define image 1 from gaussian draws
noise=randn(2,101);
im1=complex(noise(1,:),noise(2,:));

%  second image is same but for phase ramp
im2=im1;
dphase=0.5;
phase=linspace(0,100*dphase,101);
data=cos(phase)+sqrt(-1)*sin(phase);
im2=im2.*data;

%  add some gaussian noise to data arrays
noiseamp=0.2;
noise=noiseamp*randn(2,101);
cnoise=complex(noise(1,:),noise(2,:));
im1=im1+cnoise;
noise=noiseamp*randn(2,101);
cnoise=complex(noise(1,:),noise(2,:));
im2=im2+cnoise;

%  get amplitudes and interferogram
amp1=abs(im1).^2;
amp2=abs(im2).^2;
int=im1.*conj(im2);

% estimate dphase
for i=1:100;
    diff(i)=int(1,i)*conj(int(1,i+1));
end
acc=sum(diff)
pha=atan2(imag(acc),real(acc))

% dephase the interferogram
for i=1:101
    d(i)=cos(pha*i)+sqrt(-1)*sin(pha*i);
end
int=int.*d;


for i=1:10;
    a1=sum(amp1(i*10-9:i*10));
    a2=sum(amp2(i*10-9:i*10));
    csum=sum(int(i*10-9:i*10));
    corr(i)=abs(csum)/sqrt(a1*a2);
end

corr

%ph=atan2(imag(int),real(int));

