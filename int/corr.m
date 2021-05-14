clear all;
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
noiseamp=0.5;
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
int(1:10)

% total1 is the list of vectors for the un-de-phased data
for i=1:10
    total1(i)=sum(int(1:i));
end
% plot(total(1:10))
    a1=sum(amp1(1:10));
    a2=sum(amp2(1:10));
    csum=sum(int(1:10));
    precorr=abs(csum)/sqrt(a1*a2);


for i=1:1;
% estimate dphase
    for j=i*10-9:i*10;
        diff(j)=int(1,j)*conj(int(1,j+1));
    end
    acc=sum(diff(i*10-9:i*10))
    pha=atan2(imag(acc),real(acc))

% dephase the interferogram
    k=0;
    for j=i*10-9:i*10
        d(j)=cos(pha*k)+sqrt(-1)*sin(pha*k);
        k=k+1;
    end
    int(i*10-9:i*10)=int(i*10-9:i*10).*d(i*10-9:i*10);
    
    a1=sum(amp1(i*10-9:i*10));
    a2=sum(amp2(i*10-9:i*10));
    csum=sum(int(i*10-9:i*10));
    postcorr(i)=abs(csum)/sqrt(a1*a2);
end

int(1:10)
precorr
postcorr

% total2 is the de-phased vector plot
for i=1:10
    total2(i)=sum(int(1:i));
end
plot(real(total1(1:10)),imag(total1(1:10)),'r',real(total2(1:10)),imag(total2(1:10)),'b')
axis equal;

%ph=atan2(imag(int),real(int));

