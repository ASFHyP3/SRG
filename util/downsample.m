% create a chirp and downsample to lower bandwidth
clear all; close all;

s=-518518520000.000000*2;
fs=32e6;
tau=27e-6;

k=[1:8192];
t=(k-4096)/fs;

mask=ones(1,8192);
mask(find(abs(t)>tau/2))=0;

chirp=exp(j*pi*s*t.^2).*mask;
ref=chirp;

%plot(mask);

spectrum=fft(chirp);
figure(1);
plot(abs(spectrum));

% compress
refspectrum=fft(ref);
impulse=ifft(spectrum.*conj(refspectrum));

figure(2); plot(10*log10(abs(impulse(1:100))));

% take half of the spectrum
spectrum2=spectrum(4097:8192);
%spectrum2(2049:4096)=spectrum(6145:8192);

chirp2=ifft(fftshift(spectrum2));
figure(3); plot(real(chirp2));

figure(4); plot(abs(spectrum2));