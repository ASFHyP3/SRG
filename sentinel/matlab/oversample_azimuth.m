% oversample sentinel rc azimuth spectrum
clear all; close all;

%  read a burst and display
figure(1);
fp=fopen('burst8','r');
data=fread(fp,[17301*2 inf],'float32');
cpx=data(1:2:end,:)+j*data(2:2:end,:);
clear data;
cpx(1:9,:)=complex(0.,0.);
subplot(6,1,1);imagesc(abs(cpx)'); colorbar; colormap gray;
ave=mean(mean(abs(cpx)));
caxis([0 ave*3]);
title('Input burst');

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(cpx),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(6,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);
title('Burst with looks');

% plot spectrum of original burst
spect=fft(cpx,[],2);
subplot(6,1,3);imagesc(abs(spect)'); colorbar; colormap gray;
ave=mean(mean(abs(spect)));
caxis([0 ave*3]);
title('Spectrum of input burst');

% estimate Doppler centroid 
dopest=cpx(:,1:end-1).*conj(cpx(:,2:end));
n=10;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% unwrap estimates
fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();
% fit line to these
p=polyfit([21:size(cpx,2)-20],fd(21:size(cpx,2)-20),1);
fdfit=[1:size(cpx,2)]*p(1)+p(2)-size(cpx,2)/2*p(1);
phase=pi()*fdfit.*linspace(1,size(cpx,2),size(cpx,2));
cpxdop=complex(cos(phase),sin(phase));
fdorig=fd;

% flatten burst
flatcpx=repmat((cpxdop),size(cpx,1),1).*cpx;

%  plot spectrum of flattened burst
clear spect;
spect=fft(flatcpx,[],2);
subplot(6,1,4);imagesc(abs(spect)'); colorbar; colormap gray;
ave=mean(mean(abs(spect)));
caxis([0 ave*3]);
title('Spectrum of flattened burst');

%  fd for flattened burst as a check
clear dopest mask q looks  dopcent fd;
dopest=flatcpx(:,1:end-1).*conj(flatcpx(:,2:end));
n=10;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% unwrap estimates
fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();

%  oversample flattened azimuth 4
% cpx(100,:)=cos([1:1410]/1410.*2*pi()*64);
npts=size(cpx,2);
spect=fft(flatcpx,[],2);
overspec=complex(zeros(size(cpx,1),npts*4),zeros(size(cpx,1),npts*4));
overspec(:,1:npts/2)=spect(:,1:npts/2);
overspec(:,npts*4-npts/2+1:npts*4)=spect(:,npts/2+1:npts);
over=ifft(overspec,[],2);

%plot oversampled burst and its spectrum
subplot(6,1,5);imagesc(abs(over)'); colorbar; colormap gray;
ave=mean(mean(abs(over)));
caxis([0 ave*3]);
title('Oversampled flattened burst');

subplot(6,1,6);imagesc(abs(overspec)'); colorbar; colormap gray;
ave=mean(mean(abs(overspec)));
caxis([0 ave*3]);
title('Oversampled flattened spectrum');

% unfocussed processor on flattened oversampled data
figure(2);
clear mask;
n=128;
mask=ones(1,n);
unf=conv2(over,mask,'same');
subplot(6,1,1);imagesc(abs(unf)'); colorbar; colormap gray;
ave=mean(mean(abs(unf)));
caxis([0 ave*3]);
title('unfocussed proc oversampled data');

% take some looks to see image better
clear mask q looks;
n=10;
mask=ones(n);
q=conv2(abs(unf),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(6,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);
title('unfocussed with looks');

% reinject flattening
%%%  flattening function was fdfit=[1:size(cpx,2)]*p(1)+p(2)-size(cpx,2)/2*p(1);
fdfitover=[1:npts*4]*p(1)/4+p(2)-npts/2*p(1);
phase=pi()*fdfitover.*linspace(1,npts*4,npts*4);
cpxdop=complex(cos(phase),-sin(phase));

% unflatten
unflatover=repmat((cpxdop),size(over,1),1).*over;

%  plot unflattened data and its spectrum
subplot(6,1,3);imagesc(abs(unflatover)'); colorbar; colormap gray;
ave=mean(mean(abs(unflatover)));
caxis([0 ave*3]);
title('Reinject flattening phase');

unflatspec=fft(unflatover,[],2);
subplot(6,1,4);imagesc(abs(unflatspec)'); colorbar; colormap gray;
ave=mean(mean(abs(unflatspec)));
caxis([0 ave*3]);
title('Spectrum reinjected data');

%  unfocussed proc reinjected data
clear mask unf;
n=128;
mask=ones(1,n);
unf=conv2(unflatover,mask,'same');
subplot(6,1,5);imagesc(abs(unf)'); colorbar; colormap gray;
ave=mean(mean(abs(unf)));
caxis([0 ave*3]);
title('unfocussed proc reinjected data');

% take some looks to see image better
clear mask q looks;
n=10;
mask=ones(n);
q=conv2(abs(unf),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(6,1,6);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);
title('unfocussed with looks');

% get centroid of unflattened data to see if looks like original
clear dopest mask q looks dopcent fd;
dopest=unflatover(:,1:end-1).*conj(unflatover(:,2:end));
n=20;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% unwrap estimates
fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();

%  save as a flattened burst
data(1:2:size(cpx,1)*2,:)=real(over);
data(2:2:size(cpx,1)*2,:)=imag(over);
fpout=fopen('burst8over','w');
count=fwrite(fpout,data,'float32')
fclose(fpout)

% % focussed processor
% iplt=0;
% for v=6550:25:6800
% clear phase;
% ref=zeros(1,size(cpx,2))+j*zeros(1,size(cpx,2));
% npts=256;
% range=925000.
% %v=6700;
% rate=2*v*v/0.0566/range
% time=linspace(-npts/2/1681.,npts/2/1681.,npts+1);
% phase=pi()*rate*time.*time;
% ref(1:npts+1)=complex(cos(phase),sin(phase));
% refspec=repmat(fft(ref),size(cpx,1),1);
% % for i=1:size(cpx,1)
% %     foc(i,:)=ifft(refspec.*fft(cpx(i,:)));
% % end
% foc=ifft(refspec.*fft(flatcpx,[],2),[],2);
% 
% figure(3);
% iplt=iplt+1;
% subplot(3,5,iplt);imagesc(abs(foc(1300:1600,500:700))'); colormap gray;
% ave=mean(mean(abs(foc)));
% caxis([0 ave*3]);
% % % title('focussed proc');
% 
% %  looks for display
% % % n=10;
% % % mask=ones(n);
% % % q=conv2(abs(foc),mask,'same');
% % % looks=q(1:n:end,1:n:end);
% % % iplt=iplt+1;
% % % subplot(7,1,iplt);imagesc(looks'); colorbar; colormap gray;
% % % ave=mean(mean(abs(looks)));
% % % caxis([0 ave*3]);
% 
% end
% 
% % %  repeat over several velocities
% % figure(2);
% % for iv=17:19
% %     range=800000.;
% %     v=6700+(iv-1)*5;
% %     rate=2*v*v/0.0566/range
% %     time=linspace(0,(size(cpx,2)-1)/1681.,size(cpx,2));
% %     phase=pi()*rate*time.*time;
% %     ref=complex(cos(phase),sin(phase));
% %     refspec=repmat(fft(ref),size(cpx,1),1);
% %     % for i=1:size(cpx,1)
% %     %     foc(i,:)=ifft(refspec.*fft(cpx(i,:)));
% %     % end
% %     foc=ifft(refspec.*fft(cpx,[],2),[],2);
% %     quality(iv)=std(abs(reshape(foc,size(foc,1)*size(foc,2),1)));
% % %     subplot(4,1,3);imagesc(sqrt(abs(foc))'); colorbar; colormap gray;
% % %     avemag=mean(mean(abs(foc)))
% %     
% %     %  looks for display
% %     n=5;
% %     mask=ones(n);
% %     q=conv2(abs(foc),mask,'same');
% %     looks=q(1:n:end,1:n:end);
% %     %subplot(6,1,iv);imagesc(looks'); colorbar; colormap gray;
% %     if iv==18
% %         subplot(2,1,1);
% %         imagesc(sqrt(abs(foc))');
% %         colorbar;colormap gray;
% %         subplot(2,1,2);
% %         imagesc(looks');
% %         colorbar; colormap gray;
% %     end
% % end
% % figure(3); plot(quality)