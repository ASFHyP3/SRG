% test sentinel rc azimuth spectrum
clear all; close all;

%  read a burst
fp=fopen('burst8','r');
data=fread(fp,[17301*2 inf],'float32');
cpx=data(1:2:end,:)+j*data(2:2:end,:);
clear data;
cpx(1:9,:)=complex(0.,0.);
subplot(6,1,1);imagesc(abs(cpx)'); colorbar; colormap gray;
ave=mean(mean(abs(cpx)));
caxis([0 ave*3]);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(cpx),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(6,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

%  compute azimuth spectrum
spect=fft(cpx,[],2);
subplot(6,1,3);imagesc(abs(spect)'); colorbar; colormap gray;
ave=mean(mean(abs(spect)));
caxis([0 ave*3]);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(spect),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(6,1,4);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

% estimate Doppler centroid
dopest=cpx(:,1:end-1).*conj(cpx(:,2:end));
n=10;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% flatten data empirically
doparray=repmat(conj(dopcent),size(cpx,1),1);
% unwrap estimates
fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();
% fit line to these
p=polyfit([21:size(cpx,2)-20],fd(21:size(cpx,2)-20),1);
fdfit=[1:size(cpx,2)]*p(1)+p(2)-size(cpx,2)/2*p(1)-p(2);
phase=pi()*fdfit.*linspace(1,size(cpx,2),size(cpx,2));
cpxdop=complex(cos(phase),sin(phase));

flatcpx=repmat((cpxdop),size(cpx,1),1).*cpx;

%  check doppler estimation
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
% fit line to these
p=polyfit([21:size(cpx,2)-20],fd(21:size(cpx,2)-20),1);
fdfit=[1:size(cpx,2)]*p(1)+p(2)-size(cpx,2)/2*p(1)-p(2);
phase=pi()*fdfit.*linspace(1,size(cpx,2),size(cpx,2));
cpxdop=complex(cos(phase),sin(phase));

%  plot flattened spectrum
%  compute azimuth spectrum
spect=fft(flatcpx,[],2);
subplot(6,1,5);imagesc(abs(spect)'); colorbar; colormap gray;
ave=mean(mean(abs(spect)));
caxis([0 ave*3]);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(spect),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(6,1,6);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

% unfocussed processor on flattened data
n=16;
mask=ones(1,n);
unf=conv2(flatcpx,mask,'same');
figure(2);
subplot(4,1,1);imagesc(abs(unf)'); colorbar; colormap gray;
ave=mean(mean(abs(unf)));
caxis([0 ave*3]);
title('unfocussed proc');

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(unf),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

%  plot flattened data
%  compute azimuth spectrum
subplot(4,1,3);imagesc(abs(flatcpx)'); colorbar; colormap gray;
ave=mean(mean(abs(flatcpx)));
caxis([0 ave*3]);
title('flattened data');

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(flatcpx),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,4);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

% focussed processor
iplt=0;
for v=6550:25:6800
clear phase;
ref=zeros(1,size(cpx,2))+j*zeros(1,size(cpx,2));
npts=256;
range=925000.
%v=6700;
rate=2*v*v/0.0566/range
time=linspace(-npts/2/1681.,npts/2/1681.,npts+1);
phase=pi()*rate*time.*time;
ref(1:npts+1)=complex(cos(phase),sin(phase));
refspec=repmat(fft(ref),size(cpx,1),1);
% for i=1:size(cpx,1)
%     foc(i,:)=ifft(refspec.*fft(cpx(i,:)));
% end
foc=ifft(refspec.*fft(flatcpx,[],2),[],2);

figure(3);
iplt=iplt+1;
subplot(3,5,iplt);imagesc(abs(foc(1300:1600,500:700))'); colormap gray;
ave=mean(mean(abs(foc)));
caxis([0 ave*3]);
% % title('focussed proc');

%  looks for display
% % n=10;
% % mask=ones(n);
% % q=conv2(abs(foc),mask,'same');
% % looks=q(1:n:end,1:n:end);
% % iplt=iplt+1;
% % subplot(7,1,iplt);imagesc(looks'); colorbar; colormap gray;
% % ave=mean(mean(abs(looks)));
% % caxis([0 ave*3]);

end

% %  repeat over several velocities
% figure(2);
% for iv=17:19
%     range=800000.;
%     v=6700+(iv-1)*5;
%     rate=2*v*v/0.0566/range
%     time=linspace(0,(size(cpx,2)-1)/1681.,size(cpx,2));
%     phase=pi()*rate*time.*time;
%     ref=complex(cos(phase),sin(phase));
%     refspec=repmat(fft(ref),size(cpx,1),1);
%     % for i=1:size(cpx,1)
%     %     foc(i,:)=ifft(refspec.*fft(cpx(i,:)));
%     % end
%     foc=ifft(refspec.*fft(cpx,[],2),[],2);
%     quality(iv)=std(abs(reshape(foc,size(foc,1)*size(foc,2),1)));
% %     subplot(4,1,3);imagesc(sqrt(abs(foc))'); colorbar; colormap gray;
% %     avemag=mean(mean(abs(foc)))
%     
%     %  looks for display
%     n=5;
%     mask=ones(n);
%     q=conv2(abs(foc),mask,'same');
%     looks=q(1:n:end,1:n:end);
%     %subplot(6,1,iv);imagesc(looks'); colorbar; colormap gray;
%     if iv==18
%         subplot(2,1,1);
%         imagesc(sqrt(abs(foc))');
%         colorbar;colormap gray;
%         subplot(2,1,2);
%         imagesc(looks');
%         colorbar; colormap gray;
%     end
% end
% figure(3); plot(quality)