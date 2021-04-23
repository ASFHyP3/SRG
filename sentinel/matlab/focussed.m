% test sentinel rc azimuth spectrum
clear all;close all;

%  read a burst
fp=fopen('burst8','r');
data=fread(fp,[17301*2 inf],'float32');
cpx=data(1:2:end,:)+j*data(2:2:end,:);
cpx(1:9,:)=complex(0.,0.);
subplot(4,1,1);imagesc(abs(cpx)'); colorbar; colormap gray;
ave=mean(mean(abs(cpx)));
caxis([0 ave*3]);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(cpx),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

% create an oversampled version of the input data by direct oversampling

% deramp
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
% fdfit=[1:size(cpx,2)]*p(1)+p(2)%-size(cpx,2)/2*p(1);
phase=pi()*p(1)*linspace(1,size(cpx,2),size(cpx,2)).^2+2*pi*p(2)*linspace(1,size(cpx,2),size(cpx,2));
cpxdop=complex(cos(phase),sin(phase));
fdorig=fd;
% flatten burst
flatcpx=repmat((cpxdop),size(cpx,1),1).*cpx;

% %  check deramping
% dopest=flatcpx(:,1:end-1).*conj(flatcpx(:,2:end));
% n=10;
% mask=ones(n);
% q=conv2(dopest,mask,'same');
% looks=q(1:n:end,1:n:end);
% dopcent=sum(q,1);
% dopcent(end+1)=dopcent(end); %  add a point to make ends match
% dopcent=dopcent./abs(dopcent);
% % unwrap estimates
% fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();

spec=fft(flatcpx,[],2);
npts=size(flatcpx,2);
overspec=complex(zeros(size(flatcpx,1),npts*4),zeros(size(flatcpx,1),npts*4));
overspec(:,1:npts/2)=spec(:,1:npts/2);
overspec(:,npts*4-npts/2+1:npts*4)=spec(:,npts/2+1:npts);
over=ifft(overspec,[],2);

% % reramp
% fdfit=[1:size(over,2)]*p(1)/4+p(2)-size(cpx,2)/2*p(1);
phase=pi()*p(1)/4*linspace(1,size(over,2),size(over,2)).^2+2*pi*p(2)/4*linspace(1,size(over,2),size(over,2));
cpxdop=complex(cos(phase),-sin(phase));
% reramp burst
over=repmat((cpxdop),size(over,1),1).*over;

%  check reramping
dopest=over(:,1:end-1).*conj(over(:,2:end));
n=10;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% unwrap estimates
fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();



% focussed processor
figure(2);
range=800000.;
v=6780;
prf=1685.;
rate=2*v*v/0.0566/range;
npts=size(cpx,2);
% time=linspace(0,(size(cpx,2)-1)/prf,size(cpx,2));
% time=linspace(0,(npts-1)/prf,npts);
time=linspace(-(npts-1)/2/prf,(npts-1)/2/prf,npts);
phase=pi()*rate*time.*time;
ref=complex(cos(phase),sin(phase));
refspec=repmat(fft(ref),size(cpx,1),1);

foc=ifft(refspec.*fft(cpx,[],2),[],2);

%  looks for display
n=5;
mask=ones(n);
q=conv2(abs(foc),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,1);
imagesc(abs(foc)');
colorbar;colormap gray;
ave=mean(mean(abs(foc)));
caxis([0 ave*3]);
title('focussed');
subplot(4,1,2);
imagesc(looks');
colorbar; colormap gray;
ave=mean(mean(looks));
caxis([0 ave*3]);
title('looks');

%%% repeat for oversampled burst
figure(1);
subplot(4,1,3);imagesc(abs(over)'); colorbar; colormap gray;
ave=mean(mean(abs(over)));
caxis([0 ave*3]);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(over),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,4);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

% focussed processor
figure(2);
range=800000.;
v=6780;
prf=1685.*4;
rate=2*v*v/0.0566/range;
npts=size(over,2);
% time=linspace(0,(size(cpx,2)-1)/prf,size(cpx,2));
% time=linspace(0,(npts-1)/prf,npts);
time=linspace(-(npts-1)/2/prf,(npts-1)/2/prf,npts);
phase=-pi()*rate*time.*time;
ref=complex(cos(phase),sin(phase));
refspec=repmat(fft(ref),size(over,1),1);

foc=ifft(refspec.*fft(over,[],2),[],2);

%  looks for display
n=5;
mask=ones(n);
q=conv2(abs(foc),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,3);
imagesc(abs(foc)');
colorbar;colormap gray;
ave=mean(mean(abs(foc)));
caxis([0 ave*3]);
title('focussed');
subplot(4,1,4);
imagesc(looks');
colorbar; colormap gray;
ave=mean(mean(looks));
caxis([0 ave*3]);
title('looks');

