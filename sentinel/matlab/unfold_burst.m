% unfold a sentinel burst
clear all;close all;

% we are going to need the centroid rate so estimate it here
prf=1685;
v=6780;

%  read a burst
fp=fopen('burst8','r');
data=fread(fp,[17301*2 inf],'float32');
cpx=data(20001:2:end,:)+j*data(20002:2:end,:);
cpx(1:9,:)=complex(0.,0.);
subplot(4,1,1);imagesc(abs(cpx)'); colorbar; colormap gray;
ave=mean(mean(abs(cpx)));
caxis([0 ave*3]);

npts=size(cpx,2);
ns=size(cpx,1);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(cpx),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

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
p=polyfit([21:size(cpx,2)-20],fd(21:size(cpx,2)-20),1); % p(1) is rate in cycles per line
ks=p(1)*prf*prf; % ks is rate in Hz/s


% Step 1. create an oversampled version of the input data 
spec=fft(cpx,[],2);  % transform in azimuth
overspec=complex(zeros(ns,npts*3),zeros(ns,npts*3));
overspec(:,1:npts/2)=spec(:,1:npts/2);
overspec(:,npts*3-npts/2+1:npts*3)=spec(:,npts/2+1:npts);
%  plot oversampled spectrum
subplot(4,1,3);imagesc(abs(overspec)'); colorbar; colormap gray;
ave=mean(mean(abs(overspec)));
caxis([0 ave*3]);
title('oversampled spectrum');

% Step 2. replicate spectrum for three copies
overspec(:,npts/2+1:npts)=spec(:,npts/2+1:npts); % piece 2 first copy
overspec(:,npts+1:npts+npts/2)=spec(:,1:npts/2); % piece 1 first copy

overspec(:,npts*3-npts-npts/2+1:npts*3-npts)=spec(:,npts/2+1:npts); % piece 2 second copy
overspec(:,npts*3-npts+1:npts*3-npts/2)=spec(:,1:npts/2); % piece 1 second copy
%  plot replicated spectrum
subplot(4,1,4);imagesc(abs(overspec)'); colorbar; colormap gray;
ave=mean(mean(abs(overspec)));
caxis([0 ave*3]);
title('replicated spectrum');

% fprintf('boundaries %d %d %d %d %d %d\n',1,npts/2,npts/2+1,npts,npts+1,npts+npts/2);
% fprintf('boundaries %d %d %d %d %d %d\n',npts*3-npts-npts/2+1,npts*3-npts,npts*3-npts+1,npts*3-npts/2,npts*3-npts/2+1,npts*3);

% and back to time domain
timedomain=ifft(overspec,[],2);
figure(2);
subplot(4,1,1);imagesc(abs(timedomain)'); colorbar; colormap gray;
ave=mean(mean(abs(timedomain)));
caxis([0 ave*3]);
title('replicated spectrum in time domain');

% Step 3. deramp
hiprf=prf*3;
time=linspace(0,3*npts/hiprf-1/hiprf,3*npts)-3*npts/2/hiprf;  % time centered in middle of burst
phase=pi()*ks*time.^2;
deramp=complex(cos(phase),sin(phase));
deramped=repmat(deramp,ns,1).*timedomain;  % deramp burst in azimuth spectral domain
%  plot deramped spectrum
figure(2);
subplot(4,1,2);imagesc(abs(deramped)'); colorbar; colormap gray;
ave=mean(mean(abs(deramped)));
caxis([0 ave*3]);
title('deramped');

% Step 4. Limit extent in time domain
ka=-2*v*v/0.0566/900000.;
baz=ka/(ks-ka)*prf;
taz=-baz/ka;
tazprime=-prf/ks;
bazprime=-tazprime*ka;
aperturetime=npts/prf;
nfilt=tazprime/aperturetime*npts*4;
nfilt2=floor(nfilt/2);

limit=time;
limit(abs(time)<taz)=1;
limit(abs(time)>=taz)=0;
istart=find(limit(2:end)-limit(1:end-1)==1);
istop=find(limit(2:end)-limit(1:end-1)==-1);


timedomain(:,1:istart)=complex(0.,0.);
timedomain(:,istop:end)=complex(0.,0.);

subplot(4,1,3);imagesc(abs(timedomain)'); colorbar; colormap gray;
ave=mean(mean(abs(timedomain)));
caxis([0 ave*3]);
title('time gated');

% Step 5. reramp
reramped=repmat(conj(deramp),ns,1).*deramped;  % reramp burst in azimuth spectral domain
% subplot(4,1,4);imagesc(abs(reramped)'); colorbar; colormap gray;
% ave=mean(mean(abs(reramped)));
% caxis([0 ave*3]);
% title('ramp put back')

%  and back to the time domain
newburst=ifft(reramped,[],2);
%  plot new burst
subplot(4,1,4);imagesc(abs(newburst)'); colorbar; colormap gray;
ave=mean(mean(abs(newburst)));
caxis([0 ave*3]);
title('new burst')

fpout=fopen('unfolded','w');
clear data;
data(1:2:ns*2,:)=real(newburst);
data(2:2:ns*2,:)=imag(newburst);
count=fwrite(fpout,data,'float32')
fclose(fpout);


% over=ifft(overspec,[],2);
% 
% 
% 
% 
% 
% % deramp
% % estimate Doppler centroid 
% dopest=cpx(:,1:end-1).*conj(cpx(:,2:end));
% n=10;
% mask=ones(n);
% q=conv2(dopest,mask,'same');
% looks=q(1:n:end,1:n:end);
% dopcent=sum(q,1);
% dopcent(end+1)=dopcent(end); %  add a point to make ends match
% dopcent=dopcent./abs(dopcent);
% % unwrap estimates
% fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();
% % fit line to these
% p=polyfit([21:size(cpx,2)-20],fd(21:size(cpx,2)-20),1);
% % fdfit=[1:size(cpx,2)]*p(1)+p(2)%-size(cpx,2)/2*p(1);
% phase=pi()*p(1)*linspace(1,size(cpx,2),size(cpx,2)).^2+2*pi*p(2)*linspace(1,size(cpx,2),size(cpx,2));
% cpxdop=complex(cos(phase),sin(phase));
% fdorig=fd;
% % flatten burst
% flatcpx=repmat((cpxdop),size(cpx,1),1).*cpx;
% 
% % %  check deramping
% % dopest=flatcpx(:,1:end-1).*conj(flatcpx(:,2:end));
% % n=10;
% % mask=ones(n);
% % q=conv2(dopest,mask,'same');
% % looks=q(1:n:end,1:n:end);
% % dopcent=sum(q,1);
% % dopcent(end+1)=dopcent(end); %  add a point to make ends match
% % dopcent=dopcent./abs(dopcent);
% % % unwrap estimates
% % fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();
% 
% spec=fft(flatcpx,[],2);
% npts=size(flatcpx,2);
% overspec=complex(zeros(size(flatcpx,1),npts*4),zeros(size(flatcpx,1),npts*4));
% overspec(:,1:npts/2)=spec(:,1:npts/2);
% overspec(:,npts*4-npts/2+1:npts*4)=spec(:,npts/2+1:npts);
% over=ifft(overspec,[],2);
% 
% % % reramp
% % fdfit=[1:size(over,2)]*p(1)/4+p(2)-size(cpx,2)/2*p(1);
% phase=pi()*p(1)/4*linspace(1,size(over,2),size(over,2)).^2+2*pi*p(2)/4*linspace(1,size(over,2),size(over,2));
% cpxdop=complex(cos(phase),-sin(phase));
% % reramp burst
% over=repmat((cpxdop),size(over,1),1).*over;
% 
% %  check reramping
% dopest=over(:,1:end-1).*conj(over(:,2:end));
% n=10;
% mask=ones(n);
% q=conv2(dopest,mask,'same');
% looks=q(1:n:end,1:n:end);
% dopcent=sum(q,1);
% dopcent(end+1)=dopcent(end); %  add a point to make ends match
% dopcent=dopcent./abs(dopcent);
% % unwrap estimates
% fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();
% 
% 
% 
% % focussed processor
% figure(2);
% range=800000.;
% v=6780;
% prf=1685.;
% rate=2*v*v/0.0566/range;
% npts=size(cpx,2);
% % time=linspace(0,(size(cpx,2)-1)/prf,size(cpx,2));
% % time=linspace(0,(npts-1)/prf,npts);
% time=linspace(-(npts-1)/2/prf,(npts-1)/2/prf,npts);
% phase=pi()*rate*time.*time;
% ref=complex(cos(phase),sin(phase));
% refspec=repmat(fft(ref),size(cpx,1),1);
% 
% foc=ifft(refspec.*fft(cpx,[],2),[],2);
% 
% %  looks for display
% n=5;
% mask=ones(n);
% q=conv2(abs(foc),mask,'same');
% looks=q(1:n:end,1:n:end);
% subplot(4,1,1);
% imagesc(abs(foc)');
% colorbar;colormap gray;
% ave=mean(mean(abs(foc)));
% caxis([0 ave*3]);
% title('focussed');
% subplot(4,1,2);
% imagesc(looks');
% colorbar; colormap gray;
% ave=mean(mean(looks));
% caxis([0 ave*3]);
% title('looks');
% 
% %%% repeat for oversampled burst
% figure(1);
% subplot(4,1,3);imagesc(abs(over)'); colorbar; colormap gray;
% ave=mean(mean(abs(over)));
% caxis([0 ave*3]);
% 
% % take some looks
% n=10;
% mask=ones(n);
% q=conv2(abs(over),mask,'same');
% looks=q(1:n:end,1:n:end);
% subplot(4,1,4);imagesc(looks'); colorbar; colormap gray;
% ave=mean(mean(abs(looks)));
% caxis([0 ave*3]);
% 
% % focussed processor
% figure(2);
% range=800000.;
% v=6780;
% prf=1685.*4;
% rate=2*v*v/0.0566/range;
% npts=size(over,2);
% % time=linspace(0,(size(cpx,2)-1)/prf,size(cpx,2));
% % time=linspace(0,(npts-1)/prf,npts);
% time=linspace(-(npts-1)/2/prf,(npts-1)/2/prf,npts);
% phase=-pi()*rate*time.*time;
% ref=complex(cos(phase),sin(phase));
% refspec=repmat(fft(ref),size(over,1),1);
% 
% foc=ifft(refspec.*fft(over,[],2),[],2);
% 
% %  looks for display
% n=5;
% mask=ones(n);
% q=conv2(abs(foc),mask,'same');
% looks=q(1:n:end,1:n:end);
% subplot(4,1,3);
% imagesc(abs(foc)');
% colorbar;colormap gray;
% ave=mean(mean(abs(foc)));
% caxis([0 ave*3]);
% title('focussed');
% subplot(4,1,4);
% imagesc(looks');
% colorbar; colormap gray;
% ave=mean(mean(looks));
% caxis([0 ave*3]);
% title('looks');
% 
