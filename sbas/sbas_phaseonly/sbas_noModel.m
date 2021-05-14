% least squares solution for pixel time series

loaddata=1;
if loaddata == 1
    clear all; close all;
    loaddata=1;
end
iplot=0;
imovie=1-iplot;
nframes=120;

phasemin=-3;
phasemax=3;
ampmin=1000;
ampmax=15000;

%  set up movie frame
figure(10);
set(10,'Position',[100 100 1000 1000]);

if loaddata == 1
    
    % load data
    load dat_looks
    load tsx_info
    dtk=load('timedeltas.out');
    disp('Data loaded');
    cmmin=phasemin*lambda/4/pi;
    cmmax=phasemax*lambda/4/pi;
    
    imask=1;
    mask=ones(nr_s,naz_s);
    if imask == 1
        ind=find(avecc < 0.15);
        mask(ind)=0.4;
    end

% SBAS least squares
    % at Each pixel, we solve for velocity at (n-1) time inverval
    r_ref=[200 350 200 350 200 200 200];   %  reference pixel location
    az_ref=[100 100 800 800 200 800 400];
    refphase=mean(mean(phase_s(r_ref,az_ref,:),1),2);
    velocity=zeros(nr_s,naz_s,n-1);
    Tminv=pinv(Tm);
    for jj=1:naz_s
        disp(jj);
        for ii=1:nr_s
            %d=phase_s(ii,jj,:)-phase_s(r_ref,az_ref,:);
            d=phase_s(ii,jj,:)-refphase;
            d=d(:);
            velocity(ii,jj,:)=Tminv*d;
        end
    end
    save velocity_solution velocity
    % To see the time series at pixel (ix,iy):
    % Define vector dtk = [dt1 dt2 ... dt(n-1)], (n-1) time invervals between
    % the nearest two InSAR acquisition time
    % ix = 50;
    % iy = 50;
    % v = velocity(ix,iy,:);
    % v = v(:);
    % phi=zeros(n,1);
    % for kk=2:n
    %     phi(kk)=phi(kk-1)+dtk(kk-1).*v(kk-1);
    % end
    % plot(phi);
end

% amplitude histogram
histbins=10000;
amp1d=reshape(amp_s,1,size(amp_s,1)*size(amp_s,2));
amphist=hist(amp1d,histbins);
q=cumsum(amphist);
amplow=find(q<q(end)*0.1,1,'first')/histbins*max(amp1d);
amphi=find(q<q(end)*0.9,1,'last')/histbins*max(amp1d);

if iplot == 1
    % plot images and look up pixels
    stack=sum(phase_s,3);
    figure(2);
    subplot(1,2,1);
    imagesc(stack');
    axis image;
    subplot(1,2,2);
    amps=amp_s'.*mask';
    amps(amps<amplow)=amplow;
    amps(amps>amphi)=amphi;
    bw(:,:,1)=(amps-amplow)/(amphi-amplow);
    bw(:,:,2)=bw(:,:,1);
    bw(:,:,3)=bw(:,:,1);
    %imagesc(amp_s'.*mask');
    imagesc(bw);
    if min(min(amp_s)) < max(max(amp_s))/8        
        caxis([amplow amphi]);
    end
    axis image;
    print -depsc 'location_map.eps'
    for i=1:1000
%         figure(2);
%         [x,y]=ginput(1);
%         v=velocity(round(x),round(y),:);
%         v=v(:);
%         phi=zeros(n,1);
%         for kk=2:n
%             phi(kk)=phi(kk-1)+dtk(kk-1).*v(kk-1);
%         end
%         figure(3);
%         xx=linspace(0,sum(dtk),n);
%         xx(1)=0;
%         xx(2:n)=cumsum(dtk);
%         scatter(xx,phi);
%         axis([0 sum(dtk) phasemin phasemax]);
%         title(['Location: ' num2str(round(x)) ' ' num2str(round(y))]);
        figure(2);
        [x,y]=ginput(1);
        if x < 1 || y < 1 || x > nr_s || y > naz_s
            return
        end
        v=velocity(round(x),round(y),:);
        v=v(:);
        phi=zeros(n,1);
        for kk=2:n
            phi(kk)=phi(kk-1)+dtk(kk-1).*v(kk-1);
        end
        filename=strcat('totaldef',num2str(round(x)),'_',num2str(round(y)),'.eps');
        figure(3);
        xx=linspace(0,sum(dtk),n);
        xx(1)=0;
        xx(2:n)=cumsum(dtk);
        scatter(xx,phi*lambda/4/pi);
        axis([0 sum(dtk) cmmin cmmax]);
        title(['Location: ' num2str(round(x)) ' ' num2str(round(y))]);
        print('-depsc', filename);
        % secular vs annual
        p=polyfit(xx,phi'*lambda/4/pi,1);
        filename=strcat('componentsdef',num2str(round(x)),'_',num2str(round(y)),'.eps');
        figure(4);
        set(4,'Position',[500 500 800 300]);
        set(4,'PaperPosition',[1 1 8 3]);
        scatter(xx,p(1)*xx+p(2),'g','filled');
        axis([0 sum(dtk) cmmin cmmax]);
        hold on;
        title(['Location: ' num2str(round(x)) ' ' num2str(round(y))]);
        scatter(xx,phi'*lambda/4/pi-p(1)*xx-p(2),'r','filled');
        axis([0 sum(dtk) cmmin cmmax]);
        hold off;
        legend('Secular deformation','Transient deformation','Location','NorthWest');
        print('-depsc', filename);
 end
    
end

if imovie == 1
    %  make a movie
    tmin=0;
    tmax=sum(dtk);
    xx(1)=0;
    xx(2:n)=cumsum(dtk);
    figure(10);
    
    % create a color table
    colormap default;
    map=colormap;
    
    % integrate the velocities first
    phi=zeros(nr_s,naz_s,n);
    for kk=2:n
        phi(:,:,kk)=phi(:,:,kk-1)+dtk(kk-1)*velocity(:,:,kk-1);
    end
    
    amp=max(amp_s,ampmin);
    amp=min(amp_s,ampmax);
    % phimin=-6;
    % phimax=12;
    phimin=phasemin;
    phimax=phasemax;
    
    scale=1;
    if min(min(amp_s)) < max(max(amp_s))/8        
         scale=((amp-amplow))/(amphi-amplow);
    end
    scale(scale<0)=0;
    scale(scale>1)=1;
    
    clear M;
    kframe=0;
    for i=1:nframes
        t=i/nframes*tmax
        for j=1:length(xx)
            if t >= xx(j)
                f1=j;
            end
        end
        f2=min(f1+1,length(xx));
        phi1=phi(:,:,f1);
        phi2=phi(:,:,f2);
        if f1 == f2
            frac=0;
        else
            frac=(t-xx(f1))/(xx(f2)-xx(f1));
        end
        phiframe=phi1*(1-frac)+phi2*frac;
        phiframe=max(phiframe,phimin);
        phiframe=min(phiframe,phimax);
        colorframe=round((phiframe-phimin)/(phimax-phimin)*64);
        colorframe=max(colorframe,1);
        colorframe=min(colorframe,64);
        for k=1:nr_s
            for kk=1:naz_s
                red(k,kk)=map(colorframe(k,kk),1);
                green(k,kk)=map(colorframe(k,kk),2);
                blue(k,kk)=map(colorframe(k,kk),3);
            end
        end
        pic(:,:,1)=red.*mask.*scale;
        pic(:,:,2)=green.*mask.*scale;
        pic(:,:,3)=blue.*mask.*scale;
        for m=1:naz_s
            picpic(:,m,:)=pic(:,naz_s+1-m,:);
        end
        %colorframe=colorframe.*amp_s./ampmax;
        figure(10);
        image(flipdim(flipdim(imrotate(picpic,-90),1),2));
        %image(picpic);
        %caxis([-30 110]);
        axis image;
        M(i)=getframe;
        %     aviobj=addframe(aviobj,M(i));
        if mod(i,nframes/12) == 0
            figure(11);
            kframe=kframe+1;
            subplot(3,4,kframe);
            image(flipdim(imrotate(picpic,-90),1));
            axis image;
            axis off;
        end
        
    end
    % aviobj=close(aviobj);
    mpgwrite(M, map, 'slv.mpg');
    figure(11);
    print -depsc 'slv_time_series.eps'
    
end


