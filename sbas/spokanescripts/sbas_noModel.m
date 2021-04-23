% least squares solution for pixel time series
clear all; close all;

loaddata=1;
if loaddata == 1
    clear all; close all;
    loaddata=1;
end
iplot=0;
imovie=1-iplot;
nframes=120;

phasemin=-60;   %   for plot and movie scaling aesthetics
phasemax=60;

%  set up movie frame
% figure(10);
% set(10,'Position',[100 100 1000 1000]);

if loaddata == 1
    
    % load data
    load sbas_data
    load sbas_info
    dtk=load('timedeltas.out');
    disp('Data loaded');
    cmmin=phasemin*lambda/4/pi;
    cmmax=phasemax*lambda/4/pi;
    
    imask=1;
    if imask == 0
        mask=1;
    end

% SBAS least squares
    % at Each pixel, we solve for velocity at (n-1) time inverval
    r_ref=[1087 1100 740 762 756];   %  reference pixel location
    az_ref=[445 444 387 381 356];
    refphase=mean(mean(phase(r_ref,az_ref,:),1),2);
    velocity=zeros(nr,naz,n-1);
    Tminv=pinv(Tm);
    for jj=1:naz
        %disp(jj);
        for ii=1:nr
            d=phase(ii,jj,:)-refphase;
            d=d(:);
            velocity(ii,jj,:)=Tminv*d;
        end
    end
    save velocity_solution velocity
    % To see the time series at pixel (ix,iy):
    % Define vector dtk = [dt1 dt2 ... dt(n-1)], (n-1) time invervals between
    % the nearest two InSAR acquisition time
end

% amplitude histogram
histbins=1000;
amp1d=reshape(amp,1,size(amp,1)*size(amp,2));
[amphist,hbins]=hist(amp1d,histbins);
q=cumsum(amphist);
% amplow=find(q<q(end)*0.1,1,'first')/histbins*max(amp1d);
% amphi=find(q<q(end)*0.9,1,'last')/histbins*max(amp1d);
amplow=hbins(find(q<q(end)*0.1,1,'first'));
amphi=hbins(find(q<q(end)*0.9,1,'last'));
amps=amp';
amps(amps<amplow)=amplow;
amps(amps>amphi)=amphi;
scale=(amps-amplow)/(amphi-amplow);

if iplot == 1
    % plot images and look up pixels
    stack=(sum(phase,3)-sum(refphase,3))/N;
    stack1d=reshape(stack,nr*naz,1);
    [phhist,phbins]=hist(stack1d,100);
    q=cumsum(phhist);
    phlow=phbins(find(q<q(end)*0.01,1,'first'));
    phhi=phbins(find(q<q(end)*0.99,1,'last'))*2;
    figure(2);
    subplot(2,1,1);
    % create a color table
    colormap default;
    map=colormap;
    stack=max(stack,phlow);
    stack=min(stack,phhi);
    colorstack=round((stack-phlow)/(phhi-phlow)*64);
    colorstack=max(colorstack,1);
    colorstack=min(colorstack,64);
    for k=1:nr
        for kk=1:naz
            red(k,kk)=map(colorstack(k,kk),1);
            green(k,kk)=map(colorstack(k,kk),2);
            blue(k,kk)=map(colorstack(k,kk),3);
        end
    end
%     pic(:,:,1)=red'.*mask'.*scale;
%     pic(:,:,2)=green'.*mask'.*scale;
%     pic(:,:,3)=blue'.*mask'.*scale;
    pic(:,:,1)=red'.*scale;
    pic(:,:,2)=green'.*scale;
    pic(:,:,3)=blue'.*scale;
    imagesc(pic);
    axis image;
    subplot(2,1,2);
    bw(:,:,1)=(amps-amplow)/(amphi-amplow);
    bw(:,:,2)=bw(:,:,1);
    bw(:,:,3)=bw(:,:,1);
    imagesc(bw);
    if min(min(amp)) < max(max(amp))/8        
        caxis([amplow amphi]);
    end
    axis image;
    print -depsc 'location_map.eps'
    for i=1:1000
        figure(2);
        [x,y]=ginput(1);
        if x < 1 || y < 1 || x > nr || y > naz
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
        set(4,'Position',[500 500 800 600]);
        set(4,'PaperPosition',[1 1 8 6]);
        subplot(2,1,2);
        scatter(xx,p(1)*xx+p(2),'g','filled');
        axis([0 sum(dtk) cmmin cmmax]);
        hold on;
        title(['Location: ' num2str(round(x)) ' ' num2str(round(y))]);
        scatter(xx,phi'*lambda/4/pi-p(1)*xx-p(2),'r','filled');
        axis([0 sum(dtk) cmmin cmmax]);
        hold off;
        legend('Secular deformation','Transient deformation','Location','NorthWest');
        subplot(2,1,1);
        scatter(xx,phi*lambda/4/pi,'b','filled');
        legend('Total deformation','Location','NorthWest');
        axis([0 sum(dtk) cmmin cmmax]);
        hold off;
        print('-depsc', filename);
 end
    
end

if imovie == 1
    %  make a movie
    tmin=0;
    tmax=sum(dtk);
    xx(1)=0;
    xx(2:n)=cumsum(dtk);
%     figure(10);
    
    % create a color table
    colormap default;
    map=colormap;
    
    % integrate the velocities first
    phi=zeros(nr,naz,n);
    for kk=2:n
        phi(:,:,kk)=phi(:,:,kk-1)+dtk(kk-1)*velocity(:,:,kk-1);
    end
    
    phimin=phasemin;
    phimax=phasemax;
        
    v =VideoWriter('sbas.mp4');
    v.Quality=50;
    open(v);
    
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
%         for k=1:1;%nr
% %             for kk=1:naz
% %                 red(k,kk)=map(colorframe(k,kk),1);
% %                 green(k,kk)=map(colorframe(k,kk),2);
% %                 blue(k,kk)=map(colorframe(k,kk),3);
% %             end
%             %for kk=1:naz
%                 red(k,:)=map(colorframe(k,:),1);
%                 green(k,:)=map(colorframe(k,:),2);
%                 blue(k,:)=map(colorframe(k,:),3);
%             %end
%         end
%         pic(:,:,1)=red'.*scale;
%         pic(:,:,2)=green'.*scale;
%         pic(:,:,3)=blue'.*scale;
        pic(:,:,1)=scale.*reshape(map(colorframe(:,:),1),nr,naz)';
        pic(:,:,2)=scale.*reshape(map(colorframe(:,:),2),nr,naz)';
        pic(:,:,3)=scale.*reshape(map(colorframe(:,:),3),nr,naz)';
%         figure(10);
% %         image(flipdim(flipdim(imrotate(picpic,-90),1),2));
%         image(pic);
%         axis image;
        M(i)=im2frame(pic); % getframe;
        writeVideo(v,pic);
        %     aviobj=addframe(aviobj,M(i));
        if mod(i,nframes/12) == 0
            figure(11);
            kframe=kframe+1;
            subplot(3,4,kframe);
            %image(flipdim(imrotate(picpic,-90),1));
            image(pic);
            axis image;
            axis off;
        end
        
    end
    % aviobj=close(aviobj);
    close(v);
    mpgwrite(M, map, 'sbas.mpg');
    figure(11);
    print -depsc 'sbas_time_series.eps'
    figure(10);
    image(pic);
    axis image;
    print -depsc 'cumulative_deformation.eps'
    
end


