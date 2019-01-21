function topics 
%TOPICS Initial conditions for tsunami simulations.
%  TOPICS starts a GUI for the creation of bathymetry and initial
%  conditions for a wave simulation such as FUNWAVE
%

%%%% Load ETOPO1

    h = waitbar(0,'Loading ETOPO1...');
    ncid = netcdf.open('../bathy/ETOPO1_Ice_c_gmt4.grd','NC_NOWRITE');
    varid = netcdf.inqVarID(ncid,'x');
    lon = netcdf.getVar(ncid,varid);
    waitbar(1/3);
    varid = netcdf.inqVarID(ncid,'y');
    lat = netcdf.getVar(ncid,varid);
    waitbar(2/3);
    varid = netcdf.inqVarID(ncid,'z');
    z = netcdf.getVar(ncid,varid);
    waitbar(3/3);
    netcdf.close(ncid);
    close(h);

%%%% Variable declarations and default values

    xres = 400; yres = 300;
    sVal =  10; sScale = 1; nVal =  50; nScale = 1;
    wVal = -85; wScale = 1; eVal =  -60; eScale = 1;

    resVal = 8; resScale = 1/60;
    Mglob = 400; Nglob = 300;
    
    mypath = '/Users/fengyanshi/work/TOPICS/result/';

    % coseismic parameters
    xoVal = -63.88; xoScale = 1;
    yoVal = 18.887; yoScale = 1;
    strike = 95.37; dip = 20; rake = 90;
    depthVal = 21.1; depthScale = 1e3;
    lengthVal = 100; lengthScale = 1e3;
    widthVal = 50; widthScale = 1e3;
    slip = 1;

    % landslide parameters
    
    % slump parameters
    
    f = figure('Visible','off','Position',[360,500,1024,768]);
    set(f,'Toolbar','none');    % Hide the standard toolbar
    set(f,'MenuBar','none');    % Hide standard menu bar menus.

%%%% GUI setup
    
    % Plotting Panel
    phA = uipanel('Parent',f,'Title','',...
            'Position',[.05,.3,.6,.65]);
    ha = axes('Parent',phA,'Units','normalized','Position',[.1,.2,.85,.75]); 
    hpopupPT = uicontrol(phA,'Style','popupmenu',...
          'String',{'Bathymetry','Eta','U','V'},...
          'Units','normalized',...
          'Position',[.2,.02,.3,.05],...
          'Callback',{@touch_Callback});
    hpopupPS = uicontrol(phA,'Style','popupmenu',...
          'String',{'Entire world','Computational domain','Source region'},'Value',2,...
          'Units','normalized',...
          'Position',[.55,.02,.3,.05],...
          'Callback',{@touch_Callback});
   
    % Save Panel
    phF = uipanel('Parent',f,'Title','Save data',...
            'Position',[.05,.05,.9,.2]);
    uicontrol(phF,'Style','pushbutton','String','Save Data',...
          'Units','normalized',...
          'Position',[.03,.4,.1,.2],...
          'Callback',{@save_Callback});
    uicontrol(phF,'Style','pushbutton','String','Make Figures',...
          'Units','normalized',...
          'Position',[.03,.1,.1,.2],...
          'Callback',{@figures_Callback});
    uicontrol(phF,'Style','popupmenu',...
          'String',{'ASCII'},...
          'Units','normalized',...
          'Position',[.15,.4,.1,.2],...
          'Callback',{@popup_menu_Callback});
    hfile=uicontrol(phF,'Style','popupmenu',...
          'String',{'PDF','PNG','JPEG'},...
          'Units','normalized',...
          'Position',[.15,.1,.1,.2]);
    uicontrol(phF,'Style','pushbutton','String','Set Path:',...
          'Units','normalized',...
          'Position',[.55,.7,.1,.2],...
          'Callback',{@getpath_Callback});
    heditP = uicontrol(phF,'Style','edit','String',mypath,...
          'Units','normalized','Position',[.67,.7,.3,.2],...
          'Callback',{@setpath_Callback});
    uicontrol(phF,'Style','text','String','Depth',...
          'Units','normalized','Position',[.3,.7,.05,.2]);
    uicontrol(phF,'Style','text','String','Eta',...
          'Units','normalized','Position',[.35,.7,.05,.2]);
    uicontrol(phF,'Style','text','String','U',...
          'Units','normalized','Position',[.4,.7,.05,.2]);
    uicontrol(phF,'Style','text','String','V',...
          'Units','normalized','Position',[.45,.7,.05,.2]);
    hds=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',1,'Position',[.3 .4 .1 .1]);
    hhs=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',1,'Position',[.35 .4 .1 .1]);
    hus=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',1,'Position',[.4 .4 .1 .1]);
    hvs=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',1,'Position',[.45 .4 .1 .1]);
    hdf=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',1,'Position',[.3 .1 .1 .1]);
    hhf=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',1,'Position',[.35 .1 .1 .1]);
    huf=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',0,'Position',[.4 .1 .1 .1]);
    hvf=uicontrol(phF,'Style','checkbox',...
                'Units','normalized',...
                'Value',0,'Position',[.45 .1 .1 .1]);
    uicontrol(phF,'Style','text','String','Resolution:',...
          'Units','normalized','Position',[.6,.4,.1,.2]);
    hresVal = uicontrol(phF,'Style','edit','String',num2str(resVal),...
          'Units','normalized','Position',[.7,.4,.05,.2],...
          'Callback',{@touch_Callback});
    hresScale = uicontrol(phF,'Style','popupmenu',...
          'String',{'Degrees','Minutes','Seconds'},...
          'Value',2,'Units','normalized',...
          'Position',[.8,.4,.12,.2],...
          'Callback',{@touch_Callback});
    uicontrol(phF,'Style','text','String','Grid size:',...
          'Units','normalized','Position',[.6,.1,.1,.2]);
    hMglob = uicontrol(phF,'Style','text','String',num2str(Mglob),...
          'Units','normalized','Position',[.7,.1,.1,.2]);
    hNglob = uicontrol(phF,'Style','text','String',num2str(Nglob),...
          'Units','normalized','Position',[.8,.1,.1,.2]);

    % Source Panel
    phS = uipanel('Parent',f,'Title','Source Parameters',...
            'Position',[.7,.3,.25,.65]);
    uicontrol(phS,'Style','text','String','Domain:',...
          'Units','normalized','Position',[.05,.9,.3,.05]);
    hnVal = uicontrol(phS,'Style','edit','String',num2str(nVal),...
          'Units','normalized','Position',[.35,.9,.2,.05],...
          'Callback',{@touch_Callback});
    hsVal = uicontrol(phS,'Style','edit','String',num2str(sVal),...
          'Units','normalized','Position',[.35,.8,.2,.05],...
          'Callback',{@touch_Callback});
    heVal = uicontrol(phS,'Style','edit','String',num2str(eVal),...
          'Units','normalized','Position',[.55,.85,.2,.05],...
          'Callback',{@touch_Callback});
    hwVal = uicontrol(phS,'Style','edit','String',num2str(wVal),...
          'Units','normalized','Position',[.15,.85,.2,.05],...
          'Callback',{@touch_Callback});
    hnScale = uicontrol(phS,'Style','popupmenu',...
          'String',{'N','S'},...
          'Units','normalized','Position',[.55,.9,.25,.05],...
          'Callback',{@touch_Callback});
    hsScale = uicontrol(phS,'Style','popupmenu',...
          'String',{'N','S'},...
          'Units','normalized','Position',[.55,.8,.25,.05],...
          'Callback',{@touch_Callback});
    hwScale = uicontrol(phS,'Style','popupmenu',...
          'String',{'E','W'},...
          'Units','normalized','Position',[.35,.85,.25,.05],...
          'Callback',{@touch_Callback});
    heScale = uicontrol(phS,'Style','popupmenu',...
          'String',{'E','W'},...
          'Units','normalized','Position',[.75,.85,.25,.05],...
          'Callback',{@touch_Callback});
    uicontrol(phS,'Style','text','String','Tsunami type:',...
          'Units','normalized','Position',[.1,.7,.4,.05]);
    hpopupR = uicontrol(phS,'Style','popupmenu',...
          'String',{'Coseismic','Landslide','Slump'},...
          'Units','normalized',...
          'Position',[.5,.7,.45,.05],...
          'Callback',{@popup_type_Callback});

    hyoLabel=uicontrol(phS,'Style','text','String','Latitude',...
          'Units','normalized','Position',[.05,.65,.4,.05]);
    hyoVal=uicontrol(phS,'Style','edit','String',num2str(yoVal),...
          'Units','normalized','Position',[.45,.65,.3,.05],...
          'Callback',{@touch_Callback});
    hyoScale=uicontrol(phS,'Style','popupmenu',...
          'String',{'N','S'},...
          'Units','normalized','Position',[.75,.65,.25,.05],...
          'Callback',{@touch_Callback});
    hxoLabel=uicontrol(phS,'Style','text','String','Longitude',...
          'Units','normalized','Position',[.05,.6,.4,.05]);
    hxoVal=uicontrol(phS,'Style','edit','String',num2str(xoVal),...
          'Units','normalized','Position',[.45,.6,.3,.05],...
          'Callback',{@touch_Callback});
    hxoScale=uicontrol(phS,'Style','popupmenu',...
          'String',{'E','W'},...
          'Units','normalized','Position',[.75,.60,.25,.05],...
          'Callback',{@touch_Callback});
    hlengthLabel=uicontrol(phS,'Style','text','String','Length',...
          'Units','normalized','Position',[.05,.55,.4,.05]);
    hlengthVal=uicontrol(phS,'Style','edit','String',num2str(lengthVal),...
          'Units','normalized','Position',[.45,.55,.25,.05],...
          'Callback',{@touch_Callback});
    hlengthScale=uicontrol(phS,'Style','popupmenu',...
          'String',{'km','m'},...
          'Units','normalized','Position',[.7,.55,.3,.05],...
          'Callback',{@touch_Callback});
    hwidthLabel=uicontrol(phS,'Style','text','String','Width',...
          'Units','normalized','Position',[.05,.5,.4,.05]);
    hwidthVal=uicontrol(phS,'Style','edit','String',num2str(widthVal),...
          'Units','normalized','Position',[.45,.5,.25,.05],...
          'Callback',{@touch_Callback});
    hwidthScale=uicontrol(phS,'Style','popupmenu',...
          'String',{'km','m'},...
          'Units','normalized','Position',[.7,.5,.3,.05],...
          'Callback',{@popup_sScale_Callback});      
    hstrikeLabel=uicontrol(phS,'Style','text','String','Strike',...
          'Units','normalized','Position',[.05,.45,.4,.05]);
    hstrike=uicontrol(phS,'Style','edit','String',num2str(strike),...
          'Units','normalized','Position',[.45,.45,.25,.05],...
          'Callback',{@touch_Callback});
    hstrikeScale=uicontrol(phS,'Style','text','String','degrees',...
          'Units','normalized','Position',[.7,.45,.3,.05]);
    hdipLabel=uicontrol(phS,'Style','text','String','Dip',...
          'Units','normalized','Position',[.05,.4,.4,.05]);
    hdip=uicontrol(phS,'Style','edit','String',num2str(dip),...
          'Units','normalized','Position',[.45,.4,.25,.05],...
          'Callback',{@touch_Callback});
    hdipScale=uicontrol(phS,'Style','text','String','degrees',...
          'Units','normalized','Position',[.7,.4,.3,.05]);      
    hdepthLabel=uicontrol(phS,'Style','text','String','Depth',...
          'Units','normalized','Position',[.05,.35,.4,.05]);
    hdepthVal=uicontrol(phS,'Style','edit','String',num2str(depthVal),...
          'Units','normalized','Position',[.45,.35,.25,.05],...
          'Callback',{@touch_Callback});
    hdepthScale=uicontrol(phS,'Style','popupmenu',...
          'String',{'km','m'},...
          'Units','normalized','Position',[.7,.35,.3,.05],...
          'Callback',{@touch_Callback});
    hslipLabel=uicontrol(phS,'Style','text','String','Slip',...
          'Units','normalized','Position',[.05,.3,.4,.05]);
    hslip=uicontrol(phS,'Style','edit','String',num2str(slip),...
          'Units','normalized','Position',[.45,.3,.25,.05],...
          'Callback',{@touch_Callback});
    hslipScale=uicontrol(phS,'Style','text','String','m',...
          'Units','normalized','Position',[.7,.3,.3,.05]);
    hrakeLabel=uicontrol(phS,'Style','text','String','Rake',...
          'Units','normalized','Position',[.05,.25,.4,.05]);
    hrake=uicontrol(phS,'Style','edit','String',num2str(rake),...
          'Units','normalized','Position',[.45,.25,.25,.05],...
          'Callback',{@touch_Callback});
    hrakeScale=uicontrol(phS,'Style','text','String','degrees',...
          'Units','normalized','Position',[.7,.25,.3,.05]);
    hcoseismic = [hyoLabel hyoVal hyoScale hxoLabel hxoVal hxoScale ...
        hlengthLabel hlengthVal hlengthScale ...
        hwidthLabel hwidthVal hwidthScale ...
        hstrikeLabel hstrike hstrikeScale ...
        hdipLabel hdip hdipScale ...
        hdepthLabel hdepthVal hdepthScale ...
        hslipLabel hslip hslipScale ...
        hrakeLabel hrake hrakeScale];
   
%%%% GUI initialization
      
    update_axes();
    set(f,'Name','TOPICS')
    movegui(f,'center')
    set(f,'Visible','on');
    
%%%% GUI callbacks (source of all interactivity)

    function touch_Callback(source,eventdata)
        update_vars;
        update_axes;
    end
    function popup_type_Callback(source,eventdata) 
        str = get(source, 'String');
        val = get(source,'Value');
        switch str{val};
        case 'Coseismic'
            set(hcoseismic,'visible','on');
        case 'Landslide'
            set(hcoseismic,'visible','off');
        case 'Slump'
            set(hcoseismic,'visible','off');
        end
    end  
    function getpath_Callback(source,eventdata)
        mypath = uigetdir(mypath);
        set(heditP,'String',mypath);
    end
    function setpath_Callback(source,eventdata)
        mypath = get(source,'String');
    end

    function save_Callback(source,eventdata)
        my_x = linspace(wVal*wScale+resVal*resScale/2,...
                        eVal*eScale-resVal*resScale/2,Mglob);
        my_y = linspace(sVal*sScale+resVal*resScale/2,...
                        nVal*nScale-resVal*resScale/2,Nglob);
        if get(hds,'Value')
            xinit = find(lon<my_x(1),1,'last');
            if isempty(xinit),xinit=1;end;
            xskip = max(floor((my_x(2)-my_x(1))/(lon(2)-lon(1))),1);
            xfini = find(lon>my_x(end),1,'first');
            if isempty(xfini)
                xfini=length(lon);
            end;
            yinit = find(lat<my_y(1),1,'last');
            if isempty(yinit),yinit=1;end;
            yskip = max(floor((my_y(2)-my_y(1))/(lat(2)-lat(1))),1);
            yfini = find(lat>my_y(end),1,'first');
            if isempty(yfini),yfini=length(lat);end;
            my_z = -double(interp2(lon(xinit:xskip:xfini)',lat(yinit:yskip:yfini),...
                     z(xinit:xskip:xfini,yinit:yskip:yfini)',my_x',my_y,'nearest'));
            save(fullfile(mypath,'bathy.out'),'my_z','-ascii','-double','-tabs');
            my_m = 0*my_z;
            my_m(my_z>10) = 1;
            save(fullfile(mypath,'M.out'),'my_m','-ascii','-double','-tabs');
        end
        if get(hhs,'Value')
            my_z = coseismic(my_x,my_y,xoVal*xoScale,yoVal*yoScale,strike,dip,depthVal,...
                slip,lengthVal,widthVal,rake,0.0);
            save(fullfile(mypath,'H.out'),'my_z','-ascii','-double','-tabs');
        end
        if get(hus,'Value')
            my_z = zeros(Nglob,Mglob);
            save(fullfile(mypath,'U.out'),'my_z','-ascii','-double','-tabs');
        end
        if get(hvs,'Value')
            my_z = zeros(Nglob,Mglob);
            save(fullfile(mypath,'V.out'),'my_z','-ascii','-double','-tabs');
        end
    end

    function figures_Callback(source,eventdata)
        my_x = linspace(wVal*wScale+resVal*resScale/2,...
                        eVal*eScale-resVal*resScale/2,Mglob);
        my_y = linspace(sVal*sScale+resVal*resScale/2,...
                        nVal*nScale-resVal*resScale/2,Nglob);
        myfmt = get(hfile,'Value');
        switch myfmt;
        case 1
            myfmttxt = '-dpdf';
        case 2
            myfmttxt = '-dpng';
        case 3
            myfmttxt = '-djpeg';
        end
        if get(hdf,'Value')
            h = figure('visible','off','windowstyle','normal');
            ax = axes('parent',h,'nextplot','add');

            xinit = find(lon<my_x(1),1,'last');
            if isempty(xinit),xinit=1;end;
            xskip = max(floor((my_x(2)-my_x(1))/(lon(2)-lon(1))),1);
            xfini = find(lon>my_x(end),1,'first');
            if isempty(xfini)
                xfini=length(lon);
            end;
            yinit = find(lat<my_y(1),1,'last');
            if isempty(yinit),yinit=1;end;
            yskip = max(floor((my_y(2)-my_y(1))/(lat(2)-lat(1))),1);
            yfini = find(lat>my_y(end),1,'first');
            if isempty(yfini),yfini=length(lat);end;
            my_z = double(interp2(lon(xinit:xskip:xfini)',lat(yinit:yskip:yfini),...
                     z(xinit:xskip:xfini,yinit:yskip:yfini)',my_x',my_y,'nearest'));
            my_title = 'ETOPO1 Topography (m)';
            my_cmap = demcmap([min(min(my_z)) max(max(my_z))],1000);
            
            imagesc(my_x,my_y,my_z,'Parent',ax);
            title(ax,my_title);
            set(ax,'DataAspectRatio',[1 cos(mean(get(ha,'ylim'))*pi/180) 1]);

            set(ax,'YDir','normal');
            xlabel(ax,'Longitude (deg)');
            ylabel(ax,'Latitude (deg)');
            colormap(ax,my_cmap);
            colorbar('peer',ax);
        
            hold on;
            plot(xoVal*xoScale,yoVal*yoScale,'.r');
            hold off;
        
            drawnow;            
            
            print(h,fullfile(mypath,'bathy'),myfmttxt);
            close(h);
        end
        if get(hhf,'Value')
            h = figure('visible','off','windowstyle','normal');
            ax = axes('parent',h,'nextplot','add');
            
            my_z = coseismic(my_x,my_y,xoVal*xoScale,yoVal*yoScale,strike,dip,depthVal,...
                slip,lengthVal,widthVal,rake,0.0);
            my_title = 'Initial Waveheight (m)';
            my_cmap = jet(64);
            
            imagesc(my_x,my_y,my_z,'Parent',ax);
            title(ax,my_title);
            set(ax,'DataAspectRatio',[1 cos(mean(get(ha,'ylim'))*pi/180) 1]);

            caxis(ax,[-1 1]*max(max(abs(my_z))));
            
            set(ax,'YDir','normal');
            xlabel(ax,'Longitude (deg)');
            ylabel(ax,'Latitude (deg)');
            colormap(ax,my_cmap);
            colorbar('peer',ax);
            print(h,fullfile(mypath,'h'),myfmttxt);
            close(h);
        end
        if get(huf,'Value')
            h = dialog('visible','off','windowstyle','normal');
            ax = axes('parent',h,'nextplot','add');

            my_z = zeros(Mglob,Nglob);
            my_title = 'Initial North-South Velocity (m/s)';
            my_cmap = jet(64);
            
            imagesc(my_x,my_y,my_z,'Parent',ha);
            title(ha,my_title);
            set(ha,'DataAspectRatio',[1 cos(mean(get(ha,'ylim'))*pi/180) 1]);

            caxis(ax,[-1 1]*max(max(abs(my_z)))); 
            
            set(ha,'YDir','normal');
            xlabel(ha,'Longitude (deg)');
            ylabel(ha,'Latitude (deg)');
            colormap(ha,my_cmap);
            colorbar('peer',ha);
            print(h,fullfile(mypath,'u'),myfmttxt);
            close(h);
        end
        if get(hvf,'Value')
            h = dialog('visible','off','windowstyle','normal');
            ax = axes('parent',h,'nextplot','add');

            my_z = zeros(Mglob,Nglob);
            my_title = 'Initial East-West Velocity (m/s)';
            my_cmap = jet(64);

            imagesc(my_x,my_y,my_z,'Parent',ha);
            title(ha,my_title);
            set(ha,'DataAspectRatio',[1 cos(mean(get(ha,'ylim'))*pi/180) 1]);

            caxis(ax,[-1 1]*max(max(abs(my_z))));
            
            set(ha,'YDir','normal');
            xlabel(ha,'Longitude (deg)');
            ylabel(ha,'Latitude (deg)');
            colormap(ha,my_cmap);
            colorbar('peer',ha);
            print(h,fullfile(mypath,'v'),myfmttxt);
            close(h);
        end
    end

%%%% GUI utilities (may reference global-ish variables)

    function update_vars()
        nVal = str2num(get(hnVal,'String'));
        sVal = str2num(get(hsVal,'String'));
        wVal = str2num(get(hwVal,'String'));
        eVal = str2num(get(heVal,'String'));
        nScale = myscale(get(hnScale,'Value'));
        sScale = myscale(get(hsScale,'Value'));
        wScale = myscale(get(hwScale,'Value'));
        eScale = myscale(get(heScale,'Value'));
        yoVal = str2num(get(hyoVal,'String'));
        yoScale = myscale(get(hyoScale,'Value'));
        xoVal = str2num(get(hxoVal,'String'));
        xoScale = myscale(get(hxoScale,'Value'));
        lengthVal = str2num(get(hlengthVal,'String'));
        widthVal = str2num(get(hwidthVal,'String'));
        strike = str2num(get(hstrike,'String'));
        dip = str2num(get(hdip,'String'));
        depthVal = str2num(get(hdepthVal,'String'));
        rake = str2num(get(hrake,'String'));
        slip = str2num(get(hslip,'String'));
        
        resVal = str2num(get(hresVal,'String'));
        resScale = angle_call(get(hresScale,'Value'));        
        Nglob = (nVal*nScale - sVal*sScale)/(resVal*resScale);
        Mglob = (eVal*eScale - wVal*wScale)/(resVal*resScale);
        if (floor(Mglob)~=Mglob) Mglob=nan; end;
        if (floor(Nglob)~=Nglob) Nglob=nan; end;
        set(hMglob,'String',num2str(Mglob));
        set(hNglob,'String',num2str(Nglob));
    end

    function scale = myscale(val)
        switch val;
        case 1
            scale = 1;
        case 2
            scale = -1;
        end
    end

    function update_axes()
        plotsize = get(hpopupPS,'Value');
        plottype = get(hpopupPT,'Value');
        wavetype = get(hpopupR,'Value');
        switch plotsize;
        case 1
            my_x = linspace(-180,180,xres);
            my_y = linspace(-90,90,yres);
        case 2
            my_x = linspace(wVal*wScale,eVal*eScale,xres);
            my_y = linspace(sVal*sScale,nVal*nScale,yres);
        case 3
            xnear = 2;
            my_x = linspace(xoVal*xoScale-xnear,xoVal*xoScale+xnear,xres);
            my_y = linspace(yoVal*yoScale-xnear,yoVal*yoScale+xnear,yres);
        end
        switch plottype;
        case 1
            xinit = find(lon<my_x(1),1,'last');
            if isempty(xinit),xinit=1;end;
            xskip = max(floor((my_x(2)-my_x(1))/(lon(2)-lon(1))),1);
            xfini = find(lon>my_x(end),1,'first');
            if isempty(xfini)
                xfini=length(lon);
            end;
            yinit = find(lat<my_y(1),1,'last');
            if isempty(yinit),yinit=1;end;
            yskip = max(floor((my_y(2)-my_y(1))/(lat(2)-lat(1))),1);
            yfini = find(lat>my_y(end),1,'first');
            if isempty(yfini),yfini=length(lat);end;
            my_z = double(interp2(lon(xinit:xskip:xfini)',lat(yinit:yskip:yfini),...
                     z(xinit:xskip:xfini,yinit:yskip:yfini)',my_x',my_y,'nearest'));
            my_title = 'ETOPO1 Topography (m)';
            my_cmap = demcmap([min(min(my_z)) max(max(my_z))],1000);
        case 2
            switch wavetype;
            case 1
                my_z = coseismic(my_x,my_y,xoVal*xoScale,yoVal*yoScale,strike,dip,depthVal,...
                    slip,lengthVal,widthVal,rake,0.0);
            case 2
            case 3
            end
            my_title = 'Initial Waveheight (m)';
            my_cmap = jet(64);
        case 3
            switch wavetype;
            case 1
                my_z = zeros(xres,yres);
            case 2
            case 3
            end
            my_title = 'Initial North-South Velocity (m/s)';
            my_cmap = jet(64);
        case 4
            switch wavetype;
            case 1
                my_z = zeros(xres,yres);
            case 2
            case 3
            end
            my_title = 'Initial East-West Velocity (m/s)';
            my_cmap = jet(64);
        end
        imagesc(my_x,my_y,my_z,'Parent',ha);
        title(ha,my_title);
        set(ha,'DataAspectRatio',[1 cos(mean(get(ha,'ylim'))*pi/180) 1]);
        
        set(ha,'YDir','normal');
        xlabel(ha,'Longitude (deg)');
        ylabel(ha,'Latitude (deg)');
        colormap(ha,my_cmap);
        colorbar('peer',ha);
        
        % overlay
        if plottype==1
            hold on;
            plot(xoVal*xoScale,yoVal*yoScale,'.r');
            hold off;
        end
        
        drawnow;
    end

%%%% Utility functions (all variables are passed)
    function hout = coseismic(my_x,my_y,xo,yo,strike,dip,depth,...
                    slip,lengthVal,widthVal,rake,open)
%        xx = deg2km(distance(xo,yo,my_x,yo)).*sign(xo-my_x);
%        yy = deg2km(distance(xo,yo,xo,my_y)).*sign(yo-my_y);
         xx = deg2km(distance(yo,xo,my_y,xo)).*sign(yo-my_y);
         yy = deg2km(distance(yo,xo,yo,my_x)).*sign(xo-my_x);
        [E,N] = meshgrid(xx,yy);
        [uE,uN,uZ] = okada85(E,N,depth,strike,dip,lengthVal,widthVal,...
            rake,slip,open);
        hout = uZ;
    end
 
    function scale = angle_call(val)
        switch val;
        case 1
            scale = 1;
        case 2
            scale = 1/60;
        case 3
            scale = 1/3600;
        end
    end
    function scale = ns_call(source)
        str = get(source, 'String');
        val = get(source,'Value');
        switch str{val};
        case 'N'
            scale = 1;
        case 'S'
            scale = -1;
        end
    end
    function scale = ew_call(source)
        str = get(source, 'String');
        val = get(source,'Value');
        switch str{val};
        case 'E'
            scale = 1;
        case 'W'
            scale = -1;
        end
    end

%     function z=compute_bathy(Adomain,xres,yres)
%         [mylon,mylat] = meshgrid(mod(linspace(wVal*wScale-resVal*resScale/2,...
%                                     eVal*eScale-resVal*resScale/2,Mglob)+180,360)-180,...
%                                  linspace(sVal*sScale-resVal*resScale/2,...
%                                           nVal*nScale-resVal*resScale/2,Nglob));
%         myD = double(-interp2(lat,lon,z,mylat,mylon));
% 
%     end
end 
