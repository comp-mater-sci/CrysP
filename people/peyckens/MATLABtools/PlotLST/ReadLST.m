% it is assumed that the following TEXT variables have been defined prior 
%  to a call to this M-file:
% file    ='vbA',% for vbA.LST
% cryst   ='FCC' / 'BCC'
% model   ='ALAMEL' / 'MASAL1' / 'MASAL2' / 'FCTaylor'
% dg      = e.g. '0.01' / '0.025' / '0.004' / ...

%cryst='BCC'; model='ALAMEL'; dg='0.01'

switch(model)
    case {'ALAMEL','MASAL1','MASAL2'}
        NRpseudoss=2;
    case {'FCTaylor'}
        NRpseudoss=0;
    otherwise
        ERROR='Wrong kind of model name',%printit
        model,%printit
end
%% Plot ranges + ticks
XrangePlot= [0.0      0.2]; %.23])%.3])%0.15])%0.6])
XTickLIST=   0.0:0.05:0.2;

YrangePlot= [-Yextr       Yextr]; %([-4.e-3 4.e-3])%([-20.e-3 20.e-3])
YTickLIST=   -Yextr:0.010:Yextr;

%FOR TC1 and TC3::
  %YrangePlot= [-0.020       0.020]; %([-4.e-3 4.e-3])%([-20.e-3 20.e-3])
  %YTickLIST=   -0.020:0.010:0.020;
%FOR TC2::
  %YrangePlot= [-0.030       0.030]; %([-4.e-3 4.e-3])%([-20.e-3 20.e-3])
  %YTickLIST=   -0.030:0.010:0.030;
%% Open file, read all in [tall], close file

FileID= fopen([file '.LST'],'r');
tt2=textscan(FileID,'%s','Delimiter','');
[tall]=deal(tt2{1,1}); 
fclose(FileID);
clear tt2 FileID ans;
%% get NRss
%NRss=0 %init
%if cryst=='FCC' NRss=12 end
%if cryst=='BCC' NRss=24 end

% read from file:
%
line_ss=22;% for DOUBLE-VOCE (e.g. TC1,TC2,TC3)
%line_ss=14;% without hardening 

line=str2num(tall{line_ss,:});
NRss=line(2); 
clear line;
%}
%% Cut heading including slip system lines:  tall -> t1 + t2
t1_end=line_ss+NRss+5;
t2_start=t1_end+1;
t1=tall(        1:t1_end ,:);
t2=tall( t2_start:end    ,:);
clear t1_end t2_start tall line_ss;
%% get NRgrains
line3=t2{3,:};
NRgrains=str2num(line3(end-4:end));
clear line3;
%% get NRsteps
lineX=t2{11,:};%for DOUBLE-VOCE (e.g. TC1,TC2,TC3)
%lineX=t2{13,:};% without hardening 
NRsteps=str2num(lineX(end-4:end));
clear lineX;
%%
line_End=28;%init.  %for DOUBLE-VOCE (e.g. TC1,TC2,TC3)
%line_End=30;%init.  %without hardening 

GRrec=17+ceil(NRss/10); %record length of a grain
STrec=11; %record length done at end of step;
GBrec=1;  %record length of a grain boundary

for step=1:NRsteps
    for grainCouple=1:NRgrains/2

    % GBdata of this grainCouple
    %- - - - - - - - - - - - - -     
    line_Start= line_End+1;
    line_End  = line_Start + GBrec -1;   
    line=t2{line_Start,:};
    gamRLX_nrm{1,grainCouple}(step)=str2num(line(end-23:end-12));%in ALTAY, it is normalized by DvM
    gamRLX_nrm{2,grainCouple}(step)=str2num(line(end-11:end   ));%in ALTAY, it is normalized by DvM
        
    % GRAIN 1 of this grainCouple
    %- - - - - - - - - - - - - - 
    grain=2*grainCouple-1;
    line_Start= line_End+1;%!%    line_Start= line_Start + GBrec;
    line_End  = line_Start + GRrec -1;
    get_grain %extract data for this grain

    switch(model) %%repeat reading of GBdata ONLY for FC taylor!
    case {'FCTaylor'}
    % GBdata of this grainCouple
    %- - - - - - - - - - - - - -     
    line_Start= line_End+1;
    line_End  = line_Start + GBrec -1;   
    line=t2{line_Start,:};
    gamRLX_nrm{1,grainCouple}(step)=str2num(line(end-23:end-12));%in ALTAY, it is normalized by DvM
    gamRLX_nrm{2,grainCouple}(step)=str2num(line(end-11:end   ));%in ALTAY, it is normalized by DvM    
    end
    
    % GRAIN 2 of this grainCouple
    %- - - - - - - - - - - - - - 
    grain=grain+1;
    line_Start= line_End+1;%!%line_Start= line_Start + GRrec;
    line_End  = line_Start + GRrec -1;
    get_grain %extract data for this grain
    
    % 
    gamRLX{1,grainCouple}(step)=gamRLX_nrm{1,grainCouple}(step)* DvM(step);
    gamRLX{2,grainCouple}(step)=gamRLX_nrm{2,grainCouple}(step)* DvM(step);

    
    end %grainCouple-loop
line_End= line_End + STrec;
end %step-loop

clear line_Start line_End line_Start_forGB line_End_forGB;
clear GBrec GRrec STrec;
clear step grainCouple grain;
%% OLD VERSION
%{
line_End=30;%init.
GBrec=2;  %record length of a grain boundary
GRrec=9+ceil(NRss/10); %record length of a grain
STrec=11; %record length done at end of step;

for step=1:NRsteps
    %%%%%%%%%%%%%%%%%%%%%%%%%%for grain=1:NRgrains
    for grainCouple=1:NRgrains/2

    % GBdata of this grainCouple
    %- - - - - - - - - - - - - -     
    line_Start= line_End+1;
    line_End  = line_Start + GBrec -1;
    line_Start_forGB=line_Start;
    line_End_forGB=line_End;
   %%% get_GB Not yet ->DvM needs to be known, so do get_grain first!
    
    % GRAIN 1 of this grainCouple
    %- - - - - - - - - - - - - - 
    grain=2*grainCouple-1;
    line_Start= line_End+1;%!%    line_Start= line_Start + GBrec;
    line_End  = line_Start + GRrec -1;
    get_grain %extract data for this grain

    switch(model)
        case {'ALAMEL','MASAL1'}
            %nothing to be done
        case {'FCTaylor'}
            %in-between the 'couple' of grains are GBrec additional lines!
            line_Start= line_End+1;
            line_End  = line_Start + GBrec -1;
            ddd=1,
        otherwise
            ERROR='Wrong kind of model name',%printit
            model,%printit
    end
       
    % GRAIN 2 of this grainCouple
    %- - - - - - - - - - - - - - 
    grain=grain+1;
    line_Start= line_End+1;%!%line_Start= line_Start + GRrec;
    line_End  = line_Start + GRrec -1;
    get_grain %extract data for this grain
    
    % GBdata of this grainCouple
    %- - - - - - - - - - - - - -
    get_GB
    
    end %grainCouple-loop
line_End= line_End + STrec;
end %step-loop

clear line_Start line_End line_Start_forGB line_End_forGB;
clear GBrec GRrec STrec;
clear step grainCouple grain;
%}
%% Correction to ADEM:
%start ADEM-vector with 0. (so it relates to START of inc. in stead of END of inc)
ADEM=[0. ADEM];
ADEM=ADEM(1:end-1);

clear ll_t2Init ll_record ll_move ll_endStep grain step ss;

%% CALC oscil: a quantative value to quantify oscillatory behaviour 
%{
for grain=1:1:NRgrains
    oscil %add a plot in current figure
end
clear grain

%{
% iii needs to be pre-set to value 1
oscLIST{iii}=oscilLOG;
iii=iii+1;
%}
 
oscLISTall(mod,pos_ini:pos_end)=oscilLOG;
%}
%% PLOT (normalized) slip rates
%PLOT all grains
%
%Which one to plot? --- ext: extension in filename
sliprate_2plot=sliprateNaN; ext=''; 
%sliprate_2plot=sliprate_NORM; ext='NRM';

%for grain=1:1:NRgrains
for grain=grX:1:grX+1
%for grain=14:1:21
%for grain=16:1:21
%for grain=1:1:9 %41:1:50    %25:26%37:38%21:1:30%29:30%47:1:50%7:1:14%29:1:34%1:1:NRgrains%17:1:22%21:1:26%
    plot_ss
end
clear grain 
%}
%% PLOT Euler angles of (a pair of) 2 grains
%PLOT each time 'grain' AND 'grain+1'
%{
for grain=1:2:NRgrains%43:2:50%1:2:NRgrains%-117:2:21%21:2:25%
    plot_euler
end
clear grain
%}
%% PLOT relaxations of (a pair of) 2 grains
%PLOT each time for 'grain' AND 'grain+1'
%
switch(model)
    case {'ALAMEL','MASAL1','MASAL2'}
        %for grain=1:2:NRgrains
        for grain=grX:2:grX+1
        %for grain=14:2:20
        %for grain=16:2:20
        %for grain=1:2:9 %41:2:49    %43:2:50%1:2:NRgrains%-117:2:21%21:2:25%
            plot_pseudoss
        end
        clear grain
    case {'FCTaylor'}
        %nothing to be done
end
%}