%% get t_cur, the part of t2 for the current step+grain
%Assumed to be correct and known:
%  line_Start
%  line_End

t_cur= t2(line_Start:line_End, :);

%% get DvM(step) + ADEM(step)
%Only calculated if not yet existing!
if (grain==1)%ADEM does not exist
    line=t_cur{15,:};     %OLD: {7,:}
    %DvM: von Mises equivalent (macro) strain rate
    DvM(step) = str2num(line(8:25)); 
    %ADEM (Accumulated Delta_Epsilon_VonMises)
    if (step==1)
        prev=0.;
    else %step>1
        prev=ADEM(step-1);
    end
    ADEM(step)= prev + 1* DvM(step); %(assume time inc. = 1s)
    clear line prev;
end %(grain==1)-loop   

%% Calc sliprate and sliprateTOT
sliprateTOT=0.;%init
for ss=1:NRss
    % get sliprate{ss,grain}(step)
    nn=floor((ss-1)/10);  %%1..10->0; 11..20->1; 21..30->2
    line=str2num(t_cur{17+nn,:});  %OLD: {9+nn,:}
    sliprate{ss,grain}(step)=line(ss-nn*10); clear nn line;   
    sliprate{ss,grain}(step)= sliprate{ss,grain}(step) * DvM(step);%in ALTAY, it is normalized by DvM
    sliprateTOT= sliprateTOT + abs(sliprate{ss,grain}(step));
    %sliprate -> sliprateNaN: can be used for plot of s.s., prevents plotting of quasi-zero values.
    if (abs(sliprate{ss,grain}(step))<1.e-8) 
        sliprateNaN{ss,grain}(step)=NaN('single'); 
    else
        sliprateNaN{ss,grain}(step)=sliprate{ss,grain}(step);
    end    
end %ss-loop
    
%Calc normalized slip rates, normalized by sliprateTOT
for ss=1:NRss
    sliprate_NORM{ss,grain}(step)=sliprate{ss,grain}(step) ./ sliprateTOT;
end %ss-loop

clear sliprateTOT line;
%% get Euler Angles
line=t_cur{5,:};   
Euler{1,grain}(step)=str2num(line(end-42:end-29));%phi_1: 1st entry on line
Euler{2,grain}(step)=str2num(line(end-28:end-15));%PHI  : 2nd 
Euler{3,grain}(step)=str2num(line(end-14:end-1 ));%phi_2: 3rd

if (Euler{1,grain}(step)<0)%sometime phi1 from +180 to -180
    Euler{1,grain}(step)=Euler{1,grain}(step)+360.;
end

clear line;
clear t_cur;
