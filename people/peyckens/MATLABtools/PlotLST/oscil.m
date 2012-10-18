%% CALC oscil: a quantative value to quantify oscillatory behaviour 
% Assumed to be known:
%%% "grain" : nr. of the grain to be plotted

%% 

for step=2:NRsteps %step-loop
    for ss=1:NRss %ss-loop
        osc(ss)= ( sliprate{ss,grain}(step)-sliprate{ss,grain}(step-1) )^2  ;
    end %ss-loop
    osc_ssAVG{grain}(step)= sum(osc) / NRss;
    clear osc
end  %step-loop

%Take log10
oscilLOG(grain)= log( sqrt(  sum(osc_ssAVG{grain})/NRsteps)  );

clear step
