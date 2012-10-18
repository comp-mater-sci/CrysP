%to read TXT in format from Qingge

%%
%IMPORT grain1.txt / grain2.txt -> GrainSS
%file='r9ABCC_3'
%grain='grain 47'
%model='ALAMEL'
%%
cryst='BCC';
NRss=24;
NRpseudoss=2;
%% Defined legend of slip systems
switch(cryst)
    case{'FCC'}
        z{1} =texlabel(' (111){01-1}','literal');%z is CELL 
        z{2} =texlabel(' (111){-101}','literal');
        z{3} =texlabel(' (111){1-10}','literal');
        z{4} =texlabel('(-111){01-1}','literal');
        z{5} =texlabel('(-111){101} ','literal');
        z{6} =texlabel('(-111){110} ','literal');
        z{7} =texlabel('(1-11){011} ','literal');
        z{8} =texlabel('(1-11){-101}','literal');
        z{9} =texlabel('(1-11){110} ','literal');
        z{10}=texlabel('(11-1){011} ','literal');
        z{11}=texlabel('(11-1){101} ','literal');
        z{12}=texlabel('(11-1){1-10}','literal');
        leg=char(z{1},z{2},z{3},z{4},z{5},z{6},z{7},z{8},z{9},z{10},z{11},z{12}); %CELL->MATRIX
    case{'BCC'}
        z{1} =texlabel('  (01-1){111}','literal');%z is CELL
        z{2} =texlabel('  (-101){111}','literal');
        z{3} =texlabel('  (1-10){111}','literal');
        z{4} =texlabel(' (0-1-1){-1-11}','literal');
        z{5} =texlabel(' (101-1){-11}','literal');
        z{6} =texlabel('(-110-1){-11}','literal');
        z{7} =texlabel('  (01-1){-111}','literal');
        z{8} =texlabel('   (101){-111}','literal');
        z{9} =texlabel(' (-1-10){-111}','literal');
        z{10} =texlabel(' (0-1-1){1-11}','literal');
        z{11} =texlabel('  (-101){1-11}','literal');
        z{12} =texlabel('   (110){1-11}','literal');
        z{13} =texlabel(' (2-1-1){111}','literal');
        z{14} =texlabel(' (-12-1){111}','literal');
        z{15} =texlabel(' (-1-12){111}','literal');
        z{16} =texlabel(' (-21-1){-1-11}','literal');
        z{17} =texlabel(' (1-2-1){-1-11}','literal');
        z{18} =texlabel('   (112){-1-11}','literal');
        z{19} =texlabel('(-2-1-1){-111}','literal');
        z{20} =texlabel('  (12-1){-111}','literal');
        z{21} =texlabel('  (1-12){-111}','literal');
        z{22} =texlabel('  (21-1){1-11}','literal');
        z{23} =texlabel('(-1-2-1){1-11}','literal');
        z{24} =texlabel('  (-112){1-11}','literal');
        leg=char(z{1},z{2},z{3},z{4},z{5},z{6},z{7},z{8},z{9},z{10},z{11},z{12},z{13},z{14},z{15},z{16},z{17},z{18},z{19},z{20},z{21},z{22},z{23},z{24}); %CELL->MATRIX
    otherwise
        leg='UNKNOWN';
end
clear z;        

z{1}='RLX_1';
z{2}='RLX_2';
legRLX=char(z{1},z{2}); %CELL->MATRIX
clear z;
%% Define LineSpec_LIST
LineSpec_LIST{1} ='-or';%'r-';
LineSpec_LIST{2} ='-sr';%'r:';
LineSpec_LIST{3} ='-dr';%'r.';
LineSpec_LIST{4} ='-^r';%'g-';
LineSpec_LIST{5} ='-vr';%'g:';
LineSpec_LIST{6} ='-xr';%'g.';
LineSpec_LIST{7} ='-og';%'b-';
LineSpec_LIST{8} ='-sg';%'b:';
LineSpec_LIST{9} ='-dg';%'b.';
LineSpec_LIST{10}='-^g';%'k-';
LineSpec_LIST{11}='-vg';%'k:';
LineSpec_LIST{12}='-xg';%'k.';

LineSpec_LIST{13}='-ob';%'rv';
LineSpec_LIST{14}='-sb';%'r^';
LineSpec_LIST{15}='-db';%'rd';
LineSpec_LIST{16}='-^b';%'gv';
LineSpec_LIST{17}='-vb';%'g^';
LineSpec_LIST{18}='-xb';%'gd';
LineSpec_LIST{19}='-ok';%'bv';
LineSpec_LIST{20}='-sk';%'b^';
LineSpec_LIST{21}='-dk';%'bd';
LineSpec_LIST{22}='-^k';%'kv';
LineSpec_LIST{23}='-vk';%'k^';
LineSpec_LIST{24}='-xk';%'kd';

LineSpec_LIST_pseudo{1}='-sr';
LineSpec_LIST_pseudo{2}='-dg';
%%

GrainSSNaN=GrainSS;
for ii=1:length(GrainSSNaN(:,1))
for jj=1:length(GrainSSNaN(1,:))-NRpseudoss %do NOT apply to pseudo Slip Systems
    if (abs(GrainSSNaN(ii,jj))<1.e-8) 
            GrainSSNaN(ii,jj)=NaN('single'); 
    end
end
end


%% PLOT all slip systems
h=figure;
hold on, 

%real s.s.
for ss=1:NRss 
    plot(GrainSSNaN(:,ss),LineSpec_LIST{ss},'MarkerSize',7,'LineWidth',2)
end

xlabel('inc. number [/]','FontSize',16)
ylabel('slip rate [s^-^1]','FontSize',16)
%ylabel('norm. slip rate [s^-^1]','FontSize',16)

legend(leg,'location','EastOutside')
title(char([file '--- grain # ' num2str(grain)],[model],[ ]),'FontSize',12)
hold off, 
%% Automatic save of figure
%
outName=[file '-' num2str(grain)];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
clear outName
%}
%% PLOT all PSEUDO-slip systems
%
h=figure;
hold on, 
%pseudo-s.s.
for ss=1:NRpseudoss 
    plot(GrainSSNaN(:,NRss+ss),LineSpec_LIST_pseudo{ss},'MarkerSize',7,'LineWidth',1)
end

xlabel('inc. number [/]','FontSize',16)
ylabel('slip rate [s^-^1]','FontSize',16)
%ylabel('norm. slip rate [s^-^1]','FontSize',16)

legend(legRLX,'location','EastOutside')
title(char([file],[model],[ ]),'FontSize',12)
hold off, 
%}
%% Automatic save of figure
%
outName=[file '-RLX'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
clear outName
%}
%%
close all