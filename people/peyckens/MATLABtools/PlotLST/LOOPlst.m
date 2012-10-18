%% %%%%%%%%%%  %%%%%%%%%%%%


%% %%%%%%%%%% oscLOG_plot %%%%%%%%%%%%
%r
%---
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='r0Fbcc_3'; ReadLST;
model='ALAMEL';   file='r0Abcc_3'; ReadLST;
model='MASAL2';   file='r0Mbcc_3'; ReadLST;
oscLOG_plot
%
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='r0Fbcc_4'; ReadLST;
model='ALAMEL';   file='r0Abcc_4'; ReadLST;
model='MASAL2';   file='r0Mbcc_4'; ReadLST;
oscLOG_plot

close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='r9Fbcc_3'; ReadLST;
model='ALAMEL';   file='r9Abcc_3'; ReadLST;
model='MASAL2';   file='r9Mbcc_3'; ReadLST;
oscLOG_plot
%
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='r9Fbcc_4'; ReadLST;
model='ALAMEL';   file='r9Abcc_4'; ReadLST;
model='MASAL2';   file='r9Mbcc_4'; ReadLST;
oscLOG_plot


%s
%---
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='s0Fbcc_3'; ReadLST;
model='ALAMEL';   file='s0Abcc_3'; ReadLST;
model='MASAL2';   file='s0Mbcc_3'; ReadLST;
oscLOG_plot
%
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='s0Fbcc_4'; ReadLST;
model='ALAMEL';   file='s0Abcc_4'; ReadLST;
model='MASAL2';   file='s0Mbcc_4'; ReadLST;
oscLOG_plot

close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='s9Fbcc_3'; ReadLST;
model='ALAMEL';   file='s9Abcc_3'; ReadLST;
model='MASAL2';   file='s9Mbcc_3'; ReadLST;
oscLOG_plot
%
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='s9Fbcc_4'; ReadLST;
model='ALAMEL';   file='s9Abcc_4'; ReadLST;
model='MASAL2';   file='s9Mbcc_4'; ReadLST;
oscLOG_plot


%t
%---
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='t0Fbcc_3'; ReadLST;
model='ALAMEL';   file='t0Abcc_3'; ReadLST;
model='MASAL2';   file='t0Mbcc_3'; ReadLST;
oscLOG_plot
%
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='t0Fbcc_4'; ReadLST;
model='ALAMEL';   file='t0Abcc_4'; ReadLST;
model='MASAL2';   file='t0Mbcc_4'; ReadLST;
oscLOG_plot

close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='t9Fbcc_3'; ReadLST;
model='ALAMEL';   file='t9Abcc_3'; ReadLST;
model='MASAL2';   file='t9Mbcc_3'; ReadLST;
oscLOG_plot
%
close all, clear all; iii=1; cryst='BCC';  dg='0.01'; Yextr=0.03;
model='FCTaylor'; file='t9Fbcc_4'; ReadLST;
model='ALAMEL';   file='t9Abcc_4'; ReadLST;
model='MASAL2';   file='t9Mbcc_4'; ReadLST;
oscLOG_plot
%%


%{
%%%%%%%%%%%%%%model='MASAL2';
        model='MASAL2'; file='r0-Mbcc3'; ReadLST;
grX=41; model='MASAL2'; file='r0-Mbcc4'; ReadLST;
grX=41; model='MASAL2'; file='r9-Mbcc3'; ReadLST;
        model='MASAL2'; file='r9-Mbcc4'; ReadLST;

        model='MASAL2'; file='s0-Mbcc3'; ReadLST;
        model='MASAL2'; file='s0-Mbcc4'; ReadLST;
grX=45; model='MASAL2'; file='s9-Mbcc3'; ReadLST;
        model='MASAL2'; file='s9-Mbcc4'; ReadLST;

grX=29; model='MASAL2'; file='t0-Mbcc3'; ReadLST;
        model='MASAL2'; file='t0-Mbcc4'; ReadLST;
        model='MASAL2'; file='t9-Mbcc3'; ReadLST;
grX=17; model='MASAL2'; file='t9-Mbcc4'; ReadLST;
%}
%{
model='FCTaylor'; file='BTC1F'; ReadLST;
model='ALAMEL'; file='BTC1A'; ReadLST;
model='MASAL1'; file='BTC1M'; ReadLST;
model='MASAL2'; file='BTC1-M'; ReadLST;

model='FCTaylor'; file='BzF'; ReadLST;
model='ALAMEL'; file='BzA'; ReadLST;
model='MASAL1'; file='BzM'; ReadLST;
model='MASAL2'; file='Bz-M'; ReadLST;
%}

% model='FCTaylor'; file='r0Fbcc_4'; ReadLST;

%model='ALAMEL'; file='r0A_x5_4'; ReadLST;

%model='MASAL1'; file='r0Mbcc_4'; ReadLST;
%model='ALAMEL'; file='r0Abcc_4'; ReadLST;

%%%%%%%%%%%%%%model='MASAL2';
%        model='MASAL2'; file='r0-Mbcc3'; ReadLST;
%grX=41; model='MASAL2'; file='r0-Mbcc4'; ReadLST;
%grX=41; model='MASAL2'; file='r9-Mbcc3'; ReadLST;
%        model='MASAL2'; file='r9-Mbcc4'; ReadLST;

%        model='MASAL2'; file='s0-Mbcc3'; ReadLST;
%        model='MASAL2'; file='s0-Mbcc4'; ReadLST;
%grX=45; model='MASAL2'; file='s9-Mbcc3'; ReadLST;
%        model='MASAL2'; file='s9-Mbcc4'; ReadLST;

%grX=29; model='MASAL2'; file='t0-Mbcc3'; ReadLST;
%        model='MASAL2'; file='t0-Mbcc4'; ReadLST;
%        model='MASAL2'; file='t9-Mbcc3'; ReadLST;
%grX=17; model='MASAL2'; file='t9-Mbcc4'; ReadLST;



%%%%%%%%%%%%%%%%%%%%%%%% FCTAYLOR
%        model='FCTaylor'; file='r0Fbcc_3'; ReadLST;
%grX=41; model='FCTaylor'; file='r0Fbcc_4'; ReadLST;
%grX=41; model='FCTaylor'; file='r9Fbcc_3'; ReadLST;
%        model='FCTaylor'; file='r9Fbcc_4'; ReadLST;

%        model='FCTaylor'; file='s0Fbcc_3'; ReadLST;
%        model='FCTaylor'; file='s0Fbcc_4'; ReadLST;
%grX=45; model='FCTaylor'; file='s9Fbcc_3'; ReadLST;
%        model='FCTaylor'; file='s9Fbcc_4'; ReadLST;

%grX=29; model='FCTaylor'; file='t0Fbcc_3'; ReadLST;
%        model='FCTaylor'; file='t0Fbcc_4'; ReadLST;
%        model='FCTaylor'; file='t9Fbcc_3'; ReadLST;
%grX=17; model='FCTaylor'; file='t9Fbcc_4'; ReadLST;

%%%%%%%%%%%%%%%%%%%%%%%% ALAMEL and MASAL1
%model='ALAMEL'; file='r0Abcc_3'; ReadLST;model='MASAL1'; file='r0Mbcc_3'; ReadLST; %
%grX=45; model='ALAMEL'; file='r0Abcc_4'; ReadLST;model='MASAL1'; file='r0Mbcc_4'; ReadLST; %
%grX=41; model='ALAMEL'; file='r9Abcc_3'; ReadLST;model='MASAL1'; file='r9Mbcc_3'; ReadLST; %
%model='ALAMEL'; file='r9Abcc_4'; ReadLST;model='MASAL1'; file='r9Mbcc_4'; ReadLST; %

%model='ALAMEL'; file='s0Abcc_3'; ReadLST;model='MASAL1'; file='s0Mbcc_3'; ReadLST; %
%model='ALAMEL'; file='s0Abcc_4'; ReadLST;model='MASAL1'; file='s0Mbcc_4'; ReadLST; %
%grX=45; model='ALAMEL'; file='s9Abcc_3'; ReadLST;model='MASAL1'; file='s9Mbcc_3'; ReadLST; %
%model='ALAMEL'; file='s9Abcc_4'; ReadLST;model='MASAL1'; file='s9Mbcc_4'; ReadLST; %

%grX=29;model='ALAMEL'; file='t0Abcc_3'; ReadLST;model='MASAL1'; file='t0Mbcc_3'; ReadLST; %
%model='ALAMEL'; file='t0Abcc_4'; ReadLST;model='MASAL1'; file='t0Mbcc_4'; ReadLST; %
%model='ALAMEL'; file='t9Abcc_3'; ReadLST;model='MASAL1'; file='t9Mbcc_3'; ReadLST; %
%grX=27; model='ALAMEL'; file='t9Abcc_4'; ReadLST;model='MASAL1'; file='t9Mbcc_4'; ReadLST; %

%{
file='TC1-M090'; ReadLST
file='TC1-M095'; ReadLST
file='TC1-M098'; ReadLST
file='TC1-M099'; ReadLST
file='TC1-M100'; ReadLST
file='TC1-M101'; ReadLST
file='TC1-M102'; ReadLST
file='TC1-M105'; ReadLST
file='TC1-M110'; ReadLST


file='TC1-Mn00'; ReadLST
file='TC1-Mn01'; ReadLST
file='TC1-Mn02'; ReadLST
file='TC1-Mn03'; ReadLST
file='TC1-Mn04'; ReadLST
file='TC1-Mn05'; ReadLST
file='TC1-Mn06'; ReadLST
file='TC1-Mn07'; ReadLST
file='TC1-Mn08'; ReadLST
file='TC1-Mn09'; ReadLST
file='TC1-Mn10'; ReadLST



file='TC1-Mp00'; ReadLST
file='TC1-Mp01'; ReadLST
file='TC1-Mp02'; ReadLST
file='TC1-Mp04'; ReadLST
file='TC1-Mp07'; ReadLST
file='TC1-Mp10'; ReadLST


file='TC1-M0'; ReadLST
file='TC1-Md2'; ReadLST
file='TC1-Mt2'; ReadLST
file='TC1-MtE1'; ReadLST
file='TC1-MtE2'; ReadLST
file='TC1-MtE3'; ReadLST
%}