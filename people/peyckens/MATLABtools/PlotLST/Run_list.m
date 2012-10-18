
%{
%TC4
clear all, close all, cryst='BCC'; dg='0.01';
file='r9Mbcc_3'; model='MASAL1'; 
Yextr=0.02; grX=31; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='r9Fbcc_3'; model='FCTaylor'; 
Yextr=0.02; grX=31; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='r9Abcc_3'; model='ALAMEL'; 
Yextr=0.02; grX=31; ReadLST
%}
%%
%TC1
clear all, close all, cryst='BCC'; dg='0.01';
file='TC1-M1'; model='MASAL1'; 
Yextr=0.03; grX=1; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='TC1-F'; model='FCTaylor'; 
Yextr=0.03; grX=1; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='TC1-A'; model='ALAMEL'; 
Yextr=0.03; grX=1; ReadLST
%%
%TC2
clear all, close all, cryst='BCC'; dg='0.01';
file='TC2-M1'; model='MASAL1';
Yextr=0.03; grX=1; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='TC2-F'; model='FCTaylor'; 
Yextr=0.03; grX=1; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='TC2-A'; model='ALAMEL'; 
Yextr=0.03; grX=1; ReadLST

%TC3
clear all, close all, cryst='BCC'; dg='0.01';
file='TC3-M1'; model='MASAL1'; 
Yextr=0.02; grX=1; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='TC3-F'; model='FCTaylor'; 
Yextr=0.02; grX=1; ReadLST
clear all, close all, cryst='BCC'; dg='0.01';
file='TC3-A'; model='ALAMEL'; 
Yextr=0.02; grX=1; ReadLST