addpath(fullfile('..','..','..','GWToolsRpack','gwtools','Matlab'))
addpath(fullfile('..','..'))
% This script read the output data and puts them in a variable that can be
% retrieved later
%% Run model
runIWFM(0,1);
%% Base simulation model
idx = 1;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 0.01';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%% 
idx = 2;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 0.05';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%% 
idx = 3;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 0.1';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%% 
idx = 4;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 0.5';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%% 
idx = 5;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 1';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 6;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 2';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 7;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 5';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 8;
runIWFM(0,1);
IWFM41(idx,1).Desc='K_{cl} = 10';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%% SAFE
%% Base simulation model
idx = 1;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 0.01';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 2;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 0.05';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 3;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 0.1';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 4;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 0.5';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 5;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 1';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 6;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 2';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 7;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 5';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 8;
runIWFM(0,1);
SAFE(idx,1).Desc='K_{cl} = 10';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();