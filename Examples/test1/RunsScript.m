addpath(fullfile('..','..','..','GWToolsRpack','gwtools','Matlab'))
addpath(fullfile('..','..'))
% This script read the output data and puts them in a variable that can be
% retrieved later
%% Run model
runIWFM(0,1);
%% ============ Runs with or without derivatives
IWFM41.Desc = 'IWFM with Deriv';
IWFM41.SWGW = readStreamNodesBudget();
IWFM41.CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41.DH = readStreamHeadAndStage();
%%
IWFM41noDeriv.Desc = 'IWFM no Deriv';
IWFM41noDeriv.SWGW = readStreamNodesBudget();
IWFM41noDeriv.CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41noDeriv.DH = readStreamHeadAndStage();
%%
SAFEwithDeriv.Desc = 'SAFE with IWFM Deriv';
SAFEwithDeriv.SWGW = readStreamNodesBudget();
SAFEwithDeriv.CONV = readSafeTestOutput('safe_test.dat');
SAFEwithDeriv.DH = readStreamHeadAndStage();
%%
SAFEnoDeriv.Desc = 'SAFE no Deriv';
SAFEnoDeriv.SWGW = readStreamNodesBudget();
SAFEnoDeriv.CONV = readSafeTestOutput('safe_test.dat');
SAFEnoDeriv.DH = readStreamHeadAndStage();
%% =============================================
figure()
clf
plot([IWFM41.CONV.Niter]', 'LineWidth',2, 'DisplayName',IWFM41.Desc)
hold on
plot([SAFEnoDeriv.CONV.Niter]', 'LineWidth',2, 'DisplayName',SAFEnoDeriv.Desc)
plot([IWFM41noDeriv.CONV.Niter]', 'LineWidth',2, 'DisplayName',IWFM41noDeriv.Desc)
plot([SAFEwithDeriv.CONV.Niter]', '--g', 'LineWidth',2, 'DisplayName',SAFEwithDeriv.Desc)
grid on
xlabel('Time Step')
ylabel('# Newton Raphson Iterations')
legend('Location',"best")
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