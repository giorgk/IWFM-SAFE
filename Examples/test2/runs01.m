addpath(fullfile('..','..','..','GWToolsRpack','gwtools','Matlab'))
addpath(fullfile('..','..'))
igw = [85 84 83 82 81 80 79 78 77 76 75 74 73 72 71 69 70];
%% Run IWFM
runIWFM(1,1);
idx = 1;
IWFM41(idx,1).Desc='IWFM 10';
IWFM41(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
runIWFM(1,1);
idx = 2;
IWFM41(idx,1).Desc='IWFM 20';
IWFM41(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
runIWFM(1,1);
idx = 3;
IWFM41(idx,1).Desc='IWFM 30';
IWFM41(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
runIWFM(1,1);
idx = 4;
IWFM41(idx,1).Desc='IWFM 50';
IWFM41(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
runIWFM(1,1);
idx = 5;
IWFM41(idx,1).Desc='IWFM 70';
IWFM41(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
clf 
hold on
for ii = 1:length(IWFM41)
    plot([IWFM41(ii,1).CONV.Niter]','DisplayName',IWFM41(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best')
xlabel('Time Step')
ylabel('# Newton Raphson Iterations')
title('IWFM')
%% Compare the SW - GW interaction
clf 
hold on
for ii = 1:length(IWFM41)
    plot(2:17,IWFM41(ii,1).SWGW.GwIn(2:17,4),'DisplayName',IWFM41(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best','NumColumns',2)
xlabel('Stream node ID')
ylabel('m^3/month')
title ('IWFM SW - GW interaction')
%%
idx = 1;
runIWFM(1,1);
SAFE(idx,1).Desc = 'SAFE 10';
SAFE(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 2;
runIWFM(1,1);
SAFE(idx,1).Desc = 'SAFE 20';
SAFE(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 3;
runIWFM(1,1);
SAFE(idx,1).Desc = 'SAFE 30';
SAFE(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 4;
runIWFM(1,1);
SAFE(idx,1).Desc = 'SAFE 50';
SAFE(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 5;
runIWFM(1,1);
SAFE(idx,1).Desc = 'SAFE 70';
SAFE(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
clf 
hold on
for ii = 1:length(SAFE)
    plot([SAFE(ii,1).CONV.Niter]','DisplayName',SAFE(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best')
xlabel('Time Step')
ylabel('# Newton Raphson Iterations')
title('SAFE')
%%
clf 
hold on
for ii = 1:length(SAFE)
    plot(2:17,SAFE(ii,1).SWGW.GwIn(2:17,4),'DisplayName',SAFE(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best','NumColumns',2)
xlabel('Stream node ID')
ylabel('m^3/month')
title ('SAFE SW - GW interaction')
%% Test tge 3rd layer as bottom
idx = 1;
runIWFM(1,1);
SAFE3(idx,1).Desc = 'SAFE 10';
SAFE3(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE3(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE3(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 2;
runIWFM(1,1);
SAFE3(idx,1).Desc = 'SAFE 20';
SAFE3(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE3(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE3(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 3;
runIWFM(1,1);
SAFE3(idx,1).Desc = 'SAFE 30';
SAFE3(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE3(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE3(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 4;
runIWFM(1,1);
SAFE3(idx,1).Desc = 'SAFE 50';
SAFE3(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE3(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE3(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
idx = 5;
runIWFM(1,1);
SAFE3(idx,1).Desc = 'SAFE 70';
SAFE3(idx,1).SWGW = readStreamNodesBudget(fullfile('Results','Ex2_Stream_Node_Budget.hdf'),12);
SAFE3(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE3(idx,1).DH = readStreamHeadAndStage(fullfile('Results','Ex2_GW_HeadAll.out'),...
    fullfile('Results','Ex2_Stream_hyd.dat'), 12, igw);
%%
clf 
hold on
for ii = 1:length(SAFE3)
    plot(2:17,SAFE3(ii,1).SWGW.GwIn(2:17,4),'DisplayName',SAFE3(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best','NumColumns',2)
xlabel('Stream node ID')
ylabel('m^3/month')
title ('SAFE(3) SW - GW interaction')
%%
clf 
hold on
for ii = 1:length(SAFE3)
    plot(2:17,SAFE(ii,1).SWGW.GwIn(2:17,4) - SAFE3(ii,1).SWGW.GwIn(2:17,4),'DisplayName',SAFE3(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best','NumColumns',2)
xlabel('Stream node ID')
ylabel('m^3/month')
title ('SAFE(1) - SAF(3) SW - GW interaction')
%%
clf 
hold on
for ii = 1:length(SAFE3)
    plot(2:17,IWFM41(ii,1).SWGW.GwIn(2:17,4) - SAFE(ii,1).SWGW.GwIn(2:17,4),'DisplayName',SAFE3(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best','NumColumns',2)
xlabel('Stream node ID')
ylabel('m^3/month')
title ('IWFM - SAFE(1) SW - GW interaction')