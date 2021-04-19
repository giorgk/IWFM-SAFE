% Othe parameters Kc_l = 1; Anis = 100
%% IWFM ===========
idx = 1;
runIWFM(1,1);
IWFM41(idx,1).Desc = 'W_{lay1} = 10 m';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 2;
runIWFM(1,1);
IWFM41(idx,1).Desc = 'W_{lay1} = 20 m';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 3;
runIWFM(1,1);
IWFM41(idx,1).Desc = 'W_{lay1} = 30 m';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 4;
runIWFM(1,1);
IWFM41(idx,1).Desc = 'W_{lay1} = 50 m';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 5;
runIWFM(1,1);
IWFM41(idx,1).Desc = 'W_{lay1} = 70 m';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%%
idx = 6;
runIWFM(1,1);
IWFM41(idx,1).Desc = 'W_{lay1} = 90 m';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('iwfm_test.dat');
IWFM41(idx,1).DH = readStreamHeadAndStage();
%% SAFE ============
idx = 1;
runIWFM(1,1);
SAFE(idx,1).Desc = 'W_{lay1} = 10 m';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 2;
runIWFM(1,1);
SAFE(idx,1).Desc = 'W_{lay1} = 20 m';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 3;
runIWFM(1,1);
SAFE(idx,1).Desc = 'W_{lay1} = 30 m';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 4;
runIWFM(1,1);
SAFE(idx,1).Desc = 'W_{lay1} = 50 m';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 5;
runIWFM(1,1);
SAFE(idx,1).Desc = 'W_{lay1} = 70 m';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%%
idx = 6;
runIWFM(1,1);
SAFE(idx,1).Desc = 'W_{lay1} = 90 m';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');
SAFE(idx,1).DH = readStreamHeadAndStage();
%% Compare the convergance with respect to the depth of the layer
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
clf 
hold on
for ii = 1:length(SAFE)
    plot(2:17,SAFE(ii,1).SWGW.GwIn(2:17,12),'DisplayName',SAFE(ii,1).Desc, 'LineWidth',2);
end
grid on
legend('Location','best','NumColumns',2)
xlabel('Stream node ID')
ylabel('m^3/month')
title ('SAFE SW - GW interaction')