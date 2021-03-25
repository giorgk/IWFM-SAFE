% This script read the output data and puts them in a variable that can be
% retrieved later
%% Base simulation model
idx = 1;
IWFM41(idx,1).Desc='Base';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('safe_test.dat');
%% Change Kcl to 4
idx = 2;
IWFM41(idx,1).Desc='K_{cl} = 4';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('safe_test.dat');
%% Change Kcl to 6
idx = 3;
IWFM41(idx,1).Desc='K_{cl} = 6';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('safe_test.dat');
%% Change Kcl to 10
idx = 4;
IWFM41(idx,1).Desc='K_{cl} = 10';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('safe_test.dat');
%% Change Kcl to 1
idx = 5;
IWFM41(idx,1).Desc='K_{cl} = 1';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('safe_test.dat');
%% Change Kcl to 0.1
idx = 6;
IWFM41(idx,1).Desc='K_{cl} = 0.1';
IWFM41(idx,1).SWGW = readStreamNodesBudget();
IWFM41(idx,1).CONV = readSafeTestOutput('safe_test.dat');


%% SAFE
%% Base simulation model
idx = 1;
SAFE(idx,1).Desc='Base';
SAFE(idx,1).SWGW = readStreamNodesBudget();
SAFE(idx,1).CONV = readSafeTestOutput('safe_test.dat');