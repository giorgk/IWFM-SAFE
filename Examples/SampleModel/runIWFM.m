function runIWFM(pre,sim)

if pre
    system([fullfile('..','..','code','Bin','PreProcessor2015_D.exe') ...
        ' ' fullfile('Preprocessor','PreProcessor_MAIN.IN')]);  
end

if sim
    system([fullfile('..','..','code','Bin','Simulation2015_D.exe') ...
        ' ' fullfile('Simulation','Simulation_MAIN.IN')]);
end

