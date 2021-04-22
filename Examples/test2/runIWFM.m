function runIWFM(pre,sim)

if pre
    system([fullfile('..','..','code','Bin','PreProcessor2015_D.exe') ...
        ' ' fullfile('Preprocessor','Ex2_Preprocessor.in')]);  
end

if sim
    system([fullfile('..','..','code','Bin','Simulation2015_D.exe') ...
        ' ' fullfile('Simulation','Ex2_Simulation.in')]);
end

