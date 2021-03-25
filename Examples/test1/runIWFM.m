function runIWFM(pre,sim)

if pre
    system([fullfile('..','..','code','Bin','PreProcessor2015_D.exe') ...
        ' ' fullfile('Preprocessor','Ex1_Preprocessor.in')]);  
end

if sim
    system([fullfile('..','..','code','Bin','Simulation2015_D.exe') ...
        ' ' fullfile('Simulation','Ex1_Simulation.in')]);
end

