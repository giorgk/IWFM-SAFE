function out = readStreamHeadAndStage()
AllHeads = readIWFM_headalloutput(fullfile('Results','Ex1_GW_HeadAll.out'),0);
Hstage = readIWFM_StreamStage(fullfile('Results','Ex1_Stream_hyd.dat'), 12);
Head = zeros(size(Hstage.Hs));
igw = [85 84 83 82 81 80 79 78 77 76 75 74 73 72 71 69 70];
for ii = 1:12
    Head(:,ii) = AllHeads{ii+1,2}(igw);
end
out.H = Head;
out.Hs = Hstage.Hs./100000;


