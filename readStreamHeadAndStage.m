function out = readStreamHeadAndStage(headfile, streamfile, Ntimes, igw)
AllHeads = readIWFM_headalloutput(headfile,0);
Hstage = readIWFM_StreamStage(streamfile, Ntimes);
Head = zeros(size(Hstage.Hs));
for ii = 1:12
    Head(:,ii) = AllHeads{ii+1,2}(igw);
end
out.H = Head;
out.Hs = Hstage.Hs./100000;


