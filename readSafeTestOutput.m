function out = readSafeTestOutput(varargin)
if nargin > 0
    filename = varargin{1};
else
    return
end

if nargin > 1
    N = varargin{2};
else
    N = 17;
end

Niter = [];
TSiter = 1;
timestep = 1;
NRiter = 1;
fid = fopen(filename,'r');
while ~feof(fid)
    tline = fgetl(fid);
    if isempty(tline)
        continue;
    end
    C = textscan(tline, '%s %f');
    TSiter =  C{1,2};
    if TSiter == 1
        if NRiter > 1
            StrmH(NRiter:end,:) = [];
            GWH(NRiter:end,:) = [];
            DiscEl(NRiter:end,:) = [];
            Hs(NRiter:end,:) = [];
            HDiff(NRiter:end,:) = [];
            RHS(NRiter:end,:) = [];
            COEF1(NRiter:end,:) = [];
            COEF2(NRiter:end,:) = [];
            out(timestep,1).StrmH = StrmH;
            out(timestep,1).GWH = GWH;
            out(timestep,1).DiscEl = DiscEl;
            out(timestep,1).Hs = Hs;
            out(timestep,1).HDiff = HDiff;
            out(timestep,1).RHS = RHS;
            out(timestep,1).COEF1 = COEF1;
            out(timestep,1).COEF2 = COEF2;
            out(timestep,1).Niter = NRiter-1;
            timestep = timestep + 1;
        end
       NRiter = 1; 
       RHS1 = nan(100,N);
       RHS2 = nan(100,N);
       COEF1 = nan(100,N);
       COEF2 = nan(100,N);
    end
    for ii = 1:N
        tline = fgetl(fid);
        C = textscan(tline, '%f');
        StrmH(NRiter,ii) = C{1,1}(3);
        GWH(NRiter,ii) = C{1,1}(4);
        DiscEl(NRiter,ii) = C{1,1}(5);
        Hs(NRiter,ii) = C{1,1}(6);
        HDiff(NRiter,ii) = C{1,1}(7);
        RHS(NRiter,ii) = C{1,1}(8);
        COEF1(NRiter,ii) = C{1,1}(9);
        COEF2(NRiter,ii) = C{1,1}(10);
    end
    NRiter = NRiter + 1;
end

StrmH(NRiter:end,:) = [];
GWH(NRiter:end,:) = [];
DiscEl(NRiter:end,:) = [];
Hs(NRiter:end,:) = [];
HDiff(NRiter:end,:) = [];
RHS(NRiter:end,:) = [];
COEF1(NRiter:end,:) = [];
COEF2(NRiter:end,:) = [];
out(timestep,1).StrmH = StrmH;
out(timestep,1).GWH = GWH;
out(timestep,1).DiscEl = DiscEl;
out(timestep,1).Hs = Hs;
out(timestep,1).HDiff = HDiff;
out(timestep,1).RHS = RHS;
out(timestep,1).COEF1 = COEF1;
out(timestep,1).COEF2 = COEF2;
out(timestep,1).Niter = NRiter-1;

fclose(fid);
