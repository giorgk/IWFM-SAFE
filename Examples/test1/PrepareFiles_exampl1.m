%% Domain and Mesh parameters
leftLower = [0, 0];
upperRight = [2500, 1250];
dx = 156.2500;
Nrows = (upperRight(2) - leftLower(2))/dx;
Ncols = (upperRight(1) - leftLower(1))/dx;
%% create mesh
Meshpoly = [];
Xgrid = leftLower(1):dx:upperRight(1);
Ygrid = leftLower(2):dx:upperRight(2);
cnt = 1;
for r = 1:Nrows
    for c = 1:Ncols
        elem = [...
            Xgrid(c) Ygrid(r); ...
            Xgrid(c+1) Ygrid(r); ...
            Xgrid(c+1) Ygrid(r+1); ...
            Xgrid(c) Ygrid(r+1)
            ];
        %plot(elem(:,1), elem(:,2))
        %hold on
        Meshpoly(cnt,1).poly = elem;
        cnt = cnt + 1;
    end
end
%% plot mesh
clf
hold on
for ii = 1:length(Meshpoly)
    plot(Meshpoly(ii,1).poly([1:4 1],1), Meshpoly(ii,1).poly([1:4 1],2),'b','linewidth',1.5)
end
plot([leftLower(1) upperRight(1)], [625 625],'r','linewidth',2.5) 
axis equal
xlim([leftLower(1) upperRight(1)]);
ylim([leftLower(2) upperRight(2)]); 
text((leftLower(1)+upperRight(1))/2, leftLower(2)-150,'No Flow','HorizontalAlignment','center','fontsize', 16)
text((leftLower(1)+upperRight(1))/2, upperRight(2)+150,'No Flow','HorizontalAlignment','center','fontsize', 16)
text(leftLower(1) - 200, (leftLower(2)+upperRight(2))/2,'H = 95 m','HorizontalAlignment','center','fontsize', 16,'Rotation',90)
text(upperRight(1) + 100, (leftLower(2)+upperRight(2))/2,'H = 101 m','HorizontalAlignment','center','fontsize', 16,'Rotation',90)
text(2200,625+100,'River','color','r','fontsize', 16)
%print -dpng -r600 domain_aerialView
%% Make a unique list of Nodes and index the elements
MSH = [];
XY = [];
RG = [];
for ii = 1:size(Meshpoly,1)
    el = [];
    for jj = 1:size(Meshpoly(ii,1).poly,1)
        if isempty(XY)
            XY = Meshpoly(ii,1).poly(jj,:);
            id = 1;
        else
            xy = Meshpoly(ii,1).poly(jj,:);
            dst = sqrt((XY(:,1) - xy(1)).^2 + (XY(:,2) - xy(2)).^2);
            [cc, dd] = min(dst);
            if cc < 0.1
               id = dd; 
            else
                XY = [XY;xy];
                id = length(XY);
            end
        end
        el = [el id];
    end
    cc = mean(Meshpoly(ii,1).poly);
    if cc(1) > 1250
        RG = [RG;2];
    else
        RG = [RG;1];
    end
    MSH = [MSH;el];
end
%% Print the Nodes and Element files
fid = fopen(fullfile('Preprocessor', 'Ex1_Nodes.dat'),'w');
fprintf(fid, '%d\t /ND\n',size(XY,1));
fprintf(fid, '%.4f\t /FACT\n',1);
fprintf(fid, '%d\t %.2f\t %.2f\n', [[1:size(XY,1)]' XY]');
fclose(fid);

fid = fopen(fullfile('Preprocessor', 'Ex1_Elements.dat'),'w');
fprintf(fid, '%d\t /NE\n',size(MSH,1));
fprintf(fid, '%d\t /NREGN\n',length(unique(RG)));
for ii = 1:length(unique(RG))
   fprintf(fid, 'Subregion %d\t /RNAME%d\n', [ii ii]); 
end
fprintf(fid, '%d\t %d\t %d\t %d\t %d\t %d\t\n', [[1:size(MSH,1)]' MSH RG]');
fclose(fid);
%% Specify stratigraphy parameters
ElevRight = 106;
ElevLeft = 101;
BotRight = 50;
BotLeft = 50;
Nlayers = 3;
ELEV = zeros(size(XY,1),Nlayers+1);
ELEV(:,1) = ElevLeft + (ElevRight - ElevLeft)*(XY(:,1) - leftLower(1))/(upperRight(1) - leftLower(1));
ELEV(:,4) = BotLeft + (BotRight - BotLeft)*(XY(:,1) - leftLower(1))/(upperRight(1) - leftLower(1));
u = [0 0.4 0.5 1];
ELEV(:,2) = ELEV(:,1)*(1-u(2)) + ELEV(:,4)*u(2);
ELEV(:,3) = ELEV(:,1)*(1-u(3)) + ELEV(:,4)*u(3);
Thickness = -diff(ELEV,1,2);
Stream_bot_right = 100;
Stream_bot_left = 91;
%%
clf
hold on
for ii = 1:4
    plot3(XY(:,1),XY(:,2),ELEV(:,ii),'.-k','linewidth',2)
end
plot3([leftLower(1) upperRight(1)],[0 0],[95 101],'--b','linewidth',3)
plot3([leftLower(1) upperRight(1)],[0 0],[Stream_bot_left Stream_bot_right],'-m','linewidth',3)
view(0,0)
text(1600,0,90,'Layer 1 (K = 20 m/day)','fontsize', 16)
text(1600,0,80,'Layer 2 (K = 1 m/day)','fontsize', 16)
text(1600,0,60,'Layer 3 (K = 5 m/day)','fontsize', 16)
text(100,0,105,'GSE','fontsize', 16)
text(100,0,98,'WT','fontsize', 16,'color','b')
text(100,0,87,{'Stream','Bottom'},'fontsize', 16,'color','m')
text(-300,0,91,'91 m','fontsize', 16,'color','m')
text(-300,0,95,'95 m','fontsize', 16,'color','b')
text(2500,0,99,'100 m','fontsize', 16,'color','m')
text(2500,0,103,'101 m','fontsize', 16,'color','b')
% print -dpng -r600 domain_cross_section
%% Write stratigraphy
fid = fopen(fullfile('Preprocessor', 'Ex1_Stratigraphy.dat'),'w');
fprintf(fid, '%d\t /NL\n',Nlayers);
fprintf(fid, '%.1f\t /FACT\n',1);
fprintf(fid, '%d\t %.2f\t 0.0 %.2f 0.0 %.2f 0.0 %.2f\n', [[1:size(XY,1)]' ELEV(:,1) Thickness]');
fclose(fid);
%% Stream specifications
id_stream = find(XY(:,2) == 625);
[B, ind_sort] = sort(XY(id_stream,1),'descend');

id_stream(ind_sort)
% rating table
rt = [0 0 0;...
    2 734.94 10;...
    5 3299.29 15;...
    15 19033.6 36;...
    25 41568.45 2000];

Stream_bot = Stream_bot_left + (Stream_bot_right - Stream_bot_left)*(XY(id_stream(ind_sort),1) - leftLower(1))/(upperRight(1) - leftLower(1));
%% plot rating table
% plot(rt(:,1), rt(:,2),'.-')
plot(rt(1:4,1), rt(1:4,3),'.-')
hold on
plot([1.0950 1.0950], [0, 5.475],':r','linewidth', 1.5)
xlabel('max(h,h_s)')
ylabel('Wetted Perimeter')

grid on
%% Print Stream Specification file
fid = fopen(fullfile('Preprocessor', 'Ex1_StreamSpec.dat.tmp'),'w');
fprintf(fid, '#4.1\n');
fprintf(fid, '%d\t /NRH\n',1);
fprintf(fid, '%d\t /NRTB\n',size(rt,1));
fprintf(fid, '%d %d 0 REACH1\n', [1 length(id_stream)]);
fprintf(fid, '%d %d\n', [[1:length(id_stream)]' id_stream(ind_sort)]');
fprintf(fid, '%.1f\t /FACTLT\n',1);
fprintf(fid, '%.1f\t /FACTQ\n',60);
fprintf(fid, '1min\t /TUNIT\n');
for ii = 1:length(id_stream)
    fprintf(fid, '%d\t%.2f\t %.1f %.2f\t %.1f\n',[ii Stream_bot(ii) rt(1,:)]);
    fprintf(fid, '\t\t%.1f\t%.2f\t %.1f\n', rt(2:end,:)');
end
fprintf(fid, '%d\t /NSTRPINT\n',0);
fclose(fid);
%% Print a list of the specified head Boundary conditions
BC_left_id = find(abs(XY(:,1) - leftLower(1)) < eps);
BC_right_id = find(abs(XY(:,1) - upperRight(1)) < eps);
%% plot bc 
clf
hold on
plot(XY(:,1), XY(:,2),'.')
plot(XY(BC_right_id,1), XY(BC_right_id,2),'or')
plot(XY(BC_left_id,1), XY(BC_left_id,2),'og')
fprintf('%d\t1\t0\t%.1f\n',[BC_left_id 95*ones(length(BC_left_id),1); BC_right_id 101*ones(length(BC_right_id),1)]');
%% print initial head
initHead = 95 + (101 - 95)*(XY(:,1) - leftLower(1))/(upperRight(1) - leftLower(1));
fprintf('%d\t%.3f\t%.3f\t%.3f\n',[(1:size(XY,1))' repmat(initHead,1,3)]')
%%
for ii = 1:231
    fprintf('%d 131.00 131.00 131.00\n',ii);
end
%% ================== RUN =========================
% IWFM executable path
iwfm_bin = 'f:\UCDAVIS\IWFM\IWFM_2015_0_1045\iwfm-2015.0.1045_sourcecode\IWFM\IWFM-2015.0.1045\Bin\';
nTimeSteps = 12;
%% RUN Preprocessor
system([iwfm_bin 'PreProcessor2015_D.exe Preprocessor\Ex1_Preprocessor.in']);
%% RUN Simulation
system([iwfm_bin 'Simulation2015_D.exe Simulation\Ex1_Simulation.in']);
%% ============= POST PROCESSING================
nTimeSteps = 12;
XY = readC2Vsim_Nodes(fullfile('Preprocessor','Ex1_Nodes.dat'));
H = readC2Vsim_headalloutput(fullfile('Simulation','Results','Ex1_GW_HeadAll.out'));
%%
clf
tr = delaunay(XY(:,1), XY(:,2));
for ii = 1:length(H)
    ii
    trisurf(tr,XY(:,1), XY(:,2), H{ii,2}(:,1),'FaceColor','interp','FaceLighting','phong')
    camlight right
    drawnow
    pause(0.5)
    M(ii) = getframe;
    im{ii} = frame2im(M(ii));
end
%% Write as gif
filename = 'Ex1_head.gif';
for idx = 1:length(H)
    [A,map] = rgb2ind(im{idx},256);
    if idx == 1
        imwrite(A,map,filename,'gif','LoopCount',Inf,'DelayTime',1);
    else
        imwrite(A,map,filename,'gif','WriteMode','append','DelayTime',1);
    end
end
%% STREAM BUDGET
strm_bud = h5info(fullfile('Simulation','Results','Ex1_Stream_Budget.hdf'));
tmp = h5read(strm_bud.Filename,[strm_bud.Name strm_bud.Datasets(1).Name]);
%% STREAM BUDGET NODES
strm_node_bud = h5info(fullfile('Simulation','Results','Ex1_Stream_Node_Budget.hdf'));
Stream_col_names = strm_node_bud.Groups.Attributes(11).Value(2:end);
strm_nodesGW = zeros(length(strm_node_bud.Datasets), nTimeSteps);
strm_nodesUpIn = zeros(length(strm_node_bud.Datasets), nTimeSteps);
strm_nodesDwOut = zeros(length(strm_node_bud.Datasets), nTimeSteps);
for ii = 1:length(strm_node_bud.Datasets)
    id = textscan(strm_node_bud.Datasets(ii).Name,'NODE%s');
    id = str2double(id{1,1}{1,1});
    ttt = h5read(strm_node_bud.Filename,[strm_node_bud.Name strm_node_bud.Datasets(ii).Name]);
    strm_nodesGW(id,:) = ttt(7, :);
    strm_nodesUpIn(id,:) = ttt(1, :);
    strm_nodesDwOut(id,:) = ttt(2, :);
end