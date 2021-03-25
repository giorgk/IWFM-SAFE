function out = readStreamNodesBudget()
nTimeSteps = 12;
strm_node_bud = h5info(fullfile('Results','Ex1_Stream_Node_Budget.hdf'));
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

out.GwIn = strm_nodesGW;
out.UpIn = strm_nodesUpIn;
out.DwOut = strm_nodesDwOut;

