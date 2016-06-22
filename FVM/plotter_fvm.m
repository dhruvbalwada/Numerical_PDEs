% function to plot the matrices

clear all
numx = 128;
nodes = load(['grid_generator/nodes_' num2str(numx) '.txt']);
conn = load(['grid_generator/connections_' num2str(numx) '.txt']);
timestep =0.902;
initial_cond = load(['./' num2str(numx) '/Sol_' num2str(timestep) '.txt']);
% initial_cond = load(['./solution' '/Sol_' num2str(timestep) '.txt']);
% solution = load('Sol_3.txt');

for i =1:size(conn,1)
    X(i) = 0.25*(nodes(conn(i,1),1)+ nodes(conn(i,2),1) +nodes(conn(i,3),1) + nodes(conn(i,4),1));
    Y(i) = 0.25*(nodes(conn(i,1),2)+ nodes(conn(i,2),2) +nodes(conn(i,3),2) + nodes(conn(i,4),2));
end

init_p = initial_cond(:,1);
X = reshape(X,[numx,numx]);
Y = reshape(Y,[numx,numx]);
P= reshape(init_p,[numx,numx]);


%% calculate anal solution
omega = 0:0.5:200;
t = timestep;
b = log(2)/0.06^2;

for i =1:size(X,1)
    for j = 1:size(X,2)
        r1 = sqrt((X(i,j)-0.5)^2 + (Y(i,j)- 0.5)^2);
        r2 = sqrt((X(i,j)-1.5)^2 + (Y(i,j)- 0.5)^2);
        for k =1:length(omega)
            inside1(k) = exp(-omega(k)^2/4/b)/2/b *omega(k) *cos(omega(k)*t) *besselj(0,r1*omega(k))*abs(omega(1)-omega(2));
            inside2(k) = exp(-omega(k)^2/4/b)/2/b *omega(k) *cos(omega(k)*t) *besselj(0,r2*omega(k))*abs(omega(1)-omega(2));
        end
        p_anal(i,j) = sum(inside1) + sum(inside2);
    end
end

%%

figure(2)
h = pcolor(X,Y,P)
set(h,'edgecolor','none')
grid
colorbar
title('128 grid at T=0.9s')
caxis([-0.15 0.16])


figure(1)
h = pcolor(X,Y,p_anal)
set(h,'edgecolor','none')
grid
colorbar
title('Analytical Solution at T=0.9s')
caxis([-0.15 0.16])