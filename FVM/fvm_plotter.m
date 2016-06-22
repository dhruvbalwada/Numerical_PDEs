% matlab function to compare results

clear all
close all

mesh_sizes = [32, 64, 128];

for i =1:length(mesh_sizes)
    eval(['cd ' num2str(mesh_sizes(i))])
    a(i).files = dir('Sol_*');
    eval('cd ..')
    for j=1:length(a(i).files)
        a(i).times(j) = str2num(a(i).files(j).name(5:9));
    end
end

% find when both systems have solutions

for k =1:length(a(1).times)
    id1(k) = k;
    id2(k) = find(a(2).times == a(1).times(k));
    id3(k) = find(a(3).times == a(1).times(k));
end
timestep =0.35;
timeid = find(a(1).times<timestep,1,'last');

for i =1:length(mesh_sizes)
    eval(['cd ' num2str(mesh_sizes(i))])
    a(i).files = dir('Sol_*');
    nodes = load(['../grid_generator/nodes_' num2str(mesh_sizes(i)) '.txt']);
    conn = load(['../grid_generator/connections_' num2str(mesh_sizes(i)) '.txt']);
    
    for j =1:size(conn,1)
        X(j) = 0.25*(nodes(conn(j,1),1)+ nodes(conn(j,2),1) +nodes(conn(j,3),1) + nodes(conn(j,4),1));
        Y(j) = 0.25*(nodes(conn(j,1),2)+ nodes(conn(j,2),2) +nodes(conn(j,3),2) + nodes(conn(j,4),2));
    end
    grid(i).X = reshape(X,[mesh_sizes(i),mesh_sizes(i)]);
    grid(i).Y = reshape(Y,[mesh_sizes(i),mesh_sizes(i)]);
    clear X Y
    
    P = load(a(i).files(eval(['id' num2str(i) '(' num2str(timeid) ')'])).name);
    grid(i).P = reshape(P(:,1),[mesh_sizes(i),mesh_sizes(i)]);
    clear P
    eval('cd ..')
end

%%

figure(1),clf, hold all
idy1 = find(grid(1).Y(:,1)<=0.5,1,'last');
grid(1).Y(idy1,1)
plot(grid(1).X(1,:),grid(1).P(idy1,:))
idy2 = find(grid(2).Y(:,1)<=0.5,1,'last');
grid(2).Y(idy2,1)
plot(grid(2).X(1,:),grid(2).P(idy2,:))
idy3 = find(grid(3).Y(:,1)<=0.5,1,'last');
grid(3).Y(idy3,1)
plot(grid(3).X(1,:),grid(3).P(idy3,:))

% calculate analytical solution at mid mesh size
%% calculate anal solution
omega = 0:0.1:200;
t = timestep;
b = log(2)/0.06^2;
dx = 1./mesh_sizes;
% for i =1:size(X,1)
for m =1:length(mesh_sizes)
    clear p_anal 
    for j = 1:size(grid(m).X,2)
        i= eval(['idy' num2str(m)]);
        r1 = sqrt((grid(m).X(i,j)-0.5)^2 + (grid(m).Y(i,j)- 0.5)^2);
        r2 = sqrt((grid(m).X(i,j)-1.5)^2 + (grid(m).Y(i,j)- 0.5)^2);
        for k =1:length(omega)
            inside1(k) = exp(-omega(k)^2/4/b)/2/b *omega(k) *cos(omega(k)*t)*besselj(0,r1*omega(k))*abs(omega(1)-omega(2));
            inside2(k) = exp(-omega(k)^2/4/b)/2/b *omega(k) *cos(omega(k)*t)*besselj(0,r2*omega(k))*abs(omega(1)-omega(2));
        end
        p_anal(j) = sum(inside1) + sum(inside2);
        %     end
    end
    
    Perr(m) = (mean(dx(m)*(p_anal - grid(m).P(i,:)).^2))^0.5;
    
    %
    if m==2
        figure(1)
        plot(grid(2).X(1,:),p_anal)
        xlabel('X')
        ylabel('P at Y~0.5')
        legend('32','64','128','Analytical')
        title(['Solution at ' num2str(timestep) ' s'])
        fname = [num2str(timestep) '.jpeg'];
        saveas(gcf,fname,'jpeg')
    end
end

%% Plot errors now

figure(2), clf 
loglog(dx, Perr,'.','markersize',25)
xlabel('dx')
ylabel('L_2 Norm of error')
axis([10^-3 10^-1 10^-3 10^-2])