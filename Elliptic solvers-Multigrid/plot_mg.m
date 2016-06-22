% Code to look at the residual behavior of the Multigrid solver

clear all 
close all 

cd ./result_1_1

A = dir('res*');
grid_size = [256 128 64 32];


figure(1)
for i=1:length(grid_size)
    residual(i).res = load(['res_' num2str(grid_size(i)) '_1.0.txt']);

    semilogy(residual(i).res)
     hold all 
end

legend(num2str(grid_size'))
set(gca,'fontsize',16)
xlabel('Number of V-Cycles')
ylabel('Residual ||R||_{max}')


grid = [256 128 64 32];

for k=1:length(grid)

A = dir(['X_' num2str(grid(k)) '*']);
X = load(A.name);
A = dir(['Y_' num2str(grid(k)) '*']);
Y = load(A.name);
A = dir(['U_' num2str(grid(k)) '*']);
U = load(A.name);

h = 1/grid(k);

clear anal_solution
% Calculate the exact solution
for i =1:length(X)
    for j = 1:length(Y) 
        
       anal_solution(j,i) = cos(X(i))*sin(Y(j));
    end
end
error = (anal_solution-U)*h;
maxnormerr(k)= norm(error,2);
end
%%
figure(3)
loglog((1./grid), maxnormerr,'.-')
xlabel('h','fontsize',16)
ylabel('||E||_2','fontsize',16)
title('Order of the equation')
set(gca,'fontsize',16) 

cd ..