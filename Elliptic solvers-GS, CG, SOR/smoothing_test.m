clear all 

cd ./smoother

grid = [2:2:18];

for i =1:length(grid)
A =dir(['U_64_' num2str(grid(i)) '.txt']);


   U(:,:,i) = load(A.name);
end
   
%%
for i= 1:2:length(grid)
    figure(i)
    contourf(squeeze(U(:,:,i)))
    set(gca,'fontsize',16)
    xlabel('X','fontsize',16)
    ylabel('Y','fontsize',16) 
    title(['Solution at time ' num2str(grid(i))],'fontsize',16)
    saveas(gcf,[num2str(i) '.jpeg'], 'jpeg')
end

%%
figure(10),clf,  hold all
for i= 1:length(grid)
plot(squeeze(U(:,32,i)))
end
legend(num2str(grid'))
set(gca,'fontsize',16)
xlabel('Grid point number','fontsize',16)
ylabel('Solution','fontsize',16)
