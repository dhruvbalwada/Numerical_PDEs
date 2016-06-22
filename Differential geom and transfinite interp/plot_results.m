% function to calculate the covariant basis vectors on the generated grids.
%

clear all
close all

cd ../Elliptic_grid/nozzle_out/

grid_size = [32 64];



for i =1:length(grid_size)
    X = load(['X_' num2str(grid_size(i)) '.txt']);
    dxdzeta = NaN*X;
    dydzeta = NaN*X;
    dxdeta = NaN*X;
    dydeta = NaN*X;
    d2xdzeta2 = NaN*X;
    d2ydzeta2 = NaN*X;
    d2xdeta2 = NaN*X;
    d2ydeta2 = NaN*X;
    d3xdzeta3 = NaN*X;
    d3ydzeta3 = NaN*X;
    d3xdeta3 = NaN*X;
    d3ydeta3 = NaN*X;
    X = load(['X_' num2str(grid_size(i)) '.txt']);
    X = X';
   
    Y = load(['Y_' num2str(grid_size(i)) '.txt']);
     Y = Y';
    %   Xtop = load(['Xtop_' num2str(grid_size(i)) '.txt']);
    %   Ytop = load(['Ytop_' num2str(grid_size(i)) '.txt']);
    
    figure, hold all
    plot(X,Y,'.')
    %    plot(Xtop, Ytop)
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    saveas(gcf,['grid_' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    h= 1/grid_size(i);
    for j =2:grid_size(i)
        for k = 2:grid_size(i)
            dxdzeta(j,k) = (X(j,k+1) - X(j,k-1))/h;
            dydzeta(j,k) = (Y(j,k+1) - Y(j,k-1))/h;
            dxdeta(j,k) = (X(j+1,k) - X(j-1,k))/h;
            dydeta(j,k) = (Y(j+1,k) - Y(j-1,k))/h;
        end
    end
    for j =2:grid_size(i)
        for k = 2:grid_size(i)
            d2xdzeta2(j,k) = (dxdzeta(j,k+1) - dxdzeta(j,k-1))/h;
            d2ydzeta2(j,k) = (dydzeta(j,k+1) - dydzeta(j,k-1))/h;
            d2xdeta2(j,k) = (dxdeta(j+1,k) - dxdeta(j-1,k))/h;
            d2ydeta2(j,k) = (dydeta(j+1,k) - dydeta(j-1,k))/h;
        end
    end
    for j =2:grid_size(i)
        for k = 2:grid_size(i)
            d3xdzeta3(j,k) = (d2xdzeta2(j,k+1) - d2xdzeta2(j,k-1))/h;
            d3ydzeta3(j,k) = (d2ydzeta2(j,k+1) - d2ydzeta2(j,k-1))/h;
            d3xdeta3(j,k) = (d2xdeta2(j+1,k) - d2xdeta2(j-1,k))/h;
            d3ydeta3(j,k) = (d2ydeta2(j+1,k) - d2ydeta2(j-1,k))/h;
        end
    end
    
    %%
    figure
    %contourf(X,Y,dxdzeta ...
    %    ,20,'Edgecolor','none')
    h= pcolor(X,Y,dxdzeta)
    set(h,'edgecolor','none') 
    
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('dx/d\zeta')
    colormap(summer)
    colorbar
    saveas(gcf,['dxdzeta' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    %%
    figure
  %  contourf(X,Y,dydzeta ...
   %     ,20,'Edgecolor','none')
     h= pcolor(X,Y,dydzeta)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('dy/d\zeta')
    colormap(summer)
    colorbar
    saveas(gcf,['dydzeta' num2str(grid_size(i)) '.jpeg'],'jpeg')
    %%
    figure
%     contourf(X,Y,dxdeta  ...
%         ,20,'Edgecolor','none')
  h= pcolor(X,Y,dxdeta)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('dx/d\eta')
    colormap(summer)
    colorbar
    saveas(gcf,['dxdeta' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    figure
%     contourf(X,Y,dydeta ...
%         ,20,'Edgecolor','none')
  h= pcolor(X,Y,dydeta)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('dy/d\eta')
    colormap(summer)
    colorbar
    saveas(gcf,['dydeta' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    figure
%     contourf(X,Y,d2xdzeta2 ...
%         ,20,'Edgecolor','none')
     h= pcolor(X,Y,d2xdzeta2)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^2x/d\zeta^2')
    colormap(summer)
    colorbar
    saveas(gcf,['d2xdzeta2' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    figure
%     contourf(X,Y,d2xdeta2 ...
%         ,20,'Edgecolor','none')
  h= pcolor(X,Y,d2xdeta2)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^2x/d\eta^2')
    colormap(summer)
    colorbar
    saveas(gcf,['d2xdeta2' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    
    figure
%     contourf(X,Y,d2ydzeta2 ...
%         ,20,'Edgecolor','none')
    h= pcolor(X,Y,d2ydzeta2)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^2y/d\zeta^2')
    colormap(summer)
    colorbar
    saveas(gcf,['d2ydzeta2' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    figure
%     contourf(X,Y,d2ydeta2 ...
%         ,20,'Edgecolor','none')
      h= pcolor(X,Y,d2ydeta2)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^2y/d\eta^2')
    colormap(summer)
    colorbar
    saveas(gcf,['d2ydeta2' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    
    figure
%     contourf(X,Y,d3xdzeta3 ...
%         ,20,'Edgecolor','none')
     h= pcolor(X,Y,d3xdzeta3)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^3x/d\zeta^3')
    colormap(summer)
    colorbar
    saveas(gcf,['d3xdzeta3' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    figure
%     contourf(X,Y,d3xdeta3 ...
%         ,20,'Edgecolor','none')
  h= pcolor(X,Y,d3xdeta3)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^3x/d\eta^3')
    colormap(summer)
    colorbar
    saveas(gcf,['d3xdeta3' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    
    figure
%     contourf(X,Y,d3ydzeta3 ...
%         ,20,'Edgecolor','none')
      h= pcolor(X,Y,d3ydzeta3)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^3y/d\zeta^3')
    colormap(summer)
    colorbar
    saveas(gcf,['d3ydzeta3' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    figure
%     contourf(X,Y,d3ydeta3 ...
%         ,20,'Edgecolor','none')
  h= pcolor(X,Y,d3ydeta3)
    set(h,'edgecolor','none') 
    set(gca,'fontsize',14)
    xlabel('X')
    ylabel('Y')
    title('d^3y/d\eta^3')
    colormap(summer)
    colorbar
    saveas(gcf,['d3ydeta3' num2str(grid_size(i)) '.jpeg'],'jpeg')
    
    %%
    figure, hold all
    quiver(X,Y, dxdzeta, dydzeta,'color','k')
    quiver(X,Y, dxdeta, dydeta,'color','r')
    saveas(gcf,['covariant' num2str(grid_size(i)) '.jpeg'],'jpeg')
end

cd ..
