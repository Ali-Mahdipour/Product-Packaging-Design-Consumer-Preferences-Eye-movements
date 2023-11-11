clear all
close all

%% main function
img = imread('pardazesh-40-60-s.jpg');

level = graythresh(img);
im = im2bw(img,level);

%  windowSize = 10;
%  kernel = ones(windowSize) / windowSize ^ 2;
%  blurryImage = conv2(single(im), kernel, 'same');
%  im = blurryImage > 0.5; % Rethreshold
%  figure
%  imshow(im)



BW =~im;

sz=size(BW);
cc = bwconncomp(BW, 8);

% cc.NumObjects = 9;
% cc.PixelIdxList(1) = [];
% cc.PixelIdxList(2) = [];

s  = regionprops(cc, 'centroid', 'Area','Perimeter','MajorAxisLength','MinorAxisLength', 'Orientation',...
        'ConvexArea', 'ConvexHull', 'ConvexImage', 'Eccentricity', 'Extent', 'Solidity');
sz=[cc.ImageSize];

obj = cc;

for k = 1:cc.NumObjects
   BW2 = zeros(sz);
    ctd = round([s(k).Centroid]);
    BW2(cc.PixelIdxList{k}) = 1;
    %     Area = bwarea(BW);
    ContourImage = bwmorph(BW2,'remove');
    %     find the beginning point
    [~,x0]=find(ContourImage(ctd(2), :),1, 'first');
    
    contour = bwtraceboundary(ContourImage, [ctd(2), x0], 'W', 8,...
        Inf,'counterclockwise');
    X=contour(:,2);
    Y=contour(:,1);
    
    % convert the cartesian coordinates to polar coordinates
    
    obj.objects(k).centroid = s(k).Centroid;
    obj.objects(k).orientation = s(k).Orientation;
    obj.objects(k).area = s(k).Area;
    obj.objects(k).perimeter = s(k).Perimeter;
    obj.objects(k).rawXY = [X, Y];
    obj.objects(k).d1d2 = [s(k).MajorAxisLength, s(k).MinorAxisLength];
    obj.objects(k).ConvexArea = s(k).ConvexArea;
    obj.objects(k).ConvexHull = s(k).ConvexHull;
    obj.objects(k).ConvexImage = s(k).ConvexImage;
    obj.objects(k).Eccentricity = s(k).Eccentricity;
    obj.objects(k).Extent = s(k).Extent;
    obj.objects(k).Solidity = s(k).Solidity;
    
end

% calculate the slops

sf = 20; % smoothing factor

for k = 1:cc.NumObjects
     for j = 1:floor((length(obj.objects(k).rawXY))/sf)
         if j == floor((length(obj.objects(k).rawXY))/sf)
             Xstart = obj.objects(k).rawXY(j*sf, 1);
             Xend = obj.objects(k).rawXY(1, 1);
             Ystart = obj.objects(k).rawXY(j*sf, 2);
             Yend = obj.objects(k).rawXY(1, 2);
             slop(j) = atan((Yend - Ystart)/(Xend - Xstart))*180/pi;
         else
         Xstart = obj.objects(k).rawXY(j*sf, 1);
         Xend = obj.objects(k).rawXY((j+1)*sf, 1);
         Ystart = obj.objects(k).rawXY(j*sf, 2);
         Yend = obj.objects(k).rawXY((j+1)*sf, 2);
         slop(j) = atan((Yend - Ystart)/(Xend - Xstart))*180/pi;
         end


     end

% calculate the change in the slop (curvature) 
% if the slop is steep (> 80 degrees) then make the current slop with same sign as last
% slop

     for j = 1:length(slop)
         currentS = slop(j);
         if j == 1
             lastS = slop(length(slop));
         else 
             lastS = slop(j-1);
         end
         if abs(lastS) > 81
             if currentS * lastS < 0
                 currentS = -currentS;
             end
         else
             if abs(currentS) > 80
                 if currentS * lastS < 0
                 currentS = -currentS;
                 end
             end
         end
         curveS(j) = currentS - lastS; 
     end

     % keep slop values between -90 to 90 degrees
     for j = 1:length(curveS)
         if curveS(j) > 90
             curveS(j) = 180 - curveS(j);
         else
             if curveS(j) < -90
                curveS(j) = -180 - curveS(j) ;
             end
         end
     end
        mincurve = 15;
        maxcurve = 15;
        curvSoS = nansum(curveS.^2);
        curv2 = curveS(abs(curveS)> 0);
        curvMoS2 = nanmean(curveS.^2);
        curveG20mean = nanmean(abs(curveS(abs(curveS) > mincurve)));

        % sum up all the slops less than 15 degrees (smooth ones) to calculate roundness
        roundness = nansum(abs(curveS(abs(curveS) < maxcurve))); 
%(curveS(abs(curveS) > 60)).^2
     
     %obj.objects(k).curvp = curvp;
     obj.objects(k).curvSoS = curvSoS;
     obj.objects(k).roundness = roundness;
%      obj.objects(k).curvature = curv;
     obj.objects(k).curveS = curveS;
     obj.objects(k).slop = slop;
     obj.objects(k).curvMoS2 = curvMoS2;
     obj.objects(k).curv2 = curv2;
     obj.objects(k).curveG20mean = curveG20mean;
     
end

% plot the roundness
imshow(im)
hold
for k = 1:numel(s)
    c = s(k).Centroid;
    text(c(1), c(2)+20, sprintf('%d', k), ...
        'HorizontalAlignment', 'center', ...
        'VerticalAlignment', 'middle', ...
        'Color', 'white', ...
        'FontSize', 16);
end
for k = 1:cc.NumObjects
% iter = 0;
    for j = 1:floor((length(obj.objects(k).rawXY))/sf)
%         iter = iter +1;
          if j == floor((length(obj.objects(k).rawXY))/sf)
             Xstart = obj.objects(k).rawXY(j*sf, 1);
             Xend = obj.objects(k).rawXY(1, 1);
             Ystart = obj.objects(k).rawXY(j*sf, 2);
             Yend = obj.objects(k).rawXY(1, 2);
         else
         Xstart = obj.objects(k).rawXY(j*sf, 1);
         Xend = obj.objects(k).rawXY((j+1)*sf, 1);
         Ystart = obj.objects(k).rawXY(j*sf, 2);
         Yend = obj.objects(k).rawXY((j+1)*sf, 2);
          end

            if all(abs(obj.objects(k).curveS(j)) > mincurve)
                col = 'r';
            else
                col = 'b';
            end
        plot([Xend, Xstart], [Yend, Ystart], col, 'LineWidth',1)
        plot([Xend, Xstart], [Yend, Ystart], 'o', 'Color', col,  'MarkerSize', 2)
     end
end


figure
imshow(im)
hold
for k = 1:numel(s)
    c = s(k).Centroid;
    text(c(1), c(2)+20, sprintf('%d', k), ...
        'HorizontalAlignment', 'center', ...
        'VerticalAlignment', 'middle', ...
        'Color', 'white', ...
        'FontSize', 16);
end
for k = 1:cc.NumObjects
% iter = 0;
    for j = 1:floor((length(obj.objects(k).rawXY))/sf)
%         iter = iter +1;
          if j == floor((length(obj.objects(k).rawXY))/sf)
             Xstart = obj.objects(k).rawXY(j*sf, 1);
             Xend = obj.objects(k).rawXY(1, 1);
             Ystart = obj.objects(k).rawXY(j*sf, 2);
             Yend = obj.objects(k).rawXY(1, 2);
         else
         Xstart = obj.objects(k).rawXY(j*sf, 1);
         Xend = obj.objects(k).rawXY((j+1)*sf, 1);
         Ystart = obj.objects(k).rawXY(j*sf, 2);
         Yend = obj.objects(k).rawXY((j+1)*sf, 2);
          end

            if all(abs(obj.objects(k).curveS(j)) < maxcurve)
                col = 'g';
            else
                col = 'b';
            end
        plot([Xend, Xstart], [Yend, Ystart], col, 'LineWidth',1)
        plot([Xend, Xstart], [Yend, Ystart], 'o', 'Color', col,  'MarkerSize', 2)
     end
end

roundnn = [obj.objects.roundness].';
areaa = [obj.objects.area].';
perimeterr = [obj.objects.perimeter].';
ConvexAreaa = [obj.objects.ConvexArea].';
Eccentricityy = [obj.objects.Eccentricity].';
Extentt = [obj.objects.Extent].';
packageNum = [1:9]';

results_roundness = [packageNum areaa perimeterr];
results.packageNum = packageNum;
results.areaa = areaa;
results.perimeterr = perimeterr;
results.ConvexAreaa = ConvexAreaa;
results.Eccentricityy = Eccentricityy;
results.Extentt = Extentt;


% Save the table to a CSV file
writetable(struct2table(results), 'results.csv');

for k = 1:cc.NumObjects
obj.objects(k).packageNum = k; 
obj.objects(k).areaNorm = (obj.objects(k).area - min(areaa))/(max(areaa)-min(areaa));
obj.objects(k).perimeterNorm = (obj.objects(k).perimeter - min(perimeterr))/(max(perimeterr)-min(perimeterr));
obj.objects(k).ConvexAreaNorm = (obj.objects(k).ConvexArea - min(ConvexAreaa))/(max(ConvexAreaa)-min(ConvexAreaa));
obj.objects(k).EccentricityNorm = (obj.objects(k).Eccentricity - min(Eccentricityy))/(max(Eccentricityy)-min(Eccentricityy));
obj.objects(k).ExtentNorm = (obj.objects(k).Extent - min(Extentt))/(max(Extentt)-min(Extentt));
obj.objects(k).roundnessNorm = (obj.objects(k).roundness - min(roundnn))/(max(roundnn)-min(roundnn));
end

resultNorm.packageNum = [obj.objects.packageNum].';
resultNorm.areaNorm = [obj.objects.areaNorm].';
resultNorm.perimeterNorm = [obj.objects.perimeterNorm].';
resultNorm.ConvexAreaNorm = [obj.objects.ConvexAreaNorm].';
resultNorm.EccentricityNorm = [obj.objects.EccentricityNorm].';
resultNorm.ExtentNorm = [obj.objects.ExtentNorm].';
resultNorm.roundnessNorm = [obj.objects.roundnessNorm].';
writetable(struct2table(results), 'results.csv');
writetable(struct2table(resultNorm), 'resultsNorm.csv');
results_roundness = [packageNum areaa];

