#add libraries



#scripts catalog location
scriptsDir <- ".\\scripts"

#load scritp
sourceFile <- paste(
  scriptsDir,
  '\\',
  'script_2.r', sep = ''
  )
soruce(sourceFile)

#skalowanie wielowymmiarowe
d <- dist(dtmTfIdfBoundsMatrix)
fit <- cmdscale(d, eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab='Coorinate 1', ylab='Coordinate 2',
     main='Metric MDS', type='n')
text(x, y, labels = row.names(dtmTfIdfBoundsMatrix), cex=.7)