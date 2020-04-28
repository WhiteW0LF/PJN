#add libraries
library('proxy')

#change work directory
workDir <- 'D:\\uek\\PJN'
setwd(workDir)

#scripts catalog location
scriptsDir <- '.\\scripts'

#load scritp
sourceFile <- paste(
  scriptsDir,
  '\\',
  'frequence_matrix.r', sep = ''
  )
source(sourceFile)

#skalowanie wielowymmiarowe
distCos <- dist(dtmTfIdfBoundsMatrix, method = 'cosine')
mds <- cmdscale(distCos, eig=TRUE, k=2)
x <- mds$points[,1]
y <- mds$points[,2]

#przygotowanie legendy
legend <- paste(
  paste('d', 1:length(rownames(dtmTfIdfBoundsMatrix)), sep = ''),
  rownames(dtmTfIdfBoundsMatrix),
  sep = '<-'
  )

#wykres dokumnetow w przestrzeni dwuwymiarowej
plot(
  x, 
  y, 
  #xlim = c(-0.5, -0.2),
  #ylim = c(-0.2, -0.1),
  xlab='Wspolrzedna syntetyczna 1', 
  ylab='Wspolrzedna syntetyczna 2',
  main='Skalowanie wielowymiarowe', 
  col='orange'
  )
text(
  x, 
  y, 
  labels = paste('d', 1:length(rownames(dtmTfIdfBoundsMatrix)), sep = ''),
  pos = 3,
  col = 'orange'
  )

legend('bottom', legend, cex=.5, text.col = 'orange' )

#eksport wykresu do pliku .png
plotFile <-  paste(
  outputDir,
  '\\',
  'mds.png',
  sep = ''
)
png(file = plotFile)

plot(
  x,
  y,
  xlab='Wspolrzedna syntetyczna 1',
  ylab='Wspolrzedna syntetyczna 2',
  main='Skalowanie wielowymiarowe',
  col='orange'
)
text(
  x,
  y,
  labels = paste('d', 1:length(rownames(dtmTfIdfBoundsMatrix)),sep = ''), 
  pos = 3,
  col = 'orange'
)
legend('bottom', legend, cex=.65, text.col = 'orange')
dev.off()
