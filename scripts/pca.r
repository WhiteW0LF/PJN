#add libraries


#change work directory
workDir <- 'D:\\uek\\PJN'
setwd(workDir)

#scripts catalog location
scriptsDir <- '.\\scripts'

#load scritp
sourceFile <- paste(
  scriptsDir,
  '\\',
  'frequence_matrix.r', 
  sep = ''
)
source(sourceFile)

#analiza głównych skłądowych
pca <- prcomp(dtmTfIdfBounds)
x <- pca$x[,1]
y <- pca$x[,2]

#przygotowanie legendy
legend <- paste(
  paste('d', 1:length(rownames(dtmTfIdfBoundsMatrix)), sep = ''),
  rownames(dtmTfIdfBoundsMatrix),
  sep = '<-'
)

#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x, 
  y, 
  #xlim = c(-0.5, -0.2),
  #ylim = c(-0.2, -0.1),
  xlab='Współrzędna syntetyczna 1', 
  ylab='Współrzędna syntetyczna 2',
  main='Analiza głownych składowych', 
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
  'pca.png',
  sep = ''
)
png(file = plotFile)

plot(
  x,
  y,
  xlab='Współrzędna syntetyczna 1', 
  ylab='Współrzędna syntetyczna 2',
  main='Analiza głownych składowych', 
  col='orange'
)
text(
  x, 
  y, 
  labels = paste('d', 1:length(rownames(dtmTfIdfBoundsMatrix)), sep = ''),
  pos = 3,
  col = 'orange'
)
legend('bottom', legend, cex=.65, text.col = 'orange' )
dev.off()