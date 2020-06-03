#add libraries
library(lsa)

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

#analiza ukrytych wymiarów semantycznych (dekompozycja wg wartości osobliwych)
# tdmT czy dtmT ?????????
lsa <- lsa(tdmTfIdfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, współrzędne wyrazów
lsa$dk #odpowiednik macierzy V, współrzędne dokumentów
lsa$sk #odpowiednik macierzy D, znaczenie składowych

#Tu skończyliśmy
#przygotowanie współrzędnych do wykresu
coordDocs <- lsa$dk%*%diag(lsa$sk)
coordTerms <- lsa$tk%*%diag(lsa$sk)
words <- c("wiedzmin", "miecz", "yennefer", "geralt", "triss", "ciri", "nilfgaard", "grob", "marynarka", "pohulanka", "cmentarz", "inspektor", "gwiezdny", "komputer", "spock", "statek", "enterprise", "kirk", "aneta", "terapeuta", "terapia", "trener", "numerolog", "zmyslowy", "astropsycholog")
termsImportance <- diag(coordTerms%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantWords <- names(tail(sort(termsImportance), 25))
coordWords <- coordTerms[importantWords,]

x1 <- coordDocs[,1]
y1 <- coordDocs[,2]
x2 <- coordWords[,1]
y2 <- coordWords[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(coordDocs)),sep = ""),
  rownames(coordDocs),
  sep = "<-"
)

#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x1,
  y1,
  #xlim = c(-0.02,-0.01),
  #ylim = c(-0.05,0.05),
  xlab="Współrzędna syntetyczna 1", 
  ylab="Współrzędna syntetyczna 2",
  main="Analiza ukrytych wymiarów sematycznych", 
  col = "orange"
)
text(
  x1, 
  y1, 
  labels = paste("d", 1:length(rownames(dtmTfIdfBoundsMatrix)),sep = ""), 
  pos = 4,
  col = "orange"
)
points(
  x2,
  y2,
  pch = 2,
  col = "brown"
)
text(
  x2, 
  y2, 
  labels = rownames(coordWords), 
  pos = 4,
  col = "brown"
)
legend("bottomleft", legend, cex=.65, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "\\",
  "lsa.png",
  sep = ""
)
png(file = plotFile)
plot(
  x1,
  y1,
  xlim = c(-0.5,0.09),
  ylim = c(-0.01,0.03),
  xlab="Współrzędna syntetyczna 1", 
  ylab="Współrzędna syntetyczna 2",
  main="Analiza ukrytych wymiarów semantucznych", 
  col = "red"
)
text(
  x1, 
  y1, 
  labels = paste("d", 1:length(rownames(coordDocs)),sep = ""), 
  pos = 4,
  col = "red"
)
points(
  x2,
  y2,
  pch = 2,
  col = "blue"
)
text(
  x2, 
  y2, 
  labels = rownames(coordWords), 
  pos = 4,
  col = "blue"
)
legend("bottomleft", legend, cex=.65, text.col = "black")
dev.off()