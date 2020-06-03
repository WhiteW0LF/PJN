#add libraries
library(wordcloud)

#change work directory
workDir <- 'D:\\uek\\PJN'
setwd(workDir)

#scripts catalog location
scriptsDir <- '.\\scripts'

#load scritp
sourceFile <- paste(
  scriptsDir,
  '\\',
  'lda.r', 
  sep = ''
)
source(sourceFile)

#dla pierwszego dokumentu
##waga tf jako miara ważności słów
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = T))
keywordsTf1

##waga tfidf jako miara ważności słów
keywordsTfidf20 <- head(sort(dtmTfIdfBoundsMatrix[20,], decreasing = T))
keywordsTfidf20

##prawdopodobieństwo w LDA jako miara ważności słów
termsImportance1 <- c(results$topics[1,]%*%results$terms)

names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = T))
keywordsLda1

##chmury tagów
for(i in 1:20) {
  par(mai = c(0,0,0,0))
  wordcloud(corpus[i], max.words = 200, colors = brewer.pal(8,"PuOr"))
}
