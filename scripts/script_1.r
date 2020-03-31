#add libraries
library('tm')
library('hunspell')
library('stringr')

#project source
workDir <- 'D:\\uek\\PJN'
setwd(workDir)

#define project folders
inputDir <- '.\\data'
outputDir  <- '.\\results'
scriptsDir <- '.\\scripts'
workspaceDir <- '.\\workspace'

#create output folder
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)


#create corpuse
corpusDir <- paste(inputDir, '\\', 'Literatura - streszczenia - oryginal', sep = '')
corpus <- VCorpus(
   DirSource(
      corpusDir,
      pattern = '*.txt',
      encoding = 'UTF-8'
   ),
   readerControl = list(
      language='pl_PL'
   )
)

#initial transform
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))

stoplistFile <- paste(inputDir, '\\', 'stopwords_pl.txt', sep='')
stopList <- readLines(
   stoplistFile,
   encoding = 'UTF-8'
)

corpus <- tm_map(corpus, removeWords, stopList)
corpus <- tm_map(corpus, stripWhitespace)

removeChar <- content_transformer(
   function(x, pattern, replacement) 
      gsub(pattern, replacement, x)
)

#usuniecie "em dash" i 3/4 z tekstow
corpus <- tm_map(corpus, removeChar, intToUtf8(8722), '')
corpus <- tm_map(corpus, removeChar, intToUtf8(190), '')

#lematyzacja - sprawozdanie do formy podstatowej
polish <- dictionary(lang = 'pl_PL')

lemmatize <- function(text) {
   simpleText <- str_trim(as.character(text[1]))
   parsedText <- strsplit(simpleText, split = ' ')
   newTextVec <- hunspell_stem(parsedText[[1]], dict = polish)
   for (i in 1:length(newTextVec)) {
      if (length(newTextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]
      if (length(newTextVec[[i]]) > 1) newTextVec[i] <- newTextVec[[i]][1]
   }
   newText <- paste(newTextVec, collapse = ' ')
   return(newText)
}

corpus <- tm_map(corpus, content_transformer(lemmatize))

#usuniecie rozszerzen z nazw dokumentoW
cutExtention <- function(document) {
   meta(document, 'id') <- gsub(pattern = '\\.txt$', '', meta(document, 'id'))
   return(document)
}

corpus <- tm_map(corpus, cutExtention)

#export korpusu przetwarzonego do plikow textowych
preprocessedDir <- paste(
   outputDir,
   '\\',
   'Literatura - streszczenia - przetworzone',
   sep = ''
)
dir.create(preprocessedDir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessedDir)



writeLines(as.character(corpus[[1]]))

