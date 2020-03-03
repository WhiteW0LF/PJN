#add libraries
library('tm')

#project source
workDir <- 'D:\\PJN'
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
corpusDir <- paste(inputDir, '\\', 'Literatura - streszczenia - oryginał', sep = '')
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

writeLines(as.character(corpus[[1]]))
