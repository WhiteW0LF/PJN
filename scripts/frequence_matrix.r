#add libraries
library('tm')

#project source
workDir <- 'D:\\uek\\PJN'
setwd(workDir)

#define project folders
inputDir <- '.\\data'
outputDir  <- '.\\results'
workspaceDir <- '.\\workspace'

#create output folder
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)


#create corpuse
corpusDir <- paste(
  inputDir,
  '\\',
  'ksiazki - przetworzone',
  sep = ''
)

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

#usuniecie rozszerzen z nazw dokumentoW
cutExtention <- function(document) {
  meta(document, 'id') <- gsub(pattern = '\\.txt$', '', meta(document, 'id'))
  return(document)
}

corpus <- tm_map(corpus, cutExtention)

#create macierz czestosci
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
tdmTfidfAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdmBinAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdmTfIdfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfIdfBounds <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

#konwersja macierzy zadkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfIdfBoundsMatrix <- as.matrix(tdmTfIdfBounds)
dtmTfIdfBoundsMatrix <- as.matrix(dtmTfIdfBounds)

#eksport macierzy do pliku .csv
matrixFile <- paste(
  outputDir,
  '\\',
  'tdm_bound_2-16.csv',
  sep = ''
)

write.table(
  dtmTfIdfBoundsMatrix,
  file = matrixFile,
  sep = ';',
  dec = ',',
  col.names = NA
)



