getwd()
setwd("/Users/aswath/RProjects/exc")
getwd()
df <- read.csv("yelp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
               strip.white = TRUE)
df
dfd<-as.character(df[,2])
dfd
df2<-as.character(df[,1])
df2
words <- readLines(system.file("stopwords", "english.dat",
                               package = "tm"))
s<-remove_stopwords(dfd, words, lines = TRUE)
s
print(paste("****Stopwords are removed successfully****"))
n<-removeNumbers(s)
n
t<-removePunctuation(n, preserve_intra_word_dashes = FALSE)
t


#pos
dfp <- read.csv("yelpp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                strip.white = TRUE)
dfp
dfdp<-as.character(dfp[,2])
dfdp
df2p<-as.character(dfp[,1])
df2p
wordsp <- readLines(system.file("stopwords", "english.dat",
                                package = "tm"))
sp<-remove_stopwords(dfdp, words, lines = TRUE)
sp
print(paste("****Stopwords are removed successfully****"))
np<-removeNumbers(sp)
np
tp<-removePunctuation(np, preserve_intra_word_dashes = FALSE)
tp

#neg
dfn <- read.csv("yelpn.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                strip.white = TRUE)
dfn
dfdn<-as.character(dfn[,2])
dfdn
df2n<-as.character(dfn[,1])
df2n
wordsn <- readLines(system.file("stopwords", "english.dat",
                                package = "tm"))
sn<-remove_stopwords(dfdn, words, lines = TRUE)
sn
print(paste("****Stopwords are removed successfully****"))
nn<-removeNumbers(sn)
nn
tn<-removePunctuation(nn, preserve_intra_word_dashes = FALSE)
tn



Corpus<- c(t)
tm_corpus <- Corpus(VectorSource(Corpus))
str(tm_corpus)
tdm <- TermDocumentMatrix(tm_corpus)
tdm
as.matrix(tdm)

Corpusp<-c(tp)
tm_corpusp <- Corpus(VectorSource(Corpusp))
str(tm_corpus)
tdmp <- TermDocumentMatrix(tm_corpusp)
tdmp
as.matrix(tdmp)

Corpusn<-c(tn)
tm_corpusn <- Corpus(VectorSource(Corpusn))
str(tm_corpusn)
tdmn <- TermDocumentMatrix(tm_corpusn)
tdmn
as.matrix(tdmn)

inspect(tdm)
inspect(tdmp)
inspect(tdmn)


o<-findFreqTerms(tdm, 0, Inf)
o
p<-findFreqTerms(tdmp, 0, Inf)
p
q<-findFreqTerms(tdmn, 0, Inf)
q


#similarity between c1 and c2
common.CorpusCorpusp <- data.frame(term = character(0), freq = integer(0))
for(u in o){
  find <- agrep(u, q)
  if(length(find) != 0){
    common.CorpusCorpusp <- rbind(common.CorpusCorpusp, data.frame(term = u, freq = length(find)))
  }
}





#association
findAssocs(tdm, p, 0)
findAssocs(tdm, q, 0)


















#------------------------------------------------------------------------------------------------------------------

WeightFunction <- function(x, name, acronym) {
  class(x) <- c("WeightFunction", "function")
  attr(x, "name") <- name
  attr(x, "acronym") <- acronym
  x
}

# Actual TermDocumentMatrix weighting functions
weightTf <-
  WeightFunction(function(m) {
    attr(m, "weighting") <- c("term frequency", "tf")
    m
  }, "term frequency", "tf")

weightTfIdf <-
  WeightFunction(function(m, normalize = TRUE) {
    isDTM <- inherits(m, "DocumentTermMatrix")
    if (isDTM) m <- t(m)
    if (normalize) {
      cs <- col_sums(m)
      if (any(cs == 0))
        warning("empty document(s): ",
                paste(Docs(m)[cs == 0], collapse = " "))
      names(cs) <- seq_len(nDocs(m))
      m$v <- m$v / cs[m$j]
    }
    rs <- row_sums(m > 0)
    if (any(rs == 0))
      warning("unreferenced term(s): ",
              paste(Terms(m)[rs == 0], collapse = " "))
    lnrs <- log2(nDocs(m) / rs)
    lnrs[!is.finite(lnrs)] <- 0
    m <- m * lnrs
    attr(m, "weighting") <-
      c(sprintf("%s%s",
                "term frequency - inverse document frequency",
                if (normalize) " (normalized)" else ""),
        "tf-idf")
    if (isDTM) t(m) else m
  }, "term frequency - inverse document frequency", "tf-idf")

weightSMART1 <-
  WeightFunction(function(m, spec = "nnn", control = list()) {
    stopifnot(inherits(m, c("DocumentTermMatrix", "TermDocumentMatrix")),
              is.character(spec), nchar(spec) == 3L, is.list(control))
    
    term_frequency <-
      match.arg(substr(spec, 1L, 1L),
                c("n", "l", "a", "b", "L"))
    document_frequency <-
      match.arg(substr(spec, 2L, 2L),
                c("n", "t", "p"))
    normalization <-
      match.arg(substr(spec, 3L, 3L),
                c("n", "c", "u", "b"))
    
    isDTM <- inherits(m, "DocumentTermMatrix")
    if (isDTM) m <- t(m)
    
    if (normalization == "b") {
      ## Need to compute the character lengths of the documents
      ## before starting the weighting.
      charlengths <-
        tapply(nchar(Terms(m))[m$i] * m$v, m$j, sum)
    }
    
    ## Term frequency
    m$v <- switch(term_frequency,
                  ## natural
                  n = m$v,
                  ## logarithm
                  l = 1 + log2(m$v),
                  ## augmented
                  a = {
                    s <- tapply(m$v, m$j, max)
                    0.5 + (0.5 * m$v) / s[as.character(m$j)]
                  },
                  ## boolean
                  b = as.numeric(m$v > 0),
                  ## log ave
                  L = {
                    s <- tapply(m$v, m$j, mean)
                    ((1 + log2(m$v)) / (1 + log2(s[as.character(m$j)])))
                  })
    
    ## Document frequency
    rs <- row_sums(m > 0)
    if (any(rs == 0))
      warning("unreferenced term(s): ",
              paste(Terms(m)[rs == 0], collapse = " "))
    df <- switch(document_frequency,
                 ## natural
                 n = 1,
                 ## idf
                 t = log2(nDocs(m) / rs),
                 ## prob idf
                 p = max(0, log2((nDocs(m) - rs) / rs)))
    df[!is.finite(df)] <- 0
    
    ## Normalization
    cs <- col_sums(m)
    if (any(cs == 0))
      warning("empty document(s): ",
              paste(Docs(m)[cs == 0], collapse = " "))
    norm <- switch(normalization,
                   ## none
                   n = rep.int(1, nDocs(m)),
                   ## cosine
                   c = sqrt(col_sums(m ^ 2)),
                   ## pivoted unique
                   u = {
                     if (is.null(pivot <- control$pivot))
                       stop("invalid control argument pivot")
                     if (is.null(slope <- control$slope))
                       stop("invalid control argument slope")
                     (slope * sqrt(col_sums(m ^ 2)) +
                         (1 - slope) * pivot)
                   },
                   ## byte size
                   b = {
                     if (is.null(alpha <- control$alpha))
                       stop("invalid control argument alpha")
                     norm <- double(nDocs(m))
                     norm[match(names(charlengths),
                                seq_along(norm))] <-
                       charlengths ^ alpha
                     norm
                   })
    
    m <- m * df
    m$v <- m$v / norm[m$j]
    attr(m, "weighting") <- c(paste("SMART", spec), "SMART")
    
    if (isDTM) t(m) else m
  }, "SMART", "SMART")

weightBin <-
  WeightFunction(function(m) {
    m$v <- rep_len(1L, length(m$v))
    attr(m, "weighting") <- c("binary", "bin")
    m
  }, "binary", "bin")


#--------------------------------------------------------------------------------------------------------- 
#matrix1 with weighting 
matrix1= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART1(x, spec = "nnn"))
matrix1



mat1 = as.matrix(matrix1)
mat1

#container
container1 = create_container(matrix1, as.numeric(as.factor(df1[,3])),
                              trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container1

models1 = train_models(container1, algorithms=c("SVM","MAXENT"))
models1

results1 = classify_models(container1, models1)
results1 


# model summary
analytics1 = create_analytics(container1, results1)
summary(analytics1)

#Cross validation
N=10

cross_validate(container1,N,"SVM")
cross_validate(container1,N,"MAXENT")

