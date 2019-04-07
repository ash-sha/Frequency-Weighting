getwd()
setwd("/Users/aswath/RProjects/exc/paper2")
getwd()
df <- read.csv("train.csv",header = TRUE,quote="\"",stringsAsFactors= TRUE,
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



#bag
b<-bag_o_words(t, apostrophe.remove = TRUE)
b
b.mat = as.matrix(b)
b.mat
bp<-bag_o_words(tp, apostrophe.remove = TRUE)
bp
bp.mat = as.matrix(bp)
bp.mat
bn<-bag_o_words(tn, apostrophe.remove = TRUE)
bn
bn.mat = as.matrix(bn)
bn.mat


pi<-table(factor(bp, levels = unique(b)))
pi
pj1 <-as.matrix(pi)
pj1
pj<-table(factor(bn, levels = unique(b)))
pj
pj2 <-as.matrix(pj)
pj2





#frequent terms
frequent_terms <- freq_terms(t, 2000)
frequent_terms
f.mat = as.matrix(frequent_terms)
f.mat
frequent_termsp <- freq_terms(tp, 2000)
frequent_termsp
fp.mat = as.matrix(frequent_termsp)
fp.mat
frequent_termsn <- freq_terms(tn, 2000)
frequent_termsn
fn.mat = as.matrix(frequent_termsn)
fn.mat








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
                c("n", "y", "z"))
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
                 y = max((((pj1/501)^0.1)*log2((((pj1*499)+1)/((pj2*501)+1)^0.9))),(((pj2/499)^0.1)*log2((((pj2*501)+1)/((pj1*499)+1))^0.9))),
                 ## prob idf
                 z = max(log2(((nDocs(m)*pj1)+1)/((rs*501)+1))), log2(((nDocs(m)*pj2)+1)/((rs*499)+1)))
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
matrix1= create_matrix(df[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART1(x, spec = "byc"))
matrix1



mat1 = as.data.frame(inspect(matrix1))

#container
container1 = create_container(matrix1, as.numeric(as.factor(df[,3])),
                              trainSize=1:750, testSize=751:1000,virgin=FALSE)
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


#--------------------------------------------------------------------------------------------------------------------

