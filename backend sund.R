getwd()
setwd("/Users/aswath/RProjects/ex1")
getwd()
df <- read.csv("yelp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
               strip.white = TRUE)
df
dfd<-as.character(df[,2])
dfd
df2<-as.character(df[,1])
df2
Sys.setenv(WNHOME="/usr/local/WordNet-3.0")
initDict()
getwd()
setwd("/usr/local/WordNet-3.0/dict")
getDict()
getDictInstance()

p1<- readline(prompt="enter word:")
p2<- readline(prompt="enter word:")
p3<- readline(prompt="enter word:")

print(paste("your first preference words are:",p1,p2,p3))
if(initDict()) 
{
  filter1 <- getTermFilter("ExactMatchFilter",p1,TRUE)
  filter2 <- getTermFilter("ExactMatchFilter",p2,TRUE)
  filter3 <- getTermFilter("ExactMatchFilter",p3,TRUE)
  terms1 <- getIndexTerms("ADVERB", 5, filter1)
  terms2 <- getIndexTerms("ADVERB", 5, filter2)
  terms3 <- getIndexTerms("ADVERB", 5, filter3)
  x1=getSynonyms(terms1[[1]])
  x2=getSynonyms(terms2[[1]])
  x3=getSynonyms(terms3[[1]])
}
x1
x2
x3
x<-c(x1,x2,x3)
x
print(paste("*****SynonymnS for the user preferences are retrieved from WORDNET Successfully*****   "))
words <- readLines(system.file("stopwords", "english.dat",
                               package = "tm"))
s<-remove_stopwords(dfd, words, lines = TRUE)
s
print(paste("****Stopwords are removed successfully****"))
n<-removeNumbers(s)
n
t<-removePunctuation(n, preserve_intra_word_dashes = FALSE)
t
print(paste("****punctuations are removed successfully****"))
a<-1
a
count=1
for(i in 1:(length(x)))
{ 
  for(j in 1:(length(t)))
  {
    m=0
    e<-tokenize(t[j])
    for(k in 1:(length(e)))
    {
      if((e[k]==x[i]))
      {
        if(m==0)
        {
          
          m=j
          a[count]=df2[j]
          p1[count]=m
          count=count+1
          
        }
      }
    }
  }
}
#print(paste("No of documents segregated along with their",df2[j]))
#a
#a<-append(a,NA,(count-1))
#a
#for(q in 1:(count-1))
#{
# print(paste("INDEX value:",a[q]))
#} 
p1
p1<-unique(p1)
p1
print(paste("No of documents segregated along with their document id is::",length(p1)))
#print(paste("INDEX value:",a[q]))
for(l in 1:(length(p1)))   
{
  segdoc<-df[c(p1),]
}
segdoc
new1<-rbind(df,segdoc)
new1
setwd("/Users/aswath/RProjects/ex1")
getwd()
write.csv(new1[,], file = "segredoc1.CSV",row.names=FALSE)

df1 <- read.csv("segredoc1.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                strip.white = TRUE)
df1

matrix= create_matrix(df1[,2], language="english", 
                      removeStopwords=TRUE, removeNumbers=TRUE, 
                      stemWords=TRUE)
matrix
mat = as.matrix(matrix)
mat
classifier = naiveBayes(mat[1:1000,], as.factor(df1[1:1000,3]) )
classifier
predicted = predict(classifier, mat[1001:(1000+(length(p1))),])
predicted
re<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted)
re<-(re*100)
re
ree<-format(re, digits=2, nsmall=2)
ree
container = create_container(matrix, as.numeric(as.factor(df1[,3])),
                             trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container

models = train_models(container, algorithms=c("SVM","TREE"))
models
results = classify_models(container, models)
results
tr<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results[,"TREE_LABEL"])
tr<-(tr*100)
tr
trr<-format(tr, digits=2, nsmall=2)
trr
svm<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results[,"SVM_LABEL"])
svm<-(svm*100)
svm
svmm<-format(svm, digits=2, nsmall=2)
svmm

matrix1= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART(x, spec = "bpn"))
matrix1
mat1 = as.matrix(matrix1)
mat1
classifier1 = naiveBayes(mat1[1:1000,], as.factor(df1[1:1000,3]) )
classifier1
predicted1= predict(classifier1, mat1[1001:(1000+(length(p1))),])
predicted1
re1<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted1)
re1<-(re1*100)
re1
ree1<-format(re1, digits=2, nsmall=2)
ree1
container1 = create_container(matrix1, as.numeric(as.factor(df1[,3])),
                              trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container1
models1 = train_models(container1, algorithms=c("SVM","TREE"))
models1
results1 = classify_models(container1, models1)
results1
tr1<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results1[,"TREE_LABEL"])
tr1<-(tr1*100)
tr1
trr1<-format(tr1, digits=2, nsmall=2)
trr1
svm1<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results1[,"SVM_LABEL"])
svm1<-(svm1*100)
svm1
svmm1<-format(svm1, digits=2, nsmall=2)
svmm1
matrix2= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART(x, spec = "btn"))
matrix2
mat2 = as.matrix(matrix2)
mat2
classifier2 = naiveBayes(mat2[1:1000,], as.factor(df1[1:1000,3]) )
classifier2
predicted2= predict(classifier2, mat2[1001:(1000+(length(p1))),])
predicted2
re2<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted2)
re2<-(re2*100)
re2
ree2<-format(re2, digits=2, nsmall=2)
ree2
container2 = create_container(matrix2, as.numeric(as.factor(df1[,3])),
                              trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container2
models2 = train_models(container2, algorithms=c("SVM","TREE"))
models2
results2 = classify_models(container2, models2)
results2
tr2<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results2[,"TREE_LABEL"])
tr2<-(tr2*100)
tr2
trr2<-format(tr2, digits=2, nsmall=2)
trr2
svm2<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results2[,"SVM_LABEL"])
svm2<-(svm2*100)
svm2
svmm2<-format(svm2, digits=2, nsmall=2)
svmm2
matrix3= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART(x, spec = "apn"))
matrix3
mat3 = as.matrix(matrix3)
mat3
classifier3 = naiveBayes(mat3[1:1000,], as.factor(df1[1:1000,3]) )
classifer3
predicted3= predict(classifier3, mat3[1001:(1000+(length(p1))),])
predicted3
re3<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted3)
re3<-(re3*100)
re3
ree3<-format(re3, digits=2, nsmall=2)
ree3
container3 = create_container(matrix3, as.numeric(as.factor(df1[,3])),
                              trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container3
models3 = train_models(container3, algorithms=c("SVM","TREE"))
models3
results3 = classify_models(container3, models3)
results3
tr3<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results3[,"TREE_LABEL"])
tr3<-(tr3*100)
tr3
trr3<-format(tr3, digits=2, nsmall=2)
trr3
svm3<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results3[,"SVM_LABEL"])
svm3<-(svm3*100)
svm3
svmm3<-format(svm3, digits=2, nsmall=2)
svmm3
matrix4= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART(x, spec = "atn"))
matrix4
mat4 = as.matrix(matrix4)
mat4
classifier4 = naiveBayes(mat4[1:1000,], as.factor(df1[1:1000,3]) )
classifier4
predicted4= predict(classifier4, mat4[1001:(1000+(length(p1))),])
predicted4
re4<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted4)
re4<-(re4*100)
re4
ree4<-format(re4, digits=2, nsmall=2)
ree4
container4= create_container(matrix4, as.numeric(as.factor(df1[,3])),
                             trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container4
models4 = train_models(container4, algorithms=c("SVM","TREE"))
models4
results4 = classify_models(container4, models4)
results4
tr4<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results4[,"TREE_LABEL"])
tr4<-(tr4*100)
tr4
trr4<-format(tr4, digits=2, nsmall=2)
trr4
svm4<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results4[,"SVM_LABEL"])
svm4<-(svm4*100)
svm4
svmm4<-format(svm4, digits=2, nsmall=2)
svmm4
matrix5= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART(x, spec = "lpn"))
matrix5
mat5 = as.matrix(matrix5)
mat5
classifier5 = naiveBayes(mat5[1:1000,], as.factor(df1[1:1000,3]) )
classifier5
predicted5= predict(classifier5, mat5[1001:(1000+(length(p1))),])
predicted5
re5<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted5)
re5<-(re5*100)
re5
ree5<-format(re5, digits=2, nsmall=2)
ree5
container5= create_container(matrix5, as.numeric(as.factor(df1[,3])),
                             trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container5
models5 = train_models(container5, algorithms=c("SVM","TREE"))
models5
results5 = classify_models(container5, models5)
results5
tr5<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results5[,"TREE_LABEL"])
tr5<-(tr5*100)
tr5
trr5<-format(tr5, digits=2, nsmall=2)
trr5
svm5<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results5[,"SVM_LABEL"])
svm5<-(svm5*100)
svm5
svmm5<-format(svm5, digits=2, nsmall=2)
svmm5
matrix6= create_matrix(df1[,2], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       stemWords=TRUE ,weighting = function(x)
                         weightSMART(x, spec = "ltn"))
matrix6
mat6 = as.matrix(matrix6)
mat6
classifier6 = naiveBayes(mat6[1:1000,], as.factor(df1[1:1000,3]) )
classifier6
predicted6= predict(classifier6, mat6[1001:(1000+(length(p1))),])
predicted6
re6<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted6)
re6<-(re6*100)
re6
ree6<-format(re6, digits=2, nsmall=2)
ree6
container6= create_container(matrix6, as.numeric(as.factor(df1[,3])),
                             trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
container6
models6 = train_models(container6, algorithms=c("SVM","TREE"))
models6
results6 = classify_models(container6, models6)
results6
tr6<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results6[,"TREE_LABEL"])
tr6<-(tr6*100)
tr6
trr6<-format(tr6, digits=2, nsmall=2)
trr6
svm6<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results6[,"SVM_LABEL"])
svm6<-(svm6*100)
svm6
svmm6<-format(svm6, digits=2, nsmall=2)
svmm6
#re7<-format(re7,digits=2,nsmall=2)
#re7
re0<-c(re,re1,re2,re3,re4,re5,re6)
re0<-format(re0,digits=2,nsmall=2)
re0
svm0<-c(svm,svm1,svm2,svm3,svm4,svm5,svm6)
svm0<-format(svm0,digits=2,nsmall=2)
svm0
tr0<-c(tr,tr1,tr2,tr3,tr4,tr5,tr6)
tr0<-format(tr0,digits=2,nsmall=2)
tr0
getwd()
jpeg('accuracynaive.jpg')
barplot(re0, col = rainbow(20))
dev.off()
jpeg('accuracysv.jpg')
barplot(svm0, col = rainbow(20))
dev.off()
jpeg('accuracyDT.jpg')
barplot(tr0, col = rainbow(20))
dev.off()
print(paste("Barchart generated successfully!!!!!!!!!! "))

