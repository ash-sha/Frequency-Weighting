library(tm) # text mining
library(caret) # for machine learning
# load the data
r52train <- read.table("r52-train-all-terms.txt", header=FALSE, sep='\t')
r52test <- read.table("r52-test-all-terms.txt", header=FALSE, sep='\t')

# explore the structure of the  data
str(r52train)
str(r52test)
# rename variables
names(r52train) <- c("Class", "docText")
names(r52test) <- c("Class", "docText")

# convert the document text variable to character type
r52train$docText <- as.character(r52train$docText)
r52test$docText <- as.character(r52test$docText)

# create varible to denote if observation is train or test
r52train$train_test <- c("train")
r52test$train_test <- c("test")

# merge the train/test data
merged <- rbind(r52train, r52test)

# remove objects that are no longer needed 
remove(r52train, r52test)

# subset to 3 document classes only for sake of computational expense/memory
# not doing so will result in a stack overflow or long computational times for ML algorithms
merged <- merged[which(merged$Class %in% c("crude","money-fx","trade")),]

# drop unused levels in the response variable
merged$Class <- droplevels(merged$Class) 

# counts of each class in the train/test sets
table(merged$Class,merged$train_test) 

# a vector source interprets each element of the vector as a document
sourceData <- VectorSource(merged$docText)

# create the corpus
corpus <- Corpus(sourceData)

# example document before pre-processing
corpus[[20]]$content

# preprocess/clean the training corpus
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
corpus <- tm_map(corpus, removeNumbers) # remove digits
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
corpus <- tm_map(corpus, stripWhitespace) # strip extra whitespace
corpus <- tm_map(corpus, removeWords, stopwords('english')) # remove stopwords

# example document after pre-processing
corpus[[20]]$content

# create term document matrix (tdm)
tdm <- DocumentTermMatrix(corpus)

# inspecting the tdm
dim(tdm) # 993 documents, 9243 terms
colnames(tdm)[200:210] # sample of columns (words)
as.matrix(tdm)[10:20,200:210] # inspect a portion of the tdm


weightedtdm <- weightTfIdf(tdm)
as.matrix(weightedtdm)[10:20,200:210]

findFreqTerms(tdm, 250)
# convert tdm's into data frames 
tdm <- as.data.frame(inspect(tdm))
weightedtdm <- as.data.frame(inspect(weightedtdm))

# split back into train and test sets
tdmTrain <- tdm[which(merged$train_test == "train"),]
weightedTDMtrain <- weightedtdm[which(merged$train_test == "train"),]

tdmTest <-  tdm[which(merged$train_test == "test"),]
weightedTDMtest <- weightedtdm[which(merged$train_test == "test"),]

# remove objects that are no longer needed to conserve memory
remove(tdm,weightedtdm)

# append document labels as last column
tdmTrain$doc.class <- merged$Class[which(merged$train_test == "train")]
tdmTest$doc.class <- merged$Class[which(merged$train_test == "test")]
weightedTDMtrain$doc.class <- merged$Class[which(merged$train_test == "train")]
weightedTDMtest$doc.class  <- merged$Class[which(merged$train_test == "test")]
