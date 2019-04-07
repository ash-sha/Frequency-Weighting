#svm
setwd("/Users/aswath/RProjects/exc/HE/he")

# data manipulation
library(data.table)

# NLP
library(tm)
library(qdap)
library(SnowballC)
library(purrr)
library(text2vec)

# modeling
library(e1071) # for naive bayes
library(xgboost)

# load data
train <- fread("train.csv")
test <- fread("test.csv")

## Clean Data -----------------------------------------------------------

cleanData <- function(data)
{
  
  data[, Description := map_chr(Description, tolower)] # to lower
  data[, Description := map_chr(Description, function(k) gsub(pattern = "[[:punct:]]",replacement = "",x = k))] # remove punctuation
  data[, Description := map_chr(Description, function(k) gsub(pattern = "\\d+",replacement = "",x = k))] # remove digits
  data[, Description := map_chr(Description, function(k) replace_abbreviation(k))] # Sr. to Senior
  data[, Description := map_chr(Description, function(k) replace_contraction(k))] # isn't to is not
  data[, Description := map(Description, function(k) rm_stopwords(k, Top200Words, unlist = T))] # remove stopwords
  data[, Description := map(Description, function(k) stemmer(k))] # played, plays to play
  data[, Description := map(Description, function(k) k[nchar(k) > 2])] # remove two alphabet words like to, ok, po
  return (data)
  
  
}

train_clean <- cleanData(train)
test_clean <- cleanData(test)


# Bag of Words ------------------------------------------------------------

## Bag of words technique converts the list of tokens (words) into a separate column with binary values in it.
## Lets understand it.

ctext <- Corpus(VectorSource(train_clean$Description))

tdm = DocumentTermMatrix(ctext)
print(tdm)

# let's see how BOW looks like - every column becomes one feature
inspect(tdm[1:10,1:5])

## From here, we'll use text2vec package which provides immense potential for feature engineering
## we'll build two models
# a) On Bag of Words Corpus
# b) On TF-IDF Corpus
# c) 2 Gram Model - Your to-do  Task
# You can read more about TF-IDF here: http://www.tfidf.com/


## Bag of Words Model

trte_data <- rbind(train[,.(User_ID, Description)], test[,.(User_ID, Description)])
trte_data$Description <- unlist(map(trte_data$Description, paste, collapse = ","))

bow <- itoken(trte_data$Description, preprocessor = tolower ,tokenizer = word_tokenizer, ids = trte_data$User_ID)
bow_vocab <- create_vocabulary(bow)
bow_vocab # now we have converted the text into tokens. woah! every word can be converted into a feature

## But not all words will be important, Are they ? let's remove words which occur less than 200 times in whole data
pruned_bow <- prune_vocabulary(bow_vocab, term_count_min = 100)
pruned_bow

# get these vocabulary in a data frame for model training
vovec <- vocab_vectorizer(pruned_bow)
dtm_text <- create_dtm(bow, vovec)

feats <- as.data.table(as.matrix(dtm_text))
feats[1:10,1:5] # see 1st 10 rows and 1st 5 columns

# first feature set
train_feats <- feats[1:nrow(train)]
test_feats <- feats[(nrow(train)+1):nrow(feats)]

cols <- setdiff(colnames(train), c('User_ID','Is_Response','Description'))
for(x in cols)
{
  if (class(train[[x]]) == 'character')
  {
    levels <- unique(c(train[[x]], test[[x]]))
    train[[x]] <- as.numeric(factor(train[[x]], levels = levels))
    test[[x]] <- as.numeric(factor(test[[x]], levels = levels))
  }
}

## preparing data for training
train_feats <- cbind(train_feats, train[,.(Browser_Used, Device_Used, Is_Response)])
test_feats <- cbind(test_feats, test[,.(Browser_Used, Device_Used)])

train_feats[, Is_Response := ifelse(Is_Response == 'happy',1,0)]
train_feats[, Is_Response := as.factor(Is_Response)]

## naive Bayes is known to perform quite well in text classification problems

model <- svm(Is_Response ~ ., data = train_feats, laplace = 1)
preds <- predict(model, test_feats)

# make your submission
sub <- data.table(User_ID = test$User_ID, Is_Response = ifelse(preds == 1, "happy", "not_happy"))
fwrite(sub, "sub1.csv")