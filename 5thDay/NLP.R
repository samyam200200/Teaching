datasets<-read.delim("C:/Users/gocoo/OneDrive/Desktop/Projects/5thDay/Restaurant_Reviews.tsv", header = T, quote = '', stringsAsFactors = F)
install.packages("tm")
install.packages("SnowballC")
library(tm)

#creating corpus for sparse 
corpus<-VCorpus(VectorSource(datasets$Review))

#change all characters to lower
corpus<-tm_map(corpus, content_transformer(tolower))

#removing numbers
corpus<-tm_map(corpus, removeNumbers)

#removing punctuation
corpus<-tm_map(corpus, removePunctuation)

#removing non-relevant words
corpus<-tm_map(corpus, removeWords, stopwords())

#using only roots of the word (loving, loved->love)k/a stemming
corpus<-tm_map(corpus, stemDocument)

#removing extra spaces
corpus<-tm_map(corpus, stripWhitespace)

#creating bag of words model (sparse matrix)
dtm<-DocumentTermMatrix(corpus)

#filter for words occuring many times
dtm<-removeSparseTerms(dtm, 0.999) #containing 99% of columns with 1

dataset<-as.data.frame(as.matrix(dtm))
dataset$Liked<-datasets$Liked

dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 100)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm
