#Importing libraries needed for the code.
library(NLP)
library(readr)
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)


#Read the data.
sms <- read_delim("smsspamcollection.csv", ";", escape_double = FALSE, trim_ws = TRUE)

head(sms)

#Rename the columns
sms <- sms[, 1:2]
colnames(sms) <- c("Class", "Text")
str(sms)
    
#Probability of each class
table(sms$Class)
prop.table(table(sms$Class))

#Word clouds of spam and ham.

spam <- subset(sms, Class == "spam")
wordcloud(spam$Text, max.words = 60, colors = brewer.pal(7, "Paired"), random.order = FALSE)


ham <- subset(sms, Class == "ham")
wordcloud(ham$Text, max.words = 60, colors = brewer.pal(7, "Paired"), random.order = FALSE)

#Creating the corpus and TDM.

corpus <- VCorpus(VectorSource(sms$Text))

tdm <- DocumentTermMatrix(corpus, control = 
                                list(tolower = TRUE,
                                     removeNumbers = TRUE,
                                     stopwords = TRUE,
                                     removePunctuation = TRUE,
                                     stemming = TRUE))

dim(tdm)

inspect(tdm)



# Converting to data frame

findFreqTerms(tdm, lowfreq = 200) # to words that appear atleast 20 times
Words <- removeSparseTerms(tdm, 0.999) # words that appear in 99.5 percent tweets

Words <- as.data.frame(as.matrix(Words)) # converting the matrix of sparse words to data frame
colnames(Words) <- make.names(colnames(Words)) # rename column names to proper format in order to be used by R.
str(Words)
Words$Class <- sms$Class

# Splitting data into training and test subset

set.seed(16107608)
split <- sample.split(Words$Class, SplitRatio = 0.70)
train <- subset(Words, split == T)
test <- subset(Words, split == F)

## baseline model

table(test$Class)
print(paste("Predicting all messages as non-spam gives an accuracy of: ",
            100*round(table(test$Class)[1]/nrow(test), 4), "%"))

#Decision Tree algorithm
TreeTraining <- rpart(Class ~ ., data = train, method = "class", minbucket = 35)
prp(TreeTraining)
TreePredictiing <- predict(TreeTraining, test, type = "class")
table(test$Class, TreePredicting)
rpart.accuracy.table <- as.data.frame(table(test$Class, TreePredicting))
print(paste("Accuracy of the Decision Tree Model: ", 100*round(((rpart.accuracy.table$Freq[1]+rpart.accuracy.table$Freq[4])/nrow(test)), 4), "%"))


