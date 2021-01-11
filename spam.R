#Importing libraries needed for the code.

library(NLP)
library(readr)
library(caTools)
library(rpart)
library(rpart.plot)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)



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

pal=brewer.pal(8,"Set1")

spam <- subset(sms, Class == "spam")
wordcloud(spam$Text, max.words = 50, colors = pal, random.order = FALSE)

ham <- subset(sms, Class == "ham")
wordcloud(ham$Text, max.words = 50, colors = pal, random.order = FALSE)

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



#Removing Sparse Terms

findFreqTerms(tdm, lowfreq = 200) 
Words <- removeSparseTerms(tdm, 0.99) 

# Converting to data frame

Words <- as.data.frame(as.matrix(Words)) 
colnames(Words) <- make.names(colnames(Words)) 
str(Words)
Words$Class <- sms$Class

# Splitting data into training and test subset

set.seed(16107608)
split <- sample.split(Words$Class, SplitRatio = 0.70)
train <- subset(Words, split == T)
test <- subset(Words, split == F)

## Baseline model

table(test$Class)
print(paste("Accuracy of predicting all SMSs as non-spam: ",
            100*round(table(test$Class)[1]/nrow(test), 4), "%"))

#Decision Tree algorithm
TreeTraining <- rpart(Class ~ ., data = train, method = "class", minbucket = 35)
prp(TreeTraining)
TreePredicting <- predict(TreeTraining, test, type = "class")
table(test$Class, TreePredicting)
rpart.accuracy.table <- as.data.frame(table(test$Class, TreePredicting))
print(paste("Accuracy of the Decision Tree Model: ", 100*round(((rpart.accuracy.table$Freq[1]+rpart.accuracy.table$Freq[4])/nrow(test)), 4), "%"))


