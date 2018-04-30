install.packages("Amelia")
install.packages("pscl")
install.packages("ROCR")

library(Amelia)
library(pscl)
library(ROCR)

#Creating a mode function to use to fill in data later on
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Input raw training data
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

#Determine the number of missing values and unique numbers in the data
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

#Another way to visualize missing data
missmap(training.data.raw, main = "Missing values vs observed")

#Select relevant columns
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

#Replace the null values of Age field with the mean and replace the null values of Embarked field with the mode
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T) 
data$Embarked[is.na(data$Embarked)] <- getmode(data$Embarked) 

# We could have set the whole row to NULL if the Embarked field is na instead of setting those values to the mode
# data <- data[!is.na(data$Embarked),]
# rownames(data) <- NULL

#Split the data into a train (80%) and test (20%) set
train <- data[1:round(nrow(data)*0.80,0)-1,]
test <- data[round(nrow(data)*0.80,0):nrow(data)-round(nrow(data)*0.80,0),]

#Create and analyze the binary regression model
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
pR2(model)

#Determine the % of values in the test set the model predicts correctly
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
View(fitted.results)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

#Measure the performance of the model
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
