data = read.csv("D:/Top_Gear/R programming/pima_indians_diabetes_study/Diabetes.csv")
str(data) #Understand the structure of the dataset
summary(data)

# Creating Age Category column
data$Age_Cat <- ifelse(data$Age < 21, "<21", 
                       ifelse((data$Age>=21) & (data$Age<=25), "21-25", 
                              ifelse((data$Age>25) & (data$Age<=30), "25-30",
                                     ifelse((data$Age>30) & (data$Age<=35), "30-35",
                                            ifelse((data$Age>35) & (data$Age<=40), "35-40",
                                                   ifelse((data$Age>40) & (data$Age<=50), "40-50",
                                                          ifelse((data$Age>50) & (data$Age<=60), "50-60",">60")))))))

str(data$Age_Cat)

data$Age_Cat <- factor(data$Age_Cat,levels =c('<21','21-25','25-30','30-35','35-40','40-50','50-60','>60'))
table(data$Age_Cat)

# Histogram of Age
library(ggplot2)
ggplot(aes(x = Age), data=data) +
  geom_histogram(binwidth=1, color='black', fill = "#F79420") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("No of people by age")


# Barplot by Age_Cat
ggplot(aes(x = Age_Cat), data = data) +
  geom_bar(fill='steelblue')

# box plot of Age_Cat vs BMI
ggplot(aes(x=Age_Cat, y = BMI), data = data) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,70))

by(data$BMI, data$Age_Cat, summary)

# Compute correlation matrix
  data_cor <- round(cor(data[1:8]),1)
data_cor
#No strong correlation observed between variables. So, no need to drop any of them for analysis

# Split dataset into train and test sets
require(caTools)
set.seed(3)
sample = sample.split(data$Outcome, SplitRatio=0.75)
train = subset(data, sample==TRUE)
test = subset(data, sample==FALSE)

nrow(train)
nrow(test)

# distribution of Age category in Train set
table(train$Age_Cat)
AllVar <- glm(Outcome ~ ., data = train, family = binomial)
summary(AllVar)
stepMod<-step( AllVar , direction = "both",trace = 1, steps = 1000)

AllVar1 <-glm(Outcome ~ Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age_Cat,data = train, family = binomial)
summary(AllVar1)

PredictTrain <- predict(AllVar1, type = "response")
# Build confusion matrix with a threshold value of 0.5
threshold_0.5 <- table(train$Outcome, PredictTrain > 0.5)
threshold_0.5
accuracy_0.5 <- round(sum(diag(threshold_0.5))/sum(threshold_0.5),2)
accuracy_0.5

# Build confusion matrix with a threshold value of 0.7
threshold_0.7 <- table(train$Outcome, PredictTrain > 0.7)
threshold_0.7

# Accuracy
accuracy_0.7 <- round(sum(diag(threshold_0.7))/sum(threshold_0.7),2)
accuracy_0.7

# Build confusion matrix with a threshold value of 0.2
threshold_0.2 <- table(train$Outcome, PredictTrain > 0.2)
threshold_0.2

# Accuracy
accuracy_0.2 <- round(sum(diag(threshold_0.2))/sum(threshold_0.2),2)
accuracy_0.2

# Generate ROC Curves

library(ROCR)
ROCRpred = prediction(PredictTrain, train$Outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Adding threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a=0, b=1)
auc_train <- round(as.numeric(performance(ROCRpred, "auc")@y.values),2)
#legend(.8, .2, auc_train, title = "AUC", cex=1)

# Making predictions on test set

PredictTest <- predict(AllVar, type = "response", newdata = test)
# Convert probabilities to values using the below

## Based on ROC curve above, selected a threshold of 0.5
test_tab <- table(test$Outcome, PredictTest > 0.5)
test_tab
accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
# Compute test set AUC

ROCRPredTest = prediction(PredictTest, test$Outcome)
auc = round(as.numeric(performance(ROCRPredTest, "auc")@y.values),2)
auc
