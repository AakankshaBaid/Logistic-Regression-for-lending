#import file
Loans <- read.csv("Loans.csv")

str(Loans)

# Set seed 1234
library(caTools)
set.seed(1234)

#split data into test and train
split = sample.split(Loans$NotFullyPaid,SplitRatio = 0.70)
Train = subset(Loans,split == TRUE)
Test = subset(Loans,split == FALSE)

#Baseline model - Proportion of loans that will be paid in full
length(which(Test$NotFullyPaid==0))/nrow(Test)

#logistic regression
LoanLog = glm(NotFullyPaid ~ .,data=Train,family=binomial)
summary(LoanLog)

# Testing model on new data
PredictedRisk = predict(LoanLog, type="response", newdata=Test)
head(PredictedRisk)

# Adding to Test data
Test <-cbind(Test,PredictedRisk)
str(Test)

#confusion matrix 
tbl = table(Test$NotFullyPaid, PredictedRisk > 0.5)
tbl

# prob of test loan not paid in full
sum(tbl[2,])/sum(tbl)

# accuracy
accuracy=sum(diag(tbl))/sum(tbl)
accuracy

#Roc curve 
library(ROCR)
ROCRpred = prediction(PredictedRisk, Test$NotFullyPaid)

#AUC
AUC = as.numeric(performance(ROCRpred,"auc")@y.values)
AUC


#logistic regression using only IntRate
LoanLog1 = glm(NotFullyPaid ~ IntRate ,data=Train,family=binomial)
summary(LoanLog1)


# Testing new model on new data
Predict = predict(LoanLog1, type="response", newdata=Test)

# Highest prob.
max(Predict)

table(Test$NotFullyPaid, PredictedRisk > 0.5)

# ROC
ROCRpred1 = prediction(Predict, Test$NotFullyPaid)

#AUC
AUC1 = as.numeric(performance(ROCRpred1,"auc")@y.values)
AUC1


c= 10
r = 0.06
t = 3

compint = c*exp(r*t)
compint


# profit if investment is paid back in full
p1 = compint - c
p1

# profit if investment is not paid back in full
p2 = 0 - c
p2

# profit in each type of loan
Profit = (Test$NotFullyPaid==0)*exp(Test$IntRate*t)-1
#max profit
max(Profit)


HighInterest <- subset(Test, Test$IntRate >= 0.15)

#average profit
avgprof = mean((HighInterest$NotFullyPaid==0)*exp(HighInterest$IntRate*t)-1)
avgprof

#proportion of loans not paid back in full
mean(HighInterest$NotFullyPaid)

#sort loans by PredictedRisk
sort <- order(HighInterest$PredictedRisk)[1:100]

SelectedLoans <- HighInterest[sort,]

# profit
mean((SelectedLoans$NotFullyPaid==0)*exp(SelectedLoans$IntRate*t)-1)

#loans not paid back in full
sum(SelectedLoans$NotFullyPaid)


