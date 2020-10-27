train <- read.csv(file.choose() , na.strings = '')
test <- read.csv(file.choose() , na.strings = '')
# Removing NAs
library(VIM)
train <- kNN(train , k = 10)
test <- kNN(test , k = 10)
summary(train)
summary(test)
train <- subset(train , select = Loan_ID:Loan_Status)
test <- subset(test , select = Loan_ID:Property_Area)
library(dplyr)
data <- bind_rows(train , test)
data <- data[,2:13]
# CHecking correlations
cor(data$ApplicantIncome , data$CoapplicantIncome)
cor(data$ApplicantIncome , data$LoanAmount)
cor(data$LoanAmount , data$Loan_Amount_Term)

#Checking any outliers and removing
boxplot(data$ApplicantIncome)
summary(data$ApplicantIncome)
bench_Appinc <- 5516 + 1.5*IQR(data$ApplicantIncome)
data$ApplicantIncome[data$ApplicantIncome > bench_Appinc] <- bench_Appinc
boxplot(data$ApplicantIncome)
# ------------
boxplot(data$CoapplicantIncome)
summary(data$CoapplicantIncome)
bench_CoAppinc <- 2365 + 1.5*IQR(data$CoapplicantIncome)
data$CoapplicantIncome[data$CoapplicantIncome > bench_CoAppinc] <- bench_CoAppinc
boxplot(data$CoapplicantIncome)
# ------------
boxplot(data$LoanAmount)
summary(data$LoanAmount)
bench_loanam <- 161 + 1.5*IQR(data$LoanAmount)
data$LoanAmount[data$LoanAmount > bench_loanam] <- bench_loanam
boxplot(data$LoanAmount)
# ------------
# changing categoricals into factors
str(data)
data$Gender <- factor(data$Gender ,
                      levels = c('Female','Male'),
                      labels = c(0,1))
data$Married <- factor(data$Married ,
                       levels = c('No','Yes'),
                       labels = c(0,1))
data$Education <- factor(data$Education,
                         levels = c('Graduate', 'Not Graduate'),
                         labels = c(0,1))
data$Self_Employed <- factor(data$Self_Employed,
                             levels = c('No' , 'Yes'),
                             labels = c(0,1))
data$Dependents <- factor(data$Dependents,
                          levels = c('0','1','2','3+'),
                          labels = c(0,1,2,3))
data$Property_Area <- factor(data$Property_Area,
                             levels = c('Rural','Semiurban','Urban'),
                             labels = c(0,1,2))
data$Loan_Status <- factor(data$Loan_Status,
                           levels = c('N','Y'),
                           labels = c(0,1))
str(train)
# Splitting into train and test
train <- data[complete.cases(data),]
test <- data[!complete.cases(data),]
test <- test[,1:11]

library(caTools)
set.seed(123)
split <- sample.split(train$Loan_Status , SplitRatio = 0.8)
training <- subset(train , split == T)
testing <- subset(train , split == F)


library(randomForest)
fit_rf <- randomForest(Loan_Status~. , data = training)
plot(fit_rf)
pred_rf <- predict(fit_rf , testing[,-12])
table(pred_rf , testing$Loan_Status)

# Model
fit_rf1 <- randomForest(Loan_Status~. , data = train)
plot(fit_rf1)
# Predictions
pred_rf <- predict(fit_rf1 , test[,-12])
pred_rf <- data.frame(pred_rf)
pred_rf <- factor(pred_rf , levels = c(0,1), labels = c('N' , 'Y'))
write.csv(pred_rf , 'pred1.csv')
