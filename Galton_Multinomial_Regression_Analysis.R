library(data.table)
rdata <- fread(file.choose(), ,sep = ',', header = TRUE, fill = TRUE)
rdata <- rdata[,1:14]
sigmoid <- function(x,a){
  1/(1+exp(-a*x))
}
curve(sigmoid(x,0.5),-15,15)
names(rdata)
View(rdata)

install.packages('dummies')
library(dummies)
rdata$Education <- factor(x= rdata$Education, labels = c('und', 'grd','adv'))
str(rdata)
rdata.dum <- dummy.data.frame(data= rdata, names = 'Education', sep = '.',drop = TRUE)
str(rdata.dum)

rdata.dum$Education.und <- NULL

set.seed(1234)
index <- sample(1:nrow(rdata.dum), nrow(rdata.dum)*.7, replace = F)

train.rdata <- rdata.dum[index,]
test.rdata <- rdata.dum[-index,]

logit.fml <- formula(Personal_Loan ~ Age + Experience + Income + Family + CCAvg + Mortgage + Securities_Account + CD_Account + Online + CreditCard + Education.grd + Education.adv)

rdata.model <- glm(formula = logit.fml, data = train.rdata, family = 'binomial')
summary(rdata.model)

install.packages('pscl')
library(pscl)
pR2(rdata.model)
index

confint(rdata.model)

install.packages('caret')
library(caret)
prd.train <- predict(rdata.model, newdata = train.rdata, type = 'response')
pred.train.test <- ifelse(prd.train>=.5,1,0)

confusionMatrix(pred.train.test,train.rdata$Personal_Loan, positive= '1')


prd.test <- predict(rdata.model, newdata = test.rdata, type = 'response')
pred.test.test <- ifelse(prd.test>=.5,1,0)
confusionMatrix(pred.test.test,test.rdata$Personal_Loan,positive = '1')

install.packages('ROCR')
library(ROCR)

prd <- fitted(rdata.model)
prd.obj <- prediction(prd, train.rdata$Personal_Loan)
prf <- performance(prd.obj, measure = 'tpr', x.measure = 'fpr')
plot(prf)
auc <- performance(prd.obj, measure = 'auc')
auc <- auc@y.values[[1]]
auc
