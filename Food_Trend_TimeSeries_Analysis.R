library(data.table)
#data import, preprocessing
data1 <- fread(file.choose(), sep = ",", head = T)
data1 <- read.csv(file.choose(), sep = ",", head = T)

str(data1)
View(data1)
data1$Date <- as.character(data1$Date)
sspilt<- strsplit(data1$Date,split = "-")
spilt_date <- as.data.frame(strsplit(data1$Date,split = "-"))

spilt_data <- t(spilt_date)
spilt_data
a<- as.numeric(c(1:261))
str(a)
model_data <- cbind(a,spilt_data[,2],data1$View)
summary(model_data)

model_data<- as.data.frame(model_data)
model_data<-model_data[,2:3]
model_data<- cbind(a,model_data)
model_data <-model_data[,1:2]
model_data <- cbind(model_data,data1$View)
row.names(model_data) <- NULL
colnames(model_data) <- c("t", 'Month', 'View')
View(model_data)
str(model_data)
model_data
model_data_train <- model_data[1:241,]

#modeling
formula1 <- formula(View ~ t + sin(2*pi*t/52)+cos(2*pi*t/52))
trig_model <- lm( formula = formula1, data = model_data_train  )
summary(trig_model)
formula2 <- formula(View ~ t + sin(2*pi*t/52)+cos(2*pi*t/52)+sin(4*pi*t/52)+cos(4*pi*t/52))
trig_model2 <- lm( formula = formula2, data = model_data_train)
summary(trig_model2)

func1 <- function(x){
  return (2*pi*x/52)
}

#predict(trig1)
predict1 <- predict(trig_model, model_data[242:261,], type='response')
predict1
predict1_merge <- cbind(model_data[242:261,],predict1)
predict1_merge
all_data_predict <- predict(trig_model, model_data, type = 'response')
whole_data <- cbind(model_data,all_data_predict)
whole_data
#new_data1 <- as.data.frame(262:282) 
#sin_1<-lapply(new_data1$`262:282`, func1)
#sin_1<-sapply(new_data1$`262:282`, func1)
#sin_1
#sin_2<-t(sin_1)
#cos_1<-lapply(new_data1$`262:282`, func1)
#cos_1<-sapply(new_data1$`262:282`, func1)
#cos_1
#cos_2 <- t(cos_1)
#cos_2
#new_data1 <- cbind(new_data1, sin_1, cos_1)
str(new_data1)
View(new_data1)
#predict_1 <- predict(trig_model,test2, type = "response" )
#predict_1
#trig_prd1 <- read.csv(file.choose(), sep = ",", header = F)
#trig_prd1  
#trig_prd_data <- trig_prd1[,c(1,4)]
#colnames(trig_prd_data) <- c("t", "prd")
#trig_prd_data
#predict(trig2)
predict2 <- predict(trig_model2, model_data[242:261,], type='response')
predict2
predict2_merge <- cbind(model_data[242:261,],predict2)
predict2_merge
all_data_predict2 <- predict(trig_model2 , model_data, type = "response")
#predict_2 <- predict(trig_model2 , test2, type = "response")
#predict_2
testing_data1 <- cbind(model_data[242:261,],predict1)
testing_data1
whole_data2 <- cbind(model_data, all_data_predict2)
whole_data2
View(testing_data1)
trig_prd2 <- read.csv(file.choose(), sep = ",", header = F)
trig_prd_data2 <- trig_prd2[,c(1,6)]
colnames(trig_prd_data2) <- c("t", "prd")
trig_prd_data2

trig_prd2
testing_data2 <- cbind(model_data[242:261,],predict2)
testing_data2

library(car)
library(psych)
library(Metrics)
mango1<-rmse(model_data$View[242:261],predict1)
mango2<-rmse(model_data$View[242:261],predict2)
mango3<-rmse(model_data$View[242:261],predict3)
mango4<-rmse(model_data$View[242:261],pre_holt)
mango5<-rmse(model_data$View[242:261],pre_holt_m)
c(mango3,mango1,mango2,mango4,mango5)
summary(model_data)

#dummy
library(dummies)
p <- within(model_data, {
  prog <- factor(Month, levels=1:12, labels=c("m1", "m2", 
                                            "m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
})

p
dummy_data <- dummy.data.frame(data= p, names = 'prog', sep = '.',drop = TRUE)
dummy_data
dummy_data$prog.m1 <- NULL
dummy_data_train <- dummy_data[1:241,]
str(dummy_data)
formula3 <- formula(View ~ t+ prog.m2+prog.m3+prog.m4+prog.m5+prog.m6+prog.m7+prog.m8+prog.m9+prog.m10+prog.m11+prog.m12)

dummy_model <- lm(formula = formula3 , data = dummy_data_train)
summary(dummy_model)

predict3 <- predict(dummy_model, dummy_data[242:261,], type = 'response')
predict3
all_dummy_data_predict <- predict(dummy_model, dummy_data, type = 'response')

dummy_model_1 <- cbind(dummy_data[242:261,],predict3)
dummy_model_1
whole_data3 <- cbind(dummy_data, all_dummy_data_predict)
RMSE(predict3 , dummy_model_1$View)
duumy_prd <- read.csv(file.choose(), sep = ",", header= T)
duumy_prd
duumy_prd1 <- duumy_prd[,c(1, 14)]
duumy_prd2 <- duumy_prd[,c(1,13)]
duumy_prd1
duumy_prd2
#Visualization
library(ggplot2)
ggplot(whole_data3, aes(t, View),) + geom_line() + geom_line(aes(x=t, y=all_dummy_data_predict), colour="red")

ggplot(whole_data, aes(t, View),) + geom_line() + geom_line(aes(x=t, y=all_data_predict), colour="red")
ggplot(whole_data2, aes(t, View),) + geom_line() + geom_line(aes(x=t, y=all_data_predict2), colour="red")
View(whole_data2)

ggplot(whole_data, aes(a, View),) + geom_line() + geom_line(aes(x=a, y=all_data_predict), colour="red")
ggplot(testing_data2, aes(a, View),) + geom_line() + geom_line(aes(x=a, y=predict2), colour="red")
ggplot(dummy_model_1, aes(a, View),) + geom_line() + geom_line(aes(x=a, y=predict3), colour="red")


#Additive Holt-winters, Multiplicative Holt-Winters
### Holt-Winters


demand <- ts(data1$View[1:241], start = c(2012,10),frequency = 52)
plot(demand)
demand
noodle_holt <- HoltWinters(demand, seasonal = "additive")
plot(noodle_holt)
summary(noodle_holt)
noodle_holt

noodle_holt_m <- HoltWinters(demand, seasonal = "multiplicative")
plot(noodle_holt_m)
noodle_holt_m

### prediction
  
pred <- ts(data1$View[1:241], start = c(2012,10),frequency = 52)
plot(pred)

noodle_holt_pred <- HoltWinters(pred, seasonal = "additive")
noodle_holt_pred_m <- HoltWinters(pred, seasonal = "multiplicative")

pre_holt <- predict(noodle_holt_pred,20,prediction.interval = TRUE)
pre_holt_m <- predict(noodle_holt_pred_m,20,prediction.interval = TRUE)
pre_holt

plot(noodle_holt_pred,pre_holt)
plot(noodle_holt_pred_m,pre_holt_m)
pre_holt
pre_holt[,1]
### result
result <- cbind(242:261,data1$View[242:261],predict1,predict2,predict3, pre_holt[,1],pre_holt_m[,1])
result
colnames(result) <- c("t","actual","dummy","tri_model1","tri_model2","additive_holt","multiplicative_holt")
row.names(result) <- NULL
row.names(result) <- 242:261
str(result)
# (2) Additive Holt-Winters?? Multiplicative Holt-Winters ????????
# : Weighting parameter

noodle_holt_pred_11 <- HoltWinters(pred, alpha = 0.1, beta = 0.1, gamma = 0.1, seasonal = "additive")
noodle_holt_pred_m_11 <- HoltWinters(pred, alpha = 0.1, beta = 0.1, gamma = 0.1, seasonal = "multiplicative")

noodle_holt_pred_35 <- HoltWinters(pred, alpha = 0.3, beta = 0.5, gamma = 0.5, seasonal = "additive")
noodle_holt_pred_m_35 <- HoltWinters(pred, alpha = 0.3, beta = 0.5, gamma = 0.5, seasonal = "multiplicative")

noodle_holt_pred_55 <- HoltWinters(pred, alpha = 0.5, beta = 0.5, gamma = 0.5, seasonal = "additive")
noodle_holt_pred_m_55 <- HoltWinters(pred, alpha = 0.5, beta = 0.5, gamma = 0.5, seasonal = "multiplicative")

pre_holt_11 <- predict(noodle_holt_pred_11,20,prediction.interval = TRUE)
pre_holt_m_11 <- predict(noodle_holt_pred_m_11,20,prediction.interval = TRUE)

pre_holt_35 <- predict(noodle_holt_pred_35,20,prediction.interval = TRUE)
pre_holt_m_35 <- predict(noodle_holt_pred_m_35,20,prediction.interval = TRUE)

pre_holt_55 <- predict(noodle_holt_pred_55,20,prediction.interval = TRUE)
pre_holt_m_55 <- predict(noodle_holt_pred_m_55,20,prediction.interval = TRUE)

result2 <- cbind(pre_holt[,1],pre_holt_11[,1],pre_holt_35[,1],pre_holt_55[,1])
result2 <- cbind(data1$View[242:261],result2)
c(rmse(data1$View[242:261],pre_holt),rmse(data1$View[242:261],pre_holt_11),rmse(data1$View[242:261],pre_holt_35),rmse(data1$View[242:261],pre_holt_55))
colnames(result2) <- c("actual","default", "(0.1,0.1,0.1)","(0.3,0.5,0.5)","(0.5,0.5,0.5")
result2

result3 <- cbind(pre_holt_m[,1],pre_holt_m_11[,1],pre_holt_m_35[,1],pre_holt_m_55[,1])
result3 <- cbind(data1$View[242:261],result3)
colnames(result3) <- c("actual","default", "(0.1,0.1,0.1)","(0.3,0.5,0.5)","(0.5,0.5,0.5")
result3
c(rmse(data1$View[242:261],pre_holt_m),rmse(data1$View[242:261],pre_holt_m_11),rmse(data1$View[242:261],pre_holt_m_35),rmse(data1$View[242:261],pre_holt_m_55))

