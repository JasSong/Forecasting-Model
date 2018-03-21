data_trend <- read.csv(file.choose(), sep = ",", header = T)
data_trend

str(data_trend)
data_trend$Date<- as.character(data_trend$Date)
sspilt<- strsplit(data_trend$Date,split = "-")
sspilt
spilt_date <- as.data.frame(strsplit(data_trend$Date,split = "-"))

spilt_data <- t(spilt_date)
spilt_data <- as.data.frame(spilt_data)

model_data_trend <- cbind(1:260,spilt_data$V2, data_trend$View)
row.names(model_data_trend) <- NULL
str(model_data_trend)
View(model_data_trend)
model_data_trend<- as.data.frame(model_data_trend)
colnames(model_data_trend) <- c("t", "month", "View")

#simple exponential smoothing
simple_model <- ts(model_data_trend$View, c(2012,11),frequency = 52)
plot(simple_model)

simple_holt <- HoltWinters(simple_model,beta = FALSE, gamma = FALSE)
simple_holt
plot(simple_holt)
summary(simple_holt)

double_model <- ts(model_data_trend$View[1:240], c(2012,11),frequency = 52)
plot(simple_model)

double_holt <- HoltWinters(double_model, gamma = FALSE)
double_holt
plot(double_holt)
summary(double_holt)

simple_pred <- HoltWinters(model_data_trend$View[1:240], beta = FALSE, gamma = FALSE)
double_pred <- HoltWinters(model_data_trend$View[1:240], gamma = FALSE)

pre_holt <- predict(simple_pred,20,prediction.interval = TRUE)
pre_holt_m <- predict(double_pred,20,prediction.interval = TRUE)
pre_holt_m

plot(simple_pred,pre_holt)
plot(double_pred,pre_holt_m)

result4 <- cbind(model_data_trend$View[241:260],pre_holt[,1],pre_holt_m[,1])
colnames(result4) <- c("actual", "simple", "double")
result4

c(rmse(model_data_trend$View[241:260],pre_holt),rmse(model_data_trend$View[241:260],pre_holt_m))
