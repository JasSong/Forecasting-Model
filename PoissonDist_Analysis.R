library(data.table)

a <- fread(file.choose(),sep = ",", header =T)

summary(a)

p <- within(a, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

set.seed(1234)
index <- sample(1:nrow(p), nrow(p)*.7, replace = F)

train.p <- p[index,]
test.p <- p[-index,]
test.p
library(dummies)

train.p1 <- dummy.data.frame(data= train.p, names = 'prog', sep = '.',drop = TRUE)
train.p1$prog.General <- NULL

test.p1 <- dummy.data.frame(data= test.p, names = 'prog', sep = '.',drop = TRUE)
test.p1$prog.General <- NULL
test.p1
names(p.dum)

p.model <- glm(formula = num_awards ~ prog.Academic + prog.Vocational + math, data = p.dum, family = 'poisson')
p.model
library(ggplot2)
ggplot(train.p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

ggplot(train.p, aes(num_awards, fill = math)) +
  geom_histogram(binwidth=.5, position="dodge")

summary(p.model)

install.packages('sandwich')
library(sandwich)
cov.model <- vcovHC(p.model, type = "HC0")
std.err <- sqrt(diag(cov.model))
r.est <- cbind(Estimate = coef(p.model), "Robust SE" = std.err, "Pr(>|z|)" = 2*pnorm(abs(coef(p.model)/std.err), lower.tail = F)
                , LL = coef(p.model)- 1.96 * std.err
                , UL = coef(p.model)+ 1.96 * std.err)
r.est

install.packages('caret')
library(caret)
prd.train <- predict(p.model, newdata = train.p1, type = 'response')
pred.train.test <- round(prd.train)
pred.train.test
confusionMatrix(pred.train.test,train.p1$num_awards, positive= '1')


prd.test <- predict(p.model, newdata = test.p1, type = 'response')
prd.test
pred.test.test <- round(prd.test)
pred.test.test
confusionMatrix(pred.test.test,test.p1$num_awards)
confusionMatrix(pred.test.test,test.p$num_awards)
test.p
test.p1
test.p1$num_awards

pred.test.test

cbind(pred.test.test,test.p1$num_awards)
table(pred.test.test,test.p1$num_awards)

diff_test <- pred.test.test- test.p1$num_awards

mean_diff_test <- mean(diff_test)
mean_diff_test
sd_diff_test <- sd(diff_test)
sd_diff_test
t_test <- mean_diff_test/(sd_diff_test/sqrt(length(diff_test)))
t_test

t.test(pred.test.test, test.p1$num_awards, alternative = c('greater'), paired = T, conf.level = 0.95)
