#Forward regression
regfit.forward <- regsubsets(kWh ~ .-Date - Account - year -Wind_Direction -Conditions -WindDirDegrees , 
                             data = fulldata_test, nvmax = 14, method = "forward")
regfit.summary <- summary(regfit.forward)
names(regfit.summary)
regfit.summary$adjr2
regfit.summary$rss
coef(regfit.forward, 7)

x = 1:14
y = regfit.summary$adjr2
plot(x,y, xlab = "Number of variables", ylab = "Adjusted R square")
plot(regfit.forward, scale = "adjr2", main = "Adjusted R^2")

#Backward regression
regfit.backward <- regsubsets(kWh ~ .-Date - Account - year -Wind_Direction -Conditions -WindDirDegrees , 
                             data = fulldata_test, nvmax = 14, method = "backward")
regfit.summary <- summary(regfit.backward)
names(regfit.summary)
regfit.summary$adjr2
regfit.summary$rss
coef(regfit.forward, 7)

x = 1:14
y = regfit.summary$adjr2
plot(x,y, xlab = "Number of variables", ylab = "Adjusted R square")
plot(regfit.backward, scale = "adjr2", main = "AdjustedR^2")

#Stepwise regression
library(MASS)
fit <- lm(kWh~.-Date - Account - year -Wind_Direction -Conditions -WindDirDegrees,data=fulldata_test)
step <- stepAIC(fit, direction="both")
step$anova # display results
summary(fit)

#Model
smp_size <- floor(0.80 * nrow(fulldata_test))
set.seed(123)
train_ind <- sample(seq_len(nrow(fulldata_test)), size = smp_size)
train <- fulldata_test[train_ind,]
test <- fulldata_test[-train_ind,]
model1<- lm(kWh ~ month+dayOfWeek+Weekday+TemperatureF+Dew_PointF+Humidity+Peakhour , data = train)
summary(model1)
plot(model1)
ppp <- predict(model1, test)
accuracy(ppp, train$kWh)
compare <- cbind (actual=test$kWh, ppp)
mean (apply(compare, 1, min)/apply(compare, 1, max))
#Regression 
library(devtools)
install_github("dgrtwo/broom")
library(broom)
tidy_lmfit <- tidy(summary(model1))
tidy_lmfit <- tidy_lmfit[-c(3:5)]
tidy_lmfit <- rbind(c("Account",fulldata_test$Account[1]), tidy_lmfit)
tidy_lmfit[2,1] = "constant"
write.table(tidy_lmfit, file = "RegressionOutputs.csv", sep = "," , row.names = FALSE, col.names = FALSE)

#Performance Metrics
tidy_p <- tidy(accuracy(ppp, train$kWh))
tidy_p <- tidy_p[-c(1:2,5)]
tidy_p <- tidyr::gather(tidy_p)
tidy_p <- rbind(c("Account",fulldata_test$Account[1]), tidy_p)
write.table(tidy_p, file = "PerformanceMetrics.csv", sep = "," , row.names = FALSE, col.names = FALSE)

#Ridge
 
ridge_reg <- lm.ridge(kWh ~ TemperatureF+Dew_PointF+ Humidity+month+dayOfWeek+Weekday+Peakhour,train )
plot(lm.ridge(kWh ~ TemperatureF+Dew_PointF+ Humidity+month+dayOfWeek+Weekday+Peakhour ,fulldata_test,lambda = seq(0,0.1,0.001)))
write.table(ridge_re, file = "RegressionOutputs1.csv", sep = "," , row.names = FALSE, col.names = FALSE)


