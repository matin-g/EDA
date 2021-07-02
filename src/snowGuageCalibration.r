#########################################################
#--------------- MATH 189: Case study 4 ----------------#
# Names: Matin Ghaffari, James Lu                       #
# PID  : A16617005, A16687580                           #
#########################################################

#read data in
data <- read.csv("gauge.txt")
den <- data$density
gain <- data$gain
den_log_gain = data.frame(den, log(gain))

####################################################
#  q1 --> Raw data
####################################################

#plot points
plot(gain ~ den, xlab="Density (g/cm^3)", ylab="Gain", 
     main="Density vs. Gain With Regression Line")
#linear model
l = lm(gain ~ den)
abline(l, col=rgb(1,0,0,.75), lwd=2)
legend("topright", legend=c("Regression Line"),
       col=c(rgb(1,0,0,.75)), lwd=2)

#plot residuals
plot(residuals(l) / sd(residuals(l)) ~ den, 
     xlab="Density (g/cm^3)", ylab="Standardized Residuals",
     main="Residuals of Density vs. Gain")
abline(h=0, lwd=2)

####################################################
#  q2  -->  Transformed data
####################################################

#plot data
plot(log(gain) ~ den, xlab="Density (g/cm^3)", ylab="log(Gain)",
     main="Density vs. log(Gain) With Regression Line")
#linear model
l = lm(log(gain) ~ den)
abline(l, col=rgb(1,0,0,.75), lwd=2)
legend("topright", legend=c("Regression Line"),
       col=c(rgb(1,0,0,.75)), lwd=2)

#plot residuals
plot(residuals(l) / sd(residuals(l)) ~ den, xlab="Density (g/cm^3)", 
     ylab="Standardized Residuals",
     main="Residuals of Density vs. log(Gain)")
abline(h=0, lwd=2)

#plot qqnorm
qqnorm(residuals(l) / sd(residuals(l)), xlim=c(-3,3), ylim=c(-3,3))
abline(c(0,1), col = rgb(1,0,0,.75))
legend("topleft", legend=c("Standard Normal Line"),
       col=c(rgb(1,0,0,.75)), lwd=2)

####################################################
#  q3  -->  Robustness
####################################################

ten_percent_error = data$density
fifteen_percent_error = data$density
twenty_percent_error = data$density

#populate new densities
for (i in 1:length(data$density)) {
  ten_err = runif(1, -ten_percent_error[i] * .1, 
                  ten_percent_error[i] * .1)
  fif_err = runif(1, -fifteen_percent_error[i] * .15, 
                  fifteen_percent_error[i] * .15)
  tw_err = runif(1, -twenty_percent_error[i] * .2, 
                 twenty_percent_error[i] * .2)
  
  ten_percent_error[i] = ten_percent_error[i] + ten_err
  fifteen_percent_error[i] = fifteen_percent_error[i] + fif_err
  twenty_percent_error[i] = twenty_percent_error[i] + tw_err
}

#original r ** 2
original_rsq = summary(lm(l))$r.squared

#plot all new densities against gain with regression line
par(mar=c(5,5,5,5))
plot(log(gain) ~ ten_percent_error, xlab="Density (g/cm^3)", ylab="log(Gain)",
     main="10% Error Density vs. log(Gain) With Regression Line",
     xlim=c(0,.8))
abline(l, col=rgb(1,0,0,.75), lwd=2)
legend("topright", legend=c("Regression Line"),
       col=c(rgb(1,0,0,.75)), lwd=2)
ten_per_rsq = summary(lm(log(gain) ~ den, data=data.frame(den=ten_percent_error, 
                                                          gain)))$r.squared
# plot(residuals(l) / sd(residuals(l)) ~ ten_percent_error, xlab="Density", 
#      ylab="Standardized Residuals",
#      main="Residuals of 10% Error Density vs. log(Gain)")
# abline(h=0, lwd=2)
plot(log(gain) ~ fifteen_percent_error, xlab="Density (g/cm^3)", ylab="log(Gain)",
     main="15% Error Density vs. log(Gain) With Regression Line",
     xlim=c(0,.8))
abline(l, col=rgb(1,0,0,.75), lwd=2)
legend("topright", legend=c("Regression Line"),
       col=c(rgb(1,0,0,.75)), lwd=2)
fif_per_rsq = summary(lm(l, data=data.frame(den=fifteen_percent_error, 
                                            gain)))$r.squared
# plot(residuals(l) / sd(residuals(l)) ~ fifteen_percent_error, xlab="Density", 
#      ylab="Standardized Residuals",
#      main="Residuals of 15% Error Density vs. log(Gain)")
# abline(h=0, lwd=2)
plot(log(gain) ~ twenty_percent_error, xlab="Density (g/cm^3)", ylab="log(Gain)",
     main="20% Error Density vs. log(Gain) With Regression Line",
     xlim=c(0,.8))
abline(l, col=rgb(1,0,0,.75), lwd=2)
legend("topright", legend=c("Regression Line"),
       col=c(rgb(1,0,0,.75)), lwd=2)
twenty_per_rsq = summary(lm(l, data=data.frame(den=twenty_percent_error, 
                                               gain)))$r.squared
# plot(residuals(l) / sd(residuals(l)) ~ twenty_percent_error, xlab="Density", 
#      ylab="Standardized Residuals",
#      main="Residuals of 20% Error Density vs. log(Gain)")
# abline(h=0, lwd=2)

####################################################
#  q4  -->  Forward Prediction
####################################################

plot(log(gain) ~ den, xlab="Density (g/cm^3)", ylab="log(Gain)",
     main="Density vs. log(Gain) With Regression Line",)
abline(l, col=rgb(1,0,0,.75), lwd=2)
new_den = seq(min(den), max(den), length.out=length(den))

#prediction / confidence lines
prediction_lines = predict(l, newdata=data.frame(den=new_den), 
                           interval="prediction")
confidence_lines = predict(l, newdata=data.frame(den=new_den), 
                           interval="confidence")
#lower bound
lines(prediction_lines[,2] ~ new_den, lwd=1, lty=2, col=rgb(0,0,1,1))
lines(confidence_lines[,2] ~ new_den, lwd=1, lty=2, col=rgb(0,1,0,1))
#upper bound
lines(prediction_lines[,3] ~ new_den, lwd=1, lty=2, col=rgb(0,0,1,1))
lines(confidence_lines[,3] ~ new_den, lwd=1, lty=2, col=rgb(0,1,0,1))
legend("topright", legend=c("Regression Line", "95% Prediction Bands", 
                            "95% Confidence Bands"),
       col=c(rgb(1,0,0,1), rgb(0,0,1,1), rgb(0,1,0,1)), lty=c(1,2,2), 
       lwd=c(2,1,1))

lower_coeff = lm(prediction_lines[,2] ~ new_den)$coefficients
upper_coeff = lm(prediction_lines[,3] ~ new_den)$coefficients

#predicted gains
first_pred_pt = l$coefficients[1] + (l$coefficients[2] * .508)
first_pred_range = c(lower_coeff[1] + (lower_coeff[2] * .508), 
                     upper_coeff[1] + (upper_coeff[2] * .508))

second_pred_pt = l$coefficients[1] + (l$coefficients[2] * .001)
second_pred_range = c(lower_coeff[1] + (lower_coeff[2] * .001), 
                      upper_coeff[1] + (upper_coeff[2] * .001))

#observed gains
first_observed_gain_pt = median(den_log_gain[den_log_gain$den == .508, 2])
first_observed_gain_range = range(den_log_gain[den_log_gain$den == .508, 2])
second_observed_gain_pt = median(den_log_gain[den_log_gain$den == .001, 2])
second_observed_gain_range = range(den_log_gain[den_log_gain$den == .001, 2])

#mean squared errors
mse_001 = sum(l$residuals[81:90] ** 2) / 10
mse_508 = sum(l$residuals[21:30] ** 2) / 10

####################################################
#  q5  -->  Reverse Prediction
####################################################

l = lm(den~log(gain))

plot(den ~ log(gain), xlab="log(Gain)", ylab="Density (g/cm3)",
     main="log(Gain) vs Density With Regression Line",)
abline(l, col=rgb(1,0,0,.75), lwd=2)
new_gain = seq(min(log(gain)), max(log(gain)), length.out=length(log(gain)))
prediction_lines = predict(l, newdata=data.frame(den=new_gain), 
                           interval="prediction")
confidence_lines = predict(l, newdata=data.frame(den=new_gain), 
                           interval="confidence")
#lower bound for prediction
lines(prediction_lines[,2] ~ log(gain), lwd=1, lty=2, col=rgb(0,0,1,.75))
#upper bound for prediction
lines(prediction_lines[,3] ~ log(gain), lwd=1, lty=2, col=rgb(0,0,1,.75))

#lower bound for confidence
lines(confidence_lines[,2] ~ log(gain), lwd=1, lty=2, col=rgb(0,1,0,.75))
#upper bound for confidence
lines(confidence_lines[,3] ~ log(gain), lwd=1, lty=2, col=rgb(0,1,0,.75))
legend("topright", legend=c("Regression Line", "95% Prediction Bands", "95% Confidence Bands"),
       col=c(rgb(1,0,0,.75), rgb(0,0,1,.75), rgb(0,1,0,.75)), lty=c(1,2,2), lwd=c(2, 1, 1))

l = lm(den~log(gain))
p38 = data.frame(gain=38.6) 
predict(l, p38, interval="confidence")
predict(l, p38, interval="prediction")
p426 = data.frame(gain=426.7) 
predict(l, p426, interval="confidence")
predict(l, p426, interval="prediction")

#mean squared errors
mse2_38 = sum(l$residuals[81:90] ** 2) / 10
mse2_426 = sum(l$residuals[21:30] ** 2) / 10

##############################################
# q6  --> Cross validation
##############################################

library(dplyr)
aggregByMean = data %>% group_by(density) %>% summarise(gain_mu = mean(gain), log_gain_mu = mean(log(gain)))
means = as.data.frame(aggregByMean)

train = means[means$density != 0.508, ]
trainDF = as.data.frame(train)
val = means[means$density == 0.508, ]
gain_mu = train$gain_mu
l = lm(density ~ log(gain_mu), data = train)

Pi1 = predict(l, data.frame(gain_mu = 38.6), interval = "predict")
Ci1 = predict(l, data.frame(gain_mu = 38.6), interval = "confidence")
Pi2 = predict(l, data.frame(gain_mu = 426.7), interval = "predict")
Ci2 = predict(l, data.frame(gain_mu = 426.7), interval = "confidence")

Pi1
Ci1
Pi2
Ci2

plot(train[,3], train[,1] , xlab="log(Gain)", ylab="Density (g/cm^3)",
     main="Regression Line of log(Gain) vs Density with 0.508 Density Block Omitted",)
abline(l, col=rgb(1,0,0,.75), lwd=2)

#new_gain = seq(min(log(gain)), max(log(gain)), length.out=length(log(gain)))
prediction_lines = predict(l, newdata=data.frame(den=gain_mu), 
                   interval="prediction")
confidence_lines = predict(l, newdata=data.frame(den=gain_mu), 
                   interval="confidence")
#lower bound for prediction
lines(prediction_lines[,2] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,0,1,.75))
#upper bound for prediction
lines(prediction_lines[,3] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,0,1,.75))

#lower bound for confidence
lines(confidence_lines[,2] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,1,0,.75))
#upper bound for confidence
lines(confidence_lines[,3] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,1,0,.75))
legend("topright", legend=c("Regression Line", "95% Prediction Bands", "95% Confidence Bands"),
       col=c(rgb(1,0,0,.75), rgb(0,0,1,.75), rgb(0,1,0,.75)), lty=c(1,2,2), lwd=c(2, 1, 1))

aggregByMean = data %>% group_by(density) %>% summarise(gain_mu = mean(gain), log_gain_mu = mean(log(gain)))
means = as.data.frame(aggregByMean)

train = means[means$density != 0.001, ]
val = means[means$density == 0.001, ]
l = lm(density ~ log(gain_mu), data = train)

prediction = predict(l, data.frame(gain_mu = val[2]))[[1]]
prediction

Pi1 = predict(l, data.frame(gain_mu = 38.6), interval = "predict")
Ci1 = predict(l, data.frame(gain_mu = 38.6), interval = "confidence")
Pi2 = predict(l, data.frame(gain_mu = 426.7), interval = "predict")
Ci2 = predict(l, data.frame(gain_mu = 426.7), interval = "confidence")

Pi1
Ci1
Pi2
Ci2

plot(train[,3], train[,1] , xlab="log(Gain)", ylab="Density (g/cm^3)",
     main="Regression Line of log(Gain) vs Density with 0.001 Density Block Omitted",)
abline(l, col=rgb(1,0,0,.75), lwd=2)

gain_mu = train$gain_mu

#new_gain = seq(min(log(gain)), max(log(gain)), length.out=length(log(gain)))
prediction_lines = predict(l, newdata=data.frame(den=gain_mu), 
                           interval="prediction")
confidence_lines = predict(l, newdata=data.frame(den=gain_mu), 
                           interval="confidence")
#lower bound for prediction
lines(prediction_lines[,2] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,0,1,.75))
#upper bound for prediction
lines(prediction_lines[,3] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,0,1,.75))

#lower bound for confidence
lines(confidence_lines[,2] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,1,0,.75))
#upper bound for confidence
lines(confidence_lines[,3] ~ log(gain_mu), lwd=1, lty=2, col=rgb(0,1,0,.75))
legend("topright", legend=c("Regression Line", "95% Prediction Bands", "95% Confidence Bands"),
       col=c(rgb(1,0,0,.75), rgb(0,0,1,.75), rgb(0,1,0,.75)), lty=c(1,2,2), lwd=c(2, 1, 1))

#################################################
# ADVANCED ANALYSIS -> polynomial regression
#################################################

library(readxl)
head(data)

Density=data$'density'
Gain=data$gain
plot(Gain, Density, pch=16)

fit.d1 <- lm(Density ~ Gain)
fit.d2 <- lm(Density ~ poly(Gain, 2, raw=TRUE))
fit.d3 <- lm(Density ~ poly(Gain, 3, raw=TRUE))
fit.d4 <- lm(Density ~ poly(Gain, 4, raw=TRUE))
fit.d5 <- lm(Density ~ poly(Gain, 5, raw=TRUE))
fit.d6 <- lm(Density ~ poly(Gain, 6, raw=TRUE))
pts <- seq(0, 600, length.out=100)
val.d1 <- predict(fit.d1, data.frame(Gain=pts))
val.d2 <- predict(fit.d2, data.frame(Gain=pts))
val.d3 <- predict(fit.d3, data.frame(Gain=pts))
val.d4 <- predict(fit.d4, data.frame(Gain=pts))
val.d5 <- predict(fit.d5, data.frame(Gain=pts))
val.d6 <- predict(fit.d6, data.frame(Gain=pts))
#val.d6 <- predict(fit.d6, data.frame(Gain=pts))
plot(Gain, Density, pch=16, main = "Degree 1,2,3,4,5 & 6 Polynomial Regressions of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, val.d1, col="blue", lwd=2)
lines(pts, val.d2, col="red", lwd=2)
lines(pts, val.d3, col="green", lwd=2)
lines(pts, val.d4, col="orange", lwd=2)
lines(pts, val.d5, col="purple", lwd=2)
lines(pts, val.d6, col="pink", lwd=2)
legend("topright", legend=c("degree 1", "degree 2", "degree 3", "degree 4", "degree 5","degree 6"),
       col=c("blue", "red", "green", "orange", "purple", "pink"), lty=c(1,1,1), lwd=c(2, 1, 1))

CI.conf <- predict(fit.d1, data.frame(Gain=pts), interval = "confidence") #confidence interval
CI.pred <- predict(fit.d1, data.frame(Gain=pts), interval = "predict") #prediction interval
plot(Gain, Density, pch=16, col = "black", main = "Degree 1 Polynomial Regression of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)
legend("topright", legend=c("degree 1", "95% confidence interval", "95% prediction interval"),
       col=c("black", "blue", "red"), lty=c(1,2,2), lwd=c(2, 1, 1))

CI.conf <- predict(fit.d2, data.frame(Gain=pts), interval = "confidence") #confidence interval
CI.pred <- predict(fit.d2, data.frame(Gain=pts), interval = "predict") #prediction interval
plot(Gain, Density, pch=16, col = "black", main = "Degree 2 Polynomial Regression of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)
legend("topright", legend=c("degree 2", "95% confidence interval", "95% prediction interval"),
       col=c("black", "blue", "red"), lty=c(1,1,1), lwd=c(2, 1, 1))

CI.conf <- predict(fit.d3, data.frame(Gain=pts), interval = "confidence") #confidence interval
CI.pred <- predict(fit.d3, data.frame(Gain=pts), interval = "predict") #prediction interval
plot(Gain, Density, pch=16, col = "black", main = "Degree 3 Polynomial Regression of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)
legend("topright", legend=c("degree 3", "95% confidence interval", "95% prediction interval"),
       col=c("black", "blue", "red"), lty=c(1,1,1), lwd=c(2, 1, 1))

CI.conf <- predict(fit.d4, data.frame(Gain=pts), interval = "confidence") #confidence interval
CI.pred <- predict(fit.d4, data.frame(Gain=pts), interval = "predict") #prediction interval
plot(Gain, Density, pch=16, col = "black", main = "Degree 4 Polynomial Regression of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)
legend("topright", legend=c("degree 4", "95% confidence interval", "95% prediction interval"),
       col=c("black", "blue", "red"), lty=c(1,1,1), lwd=c(2, 1, 1))

CI.conf <- predict(fit.d5, data.frame(Gain=pts), interval = "confidence") #confidence interval
CI.pred <- predict(fit.d5, data.frame(Gain=pts), interval = "predict") #prediction interval
plot(Gain, Density, pch=16, col = "black", main = "Degree 5 Polynomial Regression of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)
legend("topright", legend=c("degree 5", "95% confidence interval", "95% prediction interval"),
       col=c("black", "blue", "red"), lty=c(1,1,1), lwd=c(2, 1, 1))

CI.conf <- predict(fit.d6, data.frame(Gain=pts), interval = "confidence") #confidence interval
CI.pred <- predict(fit.d6, data.frame(Gain=pts), interval = "predict") #prediction interval
plot(Gain, Density, pch=16, col = "black", main = "Degree 6 Polynomial Regression of Gain vs. Density", ylab="Density (g/cm^3)")
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)
legend("topright", legend=c("degree 6", "95% confidence interval", "95% prediction interval"),
       col=c("black", "blue", "red"), lty=c(1,1,1), lwd=c(2, 1, 1))


