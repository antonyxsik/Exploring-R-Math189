#--------------------Setup-------------------- 

#Data setup
data = read.table("gauge.txt", header=TRUE)
head(data)
summary(data)

#--------------------Problem 1--------------------

#Scatterplot
plot(data$gain ~ data$density, ylab = "Gamma Photon Count (gain)", 
     xlab = "Density (g/cm^3)", main = 'Gain vs. Density (Untransformed Data)', pch=19, col='blue')
lm_gain_density = lm(data$gain ~ data$density)
abline(lm_gain_density, col = 'red', lwd = 3)

#Summary of linear model
summary(lm_gain_density)

#Residual plot (fails linearity)
plot(residuals(lm_gain_density) ~ data$density, ylab = "Residuals", xlab = "Density (g/cm^3)", pch=19, col='blue')
abline(h = 0, col='black', lwd = 3)

#Residual histogram (fails constant variability)
hist(residuals(lm_gain_density))

#Residual KS Test (fails residual normality at 5% significance level)
ks.test(residuals(lm_gain_density), pnorm, mean=mean(residuals(lm_gain_density)), 
        sd=sd(residuals(lm_gain_density)))

#Residual QQ plot
qqnorm(residuals(lm_gain_density))
qqline(residuals(lm_gain_density))

#--------------------Problem 2--------------------

#Log Transformation
plot(log(gain) ~ density, data, ylab = "Log [Gamma Photon Count (gain)]", 
     xlab = "Density (g/cm^3)", pch=19, main='Log(Gain) vs. Density (Transformed Data)', col='blue')
lm_log_gain_density = lm(log(gain) ~ density, data)
abline(lm_log_gain_density, col = 'red', lwd = 3)

#Summary of log transformation model
summary(lm_log_gain_density)

#Residual log plot 
plot(residuals(lm_log_gain_density) ~ data$density, ylab = "Log Residuals", 
     xlab = "Density (g/cm^3)", pch=19, col='blue')
abline(h = 0, col='black', lwd = 3)

#Residual log histogram
hist(residuals(lm_log_gain_density))

#Residual log KS Test 
ks.test(residuals(lm_log_gain_density), pnorm, 
        mean=mean(residuals(lm_log_gain_density)), sd=sd(residuals(lm_log_gain_density)))

#Residual log QQ plot
qqnorm(residuals(lm_log_gain_density))
qqline(residuals(lm_log_gain_density))

#--------------------Problem 3--------------------

#Setting variables
nsim = 1000
q3data = data
q3data$gain <- log(q3data$gain)
q4data = q3data
jitterplot = q3data
  
#Jitter simulations (with jitter factor = 50(t*6 x SE(density)) = 50(2.447 x 0.03182))
set.seed(2)
jit.mean = rep(NULL, nsim)
for(i in 1:nsim) {
  q3data$density <- abs(jitter(q3data$density, factor = 3.893177))
  lm_q3 = lm(q3data$gain ~ q3data$density)
  jit_r2 = summary(lm_q3)$r.squared
  jit.mean[i] = jit_r2
}

#Mean of 1000 simulated R^2 values
mean(jit.mean)

#A sample regression of jittered densities vs. log(gain)
jitterplot$density <- abs(jitter(jitterplot$density, factor = 3.893177))
plot(gain ~ density, jitterplot, ylab = "Log [Gamma Photon Count (gain)]", 
     xlab = "Density (g/cm^3)", main = 'Log(Gain) vs. Density (Simulated & Transformed Data)', pch=19, col='blue')
lm_jitter = lm(gain ~ density, jitterplot)
abline(lm_jitter, col = 'red', lwd = 3)

#--------------------Problem 4--------------------

#Prediction bands must be used, as confidence intervals/bands only make sense in terms of simple random samples

#Prediction bands visualization
X = q4data$density
Y = q4data$gain
plot(Y ~ X, ylab = "log(Gain)", xlab = "Density (g/cm^3)",
     main = 'Log(Gain) vs. Density with Prediction Bands', pch=19)
lm1 = lm(Y ~ X)
abline(lm1, col = 'red', lwd = 3)
newX = seq(min(X), max(X), length.out=100)
pred.bands = predict(lm1, newdata = data.frame(X=newX), interval = 'prediction')
lines(pred.bands[,2] ~ newX, lwd = 2, lty = 2, col = 'blue')
lines(pred.bands[,3] ~ newX, lwd = 2, lty = 2, col = 'blue')

#Summary of regression parameters
summary(lm1)

#Point and interval estimates of the predicted gains of specified densities
q4estimates = exp(predict(lm1, newdata = data.frame(X = c(0.508, 0.001)), interval = 'prediction'))
q4estimates

#Ranges of measured gains for specified densities
range(data$gain[data$density == 0.508])
range(data$gain[data$density == 0.001])

#--------------------Problem 5--------------------

#Prediction bands visualization (reverse)
plot(X ~ Y, xlab = "log(Gain)", ylab = "Density (g/cm^3)", 
     main = 'Density vs. Log(Gain) with Prediction Bands', pch=19)
lm2 = lm(X ~ Y)
abline(lm2, col = 'red', lwd = 3)
newY = seq(min(Y), max(Y), length.out=100)
pred.bands2 = predict(lm2, newdata = data.frame(Y=newY), interval = 'prediction')
lines(pred.bands2[,2] ~ newY, lwd = 2, lty = 2, col = 'blue')
lines(pred.bands2[,3] ~ newY, lwd = 2, lty = 2, col = 'blue')

#Summary of regression parameters
summary(lm2)

#Point and interval estimates of the predicted densities of specified gains
q5estimates = predict(lm2, newdata = data.frame(Y = c(log(38.6), log(426.7))), interval = 'prediction')
q5estimates

#--------------------Problem 6--------------------

#Omitting specified densities
q6adata = q4data
q6adata = q6adata[q6adata$density != 0.508,]
q6bdata = q4data
q6bdata = q6bdata[q6bdata$density != 0.001,]

#Prediction bands visualization (0.508 omission)
X6a = q6adata$density
Y6a = q6adata$gain
plot(X6a ~ Y6a, xlab = "log(Gain)", ylab = "Density (g/cm^3)", 
     main = 'Density vs. log(Gain) (0.508 Density Block Omitted)', pch=19)
lm6a = lm(X6a ~ Y6a)
abline(lm6a, col = 'red', lwd = 3)
new6a = seq(min(Y6a), max(Y6a), length.out=100)
pred.bands6a = predict(lm6a, newdata = data.frame(Y6a = new6a), interval = 'prediction')
lines(pred.bands6a[,2] ~ new6a, lwd = 2, lty = 2, col = 'blue')
lines(pred.bands6a[,3] ~ new6a, lwd = 2, lty = 2, col = 'blue')

#Summary of regression parameters (0.508 omission)
summary(lm6a)

#Point and interval estimates (0.508 omission)
q6aestimates = predict(lm6a, newdata = data.frame(Y6a = c(log(38.6))), interval = 'prediction')
q6aestimates

#Prediction bands visualization (0.001 omission)
X6b = q6bdata$density
Y6b = q6bdata$gain
plot(X6b ~ Y6b, xlab = "log(Gain)", ylab = "Density (g/cm^3)", 
     main = 'Density vs. log(Gain) (0.001 Density Block Omitted)', pch=19)
lm6b = lm(X6b ~ Y6b)
abline(lm6b, col = 'red', lwd = 3)
new6b = seq(min(Y6b), max(Y6b), length.out=100)
pred.bands6b = predict(lm6b, newdata = data.frame(Y6b = new6b), interval = 'prediction')
lines(pred.bands6b[,2] ~ new6b, lwd = 2, lty = 2, col = 'blue')
lines(pred.bands6b[,3] ~ new6b, lwd = 2, lty = 2, col = 'blue')

#Summary of regression parameters (0.001 omission)
summary(lm6b)

#Point and interval estimates (0.001 omission)
q6bestimates = predict(lm6b, newdata = data.frame(Y6b = c(log(426.7))), interval = 'prediction')
q6bestimates

#--------------------Advanced Analysis--------------------

#Cubic transformation
plot(log(gain) ~ density, data, pch=16, xlab = 'Density (g/cm^3)', 
     main = 'Log(Gain) vs. Density (Transformed Cubic Regression)')
cubic = lm(log(gain) ~ density + I(density^2) + I(density^3), data)
lines(predict(cubic) ~ density, data, col='red', lwd=2)

#Summary of cubic model
summary(cubic)

#Residual cubic plot
plot(residuals(cubic) ~ density, data, pch=16)
abline(h = 0, lwd = 2)

#Residual cubic histogram
hist(residuals(cubic))

#Residual cubic KS Test 
ks.test(residuals(cubic), pnorm, mean=mean(residuals(cubic)), sd=sd(residuals(cubic)))

#Residual cubic QQ plot
qqnorm(residuals(cubic))
qqline(residuals(cubic))

