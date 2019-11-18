## by Midianko S.M. - Class MW9AM 

#########
# Q - 1 #
#########
x <- rnorm(998)
y = -0.12 * x + 12 + rnorm(998, 0.12, 0.06)  # y =  -0.11x + 12 + e
ex_df <- data.frame(x, y)
lm1<- lm(y ~ x, ex_df)
summary(lm1)

ex_df_full <- rbind(ex_df, c(40.12, 15.6), c(40.24, 15.7))
lm2 <- lm(y ~ x, ex_df_full)
summary(lm2)

plot(ex_df_full,
     main = "Sensitivity of Regression Models to Outliers",
     xlab = "x (units)",
     ylab = "y (units)", pch = '*')
abline(a = coef(lm1)[1],
       b = coef(lm1)[2],
       col = "blue", lwd = 1.8)
abline(a = coef(lm2)[1],
       b = coef(lm2)[2],
       col = "red", lwd = 1.8)
legend("bottomright",
       c("Regression line with no outliers involved;   slope coef = -0.1161",
         "Regression line with outliers involved;        slope coef =  0.0386"),
       fill=c("blue","red"))


##########
# Q - 2a #
##########
library(Matching)
library(arm)
data(lalonde)

attach(lalonde)

lalonde.lm <- lm(re78 ~ lalonde$age + I(lalonde$age*lalonde$age) + educ + treat + I(treat*lalonde$age) + re74 + re75)
summary(lalonde.lm)

lalonde_trt <- lalonde[which(lalonde$treat == 1), ]
lalonde_cnt <- lalonde[which(lalonde$treat == 0), ]

sim_results <- sim(lalonde.lm, n.sims =10000)
exp_val_storage <- matrix(NA, nrow = 10000, ncol = length(min(lalonde$age):max(lalonde$age)))

mean_educ_trt <- mean(lalonde_trt$educ)
mean_re74_trt <- mean(lalonde_trt$re74)
mean_re75_trt <- mean(lalonde_trt$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age*age, mean_educ_trt, 1, age,
          mean_re74_trt, mean_re75_trt)
  for (i in 1:10000) {
    exp_val_storage [i, age + 1 - min(lalonde$age)] <- sum(Xs*sim_results@coef[i,]) 
  }
}

confint_stor <- apply(exp_val_storage, 2, quantile, probs = c(0.025, 0.975)) 

confint_table <- t(data.frame(confint_stor))
#colnames(confint_table) <- c("Confint Lower Bound", "Confint Upper Bound")
confint_table <- data.frame(confint_table, mean_educ_trt, mean_re74_trt, mean_re75_trt)
rownames(confint_table) <- min(lalonde$age):max(lalonde$age)
colnames(confint_table) <-  c("Confint Lower Bound", "Confint Upper Bound","mean_educ_trt", "mean_re74_trt", "mean_re75_trt")

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(min(confint_stor)-1000, max(confint_stor)+1000), 
     main = "Confidence Intervals for Expected Values of Real Earnings in 1978 by Age \n with Education, Real Earnings in 1974 and 1975 Held at the Means", xlab = "Age (years)", 
     ylab = "Real Earning in 1978 (USD)")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = confint_stor [1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = confint_stor [2, age - min(lalonde$age) + 1],
    lwd = 2)
}

##########
# Q - 2b #
##########
exp_val_storage_cnt <- matrix(NA, nrow = 10000, ncol = length(min(lalonde$age):max(lalonde$age)))

mean_educ_cnt <- mean(lalonde_cnt$educ)
mean_re74_cnt <- mean(lalonde_cnt$re74)
mean_re75_cnt <- mean(lalonde_cnt$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age*age, mean_educ_cnt, 0, age,
          mean_re74_cnt, mean_re75_cnt)
  for (i in 1:10000) {
    exp_val_storage_cnt [i, age + 1 - min(lalonde$age)] <- sum(Xs*sim_results@coef[i,]) 
  }
}

confint_stor_cnt <- apply(exp_val_storage_cnt, 2, quantile, probs = c(0.025, 0.975)) 

confint_table_cnt <- t(data.frame(confint_stor_cnt))
colnames(confint_table_cnt) <- c("Confint Lower Bound", "Confint Upper Bound")
confint_table <- data.frame(confint_table, mean_educ_cnt, mean_re74_cnt, mean_re75_cnt)
rownames(confint_table_cnt) <- min(lalonde$age):max(lalonde$age)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(min(confint_stor_cnt)-1000, max(confint_stor_cnt)+1000), 
     main = "Confidence Intervals for Expected Values of Real Earnings in 1978 by Age \n with Education, Real Earnings in 1974 and 1975 Held at the Means", xlab = "Age (years)", 
     ylab = "Real Earning in 1978 (USD)")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = confint_stor_cnt [1, age - min(lalonde_cnt$age) + 1],
    x1 = age,
    y1 = confint_stor_cnt [2, age - min(lalonde_cnt$age) + 1],
    lwd = 2)
}
##########
# Q - 2c #
##########
# treatment effect = Y(treatment = 1) - Y(treatment = 0)

#Y(1) = intercept + age * coef_age + age^2 * coed(age^2) + educ * coef(educ) + 1* coef(treat) + 1*age*coef(treat-age) 
#+ re74 * coef(re74) + re75 *coef(re75)
#Y(0) = intercept + age * coef_age + age^2 * coed(age^2) + educ * coef(educ) +
#+ re74 * coef(re74) + re75 *coef(re75)

#Y(1) = 1* coef(treat) + 1*age*coef(treat-age) ++ 
#Y(1)- Y(0) = 1*coef(treat) + 1*age*coef(treat-age) 


treat_effect_stor <- matrix(NA, nrow = 10000, ncol = length(min(lalonde$age):max(lalonde$age)))

for (age in min(lalonde$age):max(lalonde$age)) {
  for (i in 1:10000) {
    treat_effect_stor [i, age + 1 - min(lalonde$age)] <- 1*sim_results@coef[i,5]+1*age*sim_results@coef[i,6]
  }
}

confint_stor_treat_eff <- apply(treat_effect_stor, 2, quantile, probs = c(0.025, 0.975)) 

confint_table_treat_eff <- t(data.frame(confint_stor_treat_eff ))
colnames(confint_table_treat_eff) <- c("ConfInt Lower Bound", "ConfInt Upper Bound")
confint_table_treat_eff <- data.frame(confint_table_treat_eff , mean_educ_cnt, mean_re74_cnt, mean_re75_cnt)
rownames(confint_table_treat_eff) <- min(lalonde$age):max(lalonde$age)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(min(confint_stor_treat_eff)-1000, max(confint_stor_treat_eff)+1000), 
     main = "Confidence Intervals for Treatment Effect on Real Earnings in 1978 by Age \n with Education, Real Earnings in 1974 and 1975 Held at the Means", xlab = "Age (years)", 
     ylab = "Real Earning in 1978 (USD)")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = confint_stor_treat_eff [1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = confint_stor_treat_eff [2, age - min(lalonde$age) + 1],
    lwd = 2)
}
##########
# Q - 2d #
##########
#Y(1) = intercept + age * coef_age + age^2 * coed(age^2) + educ * coef(educ) + 1 * coef(treat) + 1*age*coef(treat-age) 
#+ re74 * coef(re74) + re75 *coef(re75) + error
#Y(0) = intercept + age * coef_age + age^2 * coef(age^2) + educ * coef(educ) +
# + re74 * coef(re74) + re75 *coef(re75) + error
#Y(1)- Y(0) = 1*coef(treat) + 1*age*coef(treat-age) + error term (treatment = 1) - error term (treatment = 0)

treat_effect_stor_pred <- matrix(NA, nrow = 10000, ncol = length(min(lalonde$age):max(lalonde$age)))

error_diff <- rep(0, 10000)

for (age in min(lalonde$age):max(lalonde$age)) {
  for (i in 1:10000) {
    error_diff[i] <-  rnorm(1, 0, sim_results@sigma[i])- rnorm(1, 0, sim_results@sigma[i])
    treat_effect_stor_pred[i, age + 1 - min(lalonde$age)] <- 1*sim_results@coef[i,5]+1*age*sim_results@coef[i,6] + error_diff[i]
  }
}

confint_stor_pred_eff<- apply(treat_effect_stor_pred, 2, quantile, probs = c(0.025, 0.975)) 
confint_table_pred_eff <- t(data.frame(confint_stor_pred_eff))
colnames(confint_table_pred_eff) <- c("ConfInt Lower Bound", "ConfInt Upper Bound")
confint_table_pred_eff <- data.frame(confint_table_pred_eff, med_educ, med_re74, med_re75)
rownames(confint_table_pred_eff) <- min(lalonde$age):max(lalonde$age)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(min(confint_stor_pred_eff)-1000, max(confint_stor_pred_eff)+1000), 
     main = "Prediction Intervals for Treatment Effect on Real Earnings in 1978 by Age \n with Education, Real Earnings in 1974 and 1975 Held at the Medians", xlab = "Age (years)", 
     ylab = "Real Earning in 1978 (USD)")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = confint_stor_pred_eff [1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = confint_stor_pred_eff[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

##########
# Q - 3 #
##########
foo <- read.csv(url("https://tinyurl.com/y2prc9xq"))
head(foo)
storage <- rep(NA, 10000)
for (i in 1:10000) {
  lm_math <- lm(MATH_SCORE ~ TREATMENT, data = foo[sample(1:nrow(foo), nrow(foo), replace = T),])
  storage[i] <- lm_math$coefficients[2]
}
sim_con <- quantile(storage, c(0.025, 0.975))

lm_math2 <- lm(foo$MATH_SCORE ~ foo$TREATMENT)
r_conf <- confint(lm_math2)[2,]
df <- cbind(sim_con, r_conf)
df <- t(df)
colnames(df) <- c('2.5%', '97.5%')
rownames(df) <- c('Boostrapped ConfInt', 'Analytical ConfInt')
hist(storage, main = 'Distribution of the Bootstrapped Coefficient Values', xlab = 'Value of the Coefficient', breaks = 18, col = 'lightblue')

##########
# Q - 4 #
##########
r_sq.fn = function(Ys, pred_Ys){
  storage <- data.frame(NA, nrow=10000, ncol=1)
  for (i in 1:10000){ 
    indeces <- sample(length(Ys),length(Ys),replace=TRUE)
    RSS <- sum((Ys[indeces] - pred_Ys[indeces])**2)
    TSS <- sum((Ys[indeces] - mean(Ys[indeces]))**2)
    storage[i] <- 1- RSS/TSS
  }    
  return (mean(storage[,1]))
}
Ys <- foo$MATH_SCORE
Ys <-  Ys[!is.na(Ys)]
predYs <- predict(lm_math2)
r_sq.fn(Ys, predYs )

##########
# Q - 5 #
##########
library(boot)
foo2 <- read.csv(url("https://tinyurl.com/yx8tqf3k"))
head(foo2)
set.seed(12345)
test_set_rows <- sample(1:length(foo2$age), 2000, replace = FALSE) 

test_set <- foo2[test_set_rows,]
training_set <- foo2[-test_set_rows,]

model1 <- glm(treat ~ married ,  data = training_set, family ='binomial')
cv1 <- cv.glm(training_set, model1)$delta[1]

model2 <- glm(treat~black+hispanic, data = training_set, family ='binomial')
cv2 <- cv.glm(training_set, model2)$delta[1]

model3 <- glm(treat~black + education, data = training_set, family ='binomial')
cv3 <- cv.glm(training_set, model3)$delta[1]

model4 <- glm(treat~ education, data = training_set, family ='binomial')
cv4 <- cv.glm(training_set, model4)$delta[1]

model5<- glm(treat ~married + education, data = training_set, family ='binomial')
cv5 <- cv.glm(training_set, model5)$delta[1]


mse1_test <- mean((test_set$treat - predict(model1, newdata = test_set, type = 'response'))**2)
mse2_test <-  mean((test_set$treat - predict(model2, newdata = test_set, type = 'response'))**2)
mse3_test <-  mean((test_set$treat - predict(model3, newdata = test_set, type = 'response'))**2)
mse4_test <-  mean((test_set$treat - predict(model4, newdata = test_set, type = 'response'))**2)
mse5_test <- mean((test_set$treat - predict(model5, newdata = test_set, type = 'response'))**2)

models <- c('married','black + hispanic', ' black + education', 'education', 'married + education')
cvs <- c(0.0212744, 0.01870126, 0.0186262, 0.0218248,  0.02114747)
mse <-c(0.02278279, 0.01900264 , 0.01939359, 0.02341534 , 0.02263868)
table <- data.frame(models,cvs, mse)
table <- t(table)
rownames(table)<- c('Predictors', 'CV for training set', 'MSE for test set')
table <- t(table)