#### November 23, 2019 
#### By Svitlana Midianko 
#### Keywords: genetic matching, propensity scores, balancing covariates, 
#------------------------------------------------------------------------------------------------------------------------
######
# Q2 #
######
# replicating graph from G.King (2007) paper. The main claim is that 
# that the marginal effects of the UN interventions are highly dependent on the modeling assumptions, 
# rather than on empirical data
foo <- read.csv(file.choose())
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10,52, 35, 34)]
foo <- foo[c(-19, -47), ]
which(is.na(foo) == TRUE)
attach(foo)
names(foo)

glm <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4,
              data = foo, family = binomial)

glm.new <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
                     trnsfcap + develop + exp + decade + treaty + untype4 
               + exp:untype4 + wardur:logcost, 
                   data = foo, family = binomial)

mean.wartype <- mean(foo$wartype)
mean.logcost <- mean(foo$logcost)
mean.factnum <- mean(foo$factnum)
mean.factnum2 <- mean(foo$factnum2)
mean.trnsfcap <- mean(foo$trnsfcap)
mean.develop <- mean(foo$develop)
mean.exp <- mean(foo$exp)
mean.decade <- mean(foo$decade)
mean.treaty <- mean(foo$treaty)

get_logit <- function(X, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*X)
  return(exp(logit) / (1 + exp(logit)))
}

storage.original.treat <- rep(NA, 315)
storage.original.control <- rep(NA, 315)

for (wardur in 1:315) {
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0)
  
  storage.original.treat[wardur]  <- get_logit(X.treat, coef(glm))
  storage.original.control[wardur]  <- get_logit(X.control, coef(glm))
}

original_y <- storage.original.treat - storage.original.control
#---
storage.new.treat <- rep(NA, 315)
storage.new.control <- rep(NA, 315)

for (wardur in 1:315) {
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1, 
               1*mean.exp, wardur*mean.logcost)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp,  mean.decade, mean.treaty, 0, 
                 0*mean.exp, wardur*mean.logcost)
  storage.new.treat[wardur]  <- get_logit(X.treat, coef(glm.new))
  storage.new.control[wardur]  <- get_logit(X.control, coef(glm.new))
}
new_y <- storage.new.treat - storage.new.control

plot(1:315, original_y, type = "l",lty = 2, lwd =2, ylim = c(0, 0.8), 
     main = 'Causal Effect of Multidimensional UN Peacekeeping Operations', 
     xlab = 'Duration of wars in months', ylab = 'Marginal effects of UN peacekeeping operations')
axis(side = 1, at = c(0, seq(25, 300, 25)))
axis(side = 2, at = c(seq(0, 0.9, 0.1)))
lines(1:315, new_y, lwd = 2, ylim = c(0, 0.8))
legend(0, 0.1, legend=c("Modified model (including interaction terms)", "Original model (excluding interaction terms)"),
        lty=1:2, cex=1)
#------------------------------------------------------------------------------------------------------------------------
######
# Q3 #
######
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != "None")] <- 1
#------------------------------------------------------------------------------------------------------------------------
###### 
# Q4 # 
######
# exploring treatment effect with different models. Tr defined in Q3. 
library(Matching)
NAs <- is.na(foo$pbs5l) 
foo <-cbind(foo, Tr)

prematchbalance <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, nboots=500)
#--------------------------------------------
# simple logistic regression 
attach(foo)
GLM_2y <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 
              + trnsfcap +  develop + exp + decade+ treaty + Tr , data=foo, family="binomial")
summary(GLM_2y)
# Tr           0.7130543  0.5987033   1.191  0.23365  
GLM_5y <- glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2
              + trnsfcap + treaty +  develop + exp + decade + Tr, data=foo[!NAs,], family="binomial")
summary(GLM_5y)
# Tr           0.8233143  0.6172817   1.334 0.182278 

# retrieving actual treatment effect: since this is logistic regression, 
# the coef of the treat variable is not the treatment effect, since it is transformed in the logistic function
foo.counter_factual <- foo 
foo.counter_factual$Tr <- rep(1, nrow(foo)) - foo$Tr
counter.factuals <- predict(GLM_2y, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo))

mask <- foo$Tr == 1
unit_treat_effects[mask] <- GLM_2y$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - GLM_2y$fitted.values[!mask]
mean(unit_treat_effects)
# for 2 years #[1] 0.1139208
mo_glm2 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 
                         + trnsfcap +  develop + exp + decade+ treaty, data = foo)

foo.counter_factual5 <- foo[!NAs,]
foo.counter_factual5$Tr <- 1 - foo$Tr[!NAs]
counter.factuals5 <- predict(GLM_5y, newdata=foo.counter_factual5, type="response")
unit_treat_effects5 <- rep(NA, nrow(foo[!NAs,]))

mask <- foo[!NAs,]$Tr == 1
unit_treat_effects5[mask] <- GLM_5y$fitted.values[mask] - counter.factuals5[mask]
unit_treat_effects5[!mask] <- counter.factuals5[!mask] - GLM_5y$fitted.values[!mask]
mean(unit_treat_effects5)
# for 5 years #[1] 0.1337459
mo_glm5 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 
                        + trnsfcap +  develop + exp + decade+ treaty, data = foo[mask,])

#--------------------------------------------
# PSCORE matching 
Y_2y <- foo$pbs2l
Y_5y <- foo$pbs5l
mask <- which(!is.na(Y_5y))

GLM_pscore <- glm(Tr ~ logcost + wardur + factnum + factnum2 + 
                    exp + treaty  
               , data=foo, family="binomial")
X <- GLM_pscore$fitted.values

matchout_pscore_2y <- Match( Y = Y_2y, Tr=Tr, X=X, M =1 ,BiasAdjust = T)
mb_pscore_2y<- MatchBalance(Tr ~ logcost + wardur + factnum + factnum2 + 
                         exp + treaty,
                         data=foo, match.out = matchout_pscore_2y, nboots=1000)


matchout_pscore_5y  <- Match(Y = Y_5y[mask], Tr=Tr[mask], X=X[mask], M =1,BiasAdjust = T)
mb_pscore_5y<<- MatchBalance(Tr ~  logcost + wardur + factnum + factnum2 + 
                               exp + treaty, 
                             data=foo, match.out = matchout_pscore_5y, nboots=1000)
#--------------------------------------------
#PSCORE2 
GLM_pscore2 <- glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                    trnsfcap +treaty +develop + exp + decade 
                  + wartype:wartype + logcost:logcost + wardur:wardur +
                    trnsfcap:trnsfcap +treaty:treaty +develop:develop + exp:exp + decade:decade
                  , data=foo, family="binomial")
X <- GLM_pscore2$fitted.values

matchout_pscore_2y <- Match( Y = Y_2y, Tr=Tr, X=X, M =1,BiasAdjust = T)
mb_pscore_2y<- MatchBalance(Tr ~  wartype + logcost + wardur + factnum + factnum2 + 
                              trnsfcap +treaty +develop + exp + decade +  
                               wartype:wartype + logcost:logcost + wardur:wardur +
                              trnsfcap:trnsfcap +treaty:treaty +develop:develop + exp:exp + decade:decade,
                            data=foo, match.out = matchout_pscore_2y, nboots=1000)

matchout_pscore_5y  <- Match(Y = Y_5y[mask], Tr=Tr[mask], X=X[mask], M =1,BiasAdjust = T)
mb_pscore_5y<<- MatchBalance(Tr ~  logcost + wardur + factnum + factnum2 + 
                               exp + treaty, 
                             data=foo, match.out = matchout_pscore_5y, nboots=1000)
#--------------------------------------------
#GENMACTH
# 2 years
attach(foo)
X <- cbind(geo, euro, lac, mideast, asia, 
           wartype, logcost, wardur, factnum, 
           treaty, develop, exp, decade,  wardur*wardur,
           develop*develop, exp*exp, wartype*logcost)

genout_2y <- GenMatch(X = X, Tr = Tr,  M=1,
                       pop.size=200, max.generations=10, wait.generations=25, replace = T)

matchout.gen_2y<- Match(Y = Y_2y, X = X, M =1, Tr = Tr,  Weight.matrix=genout_2y, replace = T)

mb.out_5y <- MatchBalance(Tr ~ geo+  euro+ lac+ mideast+ asia+
                            wartype+ logcost + wardur+ factnum+
                            treaty+ develop+ exp+ decade+  wardur*wardur+
                            develop*develop+exp*exp+wartype*logcost,
                          data=foo, match.out = matchout.gen_2y, nboots=1000)
#--------------------------------------------
# 5 years

genout_5y <- GenMatch(X = X[mask,], Tr = Tr[mask],  M=1,
                   pop.size=200, max.generations=10, wait.generations=25, replace = T)

matchout.gen_5y <- Match(Y = Y_5y[mask], X = X[mask,], M =1, Tr = Tr[mask],  Weight.matrix=genout_5y, replace = T)

mb.out_5y <- MatchBalance(Tr ~ geo+  euro+ lac+ mideast+ asia+
                            wartype+ logcost + wardur+ factnum+
                            treaty+ develop+ exp+ decade+  wardur*wardur+
                            develop*develop+exp*exp+wartype*logcost,
                          data=foo[mask,], match.out = matchout.gen_5y, nboots=1000)

#GENMATCH with pscores
pscores <- GLM_pscore2$fitted.values
X <- cbind(geo, euro, lac, mideast, asia, 
                wartype,  wardur, factnum, 
                treaty, develop, exp, decade,  wardur*wardur,
                develop*develop, exp*exp, develop*exp, pscores)

genout_2y <- GenMatch(X = X, Tr = Tr,  M=1,
                      pop.size=200, max.generations=10, wait.generations=25, replace = T)

matchout.gen_2y<- Match(Y = Y_2y, X = X, M =1, Tr = Tr,  Weight.matrix=genout_2y, replace = T)

mb.out_2y <- MatchBalance(Tr ~ geo+  euro+ lac+ mideast+ asia+
                            wartype+  wardur+ factnum+
                            treaty+ develop+ exp+ decade+  wardur*wardur+
                            develop*develop+exp*exp+develop*exp + pscores,
                          data=foo, match.out = matchout.gen_2y, nboots=1000)



genout_5y <- GenMatch(X = X[mask,], Tr = Tr[mask],  M=1,
                      pop.size=200, max.generations=10, wait.generations=25, replace = T)

matchout.gen_5y <- Match(Y = Y_5y[mask], X = X[mask,], M =1, Tr = Tr[mask],  Weight.matrix=genout_5y, replace = T)

mb.out_5y <- MatchBalance(Tr ~ geo+  euro+ lac+ mideast+ asia+
                            wartype+ wardur+ factnum+
                            treaty+ develop+ exp+ decade+  wardur*wardur+
                            develop*develop+exp*exp+develop*exp +pscores[mask],
                          data=foo[mask,], match.out = matchout.gen_5y, nboots=1000)
